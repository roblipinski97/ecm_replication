
#
# SET-UP --------------------------------------------------------------
# 


### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make a copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


### For ECM eligible only  --------------------------------------------------------
patients =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) # read appropriate level of data
patients_id = patients$id # extract relevant ID codes to subset raw billing data



#
# DATA CLEANING  ---------------------------------------------------------
# 

### For each of the 3 data types (prescriptions is cleaned separately in scrip 04)

for(folder in c('Billing', 'Procedures', 'Diagnoses')){

  # Control loop 
  start1 = Sys.time()

  ### Skip any datasets (if already done) ----------------
  # if(folder %in% c('Billing', 'Procedures')){next} 
  
  ### List all files in the relevant folder ---------------------------------------------------------------------------------------------
  files_list = list.files(file.path(project_path, 'Data', 'Raw', folder), full.names = T, pattern = '.csv')
  
  ### Create empty list to store the data of a given type before merging
  temp_list <- vector("list", length(files_list))
  
  ### Read and add all the files together in a loop ---------------------------------------------------------------------------------------------
  for (i in seq_along(files_list)) { 
    
    ### Define file
    file <- files_list[i]
    
    ###  Control loop
    print(paste0(folder, ' - ', gsub('.*/', '', file)))

    ### Read file (.parquet if exists, if not read .csv + create .parquet) ---------------------------------------------------------------------------------------------
    if(file.exists(gsub('.csv', '.parquet', file))){
      temp = read_parquet(gsub('.csv', '.parquet', file))
    }else{
      temp = fread(file) %>% funique()
      write_parquet(temp, gsub('.csv', '.parquet', file))
    }

    ### Cleaning ---------------------------------------------------------------------------------------------
    temp = temp %>% 
      rename_all(tolower) %>% # ... names to lowercase
      dplyr::select(-any_of('v1')) %>% # ...remove spare columns (if they exist)
      mutate(billnr = as.character(billnr)) # ..treat bill id as character
    
  
  ### Leave only patients and dates of interest (billing).... ---------------------------------------------------------------------------------------------
    if(folder == 'Billing'){
      
      temp = temp %>% 
        mutate(id = as.character(patientidencrypted)) %>% # ID as character
        filter(id %in% patients_id) %>%  # ID in 'patients', i.e. only leave those ECM eligible
        filter(startoftreatment >= start_date1) %>% # Treatment after pre-specified start date
        filter(startoftreatment <= end_date1) %>% # Treatment before pre-specified start date
        mutate(dataset = file %>%   # Determine which dataset that is
                 gsub('.*/|care|_', '', .) %>% 
                 gsub("\\d.*", "", ., perl = TRUE)) %>% 
        dplyr::select(c(billnr, dataset, codeofhealthcareprovider, doctorid,typeoftreatment, id, gender, dateofbirth, residencecodesettlementlevel, 
                        startoftreatment, endoftreatment,
                        codeofadmissiontypeoradmitt, codeofdischargetype, dateofdeath))

    
    }else{
      ### ....or relevant billing codes (diagnoses, procedures) ---------------------------------------------------------------------------------------------
      #### (re-)read billing data to use in subsetting
      if(!exists('billing')){billing = read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing', 'Billing_all.parquet'))}
        print(pr(unique(billing$billnr) %in% unique(temp$billnr)))
        temp = inner_join(temp, billing, by = 'billnr') 
      }
  
    ### Write clean file version ---------------------------------------------------------------------------------------------
    write_parquet(temp, gsub('.csv', '.parquet', gsub('Raw', 'Clean', file)))
    
    #### Combine to the total
    temp_list[[i]] <- temp
    
    #### Clean the environment to free up some memory
    rm(temp)
    gc()
    
    #### Control loop
    end1 = Sys.time()
    print(end1-start1)
   }

    ### Combine all data.tables ---------------------------------------------------
    print('Combining')
    dta <- rbindlist(temp_list) %>% distinct
    print('All combined')
    
    ### Final filtering ---------------------------------------------------------------------------------------------
    dta = dta %>% 
      filter(startoftreatment >= start_date1) %>% filter(startoftreatment <= end_date1) %>%  # Ensure dates are in the pre-specified range
      filter(!is.na(billnr)) %>% filter(!is.na(id)) %>%  # Ensure there are no missing bill or patient ID's
      distinct() # Ensure observations are unique
    
    ### Save  ---------------------------------------------------
    write_parquet(dta, file.path(project_path, 'Data', 'Clean', folder, paste0(tolower(folder), '_all.parquet')))

  
  ### DEMOGRAPHICS - extract patient level-demographics from billing dataset... ---------------------------------------------------------------------------------------------
  if(folder == 'Billing'){
    
    dta = read_parquet(file.path(project_path, 'Data', 'Clean', 'Billing', paste0('Billing', '_all.parquet')))
    
    patient_demo = dta %>% ungroup() %>% 
                    group_by(id, gender, dateofbirth) %>% # ...select relevant variables
                    mutate(N = n()) %>% ungroup() %>%  # ... county how often its combination occurs (will be needed to adjudicate between clerical entry errors)
                    dplyr::select(c(id, gender, dateofbirth, dateofdeath, residencecodesettlementlevel, N)) %>%  # ... leave only relevant variables
                    distinct() %>%  #... in their unique combination
                    mutate(dateofbirth = ifelse(dateofbirth == 0, NA, dateofbirth), # ... replace as 0's and '' as NA's
                           gender = ifelse(gender == '', NA, gender))%>% 
      
                    group_by(id) %>% mutate(N2 = n()) %>%  ungroup() %>%  #... for each patient, count how many times their ID occurs
                    filter(!(N2>1 & (is.na(gender) | is.na(dateofbirth)))) %>%  # ... for those with more than one occurrence, remove the entries with missing values
                    group_by(id) %>% mutate(N2 = n()) %>%  #... re-calculate N
                    filter(N2 == 1 | (N2 > 1 & N == max(N))) %>%  # ... for the remaining non-unique occurrences, select the combination of variables occurring most frequently
                    slice(1) %>% # ... for all the remaining non-unique occurrences (fewer than 5 patients), randomly select the first of the demographic variables combinations (they are still grouped by ID)
                    ungroup() %>%
                    mutate(gender = dplyr::recode(gender, 'N' = 'Female', 'M' = 'Male')) %>% # ... recode variables as required
                    dplyr::select(-c(N, N2))# ... end by renaming and removing columns
    
    ### Save on its own
    write_parquet(patient_demo, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion',  'patient_demo.csv'))

    ### Save with each of the 'patient_eligible' files in 'Clean/ECM Inclusion' folder
    files1 = list.files(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion'), pattern = 'patient_eligible')
    files1 = files1[which(files1 == 'patient_eligible4_eligible_clinics.csv'):length(files1)] # leave only files on the level to which we subset the billing data (patient_eligible4_eligible_clinics) or lower
    file1 = files1[5]
    
    for(file1 in files1){
      temp = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1))) %>% mutate(id = as.character(id))
      if(any(grepl('gender', names(temp)))){next} # skip if already has demographics
      temp = left_join(temp, patient_demo) %>% filter(!is.na(gender)) %>% filter(!is.na(dateofbirth)) # NOTE: removing patients with missing demographic controls

      ### Create risk class (clean) + strata + age variables
      temp = temp %>% 
        add_column(., .after = 'class_code', strata = paste(temp$list_id, temp$class_code)) %>% 
        add_column(., .after = 'dateofbirth', age  = round(as.numeric(ymd(ecm_date) - ymd(temp$dateofbirth)) /  365.25, 0))
      
      fwrite(temp, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0(file1)), na=NA, row.names = F)
    }
    
  
  }

  # Control loop
  end1 = Sys.time()
  print(end1-start1)
  
}


#
# END OF CODE  --------------------------------------------------
#