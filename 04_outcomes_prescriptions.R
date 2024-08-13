
#
# SET-UP ---------------------------------------------------------------------------------------
# 

### This script cleans the prescription data, codes relevant outcomes based on prescription codes 
### and other prescription-related information (e.g. date, costs, status) and aggregates those outcomes per
### patient and i) month ii) year iii) treatment period


### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


### NOTES:  ---------------------------------------------------------------------------------------------
# -> Prescription codes https://www.ravimiregister.ee/Default.aspx?pv=HumRavimid.ATCPuu&ot=C&l=en#C
# -> See monitoring criteria also at: https://www.riigiteataja.ee/akt/125112011004


#
# DATA MERGING ---------------------------------------------------------------------------------------------
# 

### CHOOSE: Re-run anew? --------------------------------------------------------------------------
### Might be unncessary if merged data already created, but needed it read from Master script in full
anew = T
start1 = Sys.time()

if(anew){

  ### Read ID's of ECM patients to keep  ---------------------------------------------------------
  patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id  = as.character(id))
  ### NOTE: Mind that we only use PURE CONTROL sample here, not the expanded one with all patients from ECM randomized clinics
  patients_id = patient_ecm_eligible$id

  
  ### List all files in the relevant folder ---------------------------------------------------------------------------------------------
  files_list = list.files(file.path(project_path, 'Data', 'Raw', 'Prescriptions'), full.names = T, pattern = '.csv')
  
  ### Note that the approach to cleaning the file is similar to the '02_bills_clean.R', althoug here
  ### no billing data is available as prescriptions form a separate dataset.
  
  ### Create empty list to store the data of a given type before merging
  temp_list <- vector("list", length(files_list))
  
  ### Read and add all the files together in a loop ---------------------------------------------------------------------------------------------
  for (i in seq_along(files_list)) { 
    
    ### Define file
    file <- files_list[i]
    
    ###  Control loop
    print(paste0(gsub('.*/', '', file)))
    
    ### Read file (.parquet if exists, if not read .csv + create .parquet) ---------------------------------------------------------------------------------------------
    if(file.exists(gsub('.csv', '.parquet', file))){
      temp = read_parquet(gsub('.csv', '.parquet', file))
    }else{
      temp = fread(file) %>% funique()
      write_parquet(temp, gsub('.csv', '.parquet', file))
    }
    
    ### Cleaning (NEW)... ---------------------------------------------------------------------------------------------
    setDT(temp)
    setnames(temp, tolower(names(temp))) # ... names to lowercase
    temp[, c('v1') := NULL] # ...remove spare columns (if they exist)
    temp[, id := as.character(patientidencrypted)] # ID as character (and shorter name)
    temp = temp %>% dplyr::select(-c(patientidencrypted))
    
    #### Leaving only patients and dates of interest
    temp <- temp[id %in% patients_id & dateofprescription >= start_date1 & dateofprescription <= end_date1]
    
    ### NOTE: For now, also exclude those who purchased the prescription later than the end date ----------------
    temp <- temp[dateofpurchase <= end_date1]
    
    
    ### Write clean file version ---------------------------------------------------------------------------------------------
    write_parquet(temp, gsub('.csv', '.parquet', gsub('Raw', 'Clean', file)))
    
    #### Combine to the total
    temp_list[[i]] <- temp
    
  }
  ### Combine all data.tables ---------------------------------------------------
  dta <- rbindlist(temp_list)

  ##### Final filtering
  dta <- dta[dateofprescription >= start_date1 & dateofprescription <= end_date1 & !is.na(id), unique(.SD)]
  dta <- dta[dateofpurchase <= end_date1]
  

  ### Save  ---------------------------------------------------------------------------------------------
  write_parquet(dta, file.path(project_path, 'Data', 'Clean', 'Prescriptions', paste0('prescriptions','_all.parquet')))
    
  end1 = Sys.time()
  print(end1-start1)
  
}
 



# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------------------
#

### (Re-)Read data -------------------------------------------------------------------------------------
prescriptions_save = read_parquet(file.path(project_path, 'Data', 'Clean', 'Prescriptions', paste0('prescriptions','_all.parquet')))
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id  = as.character(id))

### Ensure we keep only patients in patient_ecm_eligible ----------------------------
prescriptions = prescriptions_save %>% filter(id %in% patient_ecm_eligible$id)

### Add assigned GP and clinic ---------------------------------------------------------------
prescriptions = left_join(prescriptions, patient_ecm_eligible %>% dplyr::select(c(id, list_id, clinic_registration_code2, gp_id)))

pr(prescriptions$healthcarefacilityid == prescriptions$clinic_registration_code2)

# Proportion of different prescription groups (for codes see for instance: https://atcddd.fhi.no/atc_ddd_index/)
round(prop.table(table(substr(prescriptions$atccode, 1, 1))),4) * 100

### New outcomes ----------------------------------------------------------------------------------------------------
prescriptions = prescriptions %>% # slice(10^6:(2*10^6)) %>% 
  rename(
         'p_price_total' = 'totalpriceofprescription', # .... some columns don't require cleaning, just clearer names
         'p_price_ehif' = 'pricepaidbyehif') %>%  
  
  # ... create new outcomes...
  mutate(
    
    ### GENERAL
    n_prescriptions = 1, # ... number of prescriptions
    prescription_status = ifelse(grepl('^0$|^10$', prescription_status), T, F), # ... status - 10 = 'purchased' AND  0 = 'compiled' (made on the spot?) -> see p,.59 (point 5.2.4) in Codebook_prescriptions.pdf in 'Documentation' folder
    
    p_price_total = ifelse(prescription_status, p_price_total, NA), # ... change prices of non-realized prescriptions to NA
    p_price_ehif_share = ifelse(p_price_total == 0 | is.na(p_price_total), NA, p_price_ehif / p_price_total), # ... share of cost paid by EHIF (need to account for the 0 values, as we can't divide by those)
    
    ### TARGETED PRESCRIPTIONS
    # A: Alimentary tract and metabolism
    p_a_diabetes      =  (grepl('^A10', atccode)),
    p_a_diabetes_p      =  (grepl('^A10', atccode) & prescription_status),
    p_a_diabetes_your     =  (grepl('^A10', atccode) & clinic_registration_code2  == healthcarefacilityid ),
    
    # B: Blood and blood forming organs
    p_b_anti_thromb     =  (grepl('^B01', atccode)),
    p_b_anti_thromb_p     =  (grepl('^B01', atccode) & prescription_status),

    p_b_anti_morrh      =  (grepl('^B02', atccode)),
    p_b_anti_morrh_p      =  (grepl('^B02', atccode) & prescription_status),

    p_b_anti_anem       =  (grepl('^B03', atccode)),
    p_b_anti_anem_p      =  (grepl('^B03', atccode) & prescription_status),

    # C: Cardiovascular system
    p_c_cardiac_therapy  =  (grepl('^C01', atccode)),
    p_c_cardiac_therapy_p  =  (grepl('^C01', atccode) & prescription_status),
    
    p_c_hypertensive  =  (grepl('^C02', atccode)),
    p_c_hypertensive_p  =  (grepl('^C02', atccode) & prescription_status),
    p_c_hypertensive_your  =  (grepl('^C02', atccode) & clinic_registration_code2  == healthcarefacilityid),
    
    p_c_diuretics  =  (grepl('^C03', atccode)),
    p_c_diuretics_p  =  (grepl('^C03', atccode) & prescription_status),
    
    p_c_beta_blockers =  (grepl('^C07', atccode)),
    p_c_beta_blockers_p =  (grepl('^C07', atccode) & prescription_status),
    p_c_beta_blockers_your =  (grepl('^C07', atccode) & clinic_registration_code2  == healthcarefacilityid),
    
    p_c_calcium_blockers =  (grepl('^C08', atccode)),
    p_c_calcium_blockers_p =  (grepl('^C08', atccode) & prescription_status),
    
    p_statins       =  (grepl('^C10', atccode)),
    p_statins_p     =  (grepl('^C10', atccode) & prescription_status),
    p_statins_your     =  (grepl('^C10', atccode) & clinic_registration_code2  == healthcarefacilityid),
    
    # J: Antiinfectives
    p_antibiotic       =  (grepl('^J01', atccode)),
    p_antibiotic_p     =  (grepl('^J01', atccode)),
    
    p_vaccines       =  (grepl('^J07', atccode)),
    p_vaccines_p     =  (grepl('^J07', atccode)),

    # R: Respiratory system
    p_r_anti_hist       =  (grepl('^R06', atccode)),
    p_r_anti_hist_p     =  (grepl('^R06', atccode) & prescription_status),

    p_key = if_any(c(p_a_diabetes, p_c_hypertensive, p_c_beta_blockers, p_statins), ~.x == TRUE),
    p_key_p = if_any(c(p_a_diabetes_p, p_c_hypertensive_p, p_c_beta_blockers_p, p_statins_p), ~.x == TRUE),
    p_key_your = if_any(c(p_a_diabetes_your, p_c_hypertensive_your, p_c_beta_blockers_your, p_statins_your), ~.x == TRUE),
    
    p_other = !p_key,
    p_other_p = (!p_key_p & prescription_status),
    p_other_your = (!p_key & clinic_registration_code2  == healthcarefacilityid)
    
    
                            
  ) %>% 
  dplyr::select(-c(healthcarefacilityid, healthcarefacilityname, doctorid, specialtyofdoctor,
                   prescription_dgn, atccode, activesubstance, 
                   codeofpackaging,nameofpackaging, numberofpackaging,
                   list_id, clinic_registration_code2, gp_id))  # ... remove spare columns
  


#write_parquet(prescriptions, file.path(project_path, 'Data/Clean', 'Prescriptions', 'dta_prescriptions_temp.parquet'))


### Dates  ------------------------------------------------------------------------------------------------------
### Get all date-relevant variables for all procedures

#prescriptions = read_parquet(file.path(project_path, 'Data/Clean','Prescriptions', 'dta_prescriptions_temp.parquet'))

prescriptions = prescriptions %>%

                    mutate(
  
                      year_month_day_prescription = ymd(dateofprescription), # ... specify year-month-day (full date)
                      year_month_day_purchase = ymd(dateofpurchase), # ... specify year-month-day (full date)
                      treat_period_prescription = ifelse(year_month_day_prescription < ymd(ecm_date), 0, 1), # ... specify treatment period
                      year_prescription = year(year_month_day_prescription),  # ... specify year
                      year_month_prescription = ifelse(is.na(year_month_day_prescription), 
                                                       NA,paste0(substr(year_month_day_prescription, 1,4), '-', substr(year_month_day_prescription, 6,7))), # ... specify year and month
                      year_rel_prescription = time_length(difftime(year_month_day_prescription,ymd(ecm_date)-1), "years") %>% sign() * # ... specify how many years before or after ECM onset
                        time_length(difftime(year_month_day_prescription, ymd(ecm_date)-1), "years") %>% abs() %>% ceiling(),
                      year_rel_prescription = ifelse(year_rel_prescription == 0, 1, year_rel_prescription), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)

                      # Also add:
                      p_delay = as.numeric(year_month_day_purchase - year_month_day_prescription)

                    ) %>% 
        dplyr::select(-c(dateofpurchase, dateofprescription, year_month_day_purchase)) # ... end by removing original date columns and purchase date (we have necessary info for that from prescription date and the difference column)


### Year_rel creation above fails to account for leap years and results in a tiny number of borderline observations, that is
### those occuring on the onset date (01/05/2021) to be classified into a wrong year. Applying mode of year_rel to all
### observations by month seems to solve the issue
prescriptions = prescriptions %>% group_by(year_month_prescription) %>% mutate(year_rel_prescription = Mode(year_rel_prescription)) %>% ungroup()


### NOTE: Also some year imbalance - looks like the 2014-2017, which is its own dataset in the raw data, has too few observations ----------------------
table(prescriptions$year_prescription)/10^6
table(prescriptions$year_purchase)/10^6


### Remove all 2009 entries (there are very few of them)
prescriptions = prescriptions %>% filter(year_prescription >= 2010)


### Save 
write_parquet(prescriptions, file.path(project_path, 'Data/Clean', 'Prescriptions', 'dta_prescriptions.parquet'))


#
# GROUP... -----------------------------------------------------------------------------------------------------------------
#


### NOTE: We can summarise using group_by() and summarise_all() [as commented out], but takes up to x20 (sic!)
### longer than fsum(), while producing EXACTLY THE SAME RESULTS (CHECKED!)
start2 = Sys.time() # To control the runtime

### Re-read data -------------------------------------------------------------------------------------
prescriptions = read_parquet(file.path(project_path, 'Data/Clean', 'Prescriptions', 'dta_prescriptions.parquet'))
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_eligible_clinics.csv')) %>% mutate(id = as.character(id))

summary(prescriptions$p_price_ehif_share)

# ...by month -----------------------------------------------------------------------------------------------------------------
prescriptions_month = fsum(prescriptions %>% 
                                filter(year_prescription >= 2018) %>%  # NOTE: We don't want any pre-2018 on a monthly basis as of now
                                 dplyr::select(-c(year_month_day_prescription)) %>% # remove variabls not needed here
                                 group_by(id, treat_period_prescription, year_prescription, year_rel_prescription, year_month_prescription)) %>% # define grouping levels
                                  mutate(
                                    p_delay = p_delay/n_prescriptions,
                                    p_price_ehif = p_price_ehif/n_prescriptions,
                                    #  prescription_status = prescription_status/n_prescriptions,
                                    # mutate(across(ends_with("_p"), ~ . / get(sub("_p", "", cur_column()))))
                                    )



# Make sure all patients are there for all months
prescriptions_month = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                prescriptions_month %>% 
                                                    dplyr::select(treat_period_prescription, year_prescription, year_rel_prescription, year_month_prescription) %>% 
                                                    distinct() %>% as.data.frame()) %>%
                                      group_by(id) %>% distinct() %>% ungroup(),
                                    prescriptions_month)%>% 
      mutate(across(-c(id, treat_period_prescription, year_prescription, year_rel_prescription, year_month_prescription), ~replace_na(.,0))) # 0's instead of NA's
    


# ...by year -----------------------------------------------------------------------------------------------------------------

prescriptions_year  = fsum(prescriptions %>%
                                 dplyr::select(-c(contains('month'), year_rel_prescription, treat_period_prescription)) %>%
                                 group_by(id, year_prescription)) %>% 
                                  mutate(
                                    p_delay = p_delay/n_prescriptions,
                                    p_price_ehif = p_price_ehif/n_prescriptions,
                                    #  prescription_status = prescription_status/n_prescriptions,
                                    # mutate(across(ends_with("_p"), ~ . / get(sub("_p", "", cur_column()))))
                                  )
# Make sure all patients are there for all years
prescriptions_year = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                   prescriptions_year %>% dplyr::select(year_prescription) %>%
                                                   distinct() %>% as.data.frame()) %>%
                                     group_by(id) %>% distinct() %>% ungroup(),
                                   prescriptions_year)%>% 
      mutate(across(-c(id, year_prescription), ~replace_na(.,0))) # 0's instead of NA's
    

# ...by year_rel -----------------------------------------------------------------------------------------------------------------
prescriptions_year_rel = fsum(prescriptions %>%
                                dplyr::select(-c(contains('month'), year_prescription, treat_period_prescription)) %>%
                                    group_by(id, year_rel_prescription)) %>% 
                                    mutate(
                                      p_delay = p_delay/n_prescriptions,
                                      p_price_ehif = p_price_ehif/n_prescriptions,
                                      #  prescription_status = prescription_status/n_prescriptions,
                                      # mutate(across(ends_with("_p"), ~ . / get(sub("_p", "", cur_column()))))
                                    )
    
# Make sure all patients are there for all years
prescriptions_year_rel = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                     prescriptions_year_rel %>% dplyr::select(year_rel_prescription) %>%
                                                      distinct() %>% as.data.frame()) %>%
                                                     group_by(id) %>% distinct() %>% ungroup(),
                                       prescriptions_year_rel)%>% 
      mutate(across(-c(id, year_rel_prescription), ~replace_na(.,0))) # 0's instead of NA's
    

# ...by period (2018-2023)  -----------------------------------------------------------------------------------------------------------------
prescriptions_period_18_23  = fsum(prescriptions %>%
                                         filter(year_prescription >= 2018) %>%
                                         dplyr::select(-c(contains('month'), contains('year'))) %>%
                                         group_by(id, treat_period_prescription))  %>% 
                                       mutate(
                                          p_delay = p_delay/n_prescriptions,
                                          p_price_ehif = p_price_ehif/n_prescriptions,
                                          #  prescription_status = prescription_status/n_prescriptions,
                                          # mutate(across(ends_with("_p"), ~ . / get(sub("_p", "", cur_column()))))
                                       )

# Make sure all patients are there for all periods
prescriptions_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                       prescriptions_period_18_23 %>% dplyr::select(treat_period_prescription) %>% distinct() %>% as.data.frame()) %>%
                                             group_by(id) %>% distinct() %>% ungroup(),
                                           prescriptions_period_18_23) %>% 
      mutate(across(-c(id, treat_period_prescription), ~replace_na(.,0))) # 0's instead of NA's
    
    
#
### Checks
dim(prescriptions_month)
dim(prescriptions_year)
dim(prescriptions_year_rel)
dim(prescriptions_period_18_23)

### Save ----
write_parquet(prescriptions_month, file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_month_18_23.parquet')) # NOTE: On purpose, we don't need pre-2018 on a monthly basis for now
write_parquet(prescriptions_year,  file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_year_18_23.parquet'))
write_parquet(prescriptions_year_rel,  file.path(project_path, 'Data/Clean','Aggregated',  'Prescriptions_outcomes_year_rel_18_23.parquet'))
write_parquet(prescriptions_period_18_23,  file.path(project_path, 'Data/Clean','Aggregated',  'Prescriptions_outcomes_period_18_23.parquet'))


Sys.time()-start1# To control the runtime


#
# END OF CODE -------------------------------------------------------------------------------------
#