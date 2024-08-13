
#
# SET-UP ---------------------------------------------------------------------------------------
# 

### This script takes the merged diagnosis data, codes relevant outcomes based on diagnosis codes 
### and other billing information (starting with the date of death) and aggregates those outcomes per
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


### NOTE: To check the mismatched number of outpatient and primary bills between 2009-2019 and 2019-2023 (see below) ---------
### 'ECM list of ID vars.xlsx' on DropBox

### For diagnosis codes check: http://icd9.chrisendres.com/ OR https://www.icd10data.com/search?s=T51.0
# diagnosis = fread(file.path(project_path, 'Data/Raw', 'Diagnosis_code_desc.csv'))


start1 = Sys.time() # To control the runtime

#
# READ DATA   ---------------------------------------------------------------------------------------
#

### Diagnosis data ---------------------------------------------------------------------------------------

### Read data... 
# .. all diagnoses in the billing data
dta_diagnosis = read_parquet(file.path(project_path, 'Data', 'Clean', 'Diagnoses', 'diagnoses_all.parquet')) %>% mutate(id  = as.character(id))


# .. codes of severe diagnoses (baed on: https://www.cms.gov/medicare/quality/initiatives/hospital-quality-initiative/outcome-and-payment-measures)
severe_codes = xlsx::read.xlsx(file.path(project_path, 'Data', 'Clean', 'Other', 'ICD codes of acute conditions.xlsx'), sheetIndex = 1)


### NOTE: LEAVE ONLY PURE CONTROL, CONTROL AND TREATMENT PATIENTS (FOR NOW) ----
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id  = as.character(id))

dta_diagnosis = dta_diagnosis %>% filter(id %in% patient_ecm_eligible$id)


### FOR NOW - ONLY SUBSET OF PATIENTS TO MORE QUICKLY CHECK IF EVERYTHING RUNS -------------------------------------
# dta_diagnosis = dta_diagnosis %>% filter(id %in% patient_ecm_eligible$id[patient_ecm_eligible$ecm_include_patient %in% c('Control', 'Treatment')])
# n_distinct(dta_diagnosis$id)


#
# EXTRACT DATE OF DEATH ---------------------------------------------------------------------------------------
#

# Discard all data before May 2021 (min. death date in the date) -> all ECM patients should be alive until then 
# dta_deaths = dta_diagnosis %>% filter(startoftreatment >= min(dta_diagnosis$dateofdeath[dta_diagnosis$dateofdeath!=0]))


### Check if date of death unique (max of 2 unique values - 0 + actual date of death)
# summary(dta_deaths$dateofdeath[dta_deaths$dateofdeath != 0])
# dta_deaths = dta_deaths %>% group_by(id) %>% mutate(N=n_distinct(dateofdeath))
# pr(dta_deaths$N)
# dta_deaths = dta_deaths %>% group_by(id) %>% filter(dateofdeath!=0) %>% mutate(N=n_distinct(dateofdeath))
# pr(dta_deaths$N)
# R: Yes


### Assign death date to a patient (rather than individual bill). This can be done in 2 WAYS:
dta_deaths = dta_diagnosis %>%
  group_by(id) %>%
  mutate(dateofdeath = max(dateofdeath)) %>%  # ... dateofdeath column - max value
  mutate(deathofdeath_discharge = (endoftreatment), # ... but patients can also be discharged as 'deceased' (codeofdischargetype == 10) - classify those as alternative death dates
         deathofdeath_discharge = ifelse(codeofdischargetype == 10, deathofdeath_discharge, 0),
         deathofdeath_discharge = max(deathofdeath_discharge),
         startoftreatment_max = max(startoftreatment) # ... also, check the latest treatment start day per patient
         ) %>% 
  ungroup() %>% 
  dplyr::select(c(id, dateofdeath, deathofdeath_discharge, startoftreatment_max)) %>% 
  distinct()


### Checks - no patients lost?
n_distinct(dta_deaths$id) == n_distinct(dta_diagnosis$id)
# R: None

### Checks - date of death vs discharge with deceased code
table(dta_deaths$dateofdeath == 0)
prop.table(table(dta_deaths$dateofdeath == 0))
table(dta_deaths$dateofdeath == 0, dta_deaths$deathofdeath_discharge == 0)
# 4,370 with date of death, of which 1,630 with deceased discharge code, but also 121 with that code but no death date


### Select all patients that are deceased on EITHER of our two measures ----------------------------------------
dta_deaths = dta_deaths  %>%  ungroup() %>% 
                filter(dateofdeath != 0 | deathofdeath_discharge != 0)%>%
                distinct()


### Now, there are couple of options, as the death dates are not consistent: -------------------------------------

# 1) dateofdeath non-missing +  no later treatment =  deathofdeath stays as final -> IDEAL SCENARIO (~85% of observation)
temp1 = dta_deaths  %>% 
          filter(dateofdeath != 0)  %>% 
          filter(startoftreatment_max <= dateofdeath)

temp1$dateofdeath_final = temp1$dateofdeath


# 2) dateofdeath non-missing + some later treatment + later discharge as 10 (deceased)  = deathofdeath_discharge stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp2 = dta_deaths  %>% 
          filter(dateofdeath != 0)  %>%
          filter(startoftreatment_max > dateofdeath) %>% 
          filter(deathofdeath_discharge != 0) %>% 
          mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(deathofdeath_discharge)))

temp2$dateofdeath_final = ifelse(temp2$diff_start > 7, 0, temp2$deathofdeath_discharge)


# 3) dateofdeath non-missing + some later treatment + NO later discharge as 10 (deceased) = dateofdeath stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp3 = dta_deaths  %>% 
  filter(dateofdeath != 0)  %>%
  filter(startoftreatment_max > dateofdeath) %>% 
  filter(deathofdeath_discharge == 0) %>% 
  mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(dateofdeath)))

temp3$dateofdeath_final = ifelse(temp3$diff_start > 7, 0, temp3$dateofdeath)


# 4) dateofdeath missing + discharged as 10 (deceased)  = deathofdeath_discharge stays as final is latest treatment <= 7 days afterwards; 0 otherwise
temp4 = dta_deaths  %>% 
  filter(dateofdeath == 0)  %>%
  filter(deathofdeath_discharge != 0)%>% 
  mutate(diff_start = as.numeric(ymd(startoftreatment_max) - ymd(deathofdeath_discharge)))

temp4$dateofdeath_final = ifelse(temp4$diff_start > 7, 0, temp4$deathofdeath_discharge)


### Checks
# (nrow(temp1) + nrow(temp2) + nrow(temp3) + nrow(temp4)) ==  nrow(dta_deaths)
### R: Should give T
# temp = rbindlist(list(temp1, temp2, temp3, temp4), fill = T)
# temp$diff = as.numeric(ymd(temp$startoftreatment_max) - ymd(temp$dateofdeath_final))
# summary(temp$diff[temp$dateofdeath_final != 0])
### R: Should not be >7


### Check if sum of rows of all possible scenarios equals total number of rows in the dataset with the deceased patiets, and reasign if so
if((nrow(temp1) + nrow(temp2) + nrow(temp3) + nrow(temp4)) ==  nrow(dta_deaths)){
  dta_deaths = rbindlist(list(temp1, temp2, temp3, temp4), fill = T) %>% 
      dplyr::select(c(id, dateofdeath_final)) %>% 
      rename('dateofdeath' = 'dateofdeath_final')
      
}

### Checks - how many deaths re-coded (add '2' to dta_deaths and dateofdeath in the 'if' condition above)
# temp = left_join(dta_deaths, dta_deaths2)
# table(temp$dateofdeath == 0, temp$dateofdeath2 == 0)
# temp[(temp$dateofdeath == 0) != (temp$dateofdeath2 == 0),] %>% View
# 24 - 11 from death to no death and 3 from no death to death


### Re-assign to dta_diagnosis (check if all patient ID's unique) ---------------------------------------
nrow(dta_deaths) == n_distinct(dta_deaths$id)

dta_diagnosis = left_join(dta_diagnosis %>% dplyr::select(-c(dateofdeath)), dta_deaths) %>% 
                    mutate(dateofdeath = ifelse(is.na(dateofdeath), 0, dateofdeath),
                           deceased    = ifelse(dateofdeath == 0, 0, 1))


### Check if now all bills start no later than 7 days after the date of death
# dta_diagnosis$diff = as.numeric(ymd(dta_diagnosis$startoftreatment) - ymd(dta_diagnosis$dateofdeath))
# dta_diagnosis$diff %>% summary()
### R: Yes, max value is 7

### Check overall mortality rate
# temp = dta_diagnosis %>% dplyr::select(c(id, deceased)) %>% distinct()
# sf(temp$deceased) # 4,362 out of 60,020 unique patients
# pr(temp$deceased) # 7.27%


### Add to patient_ecm_eligible ----------------------------------------------
dta_deaths_save = dta_deaths

dta_deaths = left_join(patient_ecm_eligible %>% dplyr::select(-any_of(c('dateofdeath'))),
                       dta_deaths_save %>% mutate(dateofdeath = ymd(dateofdeath))) %>% # ... formats that fails to parse are all the 0's, so all good
      # Clean dates...
      mutate( 
      dateofdeath = if_else(dateofdeath > ymd(20230331), NA, dateofdeath), # ... NOTE: if death date AFTER the end of the intervention evaluation period (31/03/2023, treat patient as alive)
      death_year = year(dateofdeath), # ... specify year
      death_year_month = ifelse(!is.na(death_year), paste0( year(dateofdeath), '-',  month(dateofdeath)), NA), # ... specify year and month
      death_month = month(dateofdeath), # ... specify just the month
      death_year_rel = time_length(difftime(dateofdeath, ymd(ecm_date)-1), "years") %>% sign() * # ... specify how many years before or after ECM onset
                      time_length(difftime(dateofdeath, ymd(ecm_date)-1), "years") %>% abs() %>% ceiling(),
      death_year_rel = ifelse(death_year_rel == 0, 1, death_year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
      death_treat_period = ifelse(dateofdeath < ymd(ecm_date), 0, 1) # ... specify treatment period
    ) 

### Add death rates to all ECM inclusion patient-level files ---------------------------------
# dta_deaths = read_parquet(file.path(project_path, 'Data/Clean', 'Deaths_all.parquet'))
# patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_eligible_clinics.csv')) %>% mutate(id = as.character(id))

files1=list.files(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion'), 'patient_eligible')
i = 'patient_eligible4_eligible_clinics.csv'

for(i in files1){
  
  temp = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', i)) %>% mutate(id = as.character(id))
  
  temp = left_join(temp %>% dplyr::select(-any_of(c('dateofdeath'))), dta_deaths %>% dplyr::select(id, dateofdeath))
  temp = temp %>% filter(dateofdeath >= ymd(ecm_date) | is.na(dateofdeath))
  
  print(i)
  n_distinct(temp$clinic_registration_code) %>% print
  n_distinct(temp$list_id) %>% print
  n_distinct(temp$id) %>% print
  
  fwrite(temp, file.path(project_path, 'Data/Clean/ECM Inclusion', i), na=NA, row.names = F)
}

# temp = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible5_ecm_clinics.csv')) %>% mutate(id = as.character(id))


# Keep only alive patients
dta_deaths = dta_deaths %>% filter(dateofdeath >= ymd(ecm_date) | is.na(dateofdeath))


### Save -----
fwrite(dta_deaths, file.path(project_path, 'Data/Clean/ECM Inclusion', 'deaths_all.csv'), na=NA, row.names = F)


t.test(!is.na(dta_deaths$dateofdeath[dta_deaths$ecm_include_patient %in% c('Control', 'Treatment')]) ~
         dta_deaths$ecm_include_patient[dta_deaths$ecm_include_patient %in% c('Control', 'Treatment')])

# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------------------
#

### Keep only patients in patient_ecm_eligible ----------------------------
dta_diagnosis = dta_diagnosis %>% filter(id %in% patient_ecm_eligible$id)



### Remove unnecessary columns
### NOTE: Also remove 'billnr' column. Now, this results in ~2% fewer rows 
### after collapsing by diagnosis code below, BUT we do ensure we have
### the same patient, on the same day, with the same dataset and admission type,
### so by all accounts we should treat it as a single healthcare interaction
### even if there are sometimes >1 bill numbers. 

dta_diagnosis = dta_diagnosis %>% ungroup() %>%
  dplyr::select(c(billnr, id, codeofhealthcareprovider, doctorid,
                  dataset, 
                  dateofdeath, deceased,  startoftreatment, endoftreatment, codeofdiagnos, codeofadmissiontypeoradmitt))





### Identify relevant diagnosis codes etc. -------------------------------------------------------------------------------------


dta_diagnosis = dta_diagnosis %>% 
  mutate(
    
    n_diag = 1,
    n_bill = 1,
    #n_diag = 1 + str_count(codeofdiagnos, ','),
    
    
    # Not-primary care
    n_inpatient_any = (dataset == 'inpatient'),
    n_daycare_any = (dataset == 'day'),
    n_inpatient_post_any = (dataset %in% c('inpatientnursing', 'inpatientrehabilitation')),
    n_outpatient_post_any = (dataset %in% c('outpatientnursing', 'outpatientrehabilitation')), # those are overhelmingly 99-100% done not in the clinic, so count any here
    
    
    # Severe (hospitaliztion)
    myocardial_infarction = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Acute Myocardial Infarction (AMI)']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    stroke = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Stroke']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    copd   =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Chronic Obstructive Pulmonary Disease (COPD)']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    heart_failure  =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Heart failure']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    pneumonia    =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Pneumonia']), collapse='|'), codeofdiagnos) & dataset == 'inpatient'),
    
    n_sever_diag    =  (myocardial_infarction | stroke | copd | heart_failure | pneumonia),
    
    # Severe (any)
    myocardial_infarction_any = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Acute Myocardial Infarction (AMI)']), collapse='|'), codeofdiagnos)),
    stroke_any = (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Stroke']), collapse='|'), codeofdiagnos)),
    copd_any   =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Chronic Obstructive Pulmonary Disease (COPD)']), collapse='|'), codeofdiagnos)),
    heart_failure_any  =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Heart failure']), collapse='|'), codeofdiagnos)),
    pneumonia_any    =  (grepl(paste(unique(severe_codes$idc10[severe_codes$variable == 'Pneumonia']), collapse='|'), codeofdiagnos)),
    
    n_sever_diag_any    =  (myocardial_infarction_any | stroke_any | copd_any | heart_failure_any | pneumonia_any),

        
    # Additional original 'avoidable' hospitalizations
    asthma      =  (grepl('J45', codeofdiagnos) & dataset == 'inpatient'),
    diabetes_2  =  (grepl('E11.0|E11.1|E11.2|E11.3|E11.4|E11.5|E11.6|E11.7|E11.8|E11.9', codeofdiagnos)  & dataset == 'inpatient'),
    hypertension    =  (grepl('I10|I11|I12|I13|I15', codeofdiagnos) & dataset == 'inpatient'),
    
    # n_diag_avoid    =  (asthma | diabetes_2 | copd | hypertension | heart_failure), # Works - checked manually
    
    asthma_any      =  (grepl('J45', codeofdiagnos)),
    diabetes_2_any  =  (grepl('E11.0|E11.1|E11.2|E11.3|E11.4|E11.5|E11.6|E11.7|E11.8|E11.9', codeofdiagnos)),
    hypertension_any    =  (grepl('I10|I11|I12|I13|I15', codeofdiagnos)),

    # n_diag_avoid_any   =  (asthma_any | diabetes_2_any | copd_any | hypertension_any | heart_failure_any, # Works - checked manually
    
    
    admit_ambulance    =  (grepl('E-T0001', codeofadmissiontypeoradmitt) & dataset == 'inpatient'), # see explanation of different admission codes here: https://www.riigiteataja.ee/akt/125112011004
    admit_referral    =  (grepl('E-T0011', codeofadmissiontypeoradmitt) & dataset == 'inpatient'), # see explanation of different admission codes here: https://www.riigiteataja.ee/akt/125112011004
    

    alcohol_abuse  =  (grepl('F10|Z71.4', codeofdiagnos)),
    arthritis  =  (grepl('M05|M06|M15|M16|M17|M18|M19', codeofdiagnos)),
    atrial_fibrillation  =  (grepl('I48', codeofdiagnos)),
    cancer  =  (grepl('C18|C34|C50|C61', codeofdiagnos)),
    chronic_kidney_disease  =  (grepl('N18', codeofdiagnos)),
    depression  =  (grepl('F32', codeofdiagnos)),
    substance_use  =  (grepl('F11|F12|F13|F14|F15|F16|F17|F18|F19', codeofdiagnos)),
    hepatitis_b_c  =  (grepl('B16|B17', codeofdiagnos)),
    hyperlipidemia  =  (grepl('E78', codeofdiagnos)),
    hypothyroidism = (grepl('E01|E02|E03|E89.0', codeofdiagnos)),
    
    
    # hypertensive_heart  =  (grepl('I11', codeofdiagnos)),
    # ischemic_heart_disease  =  (grepl('I21|I22|I23|I24|I25', codeofdiagnos)),
    osteoporosis  =  (grepl('M80|M81', codeofdiagnos)),
    covid_incidence  =  (grepl('U07.1', codeofdiagnos)),
    
    weight_high = (grepl('E66|R63.5', codeofdiagnos)),
    weight_low = (grepl('R63.4|R63.6|^T75.82|^X52', codeofdiagnos)),
   
    )

dta_outcomes =  inner_join(dta_diagnosis, patient_ecm_eligible %>% dplyr::select(-c(dateofdeath))) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))

### Bill-level: some variables should be coded only once per bill, so we assign 1 to first row of each bill and NA to any other rows --------
bill_vars = names(dta_diagnosis)[!grepl("(?<=n)_.*diag", names(dta_diagnosis), perl=TRUE) & grepl('^n_|^admit', names(dta_diagnosis))]

dta_diagnosis = dta_diagnosis %>% 
  group_by(billnr) %>%  mutate(across(bill_vars, ~ifelse(row_number() == 1 & . == T, ., NA))) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting



### Checks
start1-Sys.time() 


### Time difference -------------------------------------------------------------------------------------

### Calculate difference between start date of different bills by...
dta_diagnosis = dta_diagnosis %>%
  mutate(startoftreatment = ymd(startoftreatment), # ... treating dates as dates
         endoftreatment = ymd(endoftreatment)) %>%
  group_by(id) %>%  # ...grouping by patient
  arrange(startoftreatment,  .by_group = TRUE) %>%  # ... arranging by date within the groups
  mutate(start_diff = as.numeric(startoftreatment - dplyr::lag(startoftreatment, default = first(startoftreatment))), # ... calculating difference with lagged date
         start_diff = ifelse(start_diff == 0, NA, start_diff)) %>% # ... changing 0 to NA's (mostly first values, otherwise they would decrease the average difference)
  ungroup() %>% relocate('start_diff', .after = 'startoftreatment') %>%  # ... relocate to be next to startoftreatment
  group_by(billnr) %>%  mutate(start_diff = ifelse(row_number() == 1, start_diff, NA)) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting


### Repeat by dataset (primary, outpatient, inpatient etc.)
dta_diagnosis = dta_diagnosis %>%
  group_by(id, dataset) %>%  # ...grouping by patient
  arrange(startoftreatment,  .by_group = TRUE) %>%  # ... arranging by date within the groups
  mutate(start_diff_dataset = as.numeric(startoftreatment - dplyr::lag(startoftreatment, default = first(startoftreatment))), # ... calculating difference with lagged date
         start_diff_dataset = ifelse(start_diff_dataset == 0, NA, start_diff_dataset)) %>% # ... changing 0 to NA's (mostly first values, otherwise they would decrease the average difference)
  ungroup() %>% relocate('start_diff_dataset', .after = 'start_diff') %>%  # ... relocate to be next to startoftreatment
group_by(billnr) %>%  mutate(start_diff_dataset = ifelse(row_number() == 1, start_diff_dataset, NA)) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting


### Add difference between start and end of treatment
dta_diagnosis = dta_diagnosis %>% 
  add_column(.after = 'endoftreatment', 'start_end_diff' = as.numeric(dta_diagnosis$endoftreatment - dta_diagnosis$startoftreatment)) %>% 
  group_by(billnr) %>%  mutate(start_end_diff = ifelse(row_number() == 1, start_end_diff, NA)) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting


dta_diagnosis = dta_diagnosis %>% 
  add_column(.after = 'start_end_diff', 'start_end_diff_inpatient' = ifelse(dta_diagnosis$dataset == 'inpatient', dta_diagnosis$start_end_diff, NA)) %>% 
  group_by(billnr) %>%  mutate(start_end_diff_inpatient = ifelse(row_number() == 1, start_end_diff_inpatient, NA)) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting



### Re-admission and death after hospitalization -------------------------------------------------------------------------------------------------

dta_diagnosis = dta_diagnosis %>% ungroup() %>% 
  mutate(
    'readmit_30_any' = ifelse(dataset == 'inpatient' & start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
    'readmit_90_any' = ifelse(dataset == 'inpatient' & start_diff_dataset <= 90 & !is.na(start_diff_dataset), T, F),
    'readmit_30_severe' = ifelse(dataset == 'inpatient' & n_sever_diag == T & start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
    'readmit_90_severe' = ifelse(dataset == 'inpatient' & n_sever_diag == T & start_diff_dataset <= 90 & !is.na(start_diff_dataset), T, F),
    
    # 'deceased_30_any' = ifelse(start_diff_dataset <= 30 & !is.na(start_diff_dataset), T, F),
    
  ) %>% 
  relocate(c('readmit_30_any', 'readmit_90_any', 'readmit_30_severe', 'readmit_90_severe'), .after = 'start_end_diff')


### NOTE: Some patients have start_end_diff greater than the number of days of the period considered (see e.g. id = 10455147)
### This appears to accord with the data though as some patients might have multiple bills going on in the same time apparently
sum_miss(dta_diagnosis$start_end_diff[dta_diagnosis$id %in% c('10455147')])


### Save
write_parquet(dta_diagnosis, file.path(project_path, 'Data', 'Clean', 'Diagnoses', 'dta_diagnosis_temp.parquet'))


### Dates  ------------------------------------------------------------------------------------------------------
### Get all date-relevant variables for all procedures

dta_diagnosis = dta_diagnosis %>%
  ### Treatment date
  mutate(   
         year = substr(startoftreatment, 1, 4), # ... specify year
         year_month = paste0(substr(startoftreatment, 1,4), '-', substr(startoftreatment, 6,7)), # ... specify year and month
         year_month_day = ymd(startoftreatment), # ... specify year-month-day (full date)
         month = month(year_month_day), # ... specify just the month
         year_rel = time_length(difftime(year_month_day, ymd(ecm_date)-1), "years") %>% sign() * # ... specify how many years before or after ECM onset
                    time_length(difftime(year_month_day,ymd(ecm_date)-1), "years") %>% abs() %>% ceiling(),
         year_rel = ifelse(year_rel == 0, 1, year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
         treat_period = ifelse(startoftreatment < ymd(ecm_date), 0, 1) # ... specify treatment period
         ) %>% 
  relocate(.after = 'startoftreatment', c('treat_period','year', 'year_month','month', 'year_month_day', 'year_rel')) %>% 
  dplyr::select(-c(start_diff, start_diff_dataset, startoftreatment, endoftreatment, # remove as we won't need those columns as outcomes
                   deceased, dateofdeath, codeofdiagnos, dataset, codeofadmissiontypeoradmitt)) 


### Year_rel creation above fails to account for leap years and results in a tiny number of borderline observations, that is
### those occuring on the onset date (01/06/2021) to be classified into a wrong year. Applying mode of year_rel to all
### observations by month seems to solve the issue
dta_diagnosis = dta_diagnosis %>% group_by(year_month) %>% mutate(year_rel = Mode(year_rel)) %>% ungroup()


### Clean ID's
dta_diagnosis = dta_diagnosis %>% mutate(id=as.character(id))




### Save  -------------------------------------------------------------------------------------------------
write_parquet(dta_diagnosis, file.path(project_path, 'Data/Clean', 'Diagnoses', 'dta_diagnosis.parquet'))

### Remove any unnecessary columns
dta_diagnosis = dta_diagnosis %>% dplyr::select(-c(billnr, codeofhealthcareprovider, doctorid))

#
# GROUP... -----------------------------------------------------------------------------------------------------------------
#

### NOTE: We can summarise using group_by() and summarise_all() [as commented out], but takes up to x20 (sic!)
### longer than fsum(), while producing EXACTLY THE SAME RESULTS (CHECKED!)

dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses', 'dta_diagnosis.parquet'))
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_eligible_clinics.csv')) %>% mutate(id = as.character(id))

sf(patient_ecm_eligible$age)


# ...by month -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_month = fsum(dta_diagnosis %>% 
                            filter(year >= 2018) %>%  # NOTE: We don't want any pre-2018 on a monthly basis as of now
                             dplyr::select(-c(year_month_day)) %>% # remove variabls not needed here
                              group_by(id, treat_period, year, year_rel, year_month, month)) %>% # define grouping levels
                              mutate( #  if both averages and totals needed, add averages as new columns
                                start_end_diff_av = start_end_diff / n_bill,
                                # start_end_diff_inpatient_av = start_end_diff_inpatient / n_bill_inpatient,
                                n_diag_av = n_diag / n_bill
                              ) 
                              # dplyr::select(-c()) # Remove those variables not needed as total

# Make sure all patients are there for all months
dta_diagnosis_month = left_join(expand_grid(id = patient_ecm_eligible$id,
                            dta_diagnosis_month %>% dplyr::select(treat_period, year, year_rel, year_month, month) %>% distinct() %>% as.data.frame()) %>%
                            group_by(id) %>% distinct() %>% ungroup(),
          dta_diagnosis_month)%>% 
  mutate(across(-c(id, treat_period, year, year_rel, year_month, month), ~replace_na(.,0))) # 0's instead of NA's


# ...by year -----------------------------------------------------------------------------------------------------------------

dta_diagnosis_year  = fsum(dta_diagnosis %>%
                              dplyr::select(-c(contains('month'), year_rel, treat_period)) %>%
                              group_by(id, year)) %>% 
                              mutate( #  if both averages and totals needed, add averages as new columns
                                start_end_diff_av = start_end_diff / n_bill,
                                # start_end_diff_inpatient_av = start_end_diff_inpatient / n_bill_inpatient,
                                n_diag_av = n_diag / n_bill
                              ) 

# Make sure all patients are there for all years
dta_diagnosis_year = left_join(expand_grid(id = patient_ecm_eligible$id,
                                           dta_diagnosis_year %>% dplyr::select(year) %>% distinct() %>% as.data.frame()) %>%
                                              group_by(id) %>% distinct() %>% ungroup(),
                              dta_diagnosis_year)%>% 
  mutate(across(-c(id, year), ~replace_na(.,0))) # 0's instead of NA's

# ...by year_rel -----------------------------------------------------------------------------------------------------------------
dta_diagnosis_year_rel = fsum(dta_diagnosis %>%
                             dplyr::select(-c(contains('month'), year, treat_period)) %>%
                             group_by(id, year_rel)) %>% 
                            mutate( #  if both averages and totals needed, add averages as new columns
                              start_end_diff_av = start_end_diff / n_bill,
                              # start_end_diff_inpatient_av = start_end_diff_inpatient / n_bill_inpatient,
                              n_diag_av = n_diag / n_bill
                            ) 

# Make sure all patients are there for all years
dta_diagnosis_year_rel = left_join(expand_grid(id = patient_ecm_eligible$id,
                                               dta_diagnosis_year_rel %>% dplyr::select(year_rel) %>% distinct() %>% as.data.frame()) %>%
                                 group_by(id) %>% distinct() %>% ungroup(),
                                 dta_diagnosis_year_rel)%>% 
  mutate(across(-c(id, year_rel), ~replace_na(.,0))) # 0's instead of NA's



# ...by period (2018-2023)  -----------------------------------------------------------------------------------------------------------------

dta_diagnosis_period_18_23  = fsum(dta_diagnosis %>%
                                     filter(year >= 2018) %>%
                                     dplyr::select(-c(contains('month'), contains('year'))) %>%
                                group_by(id, treat_period)) %>%
                                mutate( #  if both averages and totals needed, add averages as new columns
                                  start_end_diff_av = start_end_diff / n_bill,
                                  n_diag_av = n_diag / n_bill) 

# Make sure all patients are there for all periods
dta_diagnosis_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                                   dta_diagnosis_period_18_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                   group_by(id) %>% distinct() %>% ungroup(),
                   dta_diagnosis_period_18_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's



### Checks
dim(dta_diagnosis_month)
dim(dta_diagnosis_year)
dim(dta_diagnosis_year_rel)
dim(dta_diagnosis_period_18_23)

### Save ----
write_parquet(dta_diagnosis_month, file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_month_18_23.parquet')) # NOTE: On purpose, we don't need pre-2018 on a monthly basis for now
write_parquet(dta_diagnosis_year,  file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_year_18_23.parquet'))
write_parquet(dta_diagnosis_year_rel,  file.path(project_path, 'Data/Clean', 'Aggregated', 'Diagnoses_outcomes_year_rel_18_23.parquet'))
write_parquet(dta_diagnosis_period_18_23,  file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_period_18_23.parquet'))


Sys.time() - start1 # To control the runtime


#
# END OF CODE -----------------------------------------------------------------------------------------------------------
#