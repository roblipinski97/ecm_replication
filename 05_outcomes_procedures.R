
#
# SET-UP ---------------------------------------------------------------------------------------
# 

### This script takes the merged procedures data, codes relevant outcomes based on diagnosis codes 
### and other billing information (including whether a patient actually enrolled in ECM) and aggregates those outcomes per
### patient and i) month ii) year iii) treatment period

start1 = Sys.time()


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



#
# READ DATA   ---------------------------------------------------------------------------------------
#

### Patients' status data    ---------------------------------------------------------------------------------------
### (file created in '01_id_clean.R')
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id  = as.character(id))

## Procedures costs data  --------------------------------------------------------------
### (existing file)
cost_procedures_raw = janitor::clean_names(fread(file.path(project_path, 'Data', 'Raw', 'Other', "costs_procedures (translated).csv"), encoding = 'UTF-8'))
 cost_procedures_raw %>% dplyr::select(c(hkkood, sap_hkkood,nimetus_eng, nimetus)) %>% distinct() %>% View()
# cost_procedures_raw  %>% distinct() %>% View()

cost_procedures = clean_names(fread(file.path(project_path, 'Data', 'Clean', 'Other', "costs_procedures.csv"), encoding = 'UTF-8'))

### Procedures data --------------------------------------------------------------
### (file created in '02_bills_clean.R')
dta_procedures = read_parquet(file.path(project_path, 'Data', 'Clean', 'Procedures', 'procedures_all.parquet'))


### Clean: Subset --------------------------------------------------------------------------------------------------------
### In 02_billing_clean.R the data was subset by startoftreatment, but some dateofprocedure are still wrong 
### Set wrong dateofprocedure to startoftreatment

dta_procedures = dta_procedures %>% 
  mutate(dateofprocedure = ifelse(dateofprocedure < 20090101, startoftreatment, dateofprocedure),
         dateofprocedure = ifelse(dateofprocedure > 20230331, startoftreatment, dateofprocedure))


### Remove unnecessary columns
dta_procedures = dta_procedures %>% ungroup() %>%
  dplyr::select(c(billnr, id, procedure, codeofhealthcareprovider, doctorid, dateofprocedure, nroftimes, dataset))


### Keep only patients in patient_ecm_eligible
dta_procedures = dta_procedures %>% filter(id %in% patient_ecm_eligible$id)

### Add assigned GP and clinic
dta_procedures = left_join(dta_procedures, patient_ecm_eligible %>% dplyr::select(c(id, list_id, clinic_registration_code2, gp_id)))


### Clean: Number of times --------------------------------------------------------------------------------------------------------

### Procedures can be performed more than once per bill, so we need to account for that, but 
### the 'nroftimes' variable that codes for this also needs to be cleaned a bit first

# Assign 1 if values negative
dta_procedures$nroftimes[dta_procedures$nroftimes <= 0] = 1

# Winsorize by group
min1 = min(dta_procedures$nroftimes, na.rm=T)

dta_procedures <- dta_procedures %>% 
  group_by(procedure) %>%  
  mutate(nroftimes_win =  Winsorize(nroftimes, minval = min1, maxval = NULL, probs = c(0.001, winsorize1),
                                    na.rm = FALSE, type = 7))



### Clean: Procedure codes --------------------------------------------------------------------------------------------------------
### Procedure codes are often a bit unstructured and therefore lack the requisite uniformity. 
### The operations below correct that

# Change o/O to 0
dta_procedures = dta_procedures %>% mutate(procedure = gsub('o|O', '0', procedure))

# Remove leading 0's from the procedure as they tend to vary in length making matching more difficult
# This will make codes in match 'hkkood' column in 'cost_procedures'
dta_procedures = dta_procedures %>% mutate(procedure = ifelse(test = grepl('^000',procedure,),
                                                                  yes  = gsub('^0+', '', procedure),
                                                                  no   = procedure)) %>% ungroup()




# 
# CREATE OUTCOMES -------------------------------------------------------------------------------------
#

names_start = names(dta_procedures) # save names to easily subset old/new columns

dta_procedures = dta_procedures %>% mutate(
  
  n_procedure = T, # how many procedures in total? (any procedure = T)
  
  # Primary care
  consult_include = grepl("9092", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  #consult_include2 = grepl("9091", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  #consult_include3 = grepl("9093", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_refuse = grepl("9090", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_care_plan = grepl("9095", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_gp = grepl("9044", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_gp_phone = grepl("9018", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  #consult_gp_email = grepl("9019", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_nurse = grepl("9061", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_nurse_phone = grepl("9064", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  #consult_nurse_email = grepl("9065", procedure) & clinic_registration_code2  == codeofhealthcareprovider,
  consult_any_your = grepl('9044|9018|9061|9064', procedure)  & clinic_registration_code2  == codeofhealthcareprovider,
  consult_all = grepl('9092|9090|9095|9044|9018|9061|9064', procedure) & clinic_registration_code2  == codeofhealthcareprovider,

  n_primary_your = (dataset == 'primaryhealth' & clinic_registration_code2  == codeofhealthcareprovider & !consult_all), # any other primary healthcare at your clinic
  n_outpatient_your = (dataset == 'outpatient' & clinic_registration_code2  == codeofhealthcareprovider & !consult_all), # any other outpatient healthcare at your clinic
  
  n_primary_not_your = (dataset == 'primaryhealth' & clinic_registration_code2  != codeofhealthcareprovider & !consult_all), # outpatient healthcare NOT at your clinic
  n_outpatient_not_your = (dataset == 'outpatient' & clinic_registration_code2  != codeofhealthcareprovider & !consult_all),  # primary healthcare NOT at your clinic

  # Covid-19
  covid_test = grepl("3183$|66634|66645|9519$", procedure),
  covid_vaccine = grepl("3197$|3199$|9590$|9591$|9592$|9593$|9594$|9595$|9596$|9597$|9598$|9599$", procedure),
  covid_vaccine_refuse = grepl("9589$", procedure),
  
  # Key procedures
  coronoary_bypass = grepl('1F2101', procedure),
  hip_knee_arthroplasty = grepl('0N2139|0N2140|0N2205|2650L|2651L|2652L|2655L|2656L|2674L|2951L|0N2141|2662L|2661L|2672L|0N2143', procedure), #  40214 -> no, burn deformities; knee from 2674L onwards
  
  # Intensive care
  intensive_care_1 =  grepl('2044|2070', procedure),
  intensive_care_2 =  grepl('2045|2071', procedure),
  intensive_care_3 =  grepl('2046|2072', procedure),
  intensive_care_3A =  grepl('2059|2073', procedure),
  
  intensive_care_any =  grepl('2044|2070|2045|2071|2046|2072|2059|2073', procedure),
  
  
  ### Monitoring -> source: RIIGI TEATAJA (in top->down order)
  
  m_glicihem_riigi = grepl('66118', procedure),
  m_glicihem_all = grepl('66118|6506A|9118|9050', procedure),
  
  m_creatinine_riigi = grepl('66102', procedure),
  m_creatinine_all = grepl('66102|9102|6500D', procedure),
  
  m_chol_tri_riigi = grepl('66104', procedure),
  m_chol_tri_all   = grepl('6503F|6501F|66104|6501G|66105|9105|6503G|9104|9040|9042|6502L', procedure),
  
  m_glucose_riigi = grepl('66101', procedure),
  m_glucose_all = grepl('66101|9050|9101|9131|9118|9011|6500B|9067Z', procedure),
  
  # m_ecg_riigi = grepl('6320|6322|6323', procedure), # When un-commenting, ensure it is added as outcome to m_any_riigi below
  
  m_thc_riigi = grepl('66706', procedure),
  m_any_riigi = if_any(c(m_glicihem_riigi, m_creatinine_riigi, m_chol_tri_riigi, m_glucose_riigi, m_thc_riigi), ~.x == TRUE)

  )


### Bill-level: some variables should be coded only once per bill, so we assign 1 to first row of each bill and NA to any other rows --------
bill_vars = names(dta_procedures)[!grepl("(?<=n)_.*procedure", names(dta_procedures), perl=TRUE) & grepl('^n_', names(dta_procedures))]

dta_procedures = dta_procedures %>% 
  group_by(billnr) %>%  mutate(across(bill_vars, ~ifelse(row_number() == 1 & . == T, ., NA))) %>% ungroup() # make sure only first row is kept as non-missing to avoid multiple-counting


### Nr of times -------------------------------------------------------------------------------------
### Now account for the fact the some procedures performed != 1 number of times
dta_procedures = dta_procedures %>%
  mutate(across(c(setdiff(names(dta_procedures), c(names_start, 
                                                   names(dta_procedures)[grepl('n_', names(dta_procedures))]
                                                   #names(dta_procedures)[grepl('consult_', names(dta_procedures))]
                                                   ))), ~. * nroftimes_win))




### Save data (temporary file becuase due to some memory reading issues sometimes I found it necessary to perform this script piecemeal)
write_parquet(dta_procedures, file.path(project_path, 'Data/Clean',  'Procedures', 'dta_procedures_temp.parquet'))


#
# ECM INCLUSION DATES -------------------------------------------------------------------------------------
#
### Procedures also allow us to determine which patients started on ECM programme and when. This section
### extracts and saves that information,

gc()
dta_procedures = read_parquet(file.path(project_path, 'Data/Clean',  'Procedures', 'dta_procedures_temp.parquet'))


patient_ecm_accept = dta_procedures %>% 
  filter(consult_care_plan > 0) %>% # Leave only observations care plan codes (existence of care plan = ECM enrolment)
  dplyr::select(any_of(c('id', 'dateofprocedure', 'consult_care_plan'))) %>% # Only required columns
  group_by(id) %>% # For each patient....
  mutate(ecm_status_patient = any(consult_care_plan>0)) %>% # ... code if they had ECM care plan codes...
  filter(ecm_status_patient) %>% # ..leaving only those that did... 
  mutate(ecm_status_patient_date = min(dateofprocedure)) %>% #  .. find first date at which those codes occur
  ungroup() %>% 
  dplyr::select(c(id, ecm_status_patient, ecm_status_patient_date)) %>% # Leave only unique observations...
  distinct() %>% # ... and distinct rows
  mutate(ecm_status_patient = ifelse(ecm_status_patient, 'ECM accept', 'Not ECM Accept')) %>%  # classify ECM status as either accepting or not
  mutate(ecm_status_patient_date = ymd(ecm_status_patient_date))  # treat date as date variable

### Save IDs of patients with relevant codes from BEFORE ECM rollout (i.e. pilot patients)
id_pilot = patient_ecm_accept$id[patient_ecm_accept$ecm_status_patient_date < ymd(ecm_date)] %>% unique

### Remove those with relevant codes BEFORE ECM rollout (i.e. pilot patients)
patient_ecm_accept = patient_ecm_accept %>% filter(ecm_status_patient_date > ymd(ecm_date))

### Save 
fwrite(patient_ecm_accept, na=NA, row.names = F, file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_ecm_accept.csv"))


#
# CLEAN PROCEDURES ------------------------------------------------------------------------------------------------------
#

### (Re-)read the data
dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures',  'dta_procedures_temp.parquet'))
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))

### Dates  ------------------------------------------------------------------------------------------------------
### Get all date-relevant variables for all procedures
gc()
dta_procedures = dta_procedures %>%
  ### Treatment date
  mutate(   
    year = substr(dateofprocedure, 1, 4), # ... specify year
    year_month = paste0(substr(dateofprocedure, 1,4), '-', substr(dateofprocedure, 5,6)), # ... specify year and month
    year_month_day = ymd(dateofprocedure), # ... specify year-month-day (full daye)
    month = month(year_month_day), # ... specify just the month
    
    year_rel = time_length(difftime(year_month_day, ymd(ecm_date)-1), "years") %>% sign() * # ... specify how many years before or after ECM onset
                  time_length(difftime(year_month_day, ymd(ecm_date)-1), "years") %>% abs() %>% ceiling(),
    year_rel = ifelse(year_rel == 0, 1, year_rel), # ... change 0 to 1 in year_rel (observations on the day of ECM onset)
    
    treat_period = ifelse(dateofprocedure < ecm_date, 0, 1) # ... specify treatment period
  ) %>% 
  # filter(dateofprocedure >= 20090101) %>% filter(dateofprocedure <= 20230331) %>%  # ...remove any observations outside the intervention range 
  relocate(.after = 'dateofprocedure', c('treat_period','year', 'year_month','month', 'year_month_day')) # ... order columns

### Year_rel creation above fails to account for leap years and results in a tiny number of borderline observations, that is
### those occuring on the onset date (01/06/2021) to be classified into a wrong year. Applying mode of year_rel to all
### observations by month seems to solve the issue
dta_procedures = dta_procedures %>% group_by(year_month) %>% mutate(year_rel = Mode(year_rel)) %>% ungroup()


gc()
write_parquet(dta_procedures, file.path(project_path, 'Data/Clean', 'Procedures',  'dta_procedures_temp2.parquet'))



### Prices -------------------------------------------------------------------------------------
### Merge bill information with procedure costs (mind that for now we don't use those cost data in the 
### analyeses)

# Create a month-by-month grid to know the relevant price for every month and procedure
temp = expand.grid(unique(cost_procedures$hkkood), 
            seq.Date(from = ymd(start_date1), to = ymd(ymd(end_date1)+1), by = 'months')) %>% 
        rename('hkkood' = 'Var1', 'date' = 'Var2')


# Match with costs data and leave only the months that fit between 'from' and 'until' dates for which a given price was valid
# (the prices for procedures vary, usually they increase every couple of months, this code ensures we are using the right one)
temp = left_join(cost_procedures, temp) %>% 
  filter(date <= until | is.na(until)) %>% filter(date >= from) %>% 
  mutate(year_month = paste0(year(date),
                                        '-',
                                        ifelse(nchar(month(date))==1, paste0('0', month(date)), month(date))))


# Remove and rename columns as necesssary
temp = temp %>% 
  dplyr::select(-c(procedure, procedure_class, from, until, date)) %>% 
  rename('procedure' = 'hkkood') %>% as.data.frame()

# Save
#write_parquet(temp, file.path(project_path, 'Data', 'Clean','Procedures', 'costs_temp.parquet'))
#temp = read_parquet(file.path(project_path, 'Data', 'Clean','Procedures', 'costs_temp.parquet'))

dta_procedures = read_parquet(file.path(project_path, 'Data/Clean',  'Procedures', 'dta_procedures_temp2.parquet'))
gc()

# Match with 'dta_procedures'
dta_procedures <- left_join(dta_procedures,
                             temp,
                             by = c('procedure', 'year_month'))


# Price * Nr. of times - prices are per 1 'unit' of a procedure so they need to be multiplied by the number of times a procedure was performed
dta_procedures = dta_procedures %>% mutate(price_total = price * nroftimes_win) %>% 
  relocate('price_total', .after = 'price')


# Price x Dataset - create columns with non-missing prices only for specific types of healthcare
dta_procedures = dta_procedures %>% 
  mutate(
    price_primary_your = ifelse(dataset == 'primaryhealth'  & clinic_registration_code2  == codeofhealthcareprovider , price_total, NA),
    price_primary_not_your = ifelse(dataset == 'primaryhealth'  & clinic_registration_code2  != codeofhealthcareprovider , price_total, NA),
    price_outpatient_your = ifelse(dataset == 'outpatient' & clinic_registration_code2  == codeofhealthcareprovider, price_total, NA),
    price_outpatient_not_your = ifelse(dataset == 'outpatient' & clinic_registration_code2  != codeofhealthcareprovider, price_total, NA),
    price_inpatient = ifelse(dataset == 'inpatient', price_total, NA)
  )



### Clean ID's before saving
dta_procedures = dta_procedures %>% mutate(id=as.character(id))


### Save ------------------------------------------------------------------------------
write_parquet(dta_procedures, file.path(project_path, 'Data/Clean', 'Procedures',  'dta_procedures.parquet'))

### Remove any unncessary columns
dta_procedures = dta_procedures %>% dplyr::select(-c(billnr, codeofhealthcareprovider, clinic_registration_code2, doctorid, procedure, list_id, gp_id,
                                                     dateofprocedure, dataset, price, contains('nroftimes')))


### Remove any temporary files
file.remove(file.path(project_path, 'Data/Clean',  'Procedures', 'dta_procedures_temp.parquet'))
file.remove(file.path(project_path, 'Data/Clean',  'Procedures', 'dta_procedures_temp2.parquet'))





### GROUP.... -------------------------------------------------------------------------------------
### NOTE: We can summarise using group_by() and summarise_all() [as commented out], but takes up to x20 (sic!)
### longer than fsum(), while producing EXACTLY THE SAME RESULTS (CHECKED!)
gc()
dta_procedures = read_parquet(file.path(project_path, 'Data/Clean', 'Procedures',  'dta_procedures.parquet'))
patient_ecm_eligible = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_eligible_clinics.csv')) %>% mutate(id = as.character(id))
names(dta_procedures)



###  ...by month -------------------------------------------------------------------------------------
dta_procedures_month = fsum(dta_procedures %>% 
                              filter(year >= 2018) %>%  # NOTE: We don't want any pre-2018 on a monthly basis as of now
                              dplyr::select(-c(year_month_day)) %>%
                              group_by(id, treat_period, year, year_month, month))

# Make sure all patients are there for all years
dta_procedures_month = left_join(expand_grid(id = patient_ecm_eligible$id,
                                             dta_procedures_month %>% dplyr::select(treat_period, year, year_month, month) %>% distinct() %>% as.data.frame()) %>%
                                  group_by(id) %>% distinct() %>% ungroup(),
                                 dta_procedures_month) %>% 
  mutate(across(-c(id, treat_period, year, year_month, month), ~replace_na(.,0))) # 0's instead of NA's


### ...by year -------------------------------------------------------------------------------------
dta_procedures_year  = fsum(dta_procedures %>%
                              dplyr::select(-c(contains('month'), year_rel, treat_period)) %>%
                              group_by(id, year))

# Make sure all patients are there for all years
dta_procedures_year = left_join(expand_grid(id = patient_ecm_eligible$id,
                                            dta_procedures_year %>% dplyr::select(year) %>% distinct() %>% as.data.frame()) %>%
                                    group_by(id) %>% distinct() %>% ungroup(),
                                dta_procedures_year) %>% 
  mutate(across(-c(id, year), ~replace_na(.,0))) # 0's instead of NA's


### ...by year_rel -------------------------------------------------------------------------------------
dta_procedures_year_rel  = fsum(dta_procedures %>%
                              dplyr::select(-c(contains('month'), year, treat_period)) %>%
                              group_by(id, year_rel))

# Make sure all patients are there for all years
dta_procedures_year_rel = left_join(expand_grid(id = patient_ecm_eligible$id,
                                            dta_procedures_year_rel %>% dplyr::select(year_rel) %>% distinct() %>% as.data.frame()) %>%
                                  group_by(id) %>% distinct() %>% ungroup(),
                                dta_procedures_year_rel) %>% 
  mutate(across(-c(id, year_rel), ~replace_na(.,0))) # 0's instead of NA's


### ...by period (2018-2023)  -----------------------------------------------------------------------------------------------------------------
dta_procedures_period_18_23  = fsum(dta_procedures %>%
                                  filter(year >= 2018) %>%
                                  dplyr::select(-c(contains('month'), contains('year'))) %>%
                                  group_by(id, treat_period))


# Make sure all patients are there for all periods
dta_procedures_period_18_23 = left_join(expand_grid(id = patient_ecm_eligible$id,
                                        dta_procedures_period_18_23 %>% dplyr::select(treat_period) %>% distinct() %>% as.data.frame()) %>%
                                             group_by(id) %>% distinct() %>% ungroup(),
                                  dta_procedures_period_18_23) %>% 
  mutate(across(-c(id, treat_period), ~replace_na(.,0))) # 0's instead of NA's




### Save ------------------------------------------------------------------------------
write_parquet(dta_procedures_month, file.path(project_path, 'Data/Clean', 'Aggregated',  'Procedures_outcomes_month_18_23.parquet'))
write_parquet(dta_procedures_year,  file.path(project_path, 'Data/Clean','Aggregated',  'Procedures_outcomes_year_18_23.parquet'))
write_parquet(dta_procedures_year_rel,  file.path(project_path, 'Data/Clean', 'Aggregated', 'Procedures_outcomes_year_rel_18_23.parquet'))
write_parquet(dta_procedures_period_18_23,  file.path(project_path, 'Data/Clean', 'Aggregated', 'Procedures_outcomes_period_18_23.parquet'))






### Combine with diagnoses ------------------------------------------------------------------------------
dta_procedures_month = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Procedures_outcomes_month_18_23.parquet'))
dta_procedures_year = read_parquet(file.path(project_path, 'Data/Clean','Aggregated',  'Procedures_outcomes_year_18_23.parquet'))
dta_procedures_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Procedures_outcomes_year_rel_18_23.parquet'))
dta_procedures_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Procedures_outcomes_period_18_23.parquet'))


dta_diagnosis_month = read_parquet(file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_month_18_23.parquet'))
dta_diagnosis_year = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Diagnoses_outcomes_year_18_23.parquet'))
dta_diagnosis_year_rel = read_parquet(file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_year_rel_18_23.parquet'))
dta_diagnosis_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean','Aggregated',  'Diagnoses_outcomes_period_18_23.parquet'))


### Combine with prescriptions ------------------------------------------------------------------------------

dta_prescriptions_month = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_month_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .)) %>% mutate(across(c(id, year), as.character))
dta_prescriptions_year = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_year_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id, year), as.character))
dta_prescriptions_year_rel = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_year_rel_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id), as.character))
dta_prescriptions_period_18_23 = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'Prescriptions_outcomes_period_18_23.parquet')) %>% rename_all(~sub("_prescription$", "", .))%>% mutate(across(c(id), as.character))




### Save - procedures + diagnoses + prescriptions - together  ------------------------------------------------------------------------------
write_parquet(left_join(dta_diagnosis_month, left_join(dta_procedures_month, dta_prescriptions_month)) ,  
              file.path(project_path, 'Data/Clean', 'Aggregated', 'all_outcomes_month_18_23.parquet'))
write_parquet(left_join(dta_diagnosis_year,  left_join(dta_procedures_year, dta_prescriptions_year)) ,  
              file.path(project_path, 'Data/Clean', 'Aggregated', 'all_outcomes_year_18_23.parquet'))
write_parquet(left_join(dta_diagnosis_year_rel,  left_join(dta_procedures_year_rel, dta_prescriptions_year_rel)) ,  
               file.path(project_path, 'Data/Clean', 'Aggregated', 'all_outcomes_year_rel_18_23.parquet'))
write_parquet(left_join(dta_diagnosis_period_18_23,  left_join(dta_procedures_period_18_23, dta_prescriptions_period_18_23)) ,  
              file.path(project_path, 'Data/Clean', 'Aggregated', 'all_outcomes_period_18_23.parquet'))



end1 = Sys.time()
print(end1-start1)



#
# END OF CODE  ------------------------------------------------------------------------------
#