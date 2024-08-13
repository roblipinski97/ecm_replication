
#
# SET-UP --------------------------------------------------------------------------------------------------
#

### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  source('~/path/to/r_script/00_global.R')
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make a copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)

# 
# PATIENT ID'S --------------------------------------------------------------------------------------------------
#
### CREATE: Dataset listing ID codes (encrypted + ECM) for all patients (1,605,595 rows)

### Read files
id1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_1.csv"), encoding = 'UTF-8'))
id2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "Patient_ID_2.csv"), encoding = 'UTF-8'))

### Combine
id = rbindlist(list(id1, id2))

### Clean...
id = id %>% 
  dplyr::select(-c(v1)) %>%  # ... no need for 'v1' variable 
  rename('id' = 'patient_i_dencrypted', # ... rename columns 
         'id_ecm' = 'ecm_patient_id') %>%
  mutate(id = as.character(id), id_ecm = as.character(id_ecm)) #... treat as character

### Save
fwrite(id, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'id_codes.csv'), na = NA, row.names = F)


# '--------------------------------------------------------------------------------------------------
# CLINICS--------------------------------------------------------------------------------------------------
#
### CREATE: Clinic-level file with ECM inclusion status, QBS and management scores, number of lists (410 unique clinics)
# Note: QBS and management scores are provider/GP level variables, but here those variables measure clinic-level averages

### Read file
clinic_all = fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "Clinics.csv"), encoding = 'UTF-8') %>% janitor::clean_names()
clinic_codes = fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "clinic_list_112020.csv"), encoding = 'UTF-8') %>% janitor::clean_names()


### Clean...
clinic_all = clinic_all %>% 
  rename('ecm_include_clinic' = 'ecm_status_clinic_categorical', # ... rename column(s)
         'average_clinic_management_score' = 'management_score') %>% 
  mutate(ecm_include_clinic = case_when( # ... specify whether a given clinic was or wasn't participating in ECM
        ecm_include_clinic == '1' ~ 'ECM',
        ecm_include_clinic == '2' ~ 'Not ECM',
        ecm_include_clinic == '3' ~ 'Excluded (pilot)',
        is.na(ecm_include_clinic) & number_of_clinics_in_block >= 5 ~ 'Excluded (size>4)',
        is.na(ecm_include_clinic) & number_of_clinics_in_block == 1 ~ 'Excluded (single block)'
        ))


### Add clinic codes (this is different to the registration code and is necessary to identify clinic in the billing data)
clinic_all = left_join(clinic_all,
                       clinic_codes %>% rename('clinic_registration_code' = 'registration_clinic',
                                               'clinic_registration_code2' = 'code')) %>% 
                relocate(.after = 'clinic_registration_code', 'clinic_registration_code2')

#
# LISTS/GPs (ALL) --------------------------------------------------------------------------------------------------
#
### CREATE: List-level file with ECM status, GP code, provider-level QBS scores, and clinic data (766 unique lists)

### Read file
list_all = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'treat_control_clinic_gp.csv'), encoding = 'UTF-8'))

tapply(list_all$doctor_code, list_all$list_id, n_distinct) %>% summary
tapply(list_all$list_id, list_all$doctor_code, n_distinct) %>% summary


### Clean...
list_all = list_all %>% 
  rename('clinic_registration_code' = 'registration_clinic', #... rename columns 
         'gp_id' = 'doctor_code') %>% 
  dplyr::select(clinic_registration_code,  list_id, gp_id) # ...drop and re-order columns (clinic_name, pilot, number_list, treatment, and treatment_final are all clinic-level)
        
       
### Checks 
# •	do all clinics code occur in list_all?
sf(clinic_all$clinic_registration_code %in% unique(list_all$clinic_registration_code))
# • R: All 410 clinic codes from clinic_all occur in list_all

# •	are all list IDs unique?
n_distinct(list_all$list_id) == nrow(list_all)
# • R: Yes

### Merge with clinic_all  ------------------------------------------------------------------------------
list_all = left_join(list_all, clinic_all)

### Set ECM assignment for lists based on clinic status
list_all$ecm_include_list = list_all$ecm_include_clinic


### Add participation status ------------------------------------------------------------------------------

### Read ECM participation file (single column of list_id of participating lists)
list_ecm = clean_names(fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "GP_final_participation_list.csv"), encoding = 'UTF-8'))

### Checks
# • are all participating lists in 'list_all' dataset?
sf(list_ecm$list_id %in% unique(list_all$list_id))
list_ecm$list_id[!(list_ecm$list_id %in% unique(list_all$list_id))]
list_all[(list_all$list_id %in%list_ecm$list_id) & list_all$ecm_include_clinic != 'ECM']
# • R: No, N0083 doesn't match. Also, N0184 is coded as participating, but this is a list in excluded clinic

### N0083 =  Ljudmilla Juršis = 11691071
### N0083 did participate because it had the care plans evaluated and actually enrolled some patients ->
#### BUT it's not in a randomized clinic + seems to have only a single list + has no patients in ECM randomization file

### N0184 = Mall Joost = 10714427 / 14315115
### N0184 = in excluded clinic + only enrolled one patient + has no patients in ECM randomization file


### Create new column saying which lists actually included
list_all$ecm_status_list = ifelse(test = list_all$list_id %in% list_ecm$list_id, 
                                  yes = 'Participating', 
                                  no  = ifelse(list_all$ecm_include_list == 'ECM', 'Not participating', list_all$ecm_include_list))

table(list_all$ecm_include_list, list_all$ecm_status_list, useNA = 'ifany')


### Checks
# • any participating lists not in ECM clinics
list_all[list_all$ecm_status_list == 'Participating' & list_all$ecm_include_list != 'ECM',]
# • R: Yes, N0184 in Excluded (size>4) clinic, as described above

### NOTE: Code N0083 - leave out for now ------------------

### NOTE: Code N0184 as NOT participating ------------------
list_all$ecm_status_list[list_all$list_id == 'N0184'] = 'Excluded (size>4)'


### Status from data ?
# patient_ecm_accept = read_parquet(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', paste0( 'patient_ecm_accept_from_data.csv')))
# sf(unique(list_ecm$list_id) %in% unique(patient_ecm_accept$list_id))
# unique(list_ecm$list_id)[!(unique(list_ecm$list_id) %in% unique(patient_ecm_accept$list_id))]


### Add QBS (domain II) scores per lsit ---------------------------------------------------------------
### Read the file
qbs = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', 'QBS Need Adjustment Calculations.csv'), encoding = 'UTF-8'))

qbs[9, 'v76'] = 'qbs_II' # change cell value in row 9th of QBS score column (currently this rows holds proper column names, apart from this column)

### Clean...
qbs = qbs %>% 
  row_to_names(9) %>% # move selected row to column names
  rename( # rename columns
    'list_id' = 'Nimistu\nList',
    'clinic_registration_code' = 'Registrikood\nRegistration code'
  ) %>% 
  dplyr::select(c('clinic_registration_code', 'list_id', 'qbs_II')) %>% # select only necessary columns
  filter(grepl('^N', list_id)) %>%  # select only rows that start with 'N' on 'list_id'
  mutate(qbs_II = as.numeric(qbs_II)) # treat QBS scores as numeric

### Join to list_all
list_all = left_join(list_all, qbs %>% dplyr::select(c(list_id, qbs_II)))


### Exclude list_id of providers that terminated business, based on Meyhar's email from 06/11/2020, re-sent by Dan on 13/07/2023 -------------
exclude_id1 = c('N0248', 'N0664', 'N0799', 'N0652', 'N0653', 'N0252', 'N0596', 'N0452')
unique(list_all$list_id)[unique(list_all$list_id) %in% exclude_id1]

list_all = list_all %>% filter(!(list_id %in% exclude_id1))

n_distinct(list_all$clinic_registration_code) # N clinics
n_distinct(list_all$list_id) # N lists



### Save ---------------------------------
fwrite(clinic_all, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'clinic_all.csv'), na = NA, row.names = F)
fwrite(list_all, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'list_all.csv'), na = NA, row.names = F)


# ' -------------------------------
# PATIENTS (ECM EVALUATION) ----
#
### CREATE: Patient-level dataset with ECM inclusion (enrollment) status, classification, class code, list (6,865 rows)

### Read files
patient_ecm1 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_1_bothID.csv"), encoding = 'UTF-8'))
patient_ecm2 = clean_names(fread(file.path(project_path, 'Data', 'Raw', 'ECM Inclusion', "ECM_randomization_results_2_bothID.csv"), encoding = 'UTF-8'))

### Rename and delete columns as necessary before joining

# 'v1' and 'x' spare (as below); 'treatment' and 'treatment_final' are all 1 and 'list_onhold' is all NA, plus all 3 columns are missing from 'patient_ecm2'
patient_ecm1 = patient_ecm1 %>%  dplyr::select(-c(v1, x, treatment_final, treatment, list_onhold)) 
patient_ecm2 = patient_ecm2 %>% dplyr::select(-c(v1, x))

patient_ecm = rbind(patient_ecm1, patient_ecm2)
rm(list = c('patient_ecm1', 'patient_ecm2'))



### NOTE: What are 'miss_share' and 'pid' columns? I couldn't find a right dictionary that would explain those --------

### Clean...
patient_ecm = patient_ecm %>% rename('id' = 'patient_i_dencrypted',  # ... rename columns
                                       'id_ecm' = 'ecm_patient_id',
                                       'gp_id' = 'doctor_code',
                                       'clinic_registration_code' = 'registration_clinic',
                                       'ecm_include_patient' = 'treat_ecm') %>% 
                    mutate( id = as.character(id), id_ecm = as.character(id_ecm))  %>%  #... treat as character
                    mutate(class_code = ifelse(class_code == 'Mild/moderate', 'mild', 'severe')) %>%  # ....clean class_code
                    dplyr::select(c(list_id, id, id_ecm, # ...drop and re-order columns
                                    class_code, ecm_include_patient
                                    # number_related_illness # Don't include 'number_related_illness' as this is equivalent to 'comorbidities', which we have from 'patient_eligible' dataset(s)
                                    ))


### Rename grouping column(s)
patient_ecm = patient_ecm %>% 
  mutate(ecm_include_patient = case_when(
    ecm_include_patient == 'Not Enrolled' ~ 'Control',
    ecm_include_patient == 'Enrolled' ~ 'Treatment'))

### Checks
# • What are the ECM group sizes?
sf(patient_ecm$ecm_include_patient) 
# • R:  Enrolled - 2,414   Not Enrolled - 4,451

# • Are all lists in patient_ecm present in list_all (as ECM)
table(unique(patient_ecm$list_id) %in% unique(list_all$list_id[list_all$ecm_include_list == 'ECM']))
# • R: Yes, all 98 present


### Weights --------------------------------------------------

sf(patient_ecm$ecm_include_patient)
sf(patient_ecm$class_code)
n_distinct(patient_ecm$list_id)

patient_ecm = patient_ecm %>% 
                group_by(list_id, class_code) %>% 
                mutate(N = n(),
                       N_treat = sum(ecm_include_patient=='Treatment')) %>%
                ungroup() %>% 
                group_by(list_id, class_code, ecm_include_patient) %>%
                mutate(weight = N/n()) %>% ungroup() %>% 
                group_by(list_id, class_code) %>%
                mutate(N = n())

ggplot(patient_ecm,
      aes(N_treat/N, weight, 
          group = ecm_include_patient, fill = ecm_include_patient) ) +
  geom_line(aes(group = paste(list_id, class_code)),
                linetype= 'dashed', linewidth = .2, color = 'lightgrey')+
  geom_point(shape = 21, size = 3) +
  labs(
    x = '% treated in strata',
    y = 'Weight'
  ) +
  scale_y_continuous(expand = expansion(mult = c(0,.1)),
                     breaks = seq(0,12, 2)) +
  scale_x_continuous(expand = expansion(mult = c(0.02,.1)),
                    labels = percent_format()) +
  scale_fill_manual(name = '', values = c('grey80', 'grey20')) +
  guides(fill = guide_legend(override.aes = list(size = 7)))+
  theme(
    panel.grid = element_blank()
  )


### Save
fwrite(patient_ecm, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_randomized.csv'), na = NA, row.names = F)


#
# PATIENTS (ECM ELIGIBLE) ---------------------------------------------------
#

### CREATE: Patient-level dataset listing all ECM-eligible patients by clinic, list_id + with exclusion status (87,396 rows)

### Read data (from both May 2021 and November 2022)
patient_eligible_may21  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_may21.csv"), encoding = 'UTF-8'))
patient_eligible_nov22  = clean_names(fread(file.path(project_path, 'Data','Raw',  'ECM Inclusion', "ECM_eligible_nov22.csv"), encoding = 'UTF-8'))

### Remove all empty columns
patient_eligible_may21 <- patient_eligible_may21 %>% select_if(~!all(is.na(.)))
patient_eligible_nov22 <- patient_eligible_nov22 %>% select_if(~!all(is.na(.)))

### Note that there are couple other columns but they are not needed
### lisamise_poh_kood ('add poh code') - only ~ 600 patients have non-missing values
### pats_vajadused (pats = 'patient's'? + vajadused = 'needs') - text notes (on patient's goals?)
### pats_eesmargid (eesmargid = 'goals') - only 10 patients have non-missing values
### pats_su_kontakt (contact) - only a few patients have non-missing values
### pa_kontaktid (contact) - only a few patients have non-missing values
### kutse_edast_kp ('invitation forward') - only a few patients have non-missing values
### noustumine_kp ('ascent date?') - only a few patients have non-missing values
### jk_koosolek_kp ('jk date?') - only a few patients have non-missing values
### viimane_tel_k_kp ('last telephone') - only a few patients have non-missing values
### jargmine_visiit_kp ('next visit date') - only a few patients have non-missing values
### tegevusplaan ('action plan') - only 2 patients have non-missing values
### ravi_kandumine ('transfer of treatment') - only 1 patient has non-missing values
### haiglast_lahkumine_kp ('leaving the hospital') - only 1 patient has non-missing values


### Cleaning...
patient_eligible_may21 = patient_eligible_may21 %>%  
                    rename('list_id_may21' = 'nimistu', # ... rename columns
                          'list_id_new_may21' = 'uus_nimistu',
                          'id' = 'patsient',
                          'eligible_code_may21' = 'valjaarv_p_kood',
                          'inclusion_date_may21' = 'lisamise_kp',
                          'exclusion_date_may21' = 'valjaarv_kp'
                    ) %>% 
                    dplyr::select(c(id, list_id_may21, list_id_new_may21,  # ...drop and re-order columns
                                    eligible_code_may21, inclusion_date_may21, exclusion_date_may21)) %>%
                    mutate(eligible_patient = 'Eligible', #... create new 'eligible_patient' column
                           inclusion_date_may21 = dmy(inclusion_date_may21), # ...treat date column as dates
                           exclusion_date_may21 = dmy(exclusion_date_may21),
                           id = as.character(id)) #... treat as character
              
patient_eligible_nov22 = patient_eligible_nov22 %>%  
                    rename('list_id_nov22' = 'nimistu', # ... rename columns
                           'list_id_new_nov22' = 'uus_nimistu',
                           'id' = 'patsient',
                           'eligible_code_nov22' = 'valjaarv_p_kood',
                           'inclusion_date_nov22' = 'lisamise_kp',
                           'exclusion_date_nov22' = 'valjaarv_kp',
                           'comorbidities' = 'kaasuvaid_haiguseid' ### NOTE: Comorbidities are constant, so can take them from only one dataset (checked to be correct)
                    ) %>% 
                    dplyr::select(c(id, list_id_nov22, list_id_new_nov22, eligible_code_nov22,  # ...drop and re-order columns
                                    inclusion_date_nov22, exclusion_date_nov22, comorbidities)) %>%
                    mutate(eligible_patient = 'Eligible', #... create new 'eligible_patient' column
                           inclusion_date_nov22 = dmy(inclusion_date_nov22),
                           exclusion_date_nov22 = dmy(exclusion_date_nov22),
                           id = as.character(id)) #... treat as character


patient_eligible_nov22$eligible_code_nov22 %>% sf

### Checks 
# • do patients with new list entries always have at least a second entry?
# i.e. they have one row with distinct entries on list_id and list_id_new, corresponding to old and new list_id respectively, but they also 
# have at least one other entry, with non-empty list_id and empty list_id_new, corresponding to new list_id that was not updated further
id1 = unique(patient_eligible_may21$id[patient_eligible_may21$list_id_new_may21!=''])
temp = patient_eligible_may21  %>% filter(id %in% id1) %>% filter()  %>% group_by(id) %>% mutate(N=n())
# • R: Yes -> means list_id actually has updated IDs at rows for which its corresponding list_id_new is empty

### Ensure only the newest list is kept by keeping only those rows that are empty on 'list_id_new' column - so either not updated at all or if updated, the rows after the update
patient_eligible_may21 = patient_eligible_may21  %>% filter(list_id_new_may21 == '') %>% dplyr::select(-c(list_id_new_may21)) %>% distinct()
patient_eligible_nov22 = patient_eligible_nov22  %>% filter(list_id_new_nov22 == '') %>% dplyr::select(-c(list_id_new_nov22)) %>% distinct()



### Combine May 2021 and November 2022 by patient id
patient_eligible <- inner_join(patient_eligible_nov22, 
                               patient_eligible_may21 %>% dplyr::select(c(id, contains('may21'))),
                               by = 'id')


### Checks
# • if the same patients occur in both lists
sf(patient_eligible_nov22$id %in% patient_eligible_may21$id)
sf(patient_eligible_may21$id %in% patient_eligible_nov22$id)
patient_eligible_nov22$inclusion_date_nov22[!(patient_eligible_nov22$id %in% patient_eligible_may21$id)] %>% ymd %>% summary
# • R: No, 60 more patients in Nov-22, all included after ECM onset, i.e. after 01/06/2021.

### NOTE: Combine datasets, but exclude ID's only in November one -----------------------------------------------------------------
patient_eligible = inner_join(patient_eligible_nov22, 
                                patient_eligible_may21 %>% dplyr::select(c(id, contains('may21'))),
                              by = 'id') %>% 
                    dplyr::select(c(id, list_id_nov22, list_id_may21,  # ...drop and re-order columns
                                   eligible_code_nov22, eligible_code_may21, 
                                   inclusion_date_may21, inclusion_date_nov22,
                                   exclusion_date_may21, exclusion_date_nov22,
                                   eligible_patient, comorbidities))



### NOTE: Re-code exclusion code 'JVP92' appears to be assigned to ECM control patients and 'JVP99' to deceased patients -> nullify them? ----
### Deceased ones ONLY FOR MAY 2021 as we want their post-treatment outcomes
patient_eligible = patient_eligible %>% 
  mutate(eligible_code_may21 = ifelse(eligible_code_may21 %in% c('JVP92'), '', eligible_code_may21),
         eligible_code_nov22 = ifelse(eligible_code_nov22 %in% c('JVP92','JVP99'), '', eligible_code_nov22))


sf(patient_eligible$eligible_code_nov22)

### Checks
# • Are all entries unique?
nrow(patient_eligible) - n_distinct(patient_eligible$id)
# • R: No, one patient (ID=10461959) still coded twice, so select patients by earliest exclusion date
patient_eligible = patient_eligible %>% group_by(id) %>% distinct() %>% filter(exclusion_date_nov22 == min(exclusion_date_nov22) | is.na(exclusion_date_nov22))
nrow(patient_eligible) - n_distinct(patient_eligible$id)
# • R: Now all rows have unique patient ID's


### We will be guided by May 2021 list_id and exclusion codes 
### However, we might also  want to know whether patient excluded at any date -> then check for each patient if they have any exclusion code 
### for EITHER May 2021 or November 2022 and if they do code them as '1' on new 'excluded' column, summarizing by 'id' and 'list_id_may21'
### Note that it REALLY matters if we exclude from May only or from November  -----------------------------------------------
### (changes OLS mortality results + moves down ECM treatment patients from 1,785 to 1,728, with only 10-20 change for ECM control and pure control)



patient_eligible = patient_eligible %>% 
  mutate(excluded = ifelse(eligible_code_may21 != '', # | eligible_code_nov22 != '' , # ... define excluded observations
                           1, 0)) %>% 
  mutate(excluded_nov = ifelse(eligible_code_nov22 != '', 1, 0)) %>% 
  dplyr::select(c(list_id_may21, id, comorbidities, excluded, excluded_nov, eligible_code_nov22, exclusion_date_nov22)) # ... select only relevant columns


n_distinct(patient_eligible$list_id_may21) # N lists
nrow(patient_eligible) # N patients 

### Assign list_id-may21 to list_id and remove it
patient_eligible = patient_eligible %>% mutate(list_id = list_id_may21) %>% dplyr::select(-c(list_id_may21))

### Add 'id_ecm' column (need that for all patients)
patient_eligible = left_join(patient_eligible, id)





### Combine to 'patient_ecm', coding ECM inclusion at the patient level ------------------------------
patient_eligible = left_join(patient_eligible,
                             patient_ecm %>% dplyr::select(-c(id_ecm)),
                             by = 'id') %>% 
  mutate(ecm_include_patient = ifelse(is.na(ecm_include_patient), 'Pure control', ecm_include_patient))

patient_eligible$weight %>% is.na() %>% sf

### Checks
# • list_id_may21 match list_id (from patient_ecm)?
patient_eligible$id[(patient_eligible$list_id.x != patient_eligible$list_id.y)] %>% unique
# •  R: No, for one patient (ID=10642623) it doesn't match - list_id is coded as N0326 on patient_eligible and as N0325 on patient_ecm 
### Based on 'double_evaluation.csv' (OneDrive), this patient is in fact recorded on those two lists,
### but, as per the list_id/list_id_new cleaning of patient_eligible datasets above, the new list ID assigned to him
### is in fact N0326
### NOTE: adjust list_id manually -------------
patient_eligible = patient_eligible %>% dplyr::select(-c(list_id.y)) %>% rename('list_id' = 'list_id.x')
patient_eligible$list_id[patient_eligible$id == '10642623'] = 'N0326'


### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible1_all.csv'), na = NA, row.names = F)


### NOTE: Remove excluded patients -----------------------------------
patient_eligible = patient_eligible %>% filter(excluded == 0)


n_distinct(patient_eligible$list_id) # N lists
nrow(patient_eligible) # N patients
sf(patient_eligible$ecm_include_patient)

### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible2_no_excluded.csv'), na = NA, row.names = F)


### Combine with list_all -----------------------------------
patient_eligible = inner_join(patient_eligible, list_all)

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
unique(list_all$list_id)[!(unique(list_all$list_id) %in% unique(patient_eligible$list_id))] # Lists that dropped out
nrow(patient_eligible) # N patients


### Classify not participating clinics (i.e. going down from 93 to 72 and from 143 lists to 98) -----
id1 = unique(patient_eligible$clinic_registration_code[patient_eligible$ecm_include_patient %in% c('Control', 'Treatment')])

patient_eligible = patient_eligible %>% mutate(ecm_include_clinic = ifelse(
  test = ecm_include_clinic == 'ECM' &
          clinic_registration_code %in% id1,
  yes = 'ECM accept', 
  no = ifelse(ecm_include_clinic == 'ECM',
              'ECM refuse', ecm_include_clinic)))



# n_distinct(patient_eligible$clinic_registration_code[patient_eligible$ecm_include_patient != 'Pure control'])

### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible3_lists.csv'), na = NA, row.names = F)




### Exclude non-eligible clinics ------------------------------------

# How many non-eligible?
tapply(patient_eligible$clinic_registration_code,  patient_eligible$ecm_include_clinic, n_distinct)
tapply(patient_eligible$list_id,  patient_eligible$ecm_include_clinic, n_distinct)
tapply(patient_eligible$id,  patient_eligible$ecm_include_clinic, n_distinct)

# Exclude
patient_eligible = patient_eligible %>%  filter(ecm_include_clinic %in% c('ECM accept', 'ECM refuse', 'Not ECM'))

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
sf(patient_eligible$ecm_include_clinic %in% c('ECM', 'Not ECM')) # N clinics by group
nrow(patient_eligible) # N patients

### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_eligible_clinics.csv'), na = NA, row.names = F)

### Save (pure control) --------------------------------
patient_pure_control = patient_eligible %>% filter(ecm_include_clinic %in% c('Not ECM'))
fwrite(patient_pure_control, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible4_pure_control.csv'), na = NA, row.names = F)




### Exclude control clinics ------------------------------------
patient_eligible = patient_eligible %>%  filter(ecm_include_clinic %in% c('ECM accept', 'ECM refuse'))

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
nrow(patient_eligible) # N patients

### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible5_ecm_clinics.csv'), na = NA, row.names = F)



### Exclude lists without ECM patients ----------------------------------------
temp = patient_eligible



tapply(temp$clinic_registration_code, temp$ecm_include_clinic, n_distinct) # N clinics in accepting/refusing clinics
tapply(temp$list_id, temp$ecm_include_clinic, n_distinct) # N lists in accepting/refusing clinics
tapply(temp$id, temp$ecm_include_clinic, n_distinct) # N patients in accepting/refusing clinics

# N patients on lists in accepting clinics who have no ECM patients
n_distinct(temp$id[temp$list_id %in% unique(temp$list_id[temp$ecm_include_clinic == 'ECM accept' & !(temp$list_id %in% unique(temp$list_id[temp$ecm_include_patient %in% c('Control', 'Treatment')]))])])



patient_eligible = patient_eligible[patient_eligible$list_id %in% unique(patient_eligible$list_id[patient_eligible$ecm_include_patient != 'Pure control']),]

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
nrow(patient_eligible) # N patients

# Risk and ECM status of patients for both participating and non-participating lists
tapply(patient_eligible$id, paste(patient_eligible$ecm_include_clinic, patient_eligible$ecm_status_list, patient_eligible$class_code), n_distinct)
tapply(patient_eligible$id, paste(patient_eligible$ecm_include_clinic, patient_eligible$ecm_status_list, 
                                  patient_eligible$ecm_include_patient, patient_eligible$class_code), n_distinct)


### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible6_control_treat_lists.csv'), na = NA, row.names = F)


### Exclude non-ECM patients ----------------------------------------
sf(patient_eligible$ecm_include_patient) # N patients by ECM groups
patient_eligible = patient_eligible %>% filter((ecm_include_patient %in% c('Control', 'Treatment')))

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
nrow(patient_eligible) # N patients


sf(patient_eligible$ecm_include_patient)
sf(patient_eligible$class_code)

patient_eligible %>% ungroup() %>% count(ecm_status_list, class_code)
patient_eligible %>% ungroup() %>% count(ecm_status_list, ecm_include_patient, class_code)


### Save
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible7_control_treat_patients.csv'), na = NA, row.names = F)


### Exclude non-participating providers  ------------------------------------
patient_eligible = patient_eligible %>%  filter(ecm_status_list %in% c('Participating'))

n_distinct(patient_eligible$clinic_registration_code) # N clinics
n_distinct(patient_eligible$list_id) # N lists
nrow(patient_eligible) # N patients

### Save...
fwrite(patient_eligible, file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_eligible8_participating.csv'), na = NA, row.names = F)

patient_eligible %>% names

### Checks
# • Any patients without block_categorical?
# table(is.na(patient_eligible$block_categorical))
# • R: No, all non-missing



#
# END OF CODE ---------------------------------------------------
# 