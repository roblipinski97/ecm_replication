
#
# SET-UP-------------------------------------------------------------------
# 

start1 = Sys.time()

### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

### Make copy of the file -------------------------------------------------------------------
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


18.98*24.5

425/17.2


#
# PREPARE DATA ---------------------------------------------------------------------------------------------------
#

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible7_control_treat_patients.csv")) %>% mutate(id = as.character(id))

### ECM acceptance   ---------------------------------------------------------------------------------------------------
patient_ecm_accept = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_accept.csv')) %>% mutate(id = as.character(id))

### Combine --------------------------------------------------------------
patient_ecm_eligible = left_join(patient_ecm_eligible, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 0, 1))


### Aggregated outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))

### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes =  inner_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Disaggregated outcomes --------------------------------------------------------------------
dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses', 'dta_diagnosis.parquet'))

### Find first hospitalization date ---------------------------------------------------------

### Find minimum date of hospitalization per patient-period
temp = dta_diagnosis %>% 
  filter(n_inpatient_any == T | n_inpatient_any == 1) %>% # ... filter only positive instances of the varible
  filter(treat_period == 1) %>%  # ... in the post-treatment period
  dplyr::select(c(id, year_month_day)) %>% # ... only necessary columns
  group_by(id) %>%  # ... group by patient 
  filter(year_month_day == min(year_month_day)) %>% ungroup() %>%  # ... find first instance of the variable
  distinct() %>%  # .... leave only unique rows
  rename('dateofhospitalization' = 'year_month_day')

dta_diagnosis$n_inpatient_any %>% sf


### Combine with patient-group and ID data
dta_deaths = left_join(patient_ecm_eligible, temp)


### Prepare data for survival analysis -------------------------------------------------------------------

# Code for survival time + survival status (on both variables)
dta_deaths = dta_deaths %>% 
  mutate(
         status_death = ifelse(is.na(dateofdeath), 0, 1),
         time_death = ifelse(test = is.na(dateofdeath), 
                       yes = as.numeric(ymd(end_date1) - ymd(ecm_date)),
                       no  = as.numeric(ymd(dateofdeath) - ymd(ecm_date))),
         
         status_hosp = ifelse( is.na(dateofhospitalization), 0, 1),
         time_hosp = ifelse(test = is.na(dateofhospitalization), 
                             yes = as.numeric(ymd(end_date1) - ymd(ecm_date)),
                             no  = as.numeric(ymd(dateofhospitalization) - ymd(ecm_date))))



### Right-censor hospitalization survival by death date (i.e. patient survived until death only, even if not hospitalised)
dta_deaths$time_hosp[dta_deaths$status_hosp == 0 & dta_deaths$time_hosp > dta_deaths$time_death] =
    dta_deaths$time_death[dta_deaths$status_hosp == 0 & dta_deaths$time_hosp > dta_deaths$time_death]


# Drop any missing control values
dta_deaths = dta_deaths %>% filter(!is.na(age)) 


### Save object  -------------------------------------------------------------------
### It will be used to create Table 4 and Figures 2 + 3
dta_deaths_save = dta_deaths
saveRDS(dta_deaths_save, file.path(project_path, 'Data', 'Clean', 'Aggregated', 'dta_deaths_temp.RData'))

#
# LIFE EXPECTANCY  ------------------------------------------
#

### Before running the models, calculate changes in life expectancy between treatment and control
### conditions that are reported in the Introduction of the paper

dta_deaths = dta_deaths_save %>% 
  filter(ecm_include_patient %in% c('Control', 'Treatment')) %>%  # ... only leave Control and Treatment patients
  filter(ecm_status_list == 'Participating') # ... at participating providers

life_exp  = data.frame(ecm_include_patient = c(rep('Control', 1), rep('Treatment', 1)),
                       class_code = c('mild'),
                       age_start = c(0),
                       age_end = c(120)
)


i = 1
for(i in 1:nrow(life_exp)){
  
  temp = dta_deaths[dta_deaths$ecm_include_patient == life_exp$ecm_include_patient[i] &
                      dta_deaths$class_code == life_exp$class_code[i] &
                      dta_deaths$age > life_exp$age_start[i] &
                      dta_deaths$age <= life_exp$age_end[i]]
  
  n = nrow(temp)
  d = sum(temp$status_death)
  q = 100*d/n
  L = sum(temp$time_death) / 365.25
  m = 100*d/L
  e = L/n
  
  life_exp$alive_start[i] = n
  life_exp$deaths_n[i] = d
  life_exp$years_lived[i] = L
  life_exp$deaths_prob[i] = q
  life_exp$mortality_rate[i] = m
  life_exp$life_expectancy[i] = e
}


life_exp$life_expectancy[life_exp$ecm_include_patient == 'Treatment'] %>% round(3)
life_exp$life_expectancy[life_exp$ecm_include_patient == 'Control'] %>% round(3)

(80*(12/22)*(life_exp$life_expectancy[life_exp$ecm_include_patient == 'Treatment'] - 
               life_exp$life_expectancy[life_exp$ecm_include_patient == 'Control']))  %>% round(3)



#
# MODEL LOOP --------------------------------------------------------------------
#
### This loop runs all model specifications from Table 3 and saves them as .csv
dta_deaths = dta_deaths_save

# Re-level treatment variable
dta_deaths$ecm_include_patient = factor(dta_deaths$ecm_include_patient , levels = c('Pure control','Control', 'Treatment'))
dta_deaths$class_code = factor(dta_deaths$class_code , levels = c('severe', 'mild'))


# Ensure relevant variables are strings, not numbers
dta_deaths$block_categorical = as.character(dta_deaths$block_categorical)

tapply(dta_deaths$status_death, paste0(dta_deaths$ecm_include_patient, dta_deaths$class_code), summary)


### Run models for each table panel 
for(panel1 in c('a', 'b', 'c', 'd')){
  
  print(panel1)
  
  ### Remove any model objects before next iteration
  rm( list = Filter( exists, c('m1','m2', 'm3', 'm4', 'm5', 'm6')) ) 
  
  # Prepare basic sample data...
  temp = dta_deaths %>%
    filter(ecm_status_list == 'Participating') %>% filter(!(ecm_include_patient %in% c('Pure control'))) %>% # ... leaving only desired ECM groups
    mutate(ecm_include_patient = droplevels(ecm_include_patient))  # ... dropping spare levels (code might produce an error otherwise)
  
  # Prepare extended sample data...
  temp_extend = dta_deaths %>%
    filter(!(ecm_include_patient %in% c('Pure control'))) %>% # ... leaving only desired ECM groups
    mutate(ecm_include_patient = droplevels(ecm_include_patient))  # ... dropping spare levels (code might produce an error otherwise)
  
  
  ### Panel A ----------------------------------------------------------------------------------
  if(panel1 %in% c('a')){
    
    ##### (1) DV=Hosp + design (FE) -------------------------------------------------------------------
    m1 <- lm(status_hosp  ~ strata + ecm_include_patient,
             weights = weight, data = temp)
    
    m1 = coeftest(m1, cluster.vcov(m1, temp$list_id, df_correction = T))
    
    ##### (5) DV=Mortality + design (FE) -------------------------------------------------------------------
    m5 <- lm(status_death  ~ strata + ecm_include_patient,
             weights = weight, data = temp)
    m5 = coeftest(m5, cluster.vcov(m5, temp$list_id, df_correction = T))
    
    
    ##### (2) DV=Hosp + controls -------------------------------------------------------------------
    m2 <- lm(status_hosp  ~ strata + age + gender + ecm_include_patient,
             weights = weight, data = temp)
    m2 = coeftest(m2, cluster.vcov(m2, temp$list_id, df_correction = T))
    
    
    ##### (6) DV=Mortality + controls -------------------------------------------------------------------
    m6 <- lm(status_death  ~  strata + age + gender + ecm_include_patient,
             weights = weight,data = temp)
    m6 = coeftest(m6, cluster.vcov(m6, temp$list_id, df_correction = T))

    
    ##### (3) DV=Hosp ~ instrumental variables -------------------------------------------------------------------
    m3 = iv_robust(status_hosp ~ age + gender  +  ecm_status_patient |  age + gender + ecm_include_patient , 
                   cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = temp)
    
    
    ##### (7) DV=Mortality ~ instrumental variables -------------------------------------------------------------------
    m7 = iv_robust(status_death ~  age + gender  +  ecm_status_patient |  age + gender + ecm_include_patient , 
                   cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = temp)
    
    
    ##### (4) DV=Hosp + controls [EXTENDED] -------------------------------------------------------------------
    m4 <- lm(status_hosp  ~ strata + age + gender + ecm_include_patient * ecm_status_list,
             weights = weight,  data = temp_extend)
    m4 = coeftest(m4, cluster.vcov(m4, temp_extend$list_id, df_correction = T))
    

    ##### (8) DV=Mortality + controls  [EXTENDED] -------------------------------------------------------------------
    m8 <- lm(status_death  ~ strata + age + gender + ecm_include_patient * ecm_status_list,
             weights = weight,  data = temp_extend)
    m8 = coeftest(m8, cluster.vcov(m8, temp_extend$list_id, df_correction = T))
    
    ### Panel C ----------------------------------------------------------------------------------
    }else if(panel1 %in% c('c')){
      
      
      ##### (1) DV=Hosp + design (FE) -------------------------------------------------------------------
      m1 <- lm(status_hosp  ~ strata + ecm_include_patient * class_code,
               weights = weight, data = temp)
      
      m1 = coeftest(m1, cluster.vcov(m1, temp$list_id, df_correction = T))
      
      ##### (5) DV=Mortality + design (FE) -------------------------------------------------------------------
      m5 <- lm(status_death  ~ strata + ecm_include_patient * class_code,
               weights = weight, data = temp)
      m5 = coeftest(m5, cluster.vcov(m5, temp$list_id, df_correction = T))
      
      
      ##### (2) DV=Hosp + controls -------------------------------------------------------------------
      m2 <- lm(status_hosp  ~ strata + age + gender + ecm_include_patient * class_code,
               weights = weight, data = temp)
      m2 = coeftest(m2, cluster.vcov(m2, temp$list_id, df_correction = T))
      
      
      ##### (6) DV=Mortality + controls -------------------------------------------------------------------
      m6 <- lm(status_death  ~ strata + age + gender + ecm_include_patient * class_code,
               weights = weight, data = temp)
      m6 = coeftest(m6, cluster.vcov(m6, temp$list_id, df_correction = T))
      
      
      
      ##### (3) DV=Hosp ~ instrumental variables -------------------------------------------------------------------
      m3 = iv_robust(status_hosp ~ age + gender  +  ecm_status_patient + ecm_status_patient * class_code |  age + gender + ecm_include_patient + ecm_include_patient * class_code , 
                     cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = temp)
  
      ##### (7) DV=Mortality ~ instrumental variables -------------------------------------------------------------------
      m7 = iv_robust(status_death ~ age + gender  +  ecm_status_patient + ecm_status_patient * class_code |  age + gender + ecm_include_patient + ecm_include_patient * class_code , 
                     cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = temp)
  
      ##### (4) DV=Hosp + controls [EXTENDED] -------------------------------------------------------------------
      m4 <- lm(status_hosp  ~ strata + age + gender + ecm_include_patient * ecm_status_list * class_code,
               weights = weight,  data = temp_extend)
      m4 = coeftest(m4, cluster.vcov(m4, temp_extend$list_id, df_correction = T))
      
    
      ##### (8) DV=Mortality + controls  [EXTENDED] -------------------------------------------------------------------
      m8 <- lm(status_death  ~ strata + age + gender + ecm_include_patient * ecm_status_list * class_code,
               weights = weight,  data = temp_extend)
      m8 = coeftest(m8, cluster.vcov(m8, temp_extend$list_id, df_correction = T))

  ### Panel B -----------------------------------------------
  }else if(panel1 %in% c('b')){
    
    ##### (1) DV=Hosp + design (FE) -------------------------------------------------------------------
    m1 <- coxph(Surv(time_hosp, status_hosp) ~ strata + ecm_include_patient,
                cluster = list_id, weights = weight, data = temp)
    
  
    ##### (5) DV=Mortality + design (FE) -------------------------------------------------------------------
    m5 <- coxph(Surv(time_death, status_death) ~ strata +  ecm_include_patient,
                cluster = list_id, weights = weight, data = temp)
    
    ##### (2) DV=Hosp + controls -------------------------------------------------------------------
    m2 <- coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient,
                cluster = list_id, weights = weight, data = temp)
    
    ##### (6) DV=Mortality + controls -------------------------------------------------------------------
    m6 <- coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient,
                cluster = list_id,  weights = weight, data = temp)
    
    
    ##### (3) DV=Hosp ~ instrumental variables -------------------------------------------------------------------
    first1 <- glm(formula=ecm_status_patient ~ strata + age + gender + ecm_include_patient,
                  weights = weight,  data = temp)
    
    predict1 <- predict(first1)
    
    m3 <- coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + predict1,
                cluster = list_id, weights = weight,  data = temp)
    
    ##### (7) DV=Morality ~ instrumental variables -------------------------------------------------------------------
    m7 <- coxph(Surv(time_death, status_death) ~ strata + age + gender + predict1,
                cluster = list_id,weights = weight,   data = temp)
    
    
    ##### (4) DV=Hosp + controls [status_hosp] -------------------------------------------------------------------
    m4 <- coxph(Surv(time_hosp, status_death)  ~  strata + age + gender + ecm_include_patient * ecm_status_list,
             cluster = list_id,  weights = weight, data = temp_extend)
    
    ##### (8) DV=Mortality + controls  [EXTENDED] -------------------------------------------------------------------
    m8 <- coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient * ecm_status_list,
                cluster = list_id,  weights = weight, data = temp_extend)
    m8
    
  
    ### Panel D ---------------------------------------
    }else if(panel1 == 'd'){
      
      
      ##### (1) DV=Hosp + design (FE) -------------------------------------------------------------------
      m1 <- coxph(Surv(time_hosp, status_hosp) ~ strata + ecm_include_patient * class_code,
                  cluster = list_id, weights = weight, data = temp)
      
      ##### (5) DV=Mortality + design (FE) -------------------------------------------------------------------
      m5 <- coxph(Surv(time_death, status_death) ~ strata +  ecm_include_patient * class_code,
                  cluster = list_id, weights = weight, data = temp)
      
      ##### (2) DV=Hosp + controls -------------------------------------------------------------------
      m2 <- coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient * class_code,
                  cluster = list_id, weights = weight, data = temp)
      
      ##### (6) DV=Mortality + controls -------------------------------------------------------------------
      m6 <- coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient * class_code,
                  cluster = list_id,  weights = weight, data = temp)
      
      ##### (3) DV=Hosp ~ instrumental variables -------------------------------------------------------------------
      first1 <- glm(formula=ecm_status_patient ~ strata + age + gender + ecm_include_patient,
                    weights = weight,  data = temp)
      
      temp$ecm_status_patient2 = temp$ecm_status_patient * ifelse(temp$class_code == 'mild', 1,0)
      
      first2 <- glm(formula=ecm_status_patient2 ~ strata + age + gender + ecm_include_patient * class_code,
                    weights = weight,  data = temp)
      
      predict1 <- predict(first1)
      predict2 <- predict(first2)
      
      m3 <- coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender +  class_code + predict1 + predict2,
                  cluster = list_id, weights = weight,   data = temp)
      
      ##### (6) DV=Morality ~ instrumental variables -------------------------------------------------------------------
      m7 <- coxph(Surv(time_death, status_death) ~ strata + age + gender +  class_code + predict1 + predict2,
                  cluster = list_id, weights = weight,   data = temp)
      
      
      ##### (4) DV=Hosp + controls [EXTENDED] -------------------------------------------------------------------
      m4 <- coxph(Surv(time_hosp, status_hosp)  ~  strata + age + gender + ecm_include_patient * ecm_status_list * class_code,
                  cluster = list_id,  weights = weight, data = temp_extend)
      
      ##### (8) DV=Mortality + controls  [EXTENDED] -------------------------------------------------------------------
      m8 <- coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient * ecm_status_list  * class_code,
                  cluster = list_id,  weights = weight, data = temp_extend)
      m8
      
    } 
    
 
  ### Store all model results
  if(panel1 %in% c('a', 'b')){
    m_list <- list(extract_coeftest(m1, 1), extract_coeftest(m2, 3), extract_coeftest(m3, 3), #  extract_coeftest(m4, 5),
                   extract_coeftest(m5, 1), extract_coeftest(m6, 3), extract_coeftest(m7, 3)) #,  extract_coeftest(m8, 5))
  }else if(panel1 %in% c('c', 'd')){
    m_list <- list(extract_coeftest(m1, 2), extract_coeftest(m2, 4), extract_coeftest(m3, 5), # extract_coeftest(m4, 8),
                   extract_coeftest(m5, 2), extract_coeftest(m6, 4), extract_coeftest(m7, 5)) #,  extract_coeftest(m8, 8))
  }
  
  m_list <- m_list %>% reduce(full_join, by='var')
  m_list %>% print
  
  
  
  ### Rename variables and keep only the requires ones
  m_list = m_list %>% 
    mutate(var = dplyr::recode(var, 'predict1' = 'ecm_status_patient', 'predict2' = 'ecm_status_patient:class_codemild')) %>% 
    filter(var %in% c('ecm_include_patientTreatment',
                        'ecm_include_patientTreatment:ecm_status_listParticipating',
                        'ecm_status_patient', 'ecm_status_patient:class_codemild',
                        'ecm_include_patientTreatment:ecm_status_listParticipating:class_codemild',
                        #'ecm_status_listParticipating',
                        'class_codemild',
                        'ecm_include_patientTreatment:class_codemild',
                        'ecm_status_listParticipating:class_codemild',
                        'genderMale', 'age'))
  

  
  
  ### Final clean
  m_list=m_list %>%
    # ... ordering the rows
    arrange(factor(var, levels = c('ecm_include_patientTreatment',
                                   'ecm_status_patient', 'ecm_status_patient:class_codemild',
                                   'ecm_include_patientTreatment:ecm_status_listParticipating',
                                   'ecm_include_patientTreatment:ecm_status_listParticipating:class_codemild',
                                   'ecm_status_listParticipating',
                                   'class_codemild',
                                   'ecm_include_patientTreatment:class_codemild',
                                   'ecm_status_listParticipating:class_codemild',
                                   'genderMale', 'age'))) %>%
    # ... treating 'var' as character and everything as data.frame
    mutate(var = as.character(var)) %>%as.data.frame()
  
  

  ### Assign FE + means + N rows (only last panel)
  if(panel1 == 'd'){
    m_list[nrow(m_list)+1, ] = c('\\textbf{FE}', 'Strata', 'Strata', 'Strata',  'Strata', 'Strata', 'Strata') #, 'Strata', 'Strata')
    m_list[nrow(m_list)+1, ] = c('$\\hat{x}_{\\text{control}}$', 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])),
                                 #round_flex(mean_miss(temp_extend$status_death[temp_extend$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control']))
                                 #round_flex(mean_miss(temp_extend$status_death[temp_extend$ecm_include_patient == 'Control']))
                                 )
    m_list[nrow(m_list)+1, ] = c('\\textbf{N}', 
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','), 
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','), 
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','), 
                                 #prettyNum(length(temp_extend$status_hosp[temp_extend$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','), 
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','), 
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ','),
                                 prettyNum(length(temp$status_hosp[temp$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ',')
                                 #prettyNum(length(temp_extend$status_hosp[temp_extend$ecm_include_patient %in% c('Control','Treatment')]), big.mark = ',')
                                 )
  }

  ### Save
  assign(paste0('m_panel_', panel1), m_list)
  fwrite(m_list, file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths', panel1, '.csv')))
  
}


#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#
### This section takes the .csv saved above and converts it into a text snipped that can be simply copy-pasted
### as a body of a table in Overleaf


# Re-read
m_panel_a = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths', 'a', '.csv'))) %>%   
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var)) %>% 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) # collapsing ECM assignment/uptake into a single row

m_panel_b = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths', 'b', '.csv'))) %>% 
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var)) %>% 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) # collapsing ECM assignment/uptake into a single row

m_panel_c = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths', 'c', '.csv'))) %>% 
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var)) %>% 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) # collapsing ECM assignment/uptake into a single row

m_panel_d = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths', 'd', '.csv'))) %>%   
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var),
         id = row_number()) %>% mutate(id = ifelse(var == 'age', id - 1.5, id)) %>%  # adjust for age to ensure the same order of variables 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) %>% ungroup() %>%  # collapsing ECM assignment/uptake into a single row
  arrange(as.numeric(gsub(' .*','', id))) %>% dplyr::select(-c(id))



# Combine the results
dta_table = rbindlist(list(m_panel_a, m_panel_b, m_panel_c, m_panel_d))
dta_table

# Remove risk coefficient
dta_table = dta_table %>% filter(!(var %in% 'class_codemild')) %>% filter(!grepl('strata', var, ignore.case=T))

# Clean column names
dta_table = dta_table %>% mutate(var  = dplyr::recode(var, 
                                                      'age' = 'Age (years)',
                                                      'genderMale' = 'Sex (male)',
                                                      'class_codemild' = 'Mild risk',
                                                      'ecm_status_listParticipating' = 'GP participate',
                                                      'ECM patient:ecm_status_listParticipating' = 'ECM assigned x GP participate',
                                                      'ECM patient:ecm_status_listParticipating:class_codemild' = 'ECM assigned x GP participate x Mild risk',
                                                      'ECM patient:class_codemild' = 'ECM assigned x Mild risk',
                                                      'ecm_status_listParticipating:class_codemild' = 'GP participate x Mild risk'))


# Substitute all missing for dashes
dta_table[is.na(dta_table)] = '$-$'
dta_table[dta_table == ''] = '$-$'



# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table, 1, paste, collapse = "&"), '\\')


# Add section headings
dta_table = dta_table %>% add_row(.before = 1) %>% add_row(.before = 5) %>% add_row(.before = 9)  %>% add_row(.before = 14)

dta_table$cell1[c(1)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel A: Pooled OLS', '}}\\')
dta_table$cell1[c(5)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel B: Pooled Cox Proportional-Hazards', '}}\\')
dta_table$cell1[c(9)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel C: Interacted OLS', '}}\\')
dta_table$cell1[c(14)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel D: Interacted Cox Proportional-Hazards', '}}\\')


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)



### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}




### Means ------------------------------------------------------------------
tapply(dta_deaths$status_hosp, dta_deaths$ecm_status_patient, function(x) pr(x)/100)
tapply(dta_deaths$status_death, dta_deaths$ecm_status_patient, function(x) pr(x)/100)
tapply(dta_deaths$status_death, paste0(dta_deaths$ecm_include_patient, ' - ', dta_deaths$class_code), function(x) pr(x)/100)





#
# END OF CODE ------------------------------------------------------------------------------------------------------
#