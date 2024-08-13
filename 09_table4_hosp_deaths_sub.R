
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


### Re-load data   --------------------------------------------------------------------
dta_deaths = readRDS(file.path(project_path, 'Data', 'Clean', 'Aggregated', 'dta_deaths_temp.RData'))


#
# MODEL LOOP --------------------------------------------------------------------
#
### This loop runs all model specifications from Table 4 and saves them
### as .csv

# Re-level treatment variable
dta_deaths$ecm_include_patient = factor(dta_deaths$ecm_include_patient , levels = c('Pure control','Control', 'Treatment'))
dta_deaths$class_code = factor(dta_deaths$class_code , levels = c('severe', 'mild'))


# Ensure relevant variables are strings, not numbers
dta_deaths$block_categorical = as.character(dta_deaths$block_categorical)


### Run models for each table panel 
for(panel1 in c('a', 'b', 'c', 'd')){
  
  
  print(panel1)
  ### Remove any model objects before next iteration
  rm( list = Filter( exists, c('m1','m2', 'm3', 'm4', 'm5', 'm6')) ) 
  
  # if(panel1 %in% c('a')){formula1 = 'status_hosp  ~ strata + age + gender + ecm_include_patient'}
  # if(panel1 %in% c('c')){formula1 = 'status_hosp  ~ strata + age + gender + ecm_include_patient * class_code'}
  # if(panel1 %in% c('b')){formula1 = 'Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient'}
  # if(panel1 %in% c('d')){formula1 = 'Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient * class_code'}

  # Prepare basic sample data..
  temp = dta_deaths %>%
    filter(ecm_status_list == 'Participating') %>% filter(!(ecm_include_patient %in% c('Pure control'))) %>% # ... leaving only desired ECM groups
    mutate(ecm_include_patient = droplevels(ecm_include_patient))  # ... dropping spare levels (code might produce an error otherwise)
  
  # Prepare extended sample data...
  temp_extend = dta_deaths %>%
    filter(!(ecm_include_patient %in% c('Pure control'))) %>% # ... leaving only desired ECM groups
    mutate(ecm_include_patient = droplevels(ecm_include_patient))  # ... dropping spare levels (code might produce an error otherwise)
  
  
  ### Panel A ----------------------------------------------------------------------------------
  if(panel1 %in% c('a')){
    
    temp = temp %>% filter(class_code == 'mild')
    temp_extend = temp_extend %>% filter(class_code == 'mild')
    
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
      
      temp = temp %>% filter(class_code == 'severe')
      temp_extend = temp_extend %>% filter(class_code == 'severe')
      
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

  ### Panel B -----------------------------------------------
  }else if(panel1 %in% c('b')){
    
    temp = temp %>% filter(class_code == 'mild')
    temp_extend = temp_extend %>% filter(class_code == 'mild')
    
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
      
      temp = temp %>% filter(class_code == 'severe')
      temp_extend = temp_extend %>% filter(class_code == 'severe')
      
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
      
    } 
    
 
  ### Store all model results
  m_list <- list(extract_coeftest(m1, 1), extract_coeftest(m2, 3), extract_coeftest(m3, 3), #  extract_coeftest(m4, 5),
                   extract_coeftest(m5, 1), extract_coeftest(m6, 3), extract_coeftest(m7, 3)) #,  extract_coeftest(m8, 5))
  
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
  

  
  
  ### Collapse again
  m_list=m_list %>%
  #   group_by(var) %>%
  #   summarise(across(everything(), ~ ifelse(all(is.na(.)), NA, first(na.omit(.)))))  %>% 
    arrange(factor(var, levels = c('ecm_include_patientTreatment',
                                   'ecm_status_patient', 'ecm_status_patient:class_codemild',
                                   'ecm_include_patientTreatment:ecm_status_listParticipating',
                                   'ecm_include_patientTreatment:ecm_status_listParticipating:class_codemild',
                                   'ecm_status_listParticipating',
                                   'class_codemild',
                                   'ecm_include_patientTreatment:class_codemild',
                                   'ecm_status_listParticipating:class_codemild',
                                   'genderMale', 'age'))) %>%
    mutate(var = as.character(var)) %>%
    as.data.frame()
  
  
  
  
  ### Assign FE + means + N (after panel B and D)
  if(panel1 == 'b'){
    m_list[nrow(m_list)+1, ] = c('$\\hat{x}_{\\text{control}}$', 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_hosp[temp$ecm_include_patient == 'Control'])),
                                 #round_flex(mean_miss(temp_extend$status_death[temp_extend$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control'])), 
                                 round_flex(mean_miss(temp$status_death[temp$ecm_include_patient == 'Control']))
                                 # round_flex(mean_miss(temp_extend$status_death[temp_extend$ecm_include_patient == 'Control'])
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
  fwrite(m_list, file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths_sub', panel1, '.csv')))
  
}


#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#
### This section takes the .csv saved above and converts it into a text snipped that can be simply copy-pasted
### as a body of a table in Overleaf


# Re-read
m_panel_a = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths_sub', 'a', '.csv'))) %>%   
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var)) %>% 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) # collapsing ECM assignment/uptake into a single row

m_panel_b = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths_sub', 'b', '.csv'))) %>% 
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var),
         id = row_number()) %>% mutate(id = ifelse(var == 'age', id - 1.5, id)) %>%  # adjust for age to ensure the same order of variables 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) %>% ungroup() %>%  # collapsing ECM assignment/uptake into a single row
  arrange(as.numeric(gsub(' .*','', id))) %>% dplyr::select(-c(id))

m_panel_c = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths_sub', 'c', '.csv'))) %>% 
  mutate(var  = gsub('ecm_include_patientTreatment|ecm_status_patient', 'ECM patient', var)) %>% 
  group_by(var) %>% summarize(across(everything(), ~ paste(na.omit(.), collapse = " "))) # collapsing ECM assignment/uptake into a single row

m_panel_d = fread(file.path(project_path,  'Tables','CSV', paste0('table3_hosp_deaths_sub', 'd', '.csv'))) %>%   
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

dta_table

# Substitute all missing for dashes
dta_table[is.na(dta_table)] = '$-$'
dta_table[dta_table == ''] = '$-$'



# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table, 1, paste, collapse = "&"), '\\')

dta_table

# Add section headings
dta_table = dta_table %>% add_row(.before = 1) %>% add_row(.before = 5) %>% add_row(.before = 11)  %>% add_row(.before = 15)

dta_table$cell1[c(1)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel A: Pooled WLS (mild-risk patients)', '}}\\')
dta_table$cell1[c(5)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel B: Pooled Cox Proportional-Hazards (mild-risk patients)', '}}\\')
dta_table$cell1[c(11)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel C: Pooled WLS (severe-risk patients)', '}}\\')
dta_table$cell1[c(15)] = paste0('\\multicolumn{7}{r{.89\\textwidth}}{\\textbf{',  'Panel D: Pooled Cox Proportional-Hazards (severe-risk patients)', '}}\\')


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)



### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}



#
# END OF CODE ------------------------------------------------------------------------------------------------------
#