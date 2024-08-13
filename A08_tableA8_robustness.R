

#
# SET-UP ---------------------------------------------------------------------------------------------------
# 

### Clean the environment
# rm(list=ls())
# gc()
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



### NOTES ---------------------------------------------------------------------------------------------------
# Materials:
# -> Randomization inference p-values + bootstrapped standard errors -> https://jasonkerwin.com/nonparibus/2017/09/25/randomization-inference-vs-bootstrapping-p-values/
# -> Randomization inference p-values  -> https://dimewiki.worldbank.org/Randomization_Inference#:~:text=The%20randomization%20inference%20p%2Dvalue,than%20the%20estimated%20treatment%20effect.

# -> Bonferroni + Benjamini-Hochberg procedure ->https://www.r-bloggers.com/2023/07/the-benjamini-hochberg-procedure-fdr-and-p-value-adjusted-explained/ + https://stats.libretexts.org/Bookshelves/Applied_Statistics/Biological_Statistics_(McDonald)/06%3A_Multiple_Tests/6.01%3A_Multiple_Comparisons

# -> Romano-Wolf (theory) -> Clarke, Romano and Wolf (2020) The Romano-Wolf Multiple Hypothesis Correction in Stata -> in 'Literature'

#
# PREPARE DATA ---------------------------------------------------------------------------------------------------
#

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))

# Leave only control and treatment
patient_ecm_eligible    = patient_ecm_eligible %>% filter(ecm_include_patient %in% c('Control', 'Treatment')) 


# Leave only ECM treatment and control (at participating providers) and pure control as per Ben's outline for 01/12/2023
patient_ecm_eligible = patient_ecm_eligible %>% 
  filter(ecm_status_list == 'Participating' & ecm_include_patient %in% c('Control', 'Treatment')) 

### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))

### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes =  inner_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


#
# RANDOMIZE TREATMENT ---------------------------------------------------------
#

### Re-randomize anew? --------------------------------------------------------------

anew = F

start1 = Sys.time()

if(anew){
  
  if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
  
  ### Leave only the required columns
  patient_ecm_eligible = patient_ecm_eligible %>%  dplyr::select(c(list_id, class_code, id, ecm_include_patient)) 
  
  ### Extract original treatment group numbers per list
  n_org =   patient_ecm_eligible %>% 
    filter(ecm_include_patient == 'Treatment') %>% 
    group_by(list_id, class_code) %>% mutate(N=n()) %>% ungroup() %>% 
    dplyr::select(c(list_id, class_code, N)) %>% distinct() 
  
  # Create new object to store the results
  temp_list <- vector("list", n1)
    
  # Ensure we get the same result each time
  set.seed(seed1) 
  
  ### Loop adding re-randomizations
  for(i in 1:n1){
    print(i)
    
    # Take sample of N size (as defined above per each list+class_code combination)
    rand1 <- patient_ecm_eligible %>%
      inner_join(n_org) %>%
      group_by(list_id, class_code) %>%
      sample_n(size = first(N)) %>%
      ungroup()
    
    # Add the results to the list - patients sampled above classified as Treatment, others as Control
    temp_list[[i]] =  ifelse(patient_ecm_eligible$id %in% rand1$id, 'Treatment', 'Control')
  
  }
  
  # Save
  saveRDS(temp_list, file.path(project_path, 'Data', 'Clean', 'Re-randomizations', paste0('randp', n1, ' (seed ', seed1,  ').RData')))
  
  library(tidylog) # Re-load tidylog
}

end1 = Sys.time()
end1-start1


#
# RANDOMIZATION INFERENCE P-VALUES (FIX_EST) ------------------------------------------------------------------------------------------
#

### (Re-)read the randomization data 
patient_ecm_eligible_randp = readRDS(file.path(project_path, 'Data', 'Clean', 'Re-randomizations', paste0('randp', n1, ' (seed ', seed1,  ').RData')))

### Prepare the outcomes billing data 
dta_reg_save = dta_outcomes %>% 
  filter(treat_period == 1) %>%  #... focusing on the treatment period
  dplyr::select(c(strata, list_id, class_code, weight, age, gender, ecm_include_patient, vars_main)) %>% 
  mutate(across(vars_main, ~Winsorize(as.numeric(.), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7 ))) %>% 
  mutate(across(vars_main, ~as.numeric(.) * 12/22)) %>% 
  filter(ecm_include_patient %in% c('Control', 'Treatment')) # ..filtering the groups


### Function to assign placebo treatment and re-estimate all the models
randp = function(n2, type1){
  
  min_n = min(as.numeric(gsub('[^0-9]', '', list.files(file.path(project_path, 'Data', 'Clean', 'Re-randomizations'), pattern = paste0('_', type1)))))
  if(is_empty(min_n) | is.infinite(min_n)){min_n = -1}
  if(min_n == n1){return()}
  
  for(treat1 in c((min_n+1):n2)){
    
    # Control the loop
    print(treat1)

    # Convert to 0/1 dummies
    if(type1 == 'dummy'){ 
      dta_reg = dta_reg_save %>% 
        mutate(across(vars_main, ~ifelse(. > 0, 1, 0))) 
    }else{
      dta_reg = dta_reg_save
    }
    
    ### Assign placebo treatment variable (unless treat1 = 0, then estimate regular regression model)
    if(treat1 == 0){
      dta_reg$ecm_include_patient_randp = dta_reg$ecm_include_patient 
    }else{
      dta_reg$ecm_include_patient_randp = patient_ecm_eligible_randp[[treat1]]  %>% as.vector()
    }
    
    ### Re-calculate weights
    dta_reg = dta_reg %>% 
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n())
    
    
    ### Fit the model
    fit <- feols(c(n_primary_your, n_outpatient_your, consult_care_plan, consult_include, consult_gp, consult_gp_phone, 
                   consult_nurse, consult_nurse_phone, consult_any_your, n_primary_not_your, n_outpatient_not_your,
                   n_inpatient_any, n_daycare_any, n_inpatient_post_any, n_outpatient_post_any, admit_ambulance,
                   readmit_30_any, readmit_90_any, heart_failure_any, stroke_any, myocardial_infarction_any, diabetes_2_any,
                   hyperlipidemia, weight_high, covid_incidence, covid_vaccine, m_glicihem_riigi, m_creatinine_riigi, 
                   m_chol_tri_riigi, m_glucose_riigi, m_thc_riigi, p_a_diabetes, p_c_hypertensive, p_c_beta_blockers, 
                   p_statins, p_key, p_other) ~ csw(factor(strata) + age + gender + ecm_include_patient_randp),
                 weights = dta_reg$weight,
                 data = dta_reg,
                 cluster = ~list_id,
                 ssc = ssc(cluster.adj = T)
    )
    
    if(treat1 == (min_n+1)){
      if(min_n+1 == 0){
        dta = data.frame(gsub('lhs: ', '', names(fit)), treat1, coef(fit)['ecm_include_patient_randpTreatment']) %>% rename_all(~c('var', 'iteration', type1))
      }else{
        dta = fread(file.path(project_path, 'Data', 'Clean', 'Re-randomizations', 
                    list.files(file.path(project_path, 'Data', 'Clean', 'Re-randomizations'), pattern = paste0('_', type1))[1]))
      }
    }else{
      dta = rbind(dta, data.frame(gsub('lhs: ', '', names(fit)), treat1, coef(fit)['ecm_include_patient_randpTreatment']) %>% rename_all(~c('var', 'iteration', type1)))
    }

    if(treat1%%250==0){
      fwrite(dta, file.path(project_path,  'Data','Clean', 'Re-randomizations', paste0( 'randp_reg_', treat1, '_', type1 ,'.csv')))
      if(file.exists(file.path(project_path,  'Data','Clean', 'Re-randomizations', paste0( 'randp_reg_', treat1-250, '_', type1 ,'.csv')))){
        file.remove(file.path(project_path,  'Data','Clean', 'Re-randomizations', paste0( 'randp_reg_', treat1-250, '_', type1 ,'.csv')))
      }
    }
    
   }
  # return(dta)
}


### Re-run anew? --------------------------------------------------------------
anew2 = T


if(anew2){
  randp(n1, 'count')
  randp(n1, 'dummy')
}

temp = left_join(fread(file.path(project_path,  'Data','Clean', 'Re-randomizations', 'randp_reg_10000_count.csv')),
                 fread(file.path(project_path,  'Data','Clean', 'Re-randomizations', 'randp_reg_10000_dummy.csv')))

dta_randp = temp %>%
  group_by(var) %>%
  summarize(randp_count = sum(if_else(abs(count) > first(abs(count)), 1, 0))/(n()-1),
            randp_dummy = sum_miss(if_else(abs(dummy) > first(abs(dummy)), 1, 0))/(n()-1))




#
# ROMANO-WOLF P-VALUES -----------------------------------------------------------------------------------------
#

# vars1 = vars_main[grepl('^n_', vars_main)]


for(type1 in c('count', 'dummy')){

  # Convert to 0/1 dummies
  if(type1 == 'dummy'){ 
    dta_reg = dta_reg_save %>% 
      mutate(across(vars_main, ~ifelse(. > 0, 1, 0))) 
  }else{
    dta_reg = dta_reg_save
  }
    
  fit <- feols(c(n_primary_your, n_outpatient_your, consult_care_plan, consult_include, consult_gp, consult_gp_phone, 
                 consult_nurse, consult_nurse_phone, consult_any_your, n_primary_not_your, n_outpatient_not_your,
                 n_inpatient_any, n_daycare_any, n_inpatient_post_any, n_outpatient_post_any, admit_ambulance,
                 readmit_30_any, readmit_90_any, heart_failure_any, stroke_any, myocardial_infarction_any, diabetes_2_any,
                 hyperlipidemia, weight_high, covid_incidence, covid_vaccine, m_glicihem_riigi, m_creatinine_riigi, 
                 m_chol_tri_riigi, m_glucose_riigi, m_thc_riigi, p_a_diabetes, p_c_hypertensive, p_c_beta_blockers, 
                 p_statins, p_key, p_other) ~ csw(factor(strata) + age + gender + ecm_include_patient),
               weights = dta_reg$weight,
               data = dta_reg,
               cluster = ~list_id,
               ssc = ssc(cluster.adj = T)
               )
  
  
  ### Get Romano-Wolf p-values
  library(wildrwolf)
  res_rwolf <- wildrwolf::rwolf(
    models = fit,
    param = "ecm_include_patientTreatment", 
    B = n1, 
    seed = seed1
  )
  
  if(type1 == 'count'){
    temp =  res_rwolf %>% add_column(.after = 0, vars_main) %>% rename_all(~c('var', 'model_cross',
                                                                              'beta_cross', 'se_cross', 't_value_cross', 'p_value_cross', 'romano_cross'))
  }else{
    temp =  left_join(temp %>% dplyr::select(c(var, beta_cross, p_value_cross, romano_cross)),
                      res_rwolf %>% add_column(.after = 0, vars_main) %>% rename_all(~c('var', 'model_dummy', 'beta_dummy',
                                                                                        'se_dummy', 't_value_dummy', 'p_value_dummy', 'romano_dummy')) %>% 
                      dplyr::select(c(var, beta_dummy, p_value_dummy, romano_dummy)))
  }

}
?rwolf
### Checks - beta values agree with i) 'fit' object fitted to the model argument ii) cross-section models?
# data.frame(dv = gsub('lhs: ', '', names(fit)), beta = coef(fit)['ecm_include_patientTreatment'])
# m1 = lm(n_outpatient_your ~  factor(strata)  + age + gender + ecm_include_patient, weights = weight, data = dta_reg) 
# m1 = coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T))
### R: Yes, both i) and ii) agree



### Join randomization inference and Romano-Wolf values  +  re-order columns
dta_randp_romano = left_join(dta_randp, temp) %>% 
  as.data.frame()



#
# BENJAMINI-HOCHBERG PROCEDURE -----------------------------------------------------------------------------------------
#

dta_randp_romano_bh = dta_randp_romano

dta_randp_romano_bh$bh_dummy = p.adjust(dta_randp_romano$p_value_dummy, method = 'BH')
dta_randp_romano_bh$bh_count = p.adjust(dta_randp_romano$p_value_cross, method = 'BH')

### Re-order columns
dta_randp_romano_bh = dta_randp_romano_bh %>% 
  dplyr::select(var, beta_dummy, beta_cross, p_value_dummy, p_value_cross, bh_dummy, bh_count,
                randp_dummy, randp_count, romano_dummy, romano_cross)
  

### Manual checks of the procedures
b = b = 0.551  # p-value
i = 30 # rank
m = 37 # number of tests

(b * m) / (i)

?p.adjust

### Checks: any p-values outside of 0-1 range?
# temp = as.data.frame(dta_randp_romano_bh)
# temp[12,5] = 1.2
# sapply(temp %>%dplyr::select(c(p_value_dummy, p_value_cross, bh_dummy, bh_count,
#                            randp_dummy, randp_count, romano_dummy, romano_cross)) , function(x) any(x < 0 | x > 1))
# R: No, all good


### Save
fwrite(dta_randp_romano_bh, file.path(project_path,  'Tables','CSV', 'all', paste0( 'tableA8_robustness', n1,'.csv')))



#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### Re-read the table --------------------------------------------------------------------------------------------------
dta_table_save = fread(file.path(project_path, 'Tables','CSV', 'all', paste0( 'tableA8_robustness', n1,'.csv')))
dta_table_save

### Re-order columns
dta_table_save = dta_table_save %>% relocate(c(randp_dummy, randp_count), .after = romano_cross)

### Clean coefficients -----------------------------------------------------------------------------------------------------
dta_table = dta_table_save %>%
  as.data.frame() %>% # as data.frame
  mutate(across(-var, ~round(.,3))) %>% # round 
  mutate(across(-c(var, contains('beta')), ~sig_stars(.))) # add sig. stars 
  

# ECM consultation codes - no ANCOVA
dta_table[grepl('consult_include|consult_care_plan|consult_refuse', dta_table$var),
          which(grepl('ancova', names(dta_table)))]  = '$-$'

# Anything empty
dta_table[dta_table == ''] = '$-$'


### Clean based on dictionary of outcomes ---------------------------------------------------------------------------------------------------

### Combine with dictionary
dta_table = left_join(dta_table, 
                      dict_outcomes %>% dplyr::select(c(var,name, group, order, nr))) %>%
  arrange(order, nr) %>% 
  relocate(.,c('group','name'), .after = 1) %>% 
  filter(!is.na(group) | grepl('textbf', var))

dta_table$name[grepl('textbf', dta_table$var)] = dta_table$var[grepl('textbf', dta_table$var)]
dta_table$group[grepl('textbf', dta_table$var)] = '-'


### Drop spare columns
dta_table = dta_table %>% dplyr::select(-c(var, order, nr))


# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')


# Add section headings
for(group1 in unique(dta_table$group)){
  if(group1 == '-'){next}
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{9}{r{.99\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)

### Replace 0's with <0.001
dta_table$cell1 = gsub('0$^{***}$', '<0.001$^{***}$', dta_table$cell1, fixed=T)

### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


#
# END OF CODE  ---------------------------------------------------------------------------------------------------
#