

#
# SET-UP ---------------------------------------------------------------------------------------------------
# 

start1 = Sys.time()

### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


#
# PREPARE DATA ---------------------------------------------------------------------------------------------------
#

### The whole script is a close parallel to '07_table2_and_A56_cross.R', with only small differences stemming from the
### model set-up with interaction terms here


### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))

### Leave only ECM treatment and control (at participating providers) and pure control as per Ben's outline for 01/12/2023
patient_ecm_eligible = patient_ecm_eligible %>% 
  filter(ecm_status_list == 'Participating' & ecm_include_patient %in% c('Control', 'Treatment') |
           ecm_include_clinic == 'Not ECM') 



### Add care plan quality  --------------------------------------------------------------------------------
### It will be used as one of the interaction variables

# Re-read if already exists, create anew otherwise
if(file.exists(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))){
  care = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))
}else{
  
  # Read care plan evaluation survey data
  care1 <- read.csv(file.path(project_path, 'Data', 'Raw', 'Other', "ECM Evaluation of the care plans.csv"), encoding = 'UTF-8') # Read survey data
  care2 <- read.csv(file.path(project_path, 'Data', 'Raw', 'Other', "ECM Evaluation of the care plans-b.csv"), encoding = 'UTF-8') # Read survey data
  
  # Merge using 'SET.OF.b'
  care <- merge(care1, care2, by = 'SET.OF.b')

  # Get primary component
  pc1 = stats::princomp(~b1+b2+b3+b4+b5+b6, data = care, na.action = 'na.exclude') # Exactly the same as prcomp(), but works nicer with NA's
  care$care_plan_ev_pc1 = pc1$scores[,'Comp.1']
  
  #  Remove unnecessary rows and columns
  care <-  care %>% dplyr::select(-c('PARENT_KEY', 'KEY.x','KEY.y', 'SET.OF.b', 'SubmissionDate',
                                     'deviceid', 'subscriberid', 
                                     'starttime', 'endtime',  'a1', 'b0', 'b0_r', 'b7',
                                     'list_numbers', 'b_count', 'careplan', 'careplan_num',
                                     'simid', 'devicephonenum', 'username', 'duration', 'caseid',
                                     'formdef_version', 'instanceID')) %>% 
                     rename('list_id' = 'a2')
  
  # Summarise by list
  care = care %>% group_by(list_id) %>% summarise_all(.,mean_miss)
  
  # Save
  fwrite(care, file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'), na = NA, row.names = F)
  
}

# Add care plan quality to 'patient_ecm_eligible'
patient_ecm_eligible = left_join(patient_ecm_eligible, care %>% dplyr::select(c(list_id, care_plan_ev_pc1)))
tapply(patient_ecm_eligible$list_id, patient_ecm_eligible$care_plan_ev_pc1 %>% is.na, n_distinct)



### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))

### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes =  inner_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Re-name management score variable (if it starts with A, it messes up the order of the table)
dta_outcomes = dta_outcomes %>% rename('management_score' = 'average_clinic_management_score')



#
# INTERACTION REG. FUNCTION  ---------------------------------------------------------------------------------------------------
# 
### Create a function that would automatically create regression models,
### with relevant interaction terms, extracting both the independent
### treatment coefficient and the coefficient of the interaction term in each case

interact_regression  = function(var2,  # what outcome variable should be used
                               group1, # what is the reference group?
                               group2,  # what is the treatment group?
                               year1,  # Define if the pre-treatment outcomes should be measured from 2009 or 2018 (no other options for now here)
                               risk1 # Risk group to focus on 
                      ){

  ### Define parameters
  var1 = var2
  
  ### Define dataset to use
  dta_reg_save = dta_outcomes
  if(year1 == 2009){dta_reg_save = dta_outcomes_09}
  
  ### Clean the dataset by....
  dta_reg = dta_reg_save %>% 
    mutate(var = !!rlang::ensym(var1)) %>% # ... iteratively defining the outcome variable
    filter(treat_period == 1) %>%  #... focusing on the treatment period
    mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorizing at pre-defined level
    mutate(var = as.numeric(var) * 12/22) %>% # ...annualizing
    filter(ecm_include_patient %in% c(group1, group2)) # ..filtering the groups
  
  ### Add pre-treatment outcomes of a selected variable (annualized) by....
  pre_treat = dta_reg_save %>% 
    mutate(var_pre = !!rlang::ensym(var1)) %>% # ..iteratively define outcome variable
    filter(treat_period == 0) %>%  #... focusing on the pre-treatment period
    mutate(var_pre = Winsorize(as.numeric(var_pre), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ..winsorizing at pre-defined level
    mutate(var_pre = as.numeric(var_pre) * ifelse(year1 == 2018, 12/41, 12/149)) %>%  # ...annualizing
    filter(ecm_include_patient %in% c(group1, group2)) %>% # ..filter the groups
    dplyr::select(c(id, var_pre)) # ... leave only relevant columns
  
  
  dta_reg = left_join(dta_reg, pre_treat)  
  
  # Subset to a given set of risk patients
  if(group1 == 'Control' & risk1 %in% c('mild', 'severe')){
    dta_reg = dta_reg %>% filter(class_code %in% risk1)
  }
  
  # Makes weights unity if comparing to pure control
  if(group1 == 'Pure control'){
    dta_reg$weight = 1
  }
  
  
  ### Reference category
  dta_reg$ecm_include_patient = relevel(factor(dta_reg$ecm_include_patient), ref = group1)
  
  
  ### Get means
  x1 = (dta_reg$var[dta_reg$ecm_include_patient == group1] > 0) %>% mean_miss %>% round(3) # Mean - dummy
  x2 = dta_reg$var[dta_reg$ecm_include_patient == group1] %>% mean_miss %>% round(3) # Mean - count

  
  ### Convert to 0/1 dummies
  dta_reg = dta_reg %>% mutate(var_dummy = ifelse(var > 0, 1, 0),
                               var_pre_dummy = ifelse(var_pre > 0, 1, 0))
  
  
  # Subset to a given set of risk patients and define FE
  if(risk1 == 'all'){
    dta_reg$fe = paste0(dta_reg$block_categorical, dta_reg$class_code)
  }
  if(group1 == 'Control' & risk1 %in% c('mild', 'severe')){
    dta_reg = dta_reg %>% filter(class_code %in% risk1)
    dta_reg$fe = dta_reg$block_categorical
  }
  

  ### Run models  
  if(group1 == 'Pure control'){
    # m1 = lm(var ~  factor(block_categorical) + qbs_II        * ecm_include_patient, data = dta_reg)
    # m2 = lm(var ~  factor(block_categorical) + comorbidities * ecm_include_patient, data = dta_reg)
    # m3 = lm(var ~  factor(block_categorical) + var_pre * ecm_include_patient, data = dta_reg)
  }else if(group1 == 'Control'){
    m1 = lm(var ~  factor(fe)  + age + gender + qbs_II  * ecm_include_patient, weights = weight, data = dta_reg)
    m2 = lm(var ~  factor(fe)  + age + gender + management_score  * ecm_include_patient, weights = weight, data = dta_reg)
    # m2 = lm(var ~  factor(list_id)   + age + gender+ class_code * ecm_include_patient, weights = weight, data = dta_reg)
    m3 = lm(var ~  factor(fe)   + age + gender + care_plan_ev_pc1   * ecm_include_patient, weights = weight, data = dta_reg)
    m4 = lm(var ~  factor(strata)   + age + gender + var_pre   * ecm_include_patient, weights = weight, data = dta_reg)
    #m4 = lm(var ~  factor(strata)   + age + gender + var_pre09   * ecm_include_patient, weights = weight, data = dta_reg)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }

  # If model coefficients empty, re-define model objects as empty
  if(is.na(coef(m3))[length(coef(m3))]){m3 = ''}
  if(is.na(coef(m4))[length(coef(m4))]){m4 = ''}
  
  ### Cluster SE
  m1 = extract_coeftest(coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T)), 1) %>%
    pivot_wider(names_from = var,  values_from = beta_se) %>% 
    add_column(.before = 1, 'var' = var1)
  
  names(m1) = c('var', 'beta_qbs', 'interact_qbs')

  m2 = extract_coeftest(coeftest(m2, cluster.vcov(m2, dta_reg$list_id, df_correction = T)), 1) %>%
    pivot_wider(names_from = var,  values_from = beta_se) %>% 
    add_column(.before = 1, 'var' = var1)
  
  names(m2) = c('var', 'beta_mng', 'interact_mng')
  
  
  # If everyone is 1 on the pre-treatment variables, the model would produce an error, so avoid that and produce a matching empty dataframe instead

  tryCatch({
    m3 <- extract_coeftest(coeftest(m3, cluster.vcov(m3, dta_reg$list_id, df_correction = T)), 1) %>%
      pivot_wider(names_from = var,  values_from = beta_se) %>% 
      add_column(.before = 1, 'var' = var1)
    m3 = m3[, c(1,3,2)]
    names(m3) <- c('var', 'beta_careq', 'interact_careq')
    }, error = function(e) { m3 <<- data.frame('var' = var1, beta_careq = '', interact_careq = '')})
 
  tryCatch({
    m4 <- extract_coeftest(coeftest(m4, cluster.vcov(m4, dta_reg$list_id, df_correction = T)), 1) %>%
      pivot_wider(names_from = var,  values_from = beta_se) %>% 
      add_column(.before = 1, 'var' = var1)
    names(m4) <- c('var', 'beta_pre18', 'interact_pre18')
  }, error = function(e) {m4 <<- data.frame('var' = var1, beta_pre18 = '', interact_pre18 = '')})
  
  
  ### Combine the results
  if(group1 %in% c('Pure control')){
    ### Combine all models and specify the DV
    m_all = left_join(m1, left_join(m2, m3))
  }
  if(group1 %in% c('Control')){    ### Combine all models and specify the DV
    m_all = left_join(m1, left_join(m2, left_join(m3, m4)))
  }

  # Add...
  m_all = m_all %>% 
    mutate(var = var1) %>% # ... outcome variable name
    add_column(.after = 1, 'mean_count' = x2) %>% # ... group means 
    add_column(.after = 1, 'mean_dummy' = x1)

  return(((m_all)))
  
}



#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
# 
### We will loop through all the relevant variables, using the function created above
### to create Table A6 in the Appendix

### CHOOSE: Variable type ---------------------------------------------------------------------------------------------------
type1 = 'count' # count or dummy

### CHOOSE: Define pre-treatment start year ---------------------------------------------------------------------------------------------------
year1 = 2018  # 2009 /2018

### CHOOSE: Outcomes variables --------------------------------------------------------------------------------------------------
# vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'p_delay')]

vars = vars_main


### Loop across outcome variables  ---------------------------------------------------------------------------------------------------
for(year1 in c(2018)){
  #for(risk1 in c('all','mild', 'severe')){
  for(risk1 in c('all')){
    
    if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
    
    # Prepare list to save outputs from each iteration
    temp_list <- vector("list", length(vars))
    
    for(i in seq_along(vars) ){
  
    var1 = vars[i]
    print(var1)
    
    # Get the results
    temp = interact_regression(var1, 'Control', 'Treatment', 2018, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_n'))
      
    # Save in a list
    temp_list[[i]] = temp
    print(temp)      
      
    }
    
    ### Combine into one dataframe -----------------------------------------------------------------------------
    dta_table_save = rbindlist(temp_list)
    
    
    ### Assign FE + means + N    ---------------------------------------------------------------------------------------------------
    dta_table_save = dta_table_save %>% as.data.frame()
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{FE}', rep(c('\\multicolumn{2}{m{2.27cm}}{-}', '-'), 1), rep(c('\\multicolumn{2}{m{2.27cm}}{Bloc x Risk}', '-'), 3), '\\multicolumn{2}{m{2.27cm}}{Strata}', '-')
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{Weights}',  rep(c('\\multicolumn{2}{m{2.27cm}}{-}', '-'), 1), rep(c('\\multicolumn{2}{m{2.27cm}}{Yes}', '-'), 4))
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{Controls}', rep(c('\\multicolumn{2}{m{2.27cm}}{-}', '-'), 1), rep(c('\\multicolumn{2}{m{2.27cm}}{Age, gender}', '-'), 4))
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{N}', 
                                                 paste0('\\multicolumn{2}{m{2.27cm}}{',prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control')]), big.mark = ','), '}'), '-',
                                                 paste0('\\multicolumn{2}{m{2.27cm}}{',prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment') & !is.na(dta_outcomes$average_clinic_qbs_score)]), big.mark = ','), '}'), '-',
                                                 paste0('\\multicolumn{2}{m{2.27cm}}{',prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment') & !is.na(dta_outcomes$management_score)]), big.mark = ','), '}'), '-',
                                                 paste0('\\multicolumn{2}{m{2.27cm}}{',prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment') & !is.na(dta_outcomes$care_plan_ev_pc1)]), big.mark = ','), '}'), '-',
                                                 paste0('\\multicolumn{2}{m{2.27cm}}{',prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment')]), big.mark = ','), '}'), '-')

  fwrite(dta_table_save, file.path(project_path, 'Tables','CSV', risk1, 'tableA6_interact.csv'))
  }
}

 

#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#
### This section takes the .csv saved above and converts it into a text snipped that can be simply copy-pasted
### as a body of a table in Overleaf


### CONFIRM CHOICES ----------------------------------------------------------------------------------------------------
year1 =  2018
risk1 = c('all') # Risk group to focus on 

dta_table_save = fread(file.path(project_path, 'Tables','CSV',  risk1, 'tableA6_interact.csv'))
dta_table = dta_table_save %>% as.data.frame()

# Time differences - no dummies
dta_table[grepl('diff', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# No. diagnoses/procedures - no dummies
dta_table[grepl('n_diag|n_procedures', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# Prices - no dummies
dta_table[grepl('price', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# ECM consultation codes - no ANCOVA
dta_table[grepl('consult_include|consult_care_plan|consult_refuse', dta_table$var),
          which(grepl('_pre', names(dta_table)))]  = '$-$'

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


### Clean based on dictionary of outcomes ---------------------------------------------------------------------------------------------------

### Combine with dictionary
dta_table = left_join(dta_table, 
                      dict_outcomes %>% dplyr::select(c(var,name, group, order, nr))) %>%
  arrange(order, nr) %>% 
  relocate(.,c('group','name'), .after = 1) %>% 
  filter(!is.na(group) | grepl('textbf', var))

dta_table$name[grepl('textbf', dta_table$var)] = dta_table$var[grepl('textbf', dta_table$var)]
dta_table$group[grepl('textbf', dta_table$var)] = '-'



## Remove spare columns
dta_table = dta_table %>% dplyr::select(-c(var, order, nr))


### Add phantom stars -------------------------------------------------------------------

dta_table = dta_table %>% 
  mutate(across(contains('mean'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{-}', max(str_count(., "[0-9]")) - str_count(., "[0-9]"))),
                                           .))) %>% 
  mutate(across(contains('beta'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{-}', max(str_count(., "[0-9]")) - str_count(., "[0-9]"))),
                                           .))) %>% 
  mutate(across(contains('beta'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{*}', max(str_count(., "[*]")) - str_count(., "[*]"))),
                                           .))) 

dta_table



# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')
dta_table

# Replace any cells with only dashes
dta_table$cell1 = gsub('&-&', '&', dta_table$cell1, fixed=T)
dta_table$cell1 = gsub('&-\\', '\\', dta_table$cell1, fixed=T)

# Add section headings
for(group1 in unique(dta_table$group)){
  if(group1 == '-'){next}
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{11}{r{.99\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}





### N ------
sf(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1 & !is.na(dta_outcomes$average_clinic_qbs_score)])
sf(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1 & !is.na(dta_outcomes$management_score)])
sf(dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1 & !is.na(dta_outcomes$care_plan_ev_pc1)])


#
# END OF CODE  ---------------------------------------------------------------------------------------------------
#