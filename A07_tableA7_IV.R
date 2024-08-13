

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
### 2SLS model set-up here

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))


### ECM groups   ---------------------------------------------------------------------------------------------------
patient_ecm =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible7_control_treat_patients.csv")) %>% mutate(id = as.character(id))

### ECM acceptance   ---------------------------------------------------------------------------------------------------
patient_ecm_accept = fread(file.path(project_path, 'Data', 'Clean', 'ECM Inclusion', 'patient_ecm_accept.csv')) %>% mutate(id = as.character(id))


### ECM uptake numbers (for consort chart)-------------------------------------

# Combine
temp = left_join(patient_ecm_eligible, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 0, 1))

# Leave only ECM treatment and control (at participating providers) and pure control as per Ben's outline for 01/12/2023
patient_ecm_eligible = patient_ecm_eligible %>% 
  filter(ecm_status_list == 'Participating' & ecm_include_patient %in% c('Control', 'Treatment') |
           ecm_include_clinic == 'Not ECM') 

patient_ecm_eligible_accept = left_join(patient_ecm_eligible, patient_ecm_accept) %>% 
  mutate(ecm_status_patient = ifelse(is.na(ecm_status_patient), 0, 1))


### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))


### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes =  inner_join(dta_outcomes, patient_ecm_eligible_accept) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Leave only the required ECM groupings  ---------------------------------------------------------------------------------------------------
dta_outcomes    = dta_outcomes %>% filter(!is.na(ecm_include_patient)) 

### Types of units by treatment compliance status
table(dta_outcomes$ecm_status_patient[dta_outcomes$treat_period == 1],
      dta_outcomes$ecm_include_patient[dta_outcomes$treat_period == 1])


#
# REGRESSION FUNCTION  ---------------------------------------------------------------------------------------------------
# 

### Create a function that would automatically create regression models,
### using instrumental (2SLS) model specification, with ECM assignment
### serving as the instrument and ECM uptake (having a careplan creation
### visit) as the treatment


iv_regression = function(var2,  # what outcome variable should be used
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
  x2

  #x2 = dta_reg$var[dta_reg$ecm_include_patient == group2] %>% mean_miss %>% round(2) # Mean - count (treatment)
  
  
  ### Convert to 0/1 dummies
  dta_reg = dta_reg %>% mutate(var_dummy = ifelse(var > 0, 1, 0),
                               var_pre_dummy = ifelse(var_pre > 0, 1, 0))
  
  
  ### NOTE: Only add strata for ECM control vs treatment (and NOT for Pure control vs. treatment)
  if(group1 == 'Control'){
    m1 = iv_robust(var_dummy ~ ecm_status_patient + var_pre + age + gender  | ecm_include_patient + var_pre + age + gender, 
                   cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = dta_reg)
    
    m2 = iv_robust(var ~ ecm_status_patient + var_pre  + age + gender | ecm_include_patient  + var_pre  + age + gender, 
                   cluster = list_id, weights = weight,  fixed_effects = ~strata, se_type  = 'stata', data = dta_reg)
  }else{
    print(group1)  
    break # If different groups used, the models might need to be adjusted, so break for now in that case
  }
  
  if(is.na(coef(m2))[length(coef(m2))]){m2 = ''}
  # if(is.na(coef(m3))[length(coef(m3))]){m3 = ''}
  
  
  tryCatch({m1 <-  extract_coeftest(m1, 3) %>% filter(var == 'ecm_status_patient') %>% rename('beta_dummy' = 'beta_se')}, error = function(e) {m1 <<- data.frame('var' = var1, beta_dummy = '')})
  tryCatch({m2 <-  extract_coeftest(m2, 3) %>% filter(var == 'ecm_status_patient') %>% rename('beta_count' = 'beta_se')}, error = function(e) {m2 <<- data.frame('var' = var1, beta_count = '')})

  ### Combine all models and specify the DV
  m_all = left_join(m1, m2) %>% 
    mutate(var = var1) %>% 
    # add_column(.after = 'beta_count', 'p_count' = p2) %>% # ... p-values
    # add_column(.after = 'beta_count', 'p_dummy' = p1) %>%
    add_column(.after = 1, 'mean_count' = x2) %>% # ... group means 
    add_column(.after = 1, 'mean_dummy' = x1)
  
  m_all
  
  return(m_all)  
   
}


#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
# 

### We will loop through all the relevant variables, using the function created above
### to create Table A8 in the Appendix

### CHOOSE: Outcomes variables --------------------------------------------------------------------------------------------------
# vars = names(dta_outcomes)[which(names(dta_outcomes) == 'start_end_diff'):which(names(dta_outcomes) == 'p_delay')]

vars = vars_main[!grepl('consult_care_plan|consult_include', vars_main)]


### Loop across outcome variables  ---------------------------------------------------------------------------------------------------
for(year1 in c(2018)){
  #for(risk1 in c('all','mild', 'severe')){
  for(risk1 in c('all')){
    
    if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
    
    # Prepare list to save outputs from each iteration
    temp_list <- vector("list", length(vars))
    
    for(i in seq_along(vars) ){
      
    var1 = vars[i]
    print(paste0(var1, ' - ', year1, ' - ', risk1))
      
    ### Combine....
    # ... all bits as lists
    dfs1 = list(
      iv_regression(var1, 'Control', 'Treatment', .999, risk1) %>%
        rename_with(.cols = -c(var), .fn = ~paste0(., '_c_d'))
    )
    
    # ... merge together
    library(plyr) # NOTE: 'plyr' messses with a lot of other functions, so we detach it right away
    temp = join_all( dfs = dfs1, by = 'var', type = 'left')
    detach("package:plyr", unload=TRUE)
    
    # Save in a list
    temp_list[[i]] = temp
    print(temp)
    }
    
    ### Combine into one dataframe -----------------------------------------------------------------------------
    dta_table_save = rbindlist(temp_list)
  
    
    ### Assign FE + means + N    ---------------------------------------------------------------------------------------------------
    dta_table_save = dta_table_save %>% as.data.frame()
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{FE}', '-', '-', 'Strata', 'Strata')#, 'Strata', 'Strata')
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{Controls}', '-', '-', '\\scriptsize{Age, gender}', '\\scriptsize{Age, gender}')#, '\\scriptsize{Age, gender, $DV_{18-21}$}', '\\scriptsize{Age, gender, $DV_{18-21}$}')
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{N}', 
                                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control')]), big.mark = ','), 
                                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control')]), big.mark = ','), 
                                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment')]), big.mark = ','),
                                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment')]), big.mark = ','))
                                                 # prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment')]), big.mark = ','),
                                                 # prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control', 'Treatment')]), big.mark = ','))
    
    
   fwrite(dta_table_save, file.path(project_path, 'Tables', 'CSV', risk1, 'tableA8_IV.csv'))
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

dta_table_save = fread(file.path(project_path,  'Tables', 'CSV' , risk1, 'tableA8_IV.csv'))


### Clean coefficients -----------------------------------------------------------------------------------------------------
dta_table = dta_table_save

### For some dependent variables (if present), we don't want coefficient for models with their
### dummy versions, as those would be meaningless

# Time differences - no dummies
dta_table[grepl('diff', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'

# No. diagnoses/procedures - no dummies
dta_table[grepl('n_diag|n_procedures', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'


# Prices - no dummies
dta_table[grepl('price', dta_table$var), which(grepl('_d', names(dta_table)))] = '$-$'
dta_table


# Replace any missing values
dta_table = dta_table %>% mutate(across(contains('beta'), ~ifelse(grepl('NA', .), '$-$', .)))

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



## Remove spare columns
dta_table = dta_table %>% dplyr::select(-c(var, order, nr))


### Add phantom stars -------------------------------------------------------------------
### That should make the numbers aligined in the table
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




# Remove 'pure control' if doing models by risk sub-groups
if(risk1 != 'all'){dta_table = dta_table %>% dplyr::select(-c(beta_dummy_56, beta_count_56))}


### Put all columns in one dataframe column with LaTeX table separators  -------------------------------------------------------------------
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')

# Add section headings
for(group1 in unique(dta_table$group)){
  if(group1 == '-'){next}
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{', ifelse(risk1 == 'all', 6,4), '}{r{.49\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


#
# END OF CODE  ---------------------------------------------------------------------------------------------------
#