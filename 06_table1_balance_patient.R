

#
# SET-UP ---------------------------------------------------------------------------------------------------
# 

### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

#
# PREPARE DATA ---------------------------------------------------------------------------------------------------
#

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))


# Leave only ECM treatment and control (at participating providers) and pure control as per Ben's outline for 01/12/2023
patient_ecm_eligible = patient_ecm_eligible %>% 
          filter(ecm_status_list == 'Participating' & ecm_include_patient %in% c('Control', 'Treatment') |
                   ecm_include_clinic == 'Not ECM') 


### Outcomes  ---------------------------------------------------------------------------------------------------
dta_outcomes = read_parquet(file.path(project_path, 'Data/Clean', 'Aggregated', 'All_outcomes_period_18_23.parquet')) %>% mutate(id = as.character(id))


### Combine  ---------------------------------------------------------------------------------------------------
dta_outcomes =  inner_join(dta_outcomes, patient_ecm_eligible) %>% 
  relocate(.after = 'id', c('id_ecm', 'list_id', 'ecm_include_patient', 'class_code'))


### Recode gender and risk as numeric
dta_outcomes = dta_outcomes %>% 
              mutate(gender_m = ifelse(gender == 'Male', 1, 0),
                     class_code_mild = ifelse(class_code =='mild', 1, 0))


#
# BALANCE REGRESSION FUNCTION  ---------------------------------------------------------------------------------------------------
# 
### Create a function that would automatically create balance test values, i.e.
### annual counts of a given variable for specfied two-way group comparisons.
### Specific role of the arguments placed in the function are explained below


balance_row = function(type1, # count  / dummy - what version of the outcome should be used?
                       var2, # what outcome variable should be used?
                       treat_period1, # are we running the models for pre- or post-treatment outcomes?
                       year1, # from which year should observations begin?
                       risk1  # Risk group to focus on -> c('severe') / c('mild') / c('mild', 'severe') [everyone] 
                       ){
  
  var1 = var2 # I couldn't figure out why, but without that the function wouldn't run
  
  # We need to annualize count data - define coefficient based on the period and start year used
  if(treat_period1 == 0 & year1 == 2009){coef = 12/149}
  if(treat_period1 == 0 & year1 == 2018){coef = 12/41}
  if(treat_period1 == 1){coef = 12/22}
  
  # Define dataset to use
  dta_balance = dta_outcomes
  if(year1 == 2009){dta_balance = dta_outcomes_09}
  
  
  # Clean the dataset by....
  dta_balance = dta_balance %>% 
    mutate(var = !!rlang::ensym(var1)) %>% # ... iteratively defining the outcome variable
    filter(treat_period == treat_period1) #... focusing on the treatment period
  
  ### ... for all variables apart from the demographics we are also....
  if(!(var1 %in% c('gender_m', 'class_code_mild', 'age'))){
    dta_balance = dta_balance %>% 
      mutate(var = Winsorize(as.numeric(var), minval = 0, maxval = NULL, probs = c(0.00, winsorize1), na.rm = F, type = 7)) %>%  # ...winsorizing at pre-defined level
      mutate(var = as.numeric(var) * coef)  # ... annualizing
  }
    
  
  # Convert to 0/1 dummies, if this is the option selecte %>% 
  if(type1 == 'dummy'){ 
      dta_balance = dta_balance %>% mutate(var = ifelse(var > 0, 1, 0))
    }
  
  
  # Subset to pure control and ECM patients with given risk class
  if(risk1 %in% c('mild', 'severe')){
    dta_balance = dta_balance %>% filter( (ecm_include_patient == 'Pure control') |
                                            (ecm_include_patient %in% c('Control', 'Treatment') & class_code %in% risk1) )    
  }

  
  # Re-level the grouping factors
  dta_balance$ecm_include_patient = factor(dta_balance$ecm_include_patient, levels = c('Pure control', 'Control', 'Treatment'))
  

  ### Fit balance regression models
  # Define models' formulas
  formula1 = 'factor(block_categorical) + ecm_include_patient'
  formula2 = 'factor(strata) +  ecm_include_patient'
  
  # Need to make exceptions for class_code as a DV since is not there for Pure Control so the models wouldn't run
  if(var2 == 'class_code_mild'){formula1 = gsub(' + ecm_include_patient', '', formula1, fixed=T)}
  
  
  # Estimate the models
  m1 = lm(paste("var ~ ", formula1), weights = weight,
          data = dta_balance %>%
                      filter(ecm_include_patient %in% c('Pure control', 'Control')) %>% 
                      mutate(weight = 1)) #.... Makes weights unity if comparing to pure control
  
  m2 = lm(paste("var ~ ", formula2), weights = weight,
          data = dta_balance %>% filter(ecm_include_patient %in% c('Control', 'Treatment')))
  
  ### Cluster SE
  m1 = coeftest(m1, cluster.vcov(m1, dta_balance$list_id[dta_balance$ecm_include_patient %in% c('Pure control', 'Control')], df_correction = T))
  m2 = coeftest(m2, cluster.vcov(m2, dta_balance$list_id[dta_balance$ecm_include_patient %in% c('Control', 'Treatment')], df_correction = T))
  
  
  ### Define means, standard deviations and differences
  
  # Use t-tests
  if(var2 == 'class_code_mild'){
    t1  = t.test(var ~ ecm_include_patient, data = dta_balance %>% filter(ecm_include_patient %in% c('Control', 'Treatment')))
  }else{
    t1  = t.test(var ~ ecm_include_patient, data = dta_balance %>% filter(ecm_include_patient %in% c('Pure control', 'Control')))
  }
  
  t2  = t.test(var ~ ecm_include_patient, data = dta_balance %>% filter(ecm_include_patient %in% c('Control', 'Treatment')))

  sd123 = dta_balance %>% group_by(ecm_include_patient) %>% dplyr::select(c(var)) %>% summarise_all(., sd_miss) %>% mutate(var = ifelse(is.na(var), -1, var))

  
  ### Combine all relevant values into a data.frame
  dta_balance = data.frame(
    var = var1,
    mean1 = paste0(round_flex(t1$estimate[1])), # ' (', round_flex(sd123[1,2]), ')'),
    mean2 = paste0(round_flex(t1$estimate[2])), # ' (', round_flex(sd123[2,2]), ')'),
    mean3 = paste0(round_flex(t2$estimate[2])), # ' (', round_flex(sd123[3,2]), ')'),
    
    # Paste estimate, sig. starts (if any), and standard error of the regression coeficient
    diff21 = paste0(round_flex(m1[nrow(m1),'Estimate']), gsub('[0-9.]','', sig_stars(m1[nrow(m1), 'Pr(>|t|)'])), ' (', round_flex(m1[nrow(m1),'Std. Error']), ')'),
    diff32 = paste0(round_flex(m2[nrow(m2),'Estimate']), gsub('[0-9.]','', sig_stars(m2[nrow(m2), 'Pr(>|t|)'])), ' (', round_flex(m2[nrow(m2),'Std. Error']), ')')
  
  )
  
  if(var2 == 'class_code_mild'){dta_balance[,c('mean1', 'diff21')] = ''}
  
  return(dta_balance)  
  
}


#
# CREATE TABLE  ---------------------------------------------------------------------------------------------------
#
### We will loop through all the relevant variables, using the function created above
### to create pre-treatment balance table (Table 1 in the manuscript)

### CHOOSE: Define treatment period  ---------------------------------------------------------------------------------------------------
treat_period1 = 0

### CHOOSE: Define pre-treatment start year ---------------------------------------------------------------------------------------------------
year1 = 2018

### CHOOSE: Define risk level ---------------------------------------------------------------------------------------------------
risk1 = c('all') # Risk group to focus on 

### CHOOSE: Outcome variables --------------------------------------------------------------------------------------------------
# using 'vars_main' from 00_global.R, unless otherwise required

### Adjust 'vars_main' for the balance table - add demographics, remove 'consult_care_plan', 'consult_include' (treatment related, only existing post-treatment)
vars = c('age', 'gender_m', 'class_code_mild', vars_main[!(vars_main %in% c('consult_care_plan', 'consult_include'))])

for(year1 in c(2018)){
  
  #for(risk1 in c('all','mild', 'severe')){
  for(risk1 in c('all')){
  
    
    ### Loop across outcome variables  ---------------------------------------------------------------------------------------------------
    
    if("tidylog" %in% (.packages())){detach("package:tidylog", unload=TRUE)} # Otherwise it prints way too many messages from the loop below
    
    # Prepare list to save outputs from each iteration
    temp_list <- vector("list", length(vars))
    
    for(i in seq_along(vars) ){
      
      var1 = vars[i]
      if(risk1 %in% c('mild', 'severe') & grepl('class_code', var1)){next}
      print(paste0(var1, ' - ', year1, ' - ', risk1))

      # Define type of outcome variable
      type1 = 'count'
      if(grepl(vars_dummy, var1)){type1 = 'dummy'}
      
      print(type1)
      
      # Get the results
      temp = balance_row(type1, var1, treat_period1, year1, risk1) %>% 
        rename_with(.cols = -c(var), .fn = ~paste0(., '_n'))
      
      # Save in a list
      temp_list[[i]] = temp
      print(temp)
    }
    
    ### Combine into one dataframe -----------------------------------------------------------------------------
    dta_table_save = rbindlist(temp_list)

    ### Assign FE + means + N    ---------------------------------------------------------------------------------------------------
    dta_table_save = dta_table_save %>% as.data.frame()
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{FE}', '-', '-','-', 'Block', 'Strata')
    # dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{Weights}', 'No', 'No','No', 'Yes', 'Yes')
    # dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{Controls}',  '-', '-','-', 'Age*, gender*', 'Age*, gender*')
    dta_table_save[nrow(dta_table_save)+1, ] = c('\\textbf{N}', 
                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Pure control')]), big.mark = ','), 
                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Control')]), big.mark = ','), 
                                 prettyNum(n_distinct(dta_outcomes$id[dta_outcomes$ecm_include_patient %in% c('Treatment')]), big.mark = ','), 
                                 '-', '-')
    
    
    
    ### Save object for cleaning   ---------------------------------------------------------------------------------------------------
    if(treat_period1 == 0 & year1 == 2018){fwrite(dta_table_save, file.path(project_path,'Tables', 'CSV', risk1, 'table1_balance_patient_pre.csv'))}
    if(treat_period1 == 0 & year1 == 2009){fwrite(dta_table_save, file.path(project_path,'Tables', 'CSV', risk1, 'table1_balance_patient_pre_09.csv'))}
    if(treat_period1 == 1 & year1 == 2018){fwrite(dta_table_save, file.path(project_path, 'Tables','CSV', risk1, 'table1_balance_patient_post.csv'))}
    if(treat_period1 == 1 & year1 == 2009){fwrite(dta_table_save, file.path(project_path, 'Tables','CSV', risk1, 'table1_balance_patient_post_09.csv'))}
    
    
  }
}



#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#
### This section takes the .csv saved above and converts it into a text snipped that can be simply copy-pasted
### as a body of a table in Overleaf

### CONFIRM CHOICES ----------------------------------------------------------------------------------------------------
treat_period1 = 0
year1 =  2018
risk1 = c('all') # Risk group to focus on 

### Re-read the table --------------------------------------------------------------------------------------------------
if(treat_period1 == 0 & year1 == 2018){dta_table_save = fread(file.path(project_path,'Tables', 'CSV', risk1, 'table1_balance_patient_pre.csv'))}
if(treat_period1 == 0 & year1 == 2009){dta_table_save = fread(file.path(project_path,'Tables', 'CSV', risk1, 'table1_balance_patient_pre_09.csv'))}
if(treat_period1 == 1 & year1 == 2018){dta_table_save = fread(file.path(project_path,'Tables', 'CSV', risk1,'table1_balance_patient_post.csv'))}
if(treat_period1 == 1 & year1 == 2009){dta_table_save = fread(file.path(project_path,'Tables', 'CSV', risk1,'table1_balance_patient_post_09.csv'))}


### Clean the table --------------------------------------------------------------------------------------------------
dta_table = dta_table_save

# Class_code  - asisgn '-' in the case of 'pure control' column
dta_table[grepl('class_code', dta_table$var), which(grepl('1', names(dta_table)))] = '$-$'


### Combine with dictionary (should give us clean variable names and the order to display them)
dta_table = left_join(dta_table, 
                      dict_outcomes %>% dplyr::select(c(var,name, group, order, nr))) %>%
  arrange(order, nr) %>% 
  relocate(.,c('group','name'), .after = 1) %>% 
  filter(!is.na(group) | grepl('textbf', var))

dta_table$name[grepl('textbf', dta_table$var)] = dta_table$var[grepl('textbf', dta_table$var)]
dta_table$group[grepl('textbf', dta_table$var)] = '-'


### Remove spare columns
dta_table = dta_table %>% dplyr::select(-c(var, order, nr))


### Add phantom stars -------------------------------------------------------------------
### That should make the numbers aligined in the table
dta_table = dta_table %>% 
  mutate(across(contains('mean'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{-}', max(str_count(., "[0-9]")) - str_count(., "[0-9]"))),
                                           .))) %>% 
  mutate(across(contains('diff'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{-}', max(str_count(., "[0-9]")) - str_count(., "[0-9]"))),
                                           .))) %>% 
  mutate(across(contains('diff'),  ~ifelse(row_number() <= (n() - 3), 
                                           paste0(., strrep('\\phantom{*}', max(str_count(., "[*]")) - str_count(., "[*]"))),
                                           .))) 


### Put all columns in one dataframe column with LaTeX table separators  -------------------------------------------------------------------
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')


# Add section headings
for(group1 in unique(dta_table$group)){
  if(group1 == '-'){next}
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{6}{r{.99\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace $-$ with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### Print to copy to Overleaf  ---------------------------------------------------------------------------------------------------
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


#
# END OF CODE -------------------------------------------------------------------------
#