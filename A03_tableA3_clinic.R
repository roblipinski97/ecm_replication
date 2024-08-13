

#
# SET-UP ----
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

file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


#
# PREPARE DATA ---------------------------------------------------------------------------------
#

### Patient-level data with clinic information
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))

### Re-label clinic status as ECM for both accepting and refusing clinic (i.e. treat as 'ECM' all clinics randomized into treatment, regardless
### of the treatment uptake status)

patient_ecm_eligible = patient_ecm_eligible %>%
  mutate(ecm_include_clinic = dplyr::recode(ecm_include_clinic, 'ECM accept' = 'ECM', 'ECM refuse' = 'ECM'))


### Add care plan quality  --------------------------------------------------------------------------------
### It will be on of the provider-level outcomes used in the table

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

### Add care plan quality  ---------------------------------------------------------------------------------------------------------------------
care = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Care plan evaluations.csv'))
patient_ecm_eligible = left_join(patient_ecm_eligible, care %>% dplyr::select(c(list_id, care_plan_ev_pc1)))


### Create clinic-level dataset
clinics = patient_ecm_eligible %>% dplyr::select(c(contains('clinic'), 'block_categorical',  'number_of_lists')) %>% 
          group_by(clinic_registration_code) %>% mutate(n_patients_clinic = n()) %>% ungroup() %>%
          distinct()

### Create list-level dataset
lists = patient_ecm_eligible %>% dplyr::select(c(contains('clinic'), 'block_categorical', 'qbs_II', contains('list'))) %>% 
          group_by(list_id) %>% mutate(n_patients_list = n()) %>% ungroup() %>%
          distinct()


#
# TABLES  ---------------------------------------------------------------------------------
# 

### Table A3 created here is a clinic and GP(list)-level balance table. Just like in 
### '06_table1_balance_patient.R' we create a function that will automatically generate 
### rows of the table, depending on the outcome variable and group specficed

var2 = 'n_patients_clinic'
group1 = 'ecm_include_clinic'

balance_clinic = function(var2, group1){
  
  var1 = var2
  
    # Remember, 'include' specifies whether a given unit (clinic or GP/list here) was ASSIGNED to 
    # ECM treatment, while 'status'whether it RECEIVED treatment
  
    # Based on the group specified in the argument of the function, filter and re-order the relevant
    # observations
    if(group1 == 'ecm_include_clinic'){
      temp = clinics %>% filter(ecm_include_clinic %in% c('ECM', 'Not ECM'))
      temp$group = factor(temp$ecm_include_clinic, levels = c('Not ECM', 'ECM'))
    }
    if(group1 == 'ecm_status_clinic'){
      temp = clinics %>% 
        filter(ecm_include_clinic == 'ECM') %>% 
        mutate(ecm_status_clinic = ifelse(
          clinic_registration_code %in% lists$clinic_registration_code[lists$ecm_status_list  %in% c('Participating')],
          'Participating',
          'Not participating'
        ))
      
      temp$group = factor(temp$ecm_status_clinic, levels = c('Not participating', 'Participating'))
      
    }
    if(group1 == 'ecm_include_list'){
      temp = lists %>% filter(ecm_include_clinic %in% c('ECM', 'Not ECM'))
      temp$group = factor(temp$ecm_include_list, levels = c('Not ECM', 'ECM'))
    }
    if(group1 == 'ecm_status_list'){
      temp = lists %>% filter(ecm_include_clinic == 'ECM') %>% filter(ecm_status_list %in% c('Not participating', 'Participating'))
      temp$group = factor(temp$ecm_status_list, levels = c('Not participating', 'Participating'))
    }
    
  
  # Extract means and t-tests of relevant outcomes, unless 'N' is the outcomes, in which case just count the number of observations
  if(var1 %in% c('N_clinic', 'N_list')){
    dta_temp = data.frame(mean1 = table(temp$group)[1], mean2 = table(temp$group)[2], diff = '-')
  }else{
    
    temp = temp %>% mutate(var = !!rlang::ensym(var1))
    
    m1 = lm(var ~ factor(block_categorical) + group, data = temp)
    
    if(grepl('list', group1)){
      m1 = coeftest(m1, cluster.vcov(m1, temp$clinic_registration_code, df_correction = T))
    }
    
    sd1 = temp %>% group_by(group) %>% dplyr::select(c(var)) %>% summarise_all(., sd_miss)
    t1  = t.test(temp$var  ~ temp$group)
    a1  = anova(lm(var  ~ group, data = temp))
  
    dta_temp = data.frame(
      mean1 = paste0(round_flex(t1$estimate[1]), ' (', round_flex(sd1[1,2]), ')'),
      mean2 = paste0(round_flex(t1$estimate[2]), ' (', round_flex(sd1[2,2]), ')'),
      diff  = extract_coeftest(m1, 0)$beta_se)

  }

  return(dta_temp)  
}


### Loop across outcome variables
vars_clinic = c('number_of_lists', 'average_clinic_qbs_score', 'average_clinic_management_score', 'n_patients_clinic', 'N_clinic')
vars_list   =  c('qbs_II', 'n_patients_list', 'N_list')


count = 0
for(var1 in c(vars_clinic, vars_list)){
  
  if(var1 %in% vars_clinic){
    group2 = 'clinic'
    }
  if(var1 %in% vars_list){
    group2 = 'list'
  }
  
  print(var1)
  print(group2)
  
  count = count + 1
  
  temp = cbind(
      balance_clinic(var1, paste0('ecm_include_', group2)) %>% 
        rename_with(.cols = everything(), .fn = ~paste0(., '_include')) %>% 
        add_column(., .after = 0, 'var' = var1),
      balance_clinic(var1,  paste0('ecm_status_', group2)) %>% 
        rename_with(.cols = everything(), .fn = ~paste0(., '_status'))
    )
  
  if(count == 1){
    dta_table = temp
  }else{
    dta_table = rbind(dta_table, temp)
  }

}


### Classify observation level
dta_table$group = ifelse(dta_table$var %in% vars_clinic, 'Clinic', 'Provider')

## Re-label the variable names  
dta_table = dta_table %>%
  mutate(var = dplyr::recode(var,
            'N_clinic' = 'Sample size (N)',
            'number_of_lists' = 'Lists (N)',
            'average_clinic_qbs_score' = 'QBS score',
            'average_clinic_management_score' = 'Management score',
            'n_patients_clinic' = 'Eligible patients (N)',
            'qbs_II' = 'QBS score',
            'n_patients_list' = 'N Eligible patients (N)',
            'N_list' = 'Sample size (N)',
            
            ))



### Save
fwrite(dta_table, file.path(project_path,  'Tables','CSV', 'tableA3_clinic.csv'), row.names = F, na = NA)


#
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#
### This section takes the .csv saved above and converts it into a text snipped that can be simply copy-pasted
### as a body of a table in Overleaf


### Re-read the table --------------------------------------------------------------------------------------------------
dta_table = fread(file.path(project_path,  'Tables','CSV', 'tableA3_clinic.csv')) %>% 
                dplyr::select(-c(group))


# Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table , 1, paste, collapse = "&"), '\\')

# Add section headings
for(group1 in unique(dta_table$group)){
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{77}{r{.99\\textwidth}}{\\textbf{', group1, '}}\\')
}


### Replace - with \text{-}  ---------------------------------------------------------------------------------------------------
dta_table$cell1 = gsub('-', '\\text{-}', dta_table$cell1, fixed=T)


# Print to copy to Overleaf
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}
print(cat('\n'))


#
# END OF CODE -----------------------------------------------------------------------
#