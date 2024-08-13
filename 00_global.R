

#
# PACKAGES --------------------------------------------
#
# rm(list=ls())

# Make a copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)




# devtools::install_github("s3alfisc/wildrwolf")


if( !is.element("pacman", installed.packages() )){install.packages("pacman", dep= T)}

pacman::p_load(tidyverse, haven, stringr, janitor, data.table, ggplot2, stringi, dplyr,
               foreign, car, arrow, lubridate, scales, purrr, 
               Rmpfr, foreign, plm, tidylog,
               fixest, lmtest, multiwayvcov, knitr, estimatr,
               ggtext, psych, ivtools, vip, patchwork,
               survival, zoo, pdftools, wildrwolf,
               DescTools, collapse, broom, survminer,
               update = F)


list.files(gsub('R Scripts', 'Data/Raw/Prescriptions', file.path(dirname(rstudioapi::getActiveDocumentContext()$path))),
           pattern = '.csv', full.names = T) %>% print(row.names = FALSE)



#
# SET PATH(S) ------------------------------------------------- 
#
System <- Sys.getenv(x = NULL, unset = "")


if(Sys.info()[["user"]] == "wb539995"){
  project_path  <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/ECM uptake/ECM_itt"
}else if(Sys.info()[["user"]] == "ASUS"){
  project_path  <- gsub('/R Scripts', '', dirname(rstudioapi::getActiveDocumentContext()$path))
}


#
# KEY GLOBAL PARAMETERS --------------------------------------------
#

start_date1 = 20180101  # Date of the start of the coverage
ecm_date   = 20210528  # Date of the start of the ECM intervention, set to first date of non-pilor enrollment, i.e. care plan GP visit (see XX_include_data.R)
end_date1   = 20230331  # Date of the end of the coverage
winsorize1 = 0.999  # Winsorization threshold

### Randomization inference   ---------------------------------------------------------
n1 = 10^4 # number of iterations
seed1 = 9559388 # seed - generated using random.org, on 29/11/2023 11:02:26 UTC, from numbers between 1 and 10^8



if(exists('dta_outcomes')){print(names(dta_outcomes))}

#### Outcome variables for the main tables
vars_main = c(
  
  'n_primary_your', 'n_outpatient_your',
  'consult_care_plan','consult_include', 
  'consult_gp','consult_gp_phone','consult_nurse','consult_nurse_phone' ,'consult_any_your',
  'n_primary_not_your', 'n_outpatient_not_your',
  
   'n_inpatient_any', 'n_daycare_any', 'n_inpatient_post_any', 'n_outpatient_post_any',
  'admit_ambulance', 'readmit_30_any', 'readmit_90_any',
  
  'heart_failure_any', 'stroke_any', 'myocardial_infarction_any', 'diabetes_2_any', 'hyperlipidemia', 'weight_high', 
  
  'covid_incidence', 'covid_vaccine',
  
  'm_glicihem_riigi', 'm_creatinine_riigi', 'm_chol_tri_riigi', 'm_glucose_riigi', 'm_thc_riigi', 
  'p_a_diabetes','p_c_hypertensive','p_c_beta_blockers','p_statins','p_key' ,'p_other'
  
  #'price_total', 'price_inpatient', 'price_outpatient_your', 'price_outpatient_not_your', 'price_primary_your', 'price_primary_not_your'
  )



### Outcomes we want to present as dummies (shares) in balance tables
vars_dummy = paste('gender', 'class_code',
                   'heart_failure_any', 'stroke_any', 'myocardial_infarction_any', 'diabetes_2_any', 'hyperlipidemia', 'weight_high', 
                   'consult_include', 'consult_care_plan',
                   '^m_', '^p_', '^readmit_', '^covid_',
                   sep = '|')


 
# '^m_|^p_|^covid_|^readmit_|consult_include|consult_care_plan|gender|class_code|heart_failure_any|stroke_any|myocardial_infarction_any|diabetes_2_any|hyperlipidemia|weight_high'


#
# DICTIONARY ----------------------------------------------------------------------------------------------------------
#

### NOTE: 'Outcome_dict.xlsx' is the original file where the edits are made,
### but in order to avoid loading .xlsx file, the 'Outcome_dict' sheet is saved
### as .csv and loaded dictionary here
dict_outcomes = fread(file.path(project_path, 'Data', 'Clean', 'Other', 'Outcome_dict.csv')) %>% 
  filter(!is.na(nr)) %>%  filter(!is.na(order)) %>%  select_if(~any(!is.na(.))) %>% 
  mutate(name = ifelse(grepl('TSH|ECG|ECM|GP|EHIF|COPD', name), name, str_to_sentence(name)))
  



#
# CUSTOM FUNCTIONS --------------------------------------------
#

mean_miss <- function(x){mean(x, na.rm = TRUE)}
median_miss <- function(x){median(x, na.rm = TRUE)}
sum_miss  <- function(x){sum(x, na.rm = TRUE)}
sd_miss   <- function(x){sd(x, na.rm = TRUE)}
lower_ci  <- function(x){mean_miss(x) - 1.96 * (sd_miss(x) / sqrt(length(x)))}
upper_ci  <- function(x){mean_miss(x) + 1.96 * (sd_miss(x) / sqrt(length(x)))}
min_miss  <- function(x){min(x, na.rm = TRUE)}
max_miss  <- function(x){max(x, na.rm = TRUE)}
sf        <- function(x){return(summary(factor(x)))}
pr        <- function(x){return(prop.table(table(x, useNA = "no"))*100)}
pr_na        <- function(x){return(prop.table(table(x, useNA = "ifany"))*100)}


# Flexible rounding
x = c(1.01, .3412)
round_flex = function(x){
  if(abs(x) < 1){
    x = sprintf("%.3f",round(x, 3))
  }else if(abs(x) >= 1  & abs(x) < 10){
    x = sprintf("%.2f",round(x, 2))
  }else if(abs(x) >= 10 & abs(x) < 100){
    x = sprintf("%.1f",round(x, 1))
  }else if(abs(x) >= 100){
    x= sprintf("%.0f",round(x, 0))
  }
  
  return(x)
}


# Get significance stars
sig_stars = function(var){
  
  var = ifelse(var == '<0.001', round(0.00, 2), round(as.numeric(var), 3))
  
  if(is.numeric(var)){
    #var = ifelse(test = var < 0.10 & var >= 0.05, yes  = paste0(var, '$^{\\dotr}$'), no = var)
    var = ifelse(test = var < 0.10 & var >= 0.05, yes  = paste0(var, '$^{*}$'), no = var)
    var = ifelse(test = var < 0.05  & var >= 0.01, yes  = paste0(var, '$^{**}$'), no = var)
    var = ifelse(test = var < 0.01  & var >= 0.00, yes  = paste0(var, '$^{***}$'), no = var)
    #var = ifelse(test = var < 0.001, yes  = paste0(var, '$^{***}$'), no = var)  
  }
  
  var = ifelse(is.na(var), '', var)
  
  return(var)
}

# Function to summarize all columns in the dataset
sf_dataset = function(data){
  for(var1 in names(data)){
    print(var1)
    data = data %>% mutate(value = !!rlang::ensym(var1))
    print(sf(data$value))
  }
}


# Function to find observations above X st. dev. from the mean
outlier_sd = function(data, var, x){
  lower = mean_miss(data[,var]) - x*sd_miss(data[, var])
  upper = mean_miss(data[,var]) + x*sd_miss(data[, var])
  return(which(data[,var] < lower | 
                 data[,var] > upper))
}
#outlier_sd(dta, 'covid_vaccine', 3)



# Extract coefficients from the model into a (semi-)clean LaTeX row


extract_coeftest = function(m1, length1){
  
  ### Extract coefficients
  
  # If OLS
  if(class(m1) %in% c('lm')){
    temp = summary(m1)$coefficients
    temp = data.frame(beta = temp[,1], se = temp[,2], p_value = temp[,4])
  }
  if(class(m1) %in% c('coeftest')){
    temp = temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  } 
  if(class(m1) %in% c('iv_robust')){
    temp = data.frame(beta = m1$coefficients, se = m1$std.error, p_value = m1$p.value)
  }
  
  if(class(m1) %in% c('coxph')){
    m1 = tidy(m1)
    temp = data.frame(beta = m1$estimate, se = m1$std.error, p_value = m1$p.value)
    rownames(temp) = m1$term
  }

  # length1 = 2
  # m1 = lm(var ~  class_code * ecm_include_patient, data = dta_reg)
  # m1 = coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T))
  # temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  
  temp = add_column(temp, .before = 1, 'var' = rownames(temp))
  temp = temp[(nrow(temp)-length1):nrow(temp),]
  
  ### Remove NA coefficent (but print a warning)
  if(any(is.na(temp$beta))){
    print('NA coefficients found!')
    temp = temp %>% filter(!is.na(beta))
  }
  
  ### Round numbers
  temp$beta = sapply(temp$beta, round_flex)
  temp$se   = str_trim(paste0('(', sapply(temp$se, round_flex), ')'))
  
  ### Add significance stars
  #temp$beta = ifelse(test = temp$p_value < 0.10 & temp$p_value >= 0.05, yes  = paste0(temp$beta, '$^{\\dotr}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.10 & temp$p_value >= 0.05, yes  = paste0(temp$beta, '$^{*}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.05  & temp$p_value >= 0.01, yes  = paste0(temp$beta, '$^{**}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.01, yes  = paste0(temp$beta, '$^{***}$'), no = temp$beta)
  #temp$beta = ifelse(test = temp$p_value < 0.001, yes  = paste0(temp$beta, '$^{***}$'), no = temp$beta)
  
  
  ### To long
  temp$var_clean = temp$var
  temp = gather(temp, key, beta, -c(var, var_clean, p_value, se))
  temp = temp[order(temp$var), ]
  
  ### Clean variable (row) names
  #temp = temp %>% mutate(across(c(var_clean), ~ paste0('\\multirow{2}{*}{', .x, '}')))
  #temp$var_clean[seq(2,nrow(temp), 2)] = ''
  
  
  ###  Put minuses in $ signs, as otherwise they won't print correctly in LaTeX
  temp$beta = gsub('-', '$-$', temp$beta, fixed=T)
  
  ###  Put all columns in one dataframe column with LaTeX table separators
  # temp$cell1 = paste0(apply(temp[,c('var_clean', 'beta')], 1, paste, collapse = "&"), '\\')
  
  ### Add SE to beta coefficients
  temp = temp %>% mutate(beta_se = paste(beta, se))
  
  ### Final selection of columns
  temp = temp %>% dplyr::select(c(var, beta_se))
  
  ###  Return
  return(temp)
}



### Mode
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){x = x[!is.na(x)]}
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


### Function to write log messages
write_log <- function(message, 
                      log_file = file.path(project_path, 'R Scripts', '00_Log_reports/discrepancy_report.log')) {
  write(paste(Sys.time(), " - ", message, sep = ""), file = log_file, append = TRUE)
}


### Function to write discrepancy reports to CSV
write_discrepancy_report <- function(data, step, 
                                     file_name = file.path(project_path, 'R Scripts', '00_Log_reports/discrepancy_report.log')) {
  report <- data.table(
    step = step,
    observations = nrow(data),
    variables = length(names(data))
  )
  fwrite(report, file_name, row.names = FALSE, append = TRUE)
}



#
# GGPLOT THEME --------------------------------------------
#



theme_set(  theme_minimal() +
              theme(
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(),
                axis.ticks = element_blank(),
                axis.title.x = element_text(face = 'plain', color = 'black', size = 34, family = 'Calibiri'),
                axis.title.y = element_text(face = 'plain', color = 'black', size = 34, family = 'Calibiri'),
                axis.text.x  = element_text(face = 'plain', color = 'black', size = 27, family = 'Calibiri'),
                axis.text.y  = element_text(face = 'plain', color = 'black', size = 27, family = 'Calibiri'),
                legend.text = element_text(face = 'plain', size = 28, family = 'Calibiri'),
                legend.title = element_text(face = 'plain', size = 40, family = 'Calibiri'),
                legend.position = "top",
                legend.direction  = "horizontal",
                legend.key.height = unit(2.5, "cm"),
                plot.background = element_rect(fill='white', color=NA), #transparent plot bg
                #panel.grid.major.x = element_blank(), #remove major gridlines
                #panel.grid.major.y = element_blank(), #remove major gridlines
                panel.grid.minor.x = element_blank(), #remove minor gridlines
                panel.grid.minor.y = element_blank(), #remove minor gridlines
                strip.text = element_markdown(size = 20, face = 'bold'),
                strip.background = element_rect(fill = 'white', color = NA),
                plot.title = element_markdown(color = 'black', size = 33, hjust = 0.5, family = 'Calibiri'),
                plot.subtitle = element_markdown(color = 'grey30', size = 25, hjust = 0.5, family = 'Calibiri'),
                plot.caption= element_textbox_simple(color = 'black', size = 11, hjust = 0, family = 'Calibiri'),
              )
)



#
# END OF CODE -----------------------------------------------------------
#
