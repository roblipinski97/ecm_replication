
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


#
# READ DATA -------------------------------------------------------------------
#

### Treatment groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible =  fread(file.path(project_path, 'Data','Clean',  'ECM Inclusion', "patient_eligible4_eligible_clinics.csv")) %>% mutate(id = as.character(id))

# Leave only ECM treatment and control (at participating providers) as per Ben's outline for 01/12/2023
patient_ecm_eligible = patient_ecm_eligible %>% 
  filter(ecm_status_list == 'Participating' & ecm_include_patient %in% c('Control', 'Treatment')) 

### Re-randomized groups   ---------------------------------------------------------------------------------------------------
patient_ecm_eligible_randp = readRDS(file.path(project_path, 'Data', 'Clean', 'Re-randomizations', paste0('randp', n1, ' (seed ', seed1,  ').RData')))


### Billing data ----------------------------------------------------------------------------------
dta_diagnosis = read_parquet(file.path(project_path, 'Data/Clean', 'Diagnoses', 'dta_diagnosis.parquet'))


### First hospitalization date ---------------------------------------------------------
### Find minimum date of occurrence per patient-period
temp = dta_diagnosis %>% 
  filter(n_inpatient_any == T | n_inpatient_any == 1) %>% # ... filter only positive instances of the variable
  filter(treat_period == 1) %>%  # ... in the post-treatment period
  dplyr::select(c(id, year_month_day)) %>% # ... only necessary columns
  group_by(id) %>%  # ... group by patient 
  filter(year_month_day == min(year_month_day)) %>% ungroup() %>%  # ... find first instance of the variable
  distinct() %>%  # .... leave only unique rows
  rename('dateofhospitalization' = 'year_month_day')


### Combine
dta_deaths = left_join(patient_ecm_eligible, temp)


#
# PREPARE SURVIVAL DATA -------------------------------------------------------------------
#

# Survival time + status
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


### Save object  -------------------------------------------------------------------
dta_deaths_save = dta_deaths


### Run all models to extract p-values   -------------------------------------------------------------------

### Leave only required columns
dta_deaths_sub = dta_deaths %>% dplyr::select(c(id, list_id, strata, class_code, age, gender,
                                                status_hosp, time_hosp, status_death, time_death))



#### Deaths + all    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'figureA3_randp_death_all.csv'))){
  randp_all = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      # filter(class_code %in% c('mild')) %>% 
      # fit model
      coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_all, file.path(project_path, 'Tables', 'figureA3_randp_death_all.csv'), row.names = F, na = NA)
} 


#### Deaths + mild    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'figureA3_randp_death_mild.csv'))){
  randp_mild = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      filter(class_code %in% c('mild')) %>% 
      # fit model
      coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_mild, file.path(project_path, 'Tables', 'figureA3_randp_death_mild.csv'), row.names = F, na = NA)
}


#### Deaths + severe    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'figureA3_randp_death_severe.csv'))){
  randp_severe = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      filter(class_code %in% c('severe')) %>% 
      # fit model
      coxph(Surv(time_death, status_death) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_severe, file.path(project_path, 'Tables', 'figureA3_randp_death_severe.csv'), row.names = F, na = NA)
}

#### Hosp + all    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'CSV', 'figureA3_randp_hosp_all.csv'))){
  randp_all = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      # filter(class_code %in% c('mild')) %>% 
      # fit model
      coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_all, file.path(project_path, 'Tables', 'figureA3_randp_hosp_all.csv'), row.names = F, na = NA)
} 


#### Hosp + mild    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'figureA3_randp_hosp_mild.csv'))){
  randp_mild = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      filter(class_code %in% c('mild')) %>% 
      # fit model
      coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_mild, file.path(project_path, 'Tables', 'figureA3_randp_hosp_mild.csv'), row.names = F, na = NA)
}



#### Deaths + severe    -------------------------------------------------------------------
if(!file.exists(file.path(project_path, 'Tables', 'figureA3_randp_hosp_severe.csv'))){
  randp_severe = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
    print(i)
    
    # ... create dataframe with re-randomized assignments
    data.frame(subject_id = dta_deaths$id, 
               iteration = i,
               rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
      rename_all(~c('id', 'iteration', 'ecm_include_patient_randp')) %>%
      # ... combine
      left_join(dta_deaths)%>% 
      #... re-calculate weights
      group_by(list_id, class_code) %>% 
      mutate(N = n(),
             N_treat = sum(ecm_include_patient_randp == 'Treatment')) %>%
      ungroup() %>% 
      group_by(list_id, class_code, ecm_include_patient_randp) %>%
      mutate(weight = N/n()) %>% ungroup() %>% 
      group_by(list_id, class_code) %>%
      mutate(N = n()) %>% 
      # ... filter risk group
      filter(class_code %in% c('severe')) %>% 
      # fit model
      coxph(Surv(time_hosp, status_hosp) ~ strata + age + gender + ecm_include_patient_randp,
            cluster = list_id,  weights = weight,
            data = .) %>%
      summary %>% 
      .$coefficients %>% as.data.frame() %>%  slice(nrow(.)) %>%
      mutate(iteration = i)
  })) 
  
  fwrite(randp_severe, file.path(project_path, 'Tables', 'figureA3_randp_hosp_severe.csv'), row.names = F, na = NA)
}

### Get randomization inference p-values    -------------------------------------------------------------------
# i.e. find what % of re-randomized models yields coefficient higher (in absolute terms) than the ones
# recovered using the actual randomization
temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_hosp_all.csv'))
pr(abs(temp$coef) > 0.086)

temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_hosp_mild.csv'))
pr(abs(temp$coef) > 0.081)

temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_hosp_severe.csv'))
pr(abs(temp$coef) > 0.102)


temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_death_all.csv'))
pr(abs(temp$coef) > 0.221)

temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_death_mild.csv'))
pr(abs(temp$coef) > 0.605)

temp = fread(file.path(project_path, 'Tables', 'figureA3_randp_death_severe.csv'))
pr(abs(temp$coef) > 0.099)


### Convert re-randomization to a long dataframe format, combining with outcomes   -------------------------------------------------------------------
temp = do.call(rbind, lapply(seq_along(patient_ecm_eligible_randp), function(i) {
  data.frame(subject_id = dta_deaths$id, 
             iteration = i,
             rerandomized_assignment = patient_ecm_eligible_randp[[i]]) %>% 
    rename_all(~c('id', 'iteration', 'ecm_include_patient_randp'))
})) %>%
  left_join(dta_deaths)


#
# LOOP ACROSS OUTCOMES  --------------------------------------------------------------------------------
#

for(type1 in c('hosp', 'death')){
  
  # iteratively defining the outcome variable
  if(type1 == 'hosp'){
    dta_deaths = dta_deaths %>% mutate(status = status_hosp,
                                       time = time_hosp) 
    temp = temp %>% mutate(status = status_hosp,
                           time = time_hosp) 
  }
  
  if(type1 == 'death'){
    dta_deaths = dta_deaths %>% mutate(status = status_death,
                                       time = time_death) 
    temp = temp %>% mutate(status = status_death,
                           time = time_death) 
  }
  

  ### Calculate survival rate using original treatment assignment  -------------------------------------------------------------------------------
  dta_plot <- dta_deaths %>%
    filter(ecm_include_patient == 'Treatment') %>% 
    arrange(time) %>%
    mutate(event_count = cumsum(status),
           number_at_risk = n() - lag(event_count, default = 0),
           survival_rate = (n() - event_count) / n())
  
  
  
  ### Do-the same for re-randomized data  -------------------------------------------------------------------------------
  dta_plot_randp <- temp %>%
    filter(ecm_include_patient_randp == 'Treatment') %>% 
    group_by(iteration) %>% 
    arrange(time) %>%
    mutate(event_count = cumsum(status),
           number_at_risk = n() - lag(event_count, default = 0),
           survival_rate = (n() - event_count) / n())
  
  ### Duplicate to have both total and risk-group-level survival rates  -------------------------------------------------------------------------------
  
  temp = rbind(
    dta_plot_randp %>% 
      mutate(class_code2 = ifelse(class_code == 'mild',
                                  paste0('<b>Panel B: Mild-risk patients</b> (N=', format(nrow(dta_plot[dta_plot$class_code == 'mild']), big.mark = ","),')'),
                                  paste0('<b>Panel C: Severe-risk patients</b> (N=', format(nrow(dta_plot[dta_plot$class_code == 'severe']), big.mark = ","),')'))),
    dta_plot_randp %>% 
      mutate(class_code2 = paste0('<b>Panel A: All-risk patients</b> (N=', format(nrow(dta_plot), big.mark = ","),')'))
  )
  
  #fwrite(temp, file.path(project_path, 'Tables', paste0('surival_randp_', type1,'.csv')), row.names = F, na = NA)
  
  ### Plot -------------------------------------------------------------------------------
  
  g1=ggplot() +
    geom_step(data = temp, 
              aes(x = time, y = survival_rate,
                  group = iteration),
              color = 'grey50', linewidth = .1) +
    geom_step(data = dta_plot, 
              aes(x = time, y = survival_rate),
              color = '#C87609', linewidth = 2) +
    facet_wrap(~class_code2, nrow = 1) +
    
    # scale_color_manual(values = c( '#C87609', 'grey50')) +
    # scale_size_manual(values = c(1.5, .1, 1.5, .1)) +
    guides(color = 'none', size = 'none') + 
    labs(
      
      x ="\n Days since ECM onset (June 2021)",
      y =  "\n Survival probability\n",
      
     ) +
    theme_minimal() +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(face = 'plain', color = 'black', size = 34),
      axis.title.y = element_text(face = 'plain', color = 'black', size = 34),
      axis.text.x  = element_text(face = 'plain', color = 'black', size = 27),
      axis.text.y  = element_text(face = 'plain', color = 'black', size = 27),
      legend.text = element_text(face = 'plain', size = 28),
      legend.title = element_blank(),
      legend.position = "top",
      legend.direction  = "horizontal",
      legend.key.height = unit(2.5, "cm"),
      plot.background = element_rect(fill='white', color=NA), #transparent plot bg
      #panel.grid.major.x = element_blank(), #remove major gridlines
      #panel.grid.major.y = element_blank(), #remove major gridlines
      panel.grid.minor.x = element_blank(), #remove minor gridlines
      panel.grid.minor.y = element_blank(), #remove minor gridlines
      strip.text = element_markdown(size = 27),
      strip.background = element_rect(fill = 'white', color = NA),
      plot.title = element_markdown(color = 'black', size = 41, hjust = 0.5),
      plot.subtitle = element_markdown(color = 'grey30', size = 35, hjust = 0.5),
      plot.caption= element_textbox_simple(color = 'black', size = 15, hjust = 0),
    ) 
  
  
  ggsave(file.path(project_path, 'Figures', 
                   paste0(ifelse(type1 == 'hosp', 'Figure A3 Survival Hospitalization', 'Figure A3 Survival Mortality'),
                          '_', n1, '_', seed1, '.png')),
         plot = g1, width = 78, height = 23, unit = 'cm')
  
  
  
}


temp = fread(file.path(project_path, 'Tables', paste0('surival_randp_', type1,'.csv')))

dim(temp)

#
# END OF CODE -----------------------------------------------------------------------------
#