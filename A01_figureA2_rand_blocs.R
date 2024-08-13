


#
# SET-UP  -----------------------------------------------------------------------------
# 

### Source the '00_global.R' script with required packages and functions
if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), '00_global.R'))
}

# Make a copy of the file
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('R Scripts', 'R Scripts/00_ARCHIVE', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)




#
# Figure A2 CLINIC RANDOMIZATION GRID -----------------------------------------------------------------------------
#

### Create subfigure (a) of Figure A2

### Read clinic-level data + data on stratification blocs
clinics_all = clean_names(fread(file.path(project_path,  'Data', 'Clean', 'ECM Inclusion', "clinic_all.csv"), encoding = 'UTF-8')) 
strata_blocs = clean_names(fread(file.path(project_path,  'Data', 'Raw', 'ECM Inclusion', "stratified blocks.csv"), encoding = 'UTF-8'))
patient_eligible = clean_names(fread(file.path(project_path,  'Data', 'Clean', 'ECM Inclusion', "patient_eligible7_control_treat_patients.csv"), encoding = 'UTF-8')) 


### Classify not participating clinics (i.e. going down from 93 to 72 and from 143 lists to 98)
clinics_all = clinics_all %>% mutate(ecm_include_clinic = ifelse(
                                        test = ecm_include_clinic == 'ECM' & # clinic is participating if it has 'ECM' status and at least some patients enrolling into ECM
                                                  clinic_registration_code %in% unique(patient_eligible$clinic_registration_code),
                                        yes = 'ECM accept', 
                                        no = ifelse(ecm_include_clinic == 'ECM',
                                                    'ECM refuse', ecm_include_clinic)),
                                     ecm_include_clinic = dplyr::recode(ecm_include_clinic, 'Not ECM' = "'Pure control'"))



# Add N
clinics_all = clinics_all %>% group_by(ecm_include_clinic) %>% mutate(N=n())

# Combine ecm_include_clinic and N
clinics_all$ecm_include_clinic2 = paste0('<b>',clinics_all$ecm_include_clinic, '</b><br>N=', clinics_all$N)
sf(clinics_all$ecm_include_clinic2)


# Re-order
clinics_all$ecm_include_clinic = factor(clinics_all$ecm_include_clinic, levels = c('ECM accept', 'ECM refuse', "'Pure control'",
                                                           'Excluded (pilot)', 'Excluded (size>4)', 'Excluded (single block)'))


clinics_all$ecm_include_clinic2 = factor(clinics_all$ecm_include_clinic2, levels = c(
                                                           '<b>ECM accept</b><br>N=72',
                                                           '<b>ECM refuse</b><br>N=21',
                                                           "<b>'Pure control'</b><br>N=282",
                                                           '<b>Excluded (pilot)</b><br>N=13',
                                                           '<b>Excluded (size>4)</b><br>N=19', 
                                                           '<b>Excluded (single block)</b><br>N=3'))

## Originally Meyhar specifies strata bloc lines manually (see graphs_sampling_note.do) at what 
### looks like just-below values of the min and max scores we have in strata_blocs - DO THE SAME

### Plot ---------------------------------------------------------------------------------------------------------------------------
g1 = ggplot(clinics_all, aes( y = average_clinic_management_score,  x = average_clinic_qbs_score,
                            fill = ecm_include_clinic2, 
                            shape = ecm_include_clinic2)) +
  
  geom_hline(data = strata_blocs, aes(yintercept = average_clinic_management_score_min-.25), linewidth = .4, color = 'grey65') +
  geom_vline(data = strata_blocs, aes(xintercept = average_clinic_qbs_score_min-1), linewidth = .4, color = 'grey65') +

  geom_point(size = 2.5, color = 'black', stroke = .5, alpha = .9)+
  # geom_smooth(method = 'lm', color = 'black', se = F, show.legend = F) +
  scale_fill_manual(name = 'Clinic ecm_include_clinic:',
                    values = c('black', 'grey80', '#8CB7D6','#730808',  '#A32F2F','#D35555'))+
  
  scale_shape_manual(name = '', # 'Clinic ECM status',
                     values = c(21, 21, 1, 4, 2, 6))+
  guides(fill = 'none',
         shape = guide_legend(title.position = 'left', title.hjust = .5,
                              nrow = 1,
                             override.aes = list(size=6, 
                                                 fill = c('black', 'grey60', 'white', 'transparent', 'transparent', 'transparent'),
                                                 color = c('black', 'black', 'black', 'black', 'black', 'black')
                                                 ))
  ) +
  labs(
    x = '\nAdjusted QBS score',
    y = 'Management score\n',
    # title = '<b>ECM Intervention Tracking: </b> Clinics<br>',
    # caption = paste0("<br><b>Notes: </b>",
    #                  'The figure shows clinics by their ECM inclusion ecm_include_clinic, QBS and management scores. ', 
    #                  'Group-coloured smooth lines show lines of best fit estimates using linear trend.  ', "<br>")
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(face = 'bold', color = 'black', size = 29),
    axis.title.y = element_text(face = 'bold', color = 'black', size = 29),
    axis.text.x  = element_text(face = 'plain', color = 'black', size = 24),
    axis.text.y  = element_text(face = 'plain', color = 'black', size = 24),
    legend.text = element_markdown(face = 'plain', size = 17),
    legend.title = element_text(face = 'bold', size = 30),
    legend.position = "top",
    legend.direction  = "horizontal",
    legend.key.height = unit(2.5, "cm"),
    plot.background = element_rect(fill='white', color=NA), #transparent plot bg
    panel.grid.major.x = element_blank(), #remove major gridlines
    panel.grid.major.y = element_blank(), #remove major gridlines
    panel.grid.minor.x = element_blank(), #remove minor gridlines
    panel.grid.minor.y = element_blank(), #remove minor gridlines
    strip.text = element_markdown(size = 24),
    strip.background = element_rect(fill = 'white', color = NA),
    plot.title = element_markdown(color = 'black', size = 53, hjust = 0.5),
    plot.subtitle = element_markdown(color = 'grey30', size = 25, hjust = 0.5),
    plot.caption= element_textbox_simple(color = 'black', size = 18, hjust = 0),
  ) 
  # facet_wrap(~ecm_include_clinic2)


g1

ggsave(plot=g1, file.path(project_path, 'Figures',  paste0('Figure A2a. Randomization (clinic)', ".png")),
       width = 40, height = 21, units = 'cm')


#
# Figure 2B  ECM patient randomization outcome -----------------------------------------------------------------------------
#

### Create subfigure (b) of Figure A2

### Read the original randomization file 'patient_ecm_randomized.csv' + dataframe with all lists/providers
patient_eligible = clean_names(fread(file.path(project_path,  'Data', 'Clean', 'ECM Inclusion', "patient_ecm_randomized.csv"), encoding = 'UTF-8')) 
list_all = clean_names(fread(file.path(project_path,  'Data', 'Clean', 'ECM Inclusion', "list_all.csv"), encoding = 'UTF-8')) 

### Create inclusion-risk class column
patient_eligible = patient_eligible %>% mutate(randomization_outcome = paste0(ecm_include_patient, ' (', class_code  ,')'))

### Only leave participating providers
patient_eligible = patient_eligible %>% filter(list_id %in% list_all$list_id[list_all$ecm_status_list == 'Participating'])


### Group and count by group
patient_eligible = patient_eligible %>% 
  dplyr::select(c(list_id, randomization_outcome)) %>% 
  group_by(list_id, randomization_outcome) %>% 
  mutate(N = n()) %>% 
  summarise_all(., mean) %>%
  ungroup() %>% group_by(list_id) %>% 
  mutate(N_list = sum(N))

### Order categories
patient_eligible$randomization_outcome = factor(patient_eligible$randomization_outcome, 
                                           levels = c('Treatment (severe)',  'Treatment (mild)',
                                                      'Control (severe)', 'Control (mild)'))


patient_eligible = patient_eligible %>% mutate(order = dplyr::recode(randomization_outcome,
                                                           'Treatment (severe)' = 1, 'Treatment (mild)'= 2,
                                                           'Control (severe)' = 3, 'Control (mild)' = 4))


patient_eligible = patient_eligible[order(patient_eligible$order),]

### Get number of participating providers
n2 = n_distinct(patient_eligible$list_id)
n2

### Plot ----------------------------------------------------------------------------------------------------------------------------------
g2 = ggplot(patient_eligible,
            aes(x = reorder(list_id, N_list), y = N,
                group = list_id, 
                fill = randomization_outcome )) +
  geom_bar(position="stack", stat="identity", color = 'black', size = .09) +
  
  scale_fill_manual(name = '',
                    values = c('Control (severe)' = 'grey70',
                               'Control (mild)' = 'grey90',
                               'Treatment (mild)' = 'grey40',
                               'Treatment (severe)' = 'grey10')
  )+
  
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + 
  scale_x_discrete(expand = expansion(add = c(1, 1))) + 
  
  labs(
    x = paste0('\nProvider (N=', n2, ')'), y = 'Total number of patients\n',
    # title = 'Survival rates by ECM treatment group<br>'
  ) +
  
  guides(fill = guide_legend(nrow = 1, byrow=TRUE, override.aes = list(size = 3, color = 'black'))) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(face = 'bold', color = 'black', size = 29),
    axis.title.y = element_text(face = 'bold', color = 'black', size = 29),
    axis.text.x  = element_blank(),
    axis.text.y  = element_text(face = 'plain', color = 'black', size = 24),
    legend.text = element_text(face = 'plain', size = 20),
    legend.title = element_text(face = 'bold', size = 40),
    legend.position = "top",
    legend.direction  = "horizontal",
    legend.key.height = unit(1, "cm"),
    plot.background = element_rect(fill='white', color=NA), #transparent plot bg
    #panel.grid.major.x = element_blank(), #remove major gridlines
    #panel.grid.major.y = element_blank(), #remove major gridlines
    panel.grid.minor.x = element_blank(), #remove minor gridlines
    panel.grid.minor.y = element_blank(), #remove minor gridlines
    strip.text = element_markdown(size = 24),
    strip.background = element_rect(fill = 'white', color = NA),
    plot.title = element_markdown(color = 'black', size = 53, hjust = 0.5),
    plot.subtitle = element_markdown(color = 'grey30', size = 25, hjust = 0.5),
    plot.caption= element_textbox_simple(color = 'black', size = 18, hjust = 0),
  )

g2


ggsave(plot=g2, file.path(project_path, 'Figures',  paste0('Figure A2b. Randomization (patient)', ".png")),
       width = 40, height = 20, units = 'cm')



# Combine plots -----------------------------------------------------------------------------

# g12 = g1+g2 + plot_layout(nrow = 2)

# ggsave(plot=g12, file.path(project_path, 'Figures',  paste0('Figure A2. Randomization', ".png")),
#        width = 40, height = 57, units = 'cm')



#
# END OF CODE -----------------------------------------------------------------------------
#