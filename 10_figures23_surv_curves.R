
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
# PLOT SURVIVAL RATES (PACKAGE) ---------------------------------------------------------------------
#

### Resources: https://rpkgs.datanovia.com/survminer/

### Define theme  ----------------------------------------------------------------------------------------
t1 = theme_minimal() +
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
    strip.text = element_markdown(size = 20, face = 'bold'),
    strip.background = element_rect(fill = 'white', color = NA),
    plot.title = element_markdown(color = 'black', size = 41, hjust = 0.5),
    plot.subtitle = element_markdown(color = 'grey30', size = 35, hjust = 0.5),
    plot.caption= element_textbox_simple(color = 'black', size = 15, hjust = 0),
  )


### LONGRANK TEST 
### https://en.wikipedia.org/wiki/Logrank_test
# -> The logrank test statistic compares estimates of the hazard functions of the two groups at each 
#    observed event time. It is constructed by computing the observed and expected number of events in
#    one of the groups at each observed event time and then adding these to obtain an overall summary 
#     across all-time points where there is an event.
# -> The logrank statistic can be derived as the score test for the Cox proportional hazards
#    model comparing two groups. It is therefore asymptotically equivalent to the likelihood ratio test statistic based from that model.
### https://datatab.net/tutorial/log-rank-test


### Predict() function for Cox models
# https://stat.ethz.ch/R-manual/R-devel/library/survival/html/predict.coxph.html
# ?predict


### Re-load data   --------------------------------------------------------------------
dta_deaths = readRDS(file.path(project_path, 'Data', 'Clean', 'Aggregated', 'dta_deaths_temp.RData'))


### Leave only ECM control and treatment --------------------------------------------------------------------
dta_plot = dta_deaths %>% filter(!(ecm_include_patient %in% c('Pure control'))) %>% filter(ecm_status_list == 'Participating')


### Loop to make survival curve plots -------------------------------------------

for(fig1 in c('Figure 2', 'Figure 3')){
 
  # Define outcome variables
  if(fig1 == 'Figure 2'){dta_plot = dta_plot %>% mutate(time = time_hosp, status = status_hosp)} 
  if(fig1 == 'Figure 3'){dta_plot = dta_plot %>% mutate(time = time_death, status = status_death)} 
  
  for(risk1 in c('mild', 'severe')){
    
    
    # Leave only desire risk groups
    dta_plot2 = dta_plot %>% filter(class_code %in% risk1)
    
    # Fit the survival curve and model
    fit <- survfit(Surv(time, status) ~ ecm_include_patient,
                   data = dta_plot2)
    summary(fit)
    cox_model <- coxph(Surv(time, status) ~ ecm_include_patient + age + gender, weights = weight, data = dta_plot2)
    
    y_min = .995*min(summary(fit)$lower)
    
    g1 = ggsurvplot(
      fit,
      data = dta_plot2,
      size = 1,                 # change line size
      palette = c("grey67", "black"),# custom color palettes
      conf.int = TRUE,          # Add confidence interval
      pval = F,  pval.method	 = T, pval.coord = c(0, 0.97),
      fontsize=10,
      risk.table = TRUE,        # Add risk table
      risk.table.col = "strata",# Risk table color by groups
      ylim = c(y_min, 1),         # present narrower X axis, but not affect
      xlim = c(0,668),
      
      xlab = "\n Days since ECM onset (June 2021)",   # customize X axis label.
      ylab = "\n Survival probability\n",   # customize Y axis label.
      
      break.time.by = 90,     # break X axis in time intervals by 500.
      legend.labs = c("ECM Control", "ECM Treatment"),    # Change legend labels
      risk.table.height = 0.25, # Useful to change when you have multiple groups
      risk.table.y.text.col = T, # colour risk table text annotations.
      risk.table.y.text = FALSE, # show bars instead of names in text annotations
     ggtheme = t1
    )
    
    ### Save plot
    png(file.path(project_path, 'Figures',
                  paste0(fig1, ' Survival ',
                         ifelse(fig1 == 'Figure 2', 'Hospitalization', 'Mortality'), 
                         ' (', risk1, ").png")),
        width = 6000, height = 4000, res = 210, pointsize = 20)
    
    print(g1)
    
    dev.off()
  }
}



#
# END OF CODE ------------------------------------------------------------------------------------------------------
#