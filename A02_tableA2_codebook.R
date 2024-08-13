
#
# SET-UP ----
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
# TABLE -> LaTeX  ---------------------------------------------------------------------------------------------------
#

### We will create a dictionary of all the variables to print as Table A2 in the Appendix

### Clean coefficients -----------------------------------------------------------------------------------------------------
### Use 'dict_outcomes' read in '00_global.R'

dta_table = dict_outcomes

### Anything empty - replace with a dash
dta_table[dta_table == 'x'] = '$-$'

### Order rows
dta_table = dta_table %>%
  arrange(order, nr) %>% 
  dplyr::select(c(group, name, source, codes, description))


### Put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table %>% dplyr::select(-c(group)), 1, paste, collapse = "&"), '\\')


# Add section headings
for(group1 in unique(dta_table$group)){
  min1 = min(which(dta_table$group == group1))
  dta_table = dta_table %>% add_row(.before = min1)
  dta_table$cell1[min1] = paste0('\\multicolumn{4}{r{.85\\textwidth}}{\\textbf{', group1, '}}\\')
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