
#
# MASTER SCRIPT   -----------------------------------------------------------
#


### This script sources all files needed to replicate the findings, tables, and figures 
### of draft manuscript "Motivating Improved Healthcare Using Holistic Patient Contracts"



### Starting files ----------------------------------------------------------------

### Run the below optionally on the data folders BEFORE any of the scripts, to 
### know which files should be there to start with and which are only created
### with the cleaning code, so that the 'default' folder content can be restored easily;
### this .txt. file is also supplied with the data to start with

# start_files = list.files(file.path(gsub('R Scripts', 'Data', dirname(rstudioapi::getActiveDocumentContext()$path))),
#                          full.names = T, recursive = T)
# writeLines(start_files, file.path(gsub('R Scripts', '', dirname(rstudioapi::getActiveDocumentContext()$path)), "start_files.txt"))

### Run the below optionally, based on either the re-created (lines above) or originally provides 'start_files.txt', to
### ensure only the strictly necessary data are in the folders and everything else is fully re-created with the scripts 
### sourced below

# start_files <- readLines(file.path(gsub('R Scripts', '', dirname(rstudioapi::getActiveDocumentContext()$path)), "start_files.txt")) # (re-)read the starting files list
# current_files = list.files(file.path(gsub('R Scripts', 'Data', dirname(rstudioapi::getActiveDocumentContext()$path))),
#                           full.names = T, recursive = T) # list all current files in 'Data' folder
# files_to_clean <- setdiff(current_files, start_files) # compare to lists
# for (file in files_to_clean){file.remove(file)} # remove anything that doesn't overlap

### Source all R scripts ----------------------------------------------------------------

### List and source all the scripts in order (can adjust by the user)
if(Sys.info()[["user"]] == "wb539995"){
  # XXX
}else if(Sys.info()[["user"]] == "ASUS"){
  
  script_list = list.files(file.path(dirname(rstudioapi::getActiveDocumentContext()$path)),
                            pattern = '.R', full.names = T)
  
  #for(i in script_list){
  for(i in script_list[c(19)]){
    print(i) # control sourcing
    if(grepl('000_MASTER', i)){next} # skip the master file itself
    source(i) # source
  }
}


library(NCmisc)
list.functions.in.file(script_list[1])
sessionInfo()

packageVersion('haven')


list.files(gsub('R Scripts', 'Data/Raw/ECM Inclusion', file.path(dirname(rstudioapi::getActiveDocumentContext()$path))),
           pattern = '.R', full.names = T)

#
# END OF CODE -----------------------------------------------------------
#