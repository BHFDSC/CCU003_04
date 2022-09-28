############################################################################################################################################
# 
# This is the top-level script that runs all of the diagnosis and procedure scripts for each of the six conditions. 
#   
# 
# The working directory should be changed to the same directory that contains this script. More information can be found in the README. 
# Prerequisites are as given in the listed scripts  
# 
# 
# Daniel O'Connell - 2022 - British Heart Foundation 
# 
############################################################################################################################################

#Define scripts
diag_scripts=paste0("./diagnoses/", list.files("./diagnoses"))
proc_scripts=paste0("./procedures/",list.files("./procedures"))

#Run all scripts
for (script in c(diag_scripts,proc_scripts)){
  
  print(paste0("Running: ", script))
  #Run script from here
  source(script)
  #Remove variables to clean namespace
  rm(list=ls())
  print("Finsihed without errors")
  
}