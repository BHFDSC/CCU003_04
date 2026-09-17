# Script to get ready for extraction the data used to produce the plots.
# This is necessary in order to put the plots from all 3 nations together
# in the same grid.

setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_adm_monthly.csv", stringsAsFactors = F)[,-1]
less5_any = which(data$adm_any <= 5)
less5_prim = which(data$adm_prim <= 5)
data$adm_prim[less5_prim] = NA
setwd("~/CCU003_04/Jun2022/Outputs")
write.csv(data,"data_to_plot_adm_monthly.csv")

setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_proc_monthly.csv", stringsAsFactors = F)[,-1]
less5_proc = which(data$proc <= 5)
data$proc[less5_proc] = NA
setwd("~/CCU003_04/Jun2022/Outputs")
write.csv(data,"data_to_plot_proc_monthly.csv")

setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_adm_all_monthly.csv", stringsAsFactors = F)[,-1]
# No less than five cells.
setwd("~/CCU003_04/Jun2022/Outputs")
write.csv(data,"data_to_plot_adm_all_monthly.csv")

setwd("~/CCU003_04/Jun2022/results")
data = read.csv("data_to_plot_proc_all_monthly.csv", stringsAsFactors = F)[,-1]
less5_proc = which(data$proc <= 5)
data$proc[less5_proc] = NA
setwd("~/CCU003_04/Jun2022/Outputs")
write.csv(data,"data_to_plot_proc_all_monthly.csv")