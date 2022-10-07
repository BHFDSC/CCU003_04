#Define the packages we need
packages=c("tidyverse",
           "readxl",
           "RColorBrewer",
           "scales",
           "lubridate")

#Install any missing packages
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load packages
sapply(packages, library, character.only = TRUE)

#########SET WORKING DIRECTORY###############

directory=file.path("C:", "Users", "cheemak","OneDrive - British Heart Foundation","Documents","Projects","Data Science Centre","Plots","Raw files","Naz files")

setwd(directory)

#########IMPORT ADMISSION DATA SETS#################

monthly_adm_eng <- read_csv("monthly_adm_eng.csv")
monthly_adm_sco <- read_csv("monthly_adm_sco.csv")
monthly_adm_wales <- read_csv("monthly_adm_wales.csv")

####### DRAW ADMISSION CHARTS #########

#ALL ADMISSIONS

all_monthly_admissions <- bind_rows(monthly_adm_eng, monthly_adm_sco, monthly_adm_wales) %>%
  group_by(country, adm_year, adm_month,condition) %>% 
  summarise(value = sum(value)) %>% 
 # filter(type == 0) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6
  
all_admissions_plot <- all_monthly_admissions %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.7) +
  labs(x = "Months",
       y = "Number of admissions (primary)", 
       title = "All admissions",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) +
  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

all_admissions_plot

#ELECTIVE ADMISSIONS

el_monthly_admissions <- bind_rows(monthly_adm_eng, monthly_adm_sco, monthly_adm_wales) %>%
  filter(type == 0) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6

el_admissions_plot <- el_monthly_admissions %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.7) +
  labs(x = "Months",
       y = "Number of admissions (primary)", 
       title = "Elective admissions",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) + 
  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

el_admissions_plot

#EMERGENCY ADMISSIONS

em_monthly_admissions <- bind_rows(monthly_adm_eng, monthly_adm_sco, monthly_adm_wales) %>%
  filter(type == 1) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6

em_admissions_plot <- em_monthly_admissions %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.7) +
  labs(x = "Months",
       y = "Number of admissions (primary)", 
       title = "Emergency admissions",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) + 
  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

em_admissions_plot

#########IMPORT PROCEDURE DATA SETS#################

monthly_proc_eng <- read_csv("monthly_proc_eng.csv")
monthly_proc_sco <- read_csv("monthly_proc_sco.csv")
monthly_proc_wales <- read_csv("monthly_proc_wales.csv")

####### DRAW PROCEDURE CHARTS #########

#ALL PROCS

all_monthly_procs <- bind_rows(monthly_proc_eng, monthly_proc_sco, monthly_proc_wales) %>%
  group_by(country, adm_year, adm_month,condition) %>% 
  summarise(value = sum(value)) %>% 
  # filter(type == 0) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6

all_procs_plot <- all_monthly_procs %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.7) +
  labs(x = "Months",
       y = "Number of procedures", 
       title = "All procedures",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) + 
  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

all_procs_plot

#ELECTIVE PROCS

el_monthly_procs <- bind_rows(monthly_proc_eng, monthly_proc_sco, monthly_proc_wales) %>%
  filter(type == 0) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6

el_procs_plot <- el_monthly_procs %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.7) +
  labs(x = "Months",
       y = "Number of procedures", 
       title = "Elective procedures",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) + 
  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

el_procs_plot

#EMERGENCY PROCS

em_monthly_procs <- bind_rows(monthly_proc_eng, monthly_proc_sco, monthly_proc_wales) %>%
  filter(type == 1) %>% # this needs changing as needed, e.g., for emergency, emergency == 1
  mutate(value = ifelse(value <= 6 | is.na(value), 6, value))# to make sure any missing N or N<6 was replaced with 6

em_procs_plot <- em_monthly_procs %>% 
  ggplot(aes(x=adm_month, y=value, color = adm_year)) +
  geom_line(size = 0.8) +
  labs(x = "Months",
       y = "Number of procedures", 
       title = "Emergency procedures",
       caption = "ACS= Acute Coronary Syndrome, HF = Heart Failure, TIA= Transient Ischaemic Attack, AA= Aortic Aneurysm, PAD = Peripheral Arterial Disease, VTE= Venous Thromboembolism") + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", size = 8)) + 
  scale_color_manual(values=c('#009E73','#56B4E9','#CC79A7')) +
#  scale_color_discrete(name = NULL) +
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = c("J","F","M","A","M","J","J","A","S","O","N","D"),breaks = 1:12) +
  facet_grid(country ~ factor(condition,levels = c("ACS","HF","Stroke","AA","PAD","VTE")), scales = "free")

em_procs_plot



  