library(DBI)
library('odbc')
library('tidyverse')
library('dplyr')
library('data.table')
library('ggplot2')
library("tableone")
#getwd()
#setwd("/directory") # specify the directory
con <- dbConnect( odbc::odbc(), "Databricks", timeout = 60, 
                  PWD=rstudioapi::askForPassword("Please enter your Databricks personal access token"))


#     latest data updates *****


# ccu003_04_nh_final_admissions_acs_dx_0622
# ccu003_04_nh_final_admissions_acs_op_0622
# ccu003_04_nh_final_admissions_hf_dx_0622
# ccu003_04_nh_final_admissions_hf_op_0622
# 
# ccu003_04_kmc_final_AA_admissions_dx_v5
# ccu003_04_kmc_final_AA_admissions_op_v5
# ccu003_04_kmc_final_pad_admissions_dx_v2
# ccu003_04_kmc_final_pad_admissions_op_v1
# ccu003_04_kmc_final_stroke_admissions_dx_v1
# ccu003_04_kmc_final_stroke_admissions_op_v1
# ccu003_04_kmc_final_vte_admissions_dx_v1
# ccu003_04_kmc_final_vte_admissions_op_v1


#     ACS Dx ******

nh_final_acs_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_acs_dx_0622')

# Annual	***


acs_admissions_dx_summary_electiveVsemergency <- nh_final_acs_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(ihd_diagnosis_primary = ifelse(ihd_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_ihd_primary = sum(ihd_diagnosis_primary)) %>%
  ungroup()

acs_adm_summary_electiveVsemergency <- acs_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_ihd_primary = round(mean(adm_ihd_primary))) %>%
  ungroup() %>%
  bind_rows(acs_admissions_dx_summary_electiveVsemergency) %>%
  arrange(emergency, adm_year) %>%
  mutate(condition = "acs_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_ihd_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(acs_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_adm_summary_electiveVsemergency.csv",
          row.names = T)

#	Monthly ****
acs_admissions_dx_summary_electiveVsemergency_mo <- nh_final_acs_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(ihd_diagnosis_primary = ifelse(ihd_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_ihd_primary = sum(ihd_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_ihd_primary = round(mean(adm_ihd_primary))) %>%
  ungroup() %>%
  mutate(condition = "acs_adm") %>%
  select(condition, everything()) 

write.csv(acs_admissions_dx_summary_electiveVsemergency_mo,
          "acs_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)



#     *** ACS OP ******

nh_final_acs_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_acs_op_0622')


#		Annual	

acs_admissions_op_summary_electiveVsemergency <- nh_final_acs_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(acs_op_any = ifelse(coronary_procedure == 0, 0, 1),
         acs_op_pci = ifelse(coronary_procedure %in% c(1,2), 1, 0),
         acs_op_cabg = ifelse(coronary_procedure %in% c(3), 1, 0)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("acs_")), sum) %>%
  ungroup()

acs_op_summary_electiveVsemergency <- acs_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency,adm_year) %>%
  summarise_at(vars(starts_with("acs_")), mean) %>%
  ungroup() %>%
  bind_rows(acs_admissions_op_summary_electiveVsemergency) %>%
  arrange(emergency, adm_year) %>%
  #mutate(condition = "ACS_op") %>%
  #select(condition, everything()) %>%
  pivot_longer(!c(emergency, adm_year), names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(acs_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_op_summary_electiveVsemergency.csv",
          row.names = T)

#	Monthly
acs_admissions_op_summary_electiveVsemergency_mo <- nh_final_acs_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(acs_op_any = ifelse(coronary_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(acs_op_any = sum(acs_op_any)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency,adm_year,adm_mo) %>%
  summarise(acs_op_any = round(mean(acs_op_any))) %>%
  ungroup() 

write.csv(acs_admissions_op_summary_electiveVsemergency_mo,
          "acs_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)


#     hf Dx ******

nh_final_hf_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_hf_dx_0622')

#		Annual		


hf_admissions_dx_summary_electiveVsemergency <- nh_final_hf_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_diagnosis_primary = ifelse(hf_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_hf_primary = sum(hf_diagnosis_primary)) %>%
  ungroup()

hf_adm_summary_electiveVsemergency <- hf_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_hf_primary = round(mean(adm_hf_primary))) %>%
  ungroup() %>%
  bind_rows(hf_admissions_dx_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "hf_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_hf_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(hf_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_adm_summary_electiveVsemergency.csv",
          row.names = T)
		  
		  
#			Monthly
hf_admissions_dx_summary_electiveVsemergency_mo <- nh_final_hf_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_diagnosis_primary = ifelse(hf_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_hf_primary = sum(hf_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_hf_primary = round(mean(adm_hf_primary))) %>%
  ungroup() 

summary(hf_admissions_dx_summary_electiveVsemergency_mo)

write.csv(hf_admissions_dx_summary_electiveVsemergency_mo,
          "hf_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)


#     *** HF OP ******

nh_final_hf_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_hf_op_0622')


#		Annual
hf_admissions_op_summary_electiveVsemergency <- nh_final_hf_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_op_any = ifelse(heart_failure_procedure == 0, 0, 1),
         hf_op_pm = ifelse(heart_failure_procedure %in% c(1,2), 1, 0),
         hf_op_vad = ifelse(heart_failure_procedure %in% c(3,4,5), 1, 0)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("hf_op_")), sum) %>%
  ungroup()

hf_op_summary_electiveVsemergency <- hf_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency,adm_year) %>%
  summarise_at(vars(starts_with("hf_op_")), mean) %>%
  ungroup() %>%
  bind_rows(hf_admissions_op_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  pivot_longer(!c(emergency, adm_year), names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(hf_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_op_summary_electiveVsemergency.csv",
          row.names = T)

#		Monthly
hf_admissions_op_summary_electiveVsemergency_mo <- nh_final_hf_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_op_any = ifelse(heart_failure_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(hf_op_any = sum(hf_op_any)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency,adm_year,adm_mo) %>%
  summarise(hf_op_any = round(mean(hf_op_any))) %>%
  ungroup() 

summary(hf_admissions_op_summary_electiveVsemergency_mo)

write.csv(hf_admissions_op_summary_electiveVsemergency_mo,
          "hf_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)



#     AA Dx ******

kmc_final_aa_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_AA_admissions_dx_v5')

# 	Annual

aa_admissions_dx_summary_electiveVsemergency <- kmc_final_aa_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_diagnosis_primary = ifelse(aa_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_aa_primary = sum(aa_diagnosis_primary)) %>%
  ungroup()

aa_adm_summary_electiveVsemergency <- aa_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_aa_primary = round(mean(adm_aa_primary))) %>%
  ungroup() %>%
  bind_rows(aa_admissions_dx_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "aa_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_aa_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(aa_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_adm_summary_electiveVsemergency.csv",
          row.names = T)

# Monthly
aa_admissions_dx_summary_electiveVsemergency_mo <- kmc_final_aa_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_diagnosis_primary = ifelse(aa_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_aa_primary = sum(aa_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_aa_primary = round(mean(adm_aa_primary))) %>%
  ungroup() 

write.csv(aa_admissions_dx_summary_electiveVsemergency_mo,
          "aa_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)


#     *** AA OP ******

kmc_final_aa_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_AA_admissions_op_v5')

# Annual

aa_admissions_op_summary_electiveVsemergency <- kmc_final_aa_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_procedure = ifelse(aa_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(aa_procedure = sum(aa_procedure)) %>%
  ungroup()

aa_op_summary_electiveVsemergency <- aa_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(aa_procedure = round(mean(aa_procedure))) %>%
  ungroup() %>%
  bind_rows(aa_admissions_op_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "aa_op") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = aa_procedure,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(aa_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_op_summary_electiveVsemergency.csv",
          row.names = T)


# Monthly
aa_admissions_op_summary_electiveVsemergency_mo <- kmc_final_aa_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_procedure = ifelse(aa_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(aa_procedure = sum(aa_procedure)) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(aa_procedure = round(mean(aa_procedure))) %>%
  ungroup() 

write.csv(aa_admissions_op_summary_electiveVsemergency_mo,
          "aa_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)

#     PAD Dx ******

kmc_final_pad_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_pad_admissions_dx_v2')

# 	Annual
pad_admissions_dx_summary_electiveVsemergency <- kmc_final_pad_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_diagnosis_primary = ifelse(pad_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_pad_primary = sum(pad_diagnosis_primary)) %>%
  ungroup()

pad_adm_summary_electiveVsemergency <- pad_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_pad_primary = round(mean(adm_pad_primary))) %>%
  ungroup() %>%
  bind_rows(pad_admissions_dx_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "pad_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_pad_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(pad_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_adm_summary_electiveVsemergency.csv",
          row.names = T)


# Monthly
pad_admissions_dx_summary_electiveVsemergency_mo <- kmc_final_pad_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_diagnosis_primary = ifelse(pad_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_pad_primary = sum(pad_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_pad_primary = round(mean(adm_pad_primary))) %>%
  ungroup() 

write.csv(pad_admissions_dx_summary_electiveVsemergency_mo,
          "pad_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)



#     *** PAD OP ******

kmc_final_pad_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_pad_admissions_op_v1')

# 	Annual


pad_admissions_op_summary_electiveVsemergency <- kmc_final_pad_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_op_any = ifelse(pad_procedure == 0, 0, 1),
         pad_op_lr = ifelse(pad_procedure %in% c(1), 1, 0),
         pad_op_pla = ifelse(pad_procedure %in% c(2), 1, 0)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("pad_op_")), sum) %>%
  ungroup()

pad_op_summary_electiveVsemergency <- pad_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("pad_op_")), mean) %>%
  ungroup() %>%
  bind_rows(pad_admissions_op_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  pivot_longer(!c(emergency, adm_year), names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(pad_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_op_summary_electiveVsemergency.csv",
          row.names = T)


#	Monthly
pad_admissions_op_summary_electiveVsemergency_mo <- kmc_final_pad_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_op_any = ifelse(pad_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(pad_op_any = sum(pad_op_any)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(pad_op_any = round(mean(pad_op_any))) %>%
  ungroup()

write.csv(pad_admissions_op_summary_electiveVsemergency_mo,
          "pad_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)



#     stroke Dx ******

kmc_final_stroke_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_stroke_admissions_dx_v1')

# Annual


stroke_admissions_dx_summary_electiveVsemergency <- kmc_final_stroke_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(stroke_diagnosis_primary = ifelse(stroke_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_stroke_primary = sum(stroke_diagnosis_primary)) %>%
  ungroup()

stroke_adm_summary_electiveVsemergency <- stroke_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_stroke_primary = round(mean(adm_stroke_primary))) %>%
  ungroup() %>%
  bind_rows(stroke_admissions_dx_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "stroke_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_stroke_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(stroke_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_adm_summary_electiveVsemergency.csv",
          row.names = T)
		  
# Monthly
stroke_admissions_dx_summary_electiveVsemergency_mo <- kmc_final_stroke_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(stroke_diagnosis_primary = ifelse(stroke_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_stroke_primary = sum(stroke_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_stroke_primary = round(mean(adm_stroke_primary))) %>%
  ungroup() 

write.csv(stroke_admissions_dx_summary_electiveVsemergency_mo,
          "stroke_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)


#     *** stroke OP ******

kmc_final_stroke_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_stroke_admissions_op_v1')


# Annual


stroke_admissions_op_summary_electiveVsemergency <- kmc_final_stroke_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(stroke_procedure= carotid_procedure) %>%
  mutate(stroke_op_ces = ifelse(stroke_procedure == 0, 0, 1),
         stroke_op_thr = ifelse(thrombolysis_is + thrombectomy_is >= 1, 1, 0),
         stroke_op_coil = ifelse(coiling == 1, 1, 0)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("stroke_op_")), sum) %>%
  ungroup()

stroke_op_summary_electiveVsemergency <- stroke_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise_at(vars(starts_with("stroke_op_")), mean) %>%
  ungroup() %>%
  bind_rows(stroke_admissions_op_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  pivot_longer(!c(emergency, adm_year), names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(stroke_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_op_summary_electiveVsemergency.csv",
          row.names = T)


# Monthly
stroke_admissions_op_summary_electiveVsemergency_mo <- kmc_final_stroke_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(stroke_procedure= carotid_procedure) %>%
  mutate(stroke_op_ces = ifelse(stroke_procedure == 0, 0, 1),
         stroke_op_thr = ifelse(thrombolysis_is + thrombectomy_is >= 1, 1, 0),
         stroke_op_coil = ifelse(coiling == 1, 1, 0),
         stroke_op_any = (stroke_procedure + thrombolysis_is + thrombectomy_is + coiling >= 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise_at(vars(starts_with("stroke_op_")), sum) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise_at(vars(starts_with("stroke_op_")), funs(round(mean(.)))) %>%
  ungroup() 

write.csv(stroke_admissions_op_summary_electiveVsemergency_mo,
          "stroke_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)



#     vte Dx ******

kmc_final_vte_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx_v1')

# Annual

vte_admissions_dx_summary_electiveVsemergency <- kmc_final_vte_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(vte_diagnosis_primary = ifelse(vte_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_vte_primary = sum(vte_diagnosis_primary)) %>%
  ungroup()

vte_adm_summary_electiveVsemergency <- vte_admissions_dx_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(adm_vte_primary = round(mean(adm_vte_primary))) %>%
  ungroup() %>%
  bind_rows(vte_admissions_dx_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "vte_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_vte_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(vte_adm_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_adm_summary_electiveVsemergency.csv",
          row.names = T)
		  
# Monthly
vte_admissions_dx_summary_electiveVsemergency_mo <- kmc_final_vte_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(vte_diagnosis_primary = ifelse(vte_diagnosis_primary == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_vte_primary = sum(vte_diagnosis_primary)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(adm_vte_primary = round(mean(adm_vte_primary))) %>%
  ungroup() 


write.csv(vte_admissions_dx_summary_electiveVsemergency_mo,
          "vte_admissions_dx_summary_electiveVsemergency_mo.csv",
          row.names = T)


#     *** vte OP ******

kmc_final_vte_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_op_v1')

# Annual

vte_admissions_op_summary_electiveVsemergency <- kmc_final_vte_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(vte_procedure= pe_procedure) %>%
  mutate(vte_procedure = ifelse(vte_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year) %>%
  summarise(vte_procedure = sum(vte_procedure)) %>%
  ungroup()

vte_op_summary_electiveVsemergency <- vte_admissions_op_summary_electiveVsemergency %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(emergency, adm_year) %>%
  summarise(vte_procedure = round(mean(vte_procedure))) %>%
  ungroup() %>%
  bind_rows(vte_admissions_op_summary_electiveVsemergency) %>%
  arrange(adm_year) %>%
  mutate(condition = "vte_op") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = vte_procedure,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(vte_op_summary_electiveVsemergency,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_op_summary_electiveVsemergency.csv",
          row.names = T)


# Monthly
vte_admissions_op_summary_electiveVsemergency_mo <- kmc_final_vte_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate), adm_mo = month(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(vte_procedure= pe_procedure) %>%
  mutate(vte_procedure = ifelse(vte_procedure == 0, 0, 1)) %>%
  mutate(emergency = if_else(emergency_admission == 1, "Emergency", "Elective")) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(vte_procedure = sum(vte_procedure)) %>%
  ungroup() %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, adm_year)) %>%
  group_by(emergency, adm_year, adm_mo) %>%
  summarise(vte_procedure = round(mean(vte_procedure))) %>%
  ungroup() 

write.csv(vte_admissions_op_summary_electiveVsemergency_mo,
          "vte_admissions_op_summary_electiveVsemergency_mo.csv",
          row.names = T)


