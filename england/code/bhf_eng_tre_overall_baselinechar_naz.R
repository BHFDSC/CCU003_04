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


#     ACS Dx ******

nh_final_acs_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_acs_dx_0622')



acs_admissions_dx_summary <- nh_final_acs_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(ihd_diagnosis_primary = ifelse(ihd_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_ihd_primary = sum(ihd_diagnosis_primary)) %>%
  ungroup()

acs_adm_summary <- acs_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_ihd_primary = round(mean(adm_ihd_primary))) %>%
  ungroup() %>%
  bind_rows(acs_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "acs_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_ihd_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(acs_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

acs_dx_table1 <- nh_final_acs_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(ihd_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

acs_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                           strata = "adm_year", factorVars = factorvars,
                           data = acs_dx_table1), formatOptions = list(big.mark = ","))



write.csv(acs_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_dx_table1.csv",
          row.names = T)


#     *** ACS OP ******

nh_final_acs_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_acs_op_0622')


acs_admissions_op_summary <- nh_final_acs_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(acs_op_any = ifelse(coronary_procedure == 0, 0, 1),
         acs_op_pci = ifelse(coronary_procedure %in% c(1,2), 1, 0),
         acs_op_cabg = ifelse(coronary_procedure %in% c(3), 1, 0)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("acs_")), sum) %>%
  ungroup()

acs_op_summary <- acs_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("acs_")), mean) %>%
  ungroup() %>%
  bind_rows(acs_admissions_op_summary) %>%
  arrange(adm_year) %>%
  #mutate(condition = "ACS_op") %>%
  #select(condition, everything()) %>%
  pivot_longer(!adm_year, names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(acs_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

acs_op_table1 <- nh_final_acs_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(coronary_procedure >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

acs_op_any_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = acs_op_table1), 
                       formatOptions = list(big.mark = ","))

acs_op_pci_table1 <-  print(tableone::CreateTableOne(vars = vars, 
                                                     strata = "adm_year", factorVars = factorvars,
                                                     data = acs_op_table1 %>% 
                                                       filter(coronary_procedure %in% c(1,2))), 
                            formatOptions = list(big.mark = ","))

acs_op_cabg_table1 <-  print(tableone::CreateTableOne(vars = vars, 
                                                     strata = "adm_year", factorVars = factorvars,
                                                     data = acs_op_table1 %>% 
                                                       filter(coronary_procedure %in% c(3))), 
                             formatOptions = list(big.mark = ","))


write.csv(acs_op_any_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_op_any_table1.csv",
          row.names = T)

write.csv(acs_op_pci_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_op_pci_table1.csv",
          row.names = T)

write.csv(acs_op_cabg_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/acs_op_cabg_table1.csv",
          row.names = T)



#     hf Dx ******

nh_final_hf_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_hf_dx_0622')


hf_admissions_dx_summary <- nh_final_hf_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_diagnosis_primary = ifelse(hf_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_hf_primary = sum(hf_diagnosis_primary)) %>%
  ungroup()

hf_adm_summary <- hf_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_hf_primary = round(mean(adm_hf_primary))) %>%
  ungroup() %>%
  bind_rows(hf_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "hf_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_hf_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(hf_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")


hf_dx_table1 <- nh_final_hf_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(hf_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

hf_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                               strata = "adm_year", factorVars = factorvars,
                                               data = hf_dx_table1), formatOptions = list(big.mark = ","))



write.csv(hf_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_dx_table1.csv",
          row.names = T)

#     *** HF OP ******

nh_final_hf_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_nh_final_admissions_hf_op_0622')

hf_admissions_op_summary <- nh_final_hf_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(hf_op_any = ifelse(heart_failure_procedure == 0, 0, 1),
         hf_op_pm = ifelse(heart_failure_procedure %in% c(1,2), 1, 0),
         hf_op_vad = ifelse(heart_failure_procedure %in% c(3,4,5), 1, 0)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("hf_op_")), sum) %>%
  ungroup()

hf_op_summary <- hf_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("hf_op_")), mean) %>%
  ungroup() %>%
  bind_rows(hf_admissions_op_summary) %>%
  arrange(adm_year) %>%
  pivot_longer(!adm_year, names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(hf_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

hf_op_table1 <- nh_final_hf_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(heart_failure_procedure >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

hf_op_any_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                               strata = "adm_year", factorVars = factorvars,
                                               data = hf_op_table1), 
                          formatOptions = list(big.mark = ","))

hf_op_pm_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                   strata = "adm_year", factorVars = factorvars,
                                                   data = hf_op_table1 %>%
                                                    filter(heart_failure_procedure %in% c(1,2))), 
                         formatOptions = list(big.mark = ","))

hf_op_vad_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                   strata = "adm_year", factorVars = factorvars,
                                                   data = hf_op_table1 %>%
                                                     filter(heart_failure_procedure %in% c(3,4,5))), 
                          formatOptions = list(big.mark = ","))

write.csv(hf_op_any_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_op_any_table1.csv",
          row.names = T)


write.csv(hf_op_pm_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_op_pm_table1.csv",
          row.names = T)


write.csv(hf_op_vad_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/hf_op_vad_table1.csv",
          row.names = T)

#     AA Dx ******

kmc_final_aa_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_AA_admissions_dx_v5')


aa_admissions_dx_summary <- kmc_final_aa_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_diagnosis_primary = ifelse(aa_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_aa_primary = sum(aa_diagnosis_primary)) %>%
  ungroup()

aa_adm_summary <- aa_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_aa_primary = round(mean(adm_aa_primary))) %>%
  ungroup() %>%
  bind_rows(aa_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "aa_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_aa_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(aa_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")


aa_dx_table1 <- kmc_final_aa_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(aa_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

aa_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                               strata = "adm_year", factorVars = factorvars,
                                               data = aa_dx_table1), formatOptions = list(big.mark = ","))


write.csv(aa_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_dx_table1.csv",
          row.names = T)




#     *** AA OP ******

kmc_final_aa_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_AA_admissions_op_v5')

aa_admissions_op_summary <- kmc_final_aa_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(aa_procedure = ifelse(aa_procedure == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(aa_procedure = sum(aa_procedure)) %>%
  ungroup()

aa_op_summary <- aa_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(aa_procedure = round(mean(aa_procedure))) %>%
  ungroup() %>%
  bind_rows(aa_admissions_op_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "aa_op") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = aa_procedure,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(aa_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

aa_op_table1 <- kmc_final_aa_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(aa_procedure >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

aa_op_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                               strata = "adm_year", factorVars = factorvars,
                                               data = aa_op_table1), formatOptions = list(big.mark = ","))


write.csv(aa_op_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/aa_op_table1.csv",
          row.names = T)


#     PAD Dx ******

kmc_final_pad_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_pad_admissions_dx_v2')


pad_admissions_dx_summary <- kmc_final_pad_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_diagnosis_primary = ifelse(pad_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_pad_primary = sum(pad_diagnosis_primary)) %>%
  ungroup()

pad_adm_summary <- pad_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_pad_primary = round(mean(adm_pad_primary))) %>%
  ungroup() %>%
  bind_rows(pad_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "pad_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_pad_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(pad_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")


pad_dx_table1 <- kmc_final_pad_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(pad_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

pad_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = pad_dx_table1), formatOptions = list(big.mark = ","))



write.csv(pad_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_dx_table1.csv",
          row.names = T)


#     *** PAD OP ******

kmc_final_pad_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_pad_admissions_op_v1')

table(kmc_final_pad_admissions_op$PAD_PROCEDURE)

pad_admissions_op_summary <- kmc_final_pad_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(pad_op_any = ifelse(pad_procedure == 0, 0, 1),
         pad_op_lr = ifelse(pad_procedure %in% c(1), 1, 0),
         pad_op_pla = ifelse(pad_procedure %in% c(2), 1, 0)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("pad_op_")), sum) %>%
  ungroup()

pad_op_summary <- pad_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("pad_op_")), mean) %>%
  ungroup() %>%
  bind_rows(pad_admissions_op_summary) %>%
  arrange(adm_year) %>%
  pivot_longer(!adm_year, names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(pad_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

pad_op_table1 <- kmc_final_pad_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(pad_procedure >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

pad_op_any_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = pad_op_table1), 
                           formatOptions = list(big.mark = ","))

pad_op_lr_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = pad_op_table1 %>%
                                                  filter(pad_procedure %in% c(1))), 
                          formatOptions = list(big.mark = ","))

pad_op_pla_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = pad_op_table1 %>%
                                                  filter(pad_procedure %in% c(2))),
                           formatOptions = list(big.mark = ","))


write.csv(pad_op_any_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_op_any_table1.csv",
          row.names = T)

write.csv(pad_op_lr_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_op_lr_table1.csv",
          row.names = T)


write.csv(pad_op_pla_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/pad_op_pla_table1.csv",
          row.names = T)


#     stroke Dx ******

kmc_final_stroke_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_stroke_admissions_dx_v1')

stroke_admissions_dx_summary <- kmc_final_stroke_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(stroke_diagnosis_primary = ifelse(stroke_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_stroke_primary = sum(stroke_diagnosis_primary)) %>%
  ungroup()

stroke_adm_summary <- stroke_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_stroke_primary = round(mean(adm_stroke_primary))) %>%
  ungroup() %>%
  bind_rows(stroke_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "stroke_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_stroke_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(stroke_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")


stroke_dx_table1 <- kmc_final_stroke_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(stroke_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

stroke_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                   strata = "adm_year", factorVars = factorvars,
                                                   data = stroke_dx_table1), formatOptions = list(big.mark = ","))

write.csv(stroke_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_dx_table1.csv",
          row.names = T)


#     *** stroke OP ******

kmc_final_stroke_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_stroke_admissions_op_v1')

stroke_admissions_op_summary <- kmc_final_stroke_admissions_op %>%
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
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("stroke_op_")), sum) %>%
  ungroup()

stroke_op_summary <- stroke_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise_at(vars(starts_with("stroke_op_")), mean) %>%
  ungroup() %>%
  bind_rows(stroke_admissions_op_summary) %>%
  arrange(adm_year) %>%
  pivot_longer(!adm_year, names_to = "proc", values_to = "N") %>%
  pivot_wider(names_from = adm_year, values_from = N,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))


write.csv(stroke_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

stroke_op_table1 <- kmc_final_stroke_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(stroke_procedure= carotid_procedure) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

stroke_op_ces_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                   strata = "adm_year", factorVars = factorvars,
                                                   data = stroke_op_table1 %>%
                                                     filter(stroke_procedure >= 1)), 
                          formatOptions = list(big.mark = ","))

stroke_op_thr_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                   strata = "adm_year", factorVars = factorvars,
                                                   data = stroke_op_table1 %>%
                                                     filter(thrombolysis_is + thrombectomy_is >= 1)), 
                          formatOptions = list(big.mark = ","))

stroke_op_coil_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                       strata = "adm_year", factorVars = factorvars,
                                                       data = stroke_op_table1 %>%
                                                         filter(coiling == 1)), 
                              formatOptions = list(big.mark = ","))


write.csv(stroke_op_ces_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_op_ces_table1.csv",
          row.names = T)


write.csv(stroke_op_thr_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_op_thr_table1.csv",
          row.names = T)


write.csv(stroke_op_coil_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/stroke_op_coil_table1.csv",
          row.names = T)

#     vte Dx ******

kmc_final_vte_admissions_dx <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx_v1')


vte_admissions_dx_summary <- kmc_final_vte_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  mutate(vte_diagnosis_primary = ifelse(vte_diagnosis_primary == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(adm_vte_primary = sum(vte_diagnosis_primary)) %>%
  ungroup()

vte_adm_summary <- vte_admissions_dx_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(adm_vte_primary = round(mean(adm_vte_primary))) %>%
  ungroup() %>%
  bind_rows(vte_admissions_dx_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "vte_adm") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = adm_vte_primary,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(vte_adm_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_adm_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")


vte_dx_table1 <- kmc_final_vte_admissions_dx %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  filter(vte_diagnosis_primary >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

vte_dx_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = vte_dx_table1), formatOptions = list(big.mark = ","))

write.csv(vte_dx_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_dx_table1.csv",
          row.names = T)




#     *** vte OP ******

kmc_final_vte_admissions_op <- odbc::dbGetQuery(con,'SELECT * 
                        FROM dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_op_v1')


vte_admissions_op_summary <- kmc_final_vte_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(vte_procedure= pe_procedure) %>%
  mutate(vte_procedure = ifelse(vte_procedure == 0, 0, 1)) %>%
  group_by(adm_year) %>%
  summarise(vte_procedure = sum(vte_procedure)) %>%
  ungroup()

vte_op_summary <- vte_admissions_op_summary %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, 201619, 202021)) %>%
  group_by(adm_year) %>%
  summarise(vte_procedure = round(mean(vte_procedure))) %>%
  ungroup() %>%
  bind_rows(vte_admissions_op_summary) %>%
  arrange(adm_year) %>%
  mutate(condition = "vte_op") %>%
  select(condition, everything()) %>%
  pivot_wider(names_from = adm_year, values_from = vte_procedure,
              names_prefix = "N_") %>%
  rename(avg_2016_2019 = N_201619,
         avg_2020_2021 = N_202021) %>%
  mutate(change_pre_post = round((avg_2020_2021 - avg_2016_2019) / avg_2016_2019 * 100, 2))

write.csv(vte_op_summary,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_op_summary.csv",
          row.names = T)

vars = c("age_group", "gender", "ethnic_group","charlson")
factorvars = c("age_group", "gender", "ethnic_group","charlson")

vte_op_table1 <- kmc_final_vte_admissions_op %>%
  rename_all(tolower) %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(adm_year = year(admidate)) %>%
  filter(adm_year >= 2016 & adm_year <= 2021) %>%
  rename(vte_procedure= pe_procedure) %>%
  filter(vte_procedure >= 1) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(adm_year = ifelse(adm_year >= 2016 & adm_year <= 2019, "2016_2019", "2020_2021")) %>%
  mutate(charlson = ifelse(charlson_score >= 3, 3, charlson_score)) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) 

vte_op_table1 <- print(tableone::CreateTableOne(vars = vars, 
                                                strata = "adm_year", factorVars = factorvars,
                                                data = vte_op_table1), formatOptions = list(big.mark = ","))

write.csv(vte_op_table1,
          "/mnt/efs/nazrul.islam/naz_practice_files/vte_op_table1.csv",
          row.names = T)

