# Script that reads the data extracted using data_extraction_monthly.R and
# gets it ready for plotting.

# First: group by emergency_admission, adm_year, adm_month, ethnicity,
# gender and age, and compute counts.
setwd("~/CCU003_04/Jun2022/results")
AA = read.csv("results_AA_monthly.csv", stringsAsFactors = F)[,-1]
AA = AA %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_AA_any = n(), adm_AA_prim = sum(prim))

AS = read.csv("results_AS_monthly.csv", stringsAsFactors = F)[,-1]
AS = AS %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_AS_any = n(), adm_AS_prim = sum(prim))

HF = read.csv("results_HF_monthly.csv", stringsAsFactors = F)[,-1]
HF = HF %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_HF_any = n(), adm_HF_prim = sum(prim))

IHD = read.csv("results_IHD_monthly.csv", stringsAsFactors = F)[,-1]
IHD = IHD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_IHD_any = n(), adm_IHD_prim = sum(prim))

PAD = read.csv("results_PAD_monthly.csv", stringsAsFactors = F)[,-1]
PAD = PAD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_PAD_any = n(), adm_PAD_prim = sum(prim))

VTE = read.csv("results_VTE_monthly.csv", stringsAsFactors = F)[,-1]
VTE = VTE %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_VTE_any = n(), adm_VTE_prim = sum(prim))

# Second: further grouping on emergency_admission, adm_year and adm_month.
# We also compute average counts for 2016-2019
AA_month_1 = AA %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_AA_any_tot = sum(adm_AA_any), adm_AA_prim_tot = sum(adm_AA_prim)) %>%
  mutate(CVD = "AA")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AA_month_2 = AA_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_AA_any = mean(adm_AA_any_tot), adm_AA_prim = mean(adm_AA_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "AA") %>%
  select(emergency_admission, adm_year, adm_month, adm_AA_any, adm_AA_prim, CVD)%>%
  rename(adm_any = adm_AA_any, adm_prim = adm_AA_prim)
# Join the just computed mean with the data from 2020 and 2021.
AA_month_1 = AA_month_1 %>%
  filter(adm_year >= '2020') %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_AA_any_tot, adm_prim = adm_AA_prim_tot) 
AA_month = rbind(AA_month_2,AA_month_1)


######################### AS:
AS_month_1 = AS %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_AS_any_tot = sum(adm_AS_any), adm_AS_prim_tot = sum(adm_AS_prim)) %>%
  mutate(CVD = "Stroke")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AS_month_2 = AS_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_AS_any = mean(adm_AS_any_tot), adm_AS_prim = mean(adm_AS_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "Stroke") %>%
  select(emergency_admission, adm_year, adm_month, adm_AS_any, adm_AS_prim, CVD)%>%
  rename(adm_any = adm_AS_any, adm_prim = adm_AS_prim)

# Join the just computed mean with the data from 2020 and 2021.
AS_month_1 = AS_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_AS_any_tot, adm_prim = adm_AS_prim_tot) 

AS_month = rbind(AS_month_2,AS_month_1) 

######################### HF:
HF_month_1 = HF %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_HF_any_tot = sum(adm_HF_any), adm_HF_prim_tot = sum(adm_HF_prim)) %>%
  mutate(CVD = "HF")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
HF_month_2 = HF_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_HF_any = mean(adm_HF_any_tot), adm_HF_prim = mean(adm_HF_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "HF") %>%
  select(emergency_admission, adm_year, adm_month, adm_HF_any, adm_HF_prim, CVD)%>%
  rename(adm_any = adm_HF_any, adm_prim = adm_HF_prim)

# Join the just computed mean with the data from 2020 and 2021.
HF_month_1 = HF_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_HF_any_tot, adm_prim = adm_HF_prim_tot) 

HF_month = rbind(HF_month_2,HF_month_1) 

######################### IHD:
IHD_month_1 = IHD %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_IHD_any_tot = sum(adm_IHD_any), adm_IHD_prim_tot = sum(adm_IHD_prim)) %>%
  mutate(CVD = "ACS")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
IHD_month_2 = IHD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_IHD_any = mean(adm_IHD_any_tot), adm_IHD_prim = mean(adm_IHD_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "ACS") %>%
  select(emergency_admission, adm_year, adm_month, adm_IHD_any, adm_IHD_prim, CVD)%>%
  rename(adm_any = adm_IHD_any, adm_prim = adm_IHD_prim)

# Join the just computed mean with the data from 2020 and 2021.
IHD_month_1 = IHD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_IHD_any_tot, adm_prim = adm_IHD_prim_tot) 

IHD_month = rbind(IHD_month_2,IHD_month_1) 

######################### PAD:
PAD_month_1 = PAD %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_PAD_any_tot = sum(adm_PAD_any), adm_PAD_prim_tot = sum(adm_PAD_prim)) %>%
  mutate(CVD = "PAD")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
PAD_month_2 = PAD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_PAD_any = mean(adm_PAD_any_tot), adm_PAD_prim = mean(adm_PAD_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "PAD") %>%
  select(emergency_admission, adm_year, adm_month, adm_PAD_any, adm_PAD_prim, CVD)%>%
  rename(adm_any = adm_PAD_any, adm_prim = adm_PAD_prim)

# Join the just computed mean with the data from 2020 and 2021.
PAD_month_1 = PAD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_PAD_any_tot, adm_prim = adm_PAD_prim_tot) 

PAD_month = rbind(PAD_month_2,PAD_month_1) 

######################### VTE:
VTE_month_1 = VTE %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(adm_VTE_any_tot = sum(adm_VTE_any), adm_VTE_prim_tot = sum(adm_VTE_prim)) %>%
  mutate(CVD = "VTE")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
VTE_month_2 = VTE_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(adm_VTE_any = mean(adm_VTE_any_tot), adm_VTE_prim = mean(adm_VTE_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "VTE") %>%
  select(emergency_admission, adm_year, adm_month, adm_VTE_any, adm_VTE_prim, CVD) %>%
  rename(adm_any = adm_VTE_any, adm_prim = adm_VTE_prim)

# Join the just computed mean with the data from 2020 and 2021.
VTE_month_1 = VTE_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_VTE_any_tot, adm_prim = adm_VTE_prim_tot) 

VTE_month = rbind(VTE_month_2,VTE_month_1) 

final = rbind(AA_month,AS_month,HF_month,IHD_month,PAD_month,VTE_month)
write.csv(final,"data_to_plot_adm_monthly.csv")

###############################################################################
# Now we do the same but for the procedures.
setwd("~/CCU003_04/Jun2022/results")
AA = read.csv("results_AA_proc_monthly.csv", stringsAsFactors = F)[,-1]
AA = AA %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_AA = n())
AS = read.csv("results_AS_proc_monthly.csv", stringsAsFactors = F)[,-1]
AS = AS %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_AS = n())
HF = read.csv("results_HF_proc_monthly.csv", stringsAsFactors = F)[,-1]
HF = HF %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_HF = n())
IHD = read.csv("results_IHD_proc_monthly.csv", stringsAsFactors = F)[,-1]
IHD = IHD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_IHD = n())
PAD = read.csv("results_PAD_proc_monthly.csv", stringsAsFactors = F)[,-1]
PAD = PAD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_PAD = n())
VTE = read.csv("results_VTE_proc_monthly.csv", stringsAsFactors = F)[,-1]
VTE = VTE %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_VTE = n())

AA_month_1 = AA %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_AA_tot = sum(proc_AA)) %>%
  mutate(CVD = "AA")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AA_month_2 = AA_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_AA = mean(proc_AA_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "AA") %>%
  select(emergency_admission, adm_year, adm_month, proc_AA, CVD)%>%
  rename(proc = proc_AA)

# Join the just computed mean with the data from 2020 and 2021.
AA_month_1 = AA_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_AA_tot) 

AA_month = rbind(AA_month_2,AA_month_1) 

######################### AS:
AS_month_1 = AS %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_AS_tot = sum(proc_AS)) %>%
  mutate(CVD = "Stroke")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AS_month_2 = AS_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_AS = mean(proc_AS_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "Stroke") %>%
  select(emergency_admission, adm_year, adm_month, proc_AS, CVD)%>%
  rename(proc = proc_AS)

# Join the just computed mean with the data from 2020 and 2021.
AS_month_1 = AS_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_AS_tot) 

AS_month = rbind(AS_month_2,AS_month_1) 

######################### HF:
HF_month_1 = HF %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_HF_tot = sum(proc_HF)) %>%
  mutate(CVD = "HF")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
HF_month_2 = HF_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_HF = mean(proc_HF_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "HF") %>%
  select(emergency_admission, adm_year, adm_month, proc_HF, CVD)%>%
  rename(proc = proc_HF)

# Join the just computed mean with the data from 2020 and 2021.
HF_month_1 = HF_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_HF_tot) 

HF_month = rbind(HF_month_2,HF_month_1) 

######################### IHD:
IHD_month_1 = IHD %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_IHD_tot = sum(proc_IHD)) %>%
  mutate(CVD = "ACS")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
IHD_month_2 = IHD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_IHD = mean(proc_IHD_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "ACS") %>%
  select(emergency_admission, adm_year, adm_month, proc_IHD, CVD)%>%
  rename(proc = proc_IHD)

# Join the just computed mean with the data from 2020 and 2021.
IHD_month_1 = IHD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_IHD_tot) 

IHD_month = rbind(IHD_month_2,IHD_month_1) 

######################### PAD:
PAD_month_1 = PAD %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_PAD_tot = sum(proc_PAD)) %>%
  mutate(CVD = "PAD")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
PAD_month_2 = PAD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_PAD = mean(proc_PAD_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "PAD") %>%
  select(emergency_admission, adm_year, adm_month, proc_PAD, CVD)%>%
  rename(proc = proc_PAD)

# Join the just computed mean with the data from 2020 and 2021.
PAD_month_1 = PAD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_PAD_tot) 

PAD_month = rbind(PAD_month_2,PAD_month_1) 

######################### VTE:
VTE_month_1 = VTE %>%
  group_by(emergency_admission,adm_year,adm_month) %>%
  summarize(proc_VTE_tot = sum(proc_VTE)) %>%
  mutate(CVD = "VTE")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
VTE_month_2 = VTE_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(emergency_admission,adm_month) %>%
  summarize(proc_VTE = mean(proc_VTE_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "VTE") %>%
  select(emergency_admission, adm_year, adm_month, proc_VTE, CVD)%>%
  rename(proc = proc_VTE)

# Join the just computed mean with the data from 2020 and 2021.
VTE_month_1 = VTE_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_VTE_tot) 

VTE_month = rbind(VTE_month_2,VTE_month_1) 

final = rbind(AA_month,AS_month,HF_month,IHD_month,PAD_month,VTE_month)
write.csv(final,"data_to_plot_proc_monthly.csv")



###############################################################################
#### Data manipulation to produce the files which do not have into account
#### the emergency/elective distinction (so all admissions/procedures)
setwd("~/CCU003_04/Jun2022/results")
AA = read.csv("results_AA_monthly.csv", stringsAsFactors = F)[,-1]
AA = AA %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_AA_any = n(), adm_AA_prim = sum(prim))

AS = read.csv("results_AS_monthly.csv", stringsAsFactors = F)[,-1]
AS = AS %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_AS_any = n(), adm_AS_prim = sum(prim))

HF = read.csv("results_HF_monthly.csv", stringsAsFactors = F)[,-1]
HF = HF %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_HF_any = n(), adm_HF_prim = sum(prim))

IHD = read.csv("results_IHD_monthly.csv", stringsAsFactors = F)[,-1]
IHD = IHD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_IHD_any = n(), adm_IHD_prim = sum(prim))

PAD = read.csv("results_PAD_monthly.csv", stringsAsFactors = F)[,-1]
PAD = PAD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_PAD_any = n(), adm_PAD_prim = sum(prim))

VTE = read.csv("results_VTE_monthly.csv", stringsAsFactors = F)[,-1]
VTE = VTE %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(adm_VTE_any = n(), adm_VTE_prim = sum(prim))

# Get the data ready for the plots:
# Aggregate by year and month.
AA_month_1 = AA %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_AA_any_tot = sum(adm_AA_any), adm_AA_prim_tot = sum(adm_AA_prim)) %>%
  mutate(CVD = "AA")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AA_month_2 = AA_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_AA_any = mean(adm_AA_any_tot), adm_AA_prim = mean(adm_AA_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "AA") %>%
  select(adm_year, adm_month, adm_AA_any, adm_AA_prim, CVD)%>%
  rename(adm_any = adm_AA_any, adm_prim = adm_AA_prim)

# Join the just computed mean with the data from 2020 and 2021.
AA_month_1 = AA_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_AA_any_tot, adm_prim = adm_AA_prim_tot) %>%
  ungroup()

AA_month = rbind(AA_month_2,AA_month_1) 

######################### AS:
AS_month_1 = AS %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_AS_any_tot = sum(adm_AS_any), adm_AS_prim_tot = sum(adm_AS_prim)) %>%
  mutate(CVD = "Stroke")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AS_month_2 = AS_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_AS_any = mean(adm_AS_any_tot), adm_AS_prim = mean(adm_AS_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "Stroke") %>%
  select(adm_year, adm_month, adm_AS_any, adm_AS_prim, CVD)%>%
  rename(adm_any = adm_AS_any, adm_prim = adm_AS_prim)

# Join the just computed mean with the data from 2020 and 2021.
AS_month_1 = AS_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_AS_any_tot, adm_prim = adm_AS_prim_tot) %>%
  ungroup()

AS_month = rbind(AS_month_2,AS_month_1) 

######################### HF:
HF_month_1 = HF %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_HF_any_tot = sum(adm_HF_any), adm_HF_prim_tot = sum(adm_HF_prim)) %>%
  mutate(CVD = "HF")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
HF_month_2 = HF_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_HF_any = mean(adm_HF_any_tot), adm_HF_prim = mean(adm_HF_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "HF") %>%
  select(adm_year, adm_month, adm_HF_any, adm_HF_prim, CVD)%>%
  rename(adm_any = adm_HF_any, adm_prim = adm_HF_prim)

# Join the just computed mean with the data from 2020 and 2021.
HF_month_1 = HF_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_HF_any_tot, adm_prim = adm_HF_prim_tot) %>%
  ungroup() 

HF_month = rbind(HF_month_2,HF_month_1) 

######################### IHD:
IHD_month_1 = IHD %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_IHD_any_tot = sum(adm_IHD_any), adm_IHD_prim_tot = sum(adm_IHD_prim)) %>%
  mutate(CVD = "ACS")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
IHD_month_2 = IHD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_IHD_any = mean(adm_IHD_any_tot), adm_IHD_prim = mean(adm_IHD_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "ACS") %>%
  select(adm_year, adm_month, adm_IHD_any, adm_IHD_prim, CVD)%>%
  rename(adm_any = adm_IHD_any, adm_prim = adm_IHD_prim)

# Join the just computed mean with the data from 2020 and 2021.
IHD_month_1 = IHD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_IHD_any_tot, adm_prim = adm_IHD_prim_tot) %>%
  ungroup() 

IHD_month = rbind(IHD_month_2,IHD_month_1) 

######################### PAD:
PAD_month_1 = PAD %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_PAD_any_tot = sum(adm_PAD_any), adm_PAD_prim_tot = sum(adm_PAD_prim)) %>%
  mutate(CVD = "PAD")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
PAD_month_2 = PAD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_PAD_any = mean(adm_PAD_any_tot), adm_PAD_prim = mean(adm_PAD_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "PAD") %>%
  select(adm_year, adm_month, adm_PAD_any, adm_PAD_prim, CVD)%>%
  rename(adm_any = adm_PAD_any, adm_prim = adm_PAD_prim)

# Join the just computed mean with the data from 2020 and 2021.
PAD_month_1 = PAD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_PAD_any_tot, adm_prim = adm_PAD_prim_tot) %>%
  ungroup() 

PAD_month = rbind(PAD_month_2,PAD_month_1) 

######################### VTE:
VTE_month_1 = VTE %>%
  group_by(adm_year,adm_month) %>%
  summarize(adm_VTE_any_tot = sum(adm_VTE_any), adm_VTE_prim_tot = sum(adm_VTE_prim)) %>%
  mutate(CVD = "VTE")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
VTE_month_2 = VTE_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(adm_VTE_any = mean(adm_VTE_any_tot), adm_VTE_prim = mean(adm_VTE_prim_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "VTE") %>%
  select(adm_year, adm_month, adm_VTE_any, adm_VTE_prim, CVD) %>%
  rename(adm_any = adm_VTE_any, adm_prim = adm_VTE_prim)

# Join the just computed mean with the data from 2020 and 2021.
VTE_month_1 = VTE_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(adm_any = adm_VTE_any_tot, adm_prim = adm_VTE_prim_tot) %>%
  ungroup() 

VTE_month = rbind(VTE_month_2,VTE_month_1) 

final = rbind(AA_month,AS_month,HF_month,IHD_month,PAD_month,VTE_month)
write.csv(final,"data_to_plot_adm_all_monthly.csv")



###############################################################################
# Now we do the same but for the procedures.
setwd("~/CCU003_04/Jun2022/results")
AA = read.csv("results_AA_proc_monthly.csv", stringsAsFactors = F)[,-1]
AA = AA %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_AA = n())
AS = read.csv("results_AS_proc_monthly.csv", stringsAsFactors = F)[,-1]
AS = AS %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_AS = n())
HF = read.csv("results_HF_proc_monthly.csv", stringsAsFactors = F)[,-1]
HF = HF %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_HF = n())
IHD = read.csv("results_IHD_proc_monthly.csv", stringsAsFactors = F)[,-1]
IHD = IHD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_IHD = n())
PAD = read.csv("results_PAD_proc_monthly.csv", stringsAsFactors = F)[,-1]
PAD = PAD %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_PAD = n())
VTE = read.csv("results_VTE_proc_monthly.csv", stringsAsFactors = F)[,-1]
VTE = VTE %>%
  filter(adm_year <= "2021") %>%
  group_by(emergency_admission,adm_year,adm_month,ethnic_group,gender,age_group) %>%
  summarize(proc_VTE = n())

AA_month_1 = AA %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_AA_tot = sum(proc_AA)) %>%
  mutate(CVD = "AA")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AA_month_2 = AA_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_AA = mean(proc_AA_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "AA") %>%
  select(adm_year, adm_month, proc_AA, CVD)%>%
  rename(proc = proc_AA)

# Join the just computed mean with the data from 2020 and 2021.
AA_month_1 = AA_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_AA_tot) %>%
  ungroup()  

AA_month = rbind(AA_month_2,AA_month_1) 

######################### AS:
AS_month_1 = AS %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_AS_tot = sum(proc_AS)) %>%
  mutate(CVD = "Stroke")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
AS_month_2 = AS_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_AS = mean(proc_AS_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "Stroke") %>%
  select(adm_year, adm_month, proc_AS, CVD)%>%
  rename(proc = proc_AS)

# Join the just computed mean with the data from 2020 and 2021.
AS_month_1 = AS_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_AS_tot) %>%
  ungroup()  

AS_month = rbind(AS_month_2,AS_month_1) 

######################### HF:
HF_month_1 = HF %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_HF_tot = sum(proc_HF)) %>%
  mutate(CVD = "HF")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
HF_month_2 = HF_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_HF = mean(proc_HF_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "HF") %>%
  select(adm_year, adm_month, proc_HF, CVD)%>%
  rename(proc = proc_HF)

# Join the just computed mean with the data from 2020 and 2021.
HF_month_1 = HF_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_HF_tot) %>%
  ungroup()  

HF_month = rbind(HF_month_2,HF_month_1) 

######################### IHD:
IHD_month_1 = IHD %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_IHD_tot = sum(proc_IHD)) %>%
  mutate(CVD = "ACS")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
IHD_month_2 = IHD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_IHD = mean(proc_IHD_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "ACS") %>%
  select(adm_year, adm_month, proc_IHD, CVD)%>%
  rename(proc = proc_IHD)

# Join the just computed mean with the data from 2020 and 2021.
IHD_month_1 = IHD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_IHD_tot) %>%
  ungroup()  

IHD_month = rbind(IHD_month_2,IHD_month_1) 

######################### PAD:
PAD_month_1 = PAD %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_PAD_tot = sum(proc_PAD)) %>%
  mutate(CVD = "PAD")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
PAD_month_2 = PAD_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_PAD = mean(proc_PAD_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "PAD") %>%
  select(adm_year, adm_month, proc_PAD, CVD)%>%
  rename(proc = proc_PAD)

# Join the just computed mean with the data from 2020 and 2021.
PAD_month_1 = PAD_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_PAD_tot) %>%
  ungroup()  

PAD_month = rbind(PAD_month_2,PAD_month_1) 

######################### VTE:
VTE_month_1 = VTE %>%
  group_by(adm_year,adm_month) %>%
  summarize(proc_VTE_tot = sum(proc_VTE)) %>%
  mutate(CVD = "VTE")
# Compute the mean of the admissions from 2016-2019 (we exclude 2015).
VTE_month_2 = VTE_month_1 %>%
  filter(adm_year < "2020" & adm_year > "2015") %>%
  group_by(adm_month) %>%
  summarize(proc_VTE = mean(proc_VTE_tot)) %>%
  mutate(adm_year = "2016-2019", CVD = "VTE") %>%
  select(adm_year, adm_month, proc_VTE, CVD)%>%
  rename(proc = proc_VTE)

# Join the just computed mean with the data from 2020 and 2021.
VTE_month_1 = VTE_month_1 %>%
  filter(adm_year >= 2020) %>%
  mutate(adm_year = as.character(adm_year)) %>%
  rename(proc = proc_VTE_tot) %>%
  ungroup()  

VTE_month = rbind(VTE_month_2,VTE_month_1) 

final = rbind(AA_month,AS_month,HF_month,IHD_month,PAD_month,VTE_month)
write.csv(final,"data_to_plot_proc_all_monthly.csv")
