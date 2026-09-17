#This script combines all the data from the various conditions, so they can all be plotted together
#It also contains a few ad-hoc routines that were run to generate the tables and figures for the paper 

library(stringr)
library(tidyverse)
library(lubridate)
library(ggh4x)

#Define the directories that hold the data needed to combine 
top_dir="../.."
combined_data_files=list.files(file.path(top_dir,"data","combine"))

#Get the CSVs of diagnosis and procedures separately
dx_files=paste0(top_dir,"/data/combine/",combined_data_files[endsWith(combined_data_files,"_dx.csv")])
op_files=paste0(top_dir,"/data/combine/",combined_data_files[endsWith(combined_data_files,"_op.csv")])

#Preallocate
df_dx=data.frame()

for (dx_file in dx_files){ 
  
  #Read it into memory
  df_this=read.csv(dx_file)
  
  #Figure out the condition from the filename
  df_this$Condition=str_split(basename(dx_file),"_")[[1]][1]
  
  #Rename some columns to remove condition-dependency
  # df_this=
  #   df_this %>% 
  #   rename(DIAGNOSIS_ANY_POSITION="AA_DIAGNOSIS_ANY_POSITION",
  #          DIAGNOSIS_DISCHARGE="AA_DIAGNOSIS_DISCHARGE",
  #          DIAGNOSIS_DISCHARGE_POSITION="AA_DIAGNOSIS_DISCHARGE_POSITION",
  #          DIAGNOSIS_PRIMARY="AA_DIAGNOSIS_PRIMARY")
  
  #Remove these columns if they exist
  df_this = df_this %>% 
    select(-any_of(c("hier2","hier2pos","last_ep_hier2","last_ep_hier2pos","last_ep_d1_h2")))
  
  df_dx=rbind(df_dx,df_this)
}


#Preallocate
df_op=data.frame()

for (op_file in op_files){ 
  
  #Read it into memory
  df_this=read.csv(op_file)
  
  #Figure out the condition from the filename
  condition=str_split(basename(op_file),"_")[[1]][1]
  df_this$Condition=condition
  
  #Rename some columns to remove condition-dependency
  # df_this=
  #   df_this %>% 
  #   rename(DIAGNOSIS_ANY_POSITION=
  #            paste0(condition,"_DIAGNOSIS_ANY_POSITION"),
  #          
  #          DIAGNOSIS_DISCHARGE=
  #            paste0(condition,"_DIAGNOSIS_DISCHARGE"),
  #          
  #          DIAGNOSIS_DISCHARGE_POSITION=
  #            paste0(condition,"_DIAGNOSIS_DISCHARGE_POSITION"),
  #          
  #          DIAGNOSIS_PRIMARY=
  #            paste0(condition,"_DIAGNOSIS_PRIMARY"),
  #          
  #   )
  
  #Remove these columns if they exist
  df_this = df_this %>% 
    select(-any_of(c("hier2","hier2pos","last_ep_hier2","last_ep_hier2pos","last_ep_d1_h2")))
  
  df_op=dplyr::bind_rows(df_op,df_this)
  
}

# df_op[df_op$Condition=="STROKE",]$Condition="Stroke"
# df_op[df_op$Condition=="IHD",]$Condition="ACS"
# df_dx[df_dx$Condition=="IHD",]$Condition="ACS"


###########################
#Get info for tables 

#Populate the tables for the paper - see Lucy's email for more info 

#Interrogate data for table info

df_dx = df_dx %>% 
        mutate(age_group = cut(ADMIAGE, breaks = c(-1,49,59,69,79,1000)),
                admi_year=year(ADMIDATE),
                admi_month=month(ADMIDATE),
                year_group = cut(admi_year, breaks = c(2015,2019,2021,2022)),
                year_group_split = cut(admi_year, breaks = c(2015,2019,2020,2021,2022)),
                c_score_group=case_when(score_sum==0~0,
                                       score_sum==1~1,
                                       score_sum==2~2,
                                       score_sum>=3~3),
               eth_readable = case_when(ETHNOS == "0" ~ "white" ,
                                               ETHNOS %in% c("1","2","3") ~ "black" ,
                                               ETHNOS %in% c("4","5","6","7") ~ "asian",
                                               ETHNOS == "8" ~ "other",  
                                               ETHNOS %in% c("A","B") ~ "white",
                                               ETHNOS %in% c("D","E","F","G") ~ "mixed",
                                               ETHNOS %in% c("H","J","K","L") ~ "asian",
                                               ETHNOS %in% c("M","N","P") ~ "black",
                                               ETHNOS %in% c("R","T") ~ "asian",
                                               ETHNOS %in% c("S") ~ "other" ,
                                               ETHNOS %in% c("Z","9","")~ "unknown",
                                               TRUE ~ "unknown"),
               prim_diag = ifelse(DIAGNOSIS_PRIMARY == 0, 0, 1))


df_op = df_op %>% 
  mutate(age_group = cut(ADMIAGE, breaks = c(-1,49,59,69,79,1000)),
         admi_year=year(ADMIDATE),
         admi_month=month(ADMIDATE),
         year_group = cut(admi_year, breaks = c(2015,2019,2021,2022)),
         year_group_split = cut(admi_year, breaks = c(2015,2019,2020,2021,2022)),
         c_score_group=case_when(score_sum==0~0,
                                 score_sum==1~1,
                                 score_sum==2~2,
                                 score_sum>=3~3),
         eth_readable = case_when(ETHNOS == "0" ~ "white" ,
                                  ETHNOS %in% c("1","2","3") ~ "black" ,
                                  ETHNOS %in% c("4","5","6","7") ~ "asian",
                                  ETHNOS == "8" ~ "other",  
                                  ETHNOS %in% c("A","B") ~ "white",
                                  ETHNOS %in% c("D","E","F","G") ~ "mixed",
                                  ETHNOS %in% c("H","J","K","L") ~ "asian",
                                  ETHNOS %in% c("M","N","P") ~ "black",
                                  ETHNOS %in% c("R","T") ~ "asian",
                                  ETHNOS %in% c("S") ~ "other" ,
                                  ETHNOS %in% c("Z","9","")~ "unknown",
                                  TRUE ~ "unknown"),
         prim_diag = ifelse(DIAGNOSIS_PRIMARY == 0, 0, 1))


#################

#Monthly
table_data_monthly <- df_dx %>%
  filter(year_group_split!=	"(2021,2022]") %>% 
  mutate(diagnosis_any = ifelse(DIAGNOSIS_ANY_POSITION == 0, 0, 1)) %>%
  mutate(adm_year = year(ADMIDATE), adm_month = month(ADMIDATE), adm_week = week(ADMIDATE)) %>%
  group_by(Condition,EMERGENCY_ADMISSION, year_group_split, adm_month) %>%
  summarise(adm_any = sum(diagnosis_any,na.rm=TRUE), 
            adm_prim = sum(prim_diag,na.rm=TRUE)) %>%
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~adm_any/4,
                             TRUE~adm_any),
         adm_prim=case_when(year_group_split=="(2015,2019]"~adm_prim/4,
                              TRUE~adm_prim),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition,
         emergency_admission=EMERGENCY_ADMISSION)

table_data_monthly_nosplit <- df_dx %>%
  filter(year_group_split!=	"(2021,2022]") %>% 
  mutate(diagnosis_any = ifelse(DIAGNOSIS_ANY_POSITION == 0, 0, 1)) %>%
  mutate(diagnosis_primary = ifelse(DIAGNOSIS_PRIMARY == 0, 0, 1)) %>%
  mutate(adm_year = year(ADMIDATE), adm_month = month(ADMIDATE), adm_week = week(ADMIDATE)) %>%
  group_by(Condition, year_group_split, adm_month) %>%
  summarise(adm_any = sum(diagnosis_any,na.rm=TRUE), 
            adm_prim = sum(diagnosis_primary,na.rm=TRUE)) %>%
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~adm_any/4,
                           TRUE~adm_any),
         adm_prim=case_when(year_group_split=="(2015,2019]"~adm_prim/4,
                            TRUE~adm_prim),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition)


#Mask values <5
table_data_monthly_nosplit[table_data_monthly_nosplit$adm_any<5,]$adm_any=NA
table_data_monthly_nosplit[table_data_monthly_nosplit$adm_prim<5,]$adm_prim=NA

table_data_monthly[table_data_monthly$adm_any<5,]$adm_any=NA
table_data_monthly[table_data_monthly$adm_prim<5,]$adm_prim=NA

#Save
write.csv(table_data_monthly_nosplit,"../../outputs/table/table_data_monthly_nosplit.csv",row.names=FALSE)
write.csv(table_data_monthly,"../../outputs/table/table_data_monthly.csv",row.names=FALSE)


#Now repeat for procedures

table_data_monthly_op <- df_op %>%
  filter(year_group_split!=	"(2021,2022]") %>% 
  mutate(diagnosis_any = ifelse(DIAGNOSIS_ANY_POSITION == 0, 0, 1)) %>%
  mutate(adm_year = year(ADMIDATE), adm_month = month(ADMIDATE), adm_week = week(ADMIDATE)) %>%
  group_by(Condition,EMERGENCY_ADMISSION, year_group_split, adm_month) %>%
  summarise(adm_any = sum(diagnosis_any,na.rm=TRUE), 
            adm_prim = sum(prim_diag,na.rm=TRUE)) %>%
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~adm_any/4,
                           TRUE~adm_any),
         adm_prim=case_when(year_group_split=="(2015,2019]"~adm_prim/4,
                            TRUE~adm_prim),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition,
         emergency_admission=EMERGENCY_ADMISSION)


table_data_monthly_nosplit_op <- df_op %>%
  filter(year_group_split!=	"(2021,2022]") %>% 
  mutate(diagnosis_any = ifelse(DIAGNOSIS_ANY_POSITION == 0, 0, 1)) %>%
  mutate(diagnosis_primary = ifelse(DIAGNOSIS_PRIMARY == 0, 0, 1)) %>%
  mutate(adm_year = year(ADMIDATE), adm_month = month(ADMIDATE), adm_week = week(ADMIDATE)) %>%
  group_by(Condition, year_group_split, adm_month) %>%
  summarise(adm_any = sum(diagnosis_any,na.rm=TRUE), 
            adm_prim = sum(diagnosis_primary,na.rm=TRUE)) %>%
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~adm_any/4,
                           TRUE~adm_any),
         adm_prim=case_when(year_group_split=="(2015,2019]"~adm_prim/4,
                            TRUE~adm_prim),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition)


table_data_monthly_nosplit_op[table_data_monthly_nosplit_op$adm_any<5,]$adm_any=NA
table_data_monthly_nosplit_op[table_data_monthly_nosplit_op$adm_prim<5,]$adm_prim=NA

table_data_monthly_op[table_data_monthly_op$adm_any<5,]$adm_any=NA
table_data_monthly_op[table_data_monthly_op$adm_prim<5,]$adm_prim=NA

#Save
write.csv(table_data_monthly_nosplit_op,"../../outputs/table/table_data_monthly_nosplit_op.csv",row.names=FALSE)
write.csv(table_data_monthly_op,"../../outputs/table/table_data_monthly_op.csv",row.names=FALSE)

#And now sub-procedures - first split by emergency/elective
df_table_sub_split=
  df_op %>% 
  group_by(admi_year,admi_month,Condition,EMERGENCY_ADMISSION) %>% 
  summarize(count=n())

conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  
  for (filt_group in filt_groups){
    
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      group_by(admi_year,admi_month,EMERGENCY_ADMISSION) %>% 
      summarize(count=n()) %>% 
      mutate("Condition" = filt_group)
    
    df_table_sub_split=rbind(df_this,df_table_sub_split)
    
  }
}


df_table_sub_split <- df_table_sub_split %>%
  mutate(year_group_split =cut(admi_year, breaks = c(2015,2019,2020,2021,2022))) %>% 
  filter(year_group_split!=	"(2021,2022]") %>% 
  group_by(Condition,EMERGENCY_ADMISSION, year_group_split, admi_month) %>%
  summarise(count = as.double(sum(count,na.rm=TRUE)))%>% 
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~count/4,
                           TRUE~count),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition,
         emergency_admission=EMERGENCY_ADMISSION) %>% 
  select(-count)




#Then totals
df_table_sub=
  df_op %>% 
  group_by(admi_year,admi_month,Condition,EMERGENCY_ADMISSION) %>% 
  summarize(count=n())

#Then get specific conditions on top of that
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  
  for (filt_group in filt_groups){
    
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      group_by(admi_year,admi_month,EMERGENCY_ADMISSION) %>% 
      summarize(count=n()) %>% 
      mutate("Condition" = filt_group)
    
    df_table_sub=rbind(df_this,df_table_sub)
    
  }
}

df_table_sub <- df_table_sub %>%
  mutate(year_group_split =cut(admi_year, breaks = c(2015,2019,2020,2021,2022))) %>% 
  filter(year_group_split!=	"(2021,2022]") %>% 
  group_by(Condition,EMERGENCY_ADMISSION, year_group_split, admi_month) %>%
  summarise(count = as.double(sum(count,na.rm=TRUE)))%>% 
  mutate(adm_any=case_when(year_group_split=="(2015,2019]"~count/4,
                           TRUE~count),
         year_group_split=case_when(year_group_split=="(2015,2019]"~"2016-2019",
                                    year_group_split=="(2019,2020]"~"2020",
                                    year_group_split=="(2020,2021]"~"2021")) %>% 
  rename(adm_year=year_group_split,
         CVD=Condition,
         emergency_admission=EMERGENCY_ADMISSION) %>% 
  select(-count)



#Mask 
df_table_sub_split[df_table_sub_split$adm_any<5,]$adm_any=NA
df_table_sub[df_table_sub$adm_any<5,]$adm_any=NA



#Merge with cross, to fill in missing values

df_table_sub_split_dummy=tidyr::crossing("CVD"=unique(df_table_sub_split$CVD),
                                         "adm_year"=unique(df_table_sub_split$adm_year),
                                         "emergency_admission"=unique(df_table_sub_split$emergency_admission),
                                         "admi_month"=unique(df_table_sub_split$admi_month)) 
df_table_sub_split = merge(df_table_sub_split_dummy,df_table_sub_split,all=TRUE)


df_table_sub_dummy=tidyr::crossing("CVD"=unique(df_table_sub$CVD),
                                   "adm_year"=unique(df_table_sub$adm_year),
                                   "emergency_admission"=unique(df_table_sub$emergency_admission),
                                   "admi_month"=unique(df_table_sub$admi_month)) 
df_table_sub = merge(df_table_sub_dummy,df_table_sub,all=TRUE)


#Save
write.csv(df_table_sub_split,"../../outputs/table/df_table_sub_split.csv",row.names=FALSE)
write.csv(df_table_sub,"../../outputs/table/df_table_sub.csv",row.names=FALSE)


#Get monthly data
df_table_monthly_emerelec=
  df_dx %>% 
  group_by(year_group_split,admi_month,Condition,EMERGENCY_ADMISSION) %>% 
  summarize(count=n())

df_table_monthly=
  df_dx %>% 
  group_by(year_group_split,admi_month,Condition) %>% 
  summarize(count=n())



########################


#For the rest, filter df_dx so that it only contains the primary diagnoses for the main conditions
df_dx=df_dx %>%
  filter(prim_diag==1) %>% 
  select(-prim_diag)

  

#First get counts of diagnoses
df_table=
  df_dx %>% 
  group_by(admi_year,Condition,EMERGENCY_ADMISSION) %>% 
  summarize(count=n())

#Then get specific conditions on top of that
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){

filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]

  for (filt_group in filt_groups){
    
    
    df_this = df_op %>% 
           filter(!!as.symbol(filt_group)==1) %>% 
           group_by(admi_year,EMERGENCY_ADMISSION) %>% 
           summarize(count=n()) %>% 
           mutate("Condition" = filt_group)
    
    df_table=rbind(df_this,df_table)


  }
}


#Add in the year groups here
df_table = df_table %>% 
           mutate(year_group = cut(admi_year, breaks = c(2015,2019,2021,2022)))
#Change names to make them more readable
df_table[df_table$Condition=="AA_repair",]$Condition = "AA repair"
df_table[df_table$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_table[df_table$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_table[df_table$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_table[df_table$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_table[df_table$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_table[df_table$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_table[df_table$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_table[df_table$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_table[df_table$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"


#Now get the yearly averages
df_table_avg=
  df_table %>% 
  group_by(Condition,year_group,EMERGENCY_ADMISSION) %>% 
  filter(year_group!="(2021,2022]") %>% 
  summarize(average=mean(count)) 


df_table_avg$year_group=as.character(df_table_avg$year_group)
df_table_avg[df_table_avg$year_group=="(2015,2019]",]$year_group = "2016-2019"
df_table_avg[df_table_avg$year_group=="(2019,2021]",]$year_group = "2020-2021"
  
#Get differences across year groups 
df_table_diffs=df_table_avg %>% 
  group_by(Condition,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="2020-2021"] - average[year_group=="2016-2019"],
            perc_diff=count_diff/average[year_group=="2016-2019"]*100)
  

#### Appendix Table
#For the appendix table, we want further breakdowns by sex, ethnicity and Charlson index

#Firstly sex
df_supp_sex=
  df_dx %>% 
  filter(year_group!="(2021,2022]",
         SEX %in% c(1,2)) %>% 
  group_by(Condition,year_group,SEX,EMERGENCY_ADMISSION) %>% 

  summarize(count=n()) %>% 
  mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                              year_group=="(2019,2021]"~count/2))


#And for the sub-procedures
df_supp_sex_op=data.frame()
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  for (filt_group in filt_groups){
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      group_by(year_group,SEX,EMERGENCY_ADMISSION) %>% 
      summarize(count = n()) %>% 
      filter(year_group!="(2021,2022]",
             SEX %in% c(1,2)) %>% 
      mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                               year_group=="(2019,2021]"~count/2),
             "Condition" = filt_group)
    
    df_supp_sex_op=rbind(df_this,df_supp_sex_op)
    
  }
}

#Make names easier to read
df_out_sex=rbind(df_supp_sex,df_supp_sex_op) 
df_out_sex[df_out_sex$Condition=="AA_repair",]$Condition = "AA repair"
df_out_sex[df_out_sex$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_sex[df_out_sex$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_sex[df_out_sex$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_sex[df_out_sex$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_sex[df_out_sex$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_sex[df_out_sex$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_sex[df_out_sex$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_sex[df_out_sex$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_sex[df_out_sex$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_sex$year_group=as.character(df_out_sex$year_group)
df_out_sex$SEX=as.character(df_out_sex$SEX)
df_out_sex[df_out_sex$year_group=="(2015,2019]",]$year_group = "2016-2019"
df_out_sex[df_out_sex$year_group=="(2019,2021]",]$year_group = "2020-2021"
df_out_sex[df_out_sex$SEX=="1",]$SEX = "Male"
df_out_sex[df_out_sex$SEX=="2",]$SEX = "Female"



#Repeat for age groups
df_supp_age=
  df_dx %>% 
  group_by(Condition,year_group,age_group,EMERGENCY_ADMISSION) %>% 
  summarize(count = n()) %>% 
  filter(year_group!="(2021,2022]") %>% 
  mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                           year_group=="(2019,2021]"~count/2))

#And for the sub-procedures
df_supp_age_op=data.frame()
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  for (filt_group in filt_groups){
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      group_by(year_group,age_group,EMERGENCY_ADMISSION) %>% 
      summarize(count = n()) %>% 
      filter(year_group!="(2021,2022]") %>% 
      mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                               year_group=="(2019,2021]"~count/2),
             "Condition" = filt_group)
    
    df_supp_age_op=rbind(df_this,df_supp_age_op)
    
  }
}

#Change names
df_out_age=rbind(df_supp_age,df_supp_age_op)
df_out_age[df_out_age$Condition=="AA_repair",]$Condition = "AA repair"
df_out_age[df_out_age$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_age[df_out_age$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_age[df_out_age$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_age[df_out_age$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_age[df_out_age$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_age[df_out_age$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_age[df_out_age$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_age[df_out_age$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_age[df_out_age$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_age$year_group=as.character(df_out_age$year_group)
df_out_age$age_group=as.character(df_out_age$age_group)
df_out_age[df_out_age$year_group=="(2015,2019]",]$year_group = "2016-2019"
df_out_age[df_out_age$year_group=="(2019,2021]",]$year_group = "2020-2021"
df_out_age[df_out_age$age_group=="(-1,49]",]$age_group = "0-49"
df_out_age[df_out_age$age_group=="(49,59]",]$age_group = "50-59"
df_out_age[df_out_age$age_group=="(59,69]",]$age_group = "60-69"
df_out_age[df_out_age$age_group=="(69,79]",]$age_group = "70-79"
df_out_age[df_out_age$age_group=="(79,1e+03]",]$age_group = "80+"

#Repeat for Charlson groups
df_supp_cha=
  df_dx %>% 
  group_by(Condition,year_group,c_score_group,EMERGENCY_ADMISSION) %>% 
  summarize(count = n()) %>% 
  filter(year_group!="(2021,2022]") %>% 
  mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                           year_group=="(2019,2021]"~count/2))

#And for the sub-procedures
df_supp_cha_op=data.frame()
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  for (filt_group in filt_groups){
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      group_by(year_group,c_score_group,EMERGENCY_ADMISSION) %>% 
      summarize(count = n()) %>% 
      filter(year_group!="(2021,2022]") %>% 
      mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                               year_group=="(2019,2021]"~count/2),
             "Condition" = filt_group)
    
    df_supp_cha_op=rbind(df_this,df_supp_cha_op)
    
  }
}

#Change names
df_out_cha=rbind(df_supp_cha,df_supp_cha_op)
df_out_cha[df_out_cha$Condition=="AA_repair",]$Condition = "AA repair"
df_out_cha[df_out_cha$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_cha[df_out_cha$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_cha[df_out_cha$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_cha[df_out_cha$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_cha[df_out_cha$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_cha[df_out_cha$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_cha[df_out_cha$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_cha[df_out_cha$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_cha[df_out_cha$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_cha$year_group=as.character(df_out_cha$year_group)
df_out_cha$c_score_group=as.character(df_out_cha$c_score_group)
df_out_cha[df_out_cha$year_group=="(2015,2019]",]$year_group = "2016-2019"
df_out_cha[df_out_cha$year_group=="(2019,2021]",]$year_group = "2020-2021"
df_out_cha[df_out_cha$c_score_group=="3",]$c_score_group = "3+"
df_out_cha = df_out_cha %>%  rename("Charlson_score"=c_score_group) 


#Repeat for ethnicity groups
df_supp_eth=
  df_dx %>% 
  group_by(Condition,year_group,eth_readable,EMERGENCY_ADMISSION) %>% 
  summarize(count = n()) %>% 
  filter(year_group!="(2021,2022]") %>% 
  mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                           year_group=="(2019,2021]"~count/2))

#And for the sub-procedures
df_supp_eth_op=data.frame()
conditions=c("AA","HF","ACS","PAD","stroke","VTE")
for (condition in conditions){
  filt_groups=colnames(df_op)[startsWith(colnames(df_op),condition)]
  for (filt_group in filt_groups){
    
    df_this = df_op %>% 
      filter(!!as.symbol(filt_group)==1) %>% 
      mutate(admi_year=year(ADMIDATE),
             year_group = cut(admi_year, breaks = c(2015,2019,2021,2022))) %>% 
      group_by(year_group,eth_readable,EMERGENCY_ADMISSION) %>% 
      summarize(count = n()) %>% 
      filter(year_group!="(2021,2022]") %>% 
      mutate(average=case_when(year_group=="(2015,2019]"~count/4,
                               year_group=="(2019,2021]"~count/2),
             "Condition" = filt_group)
    
    df_supp_eth_op=rbind(df_this,df_supp_eth_op)
    
  }
}

#Replace some names
df_out_eth=rbind(df_supp_eth_op,df_supp_eth)
df_out_eth[df_out_eth$Condition=="AA_repair",]$Condition = "AA repair"
df_out_eth[df_out_eth$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_eth[df_out_eth$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_eth[df_out_eth$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_eth[df_out_eth$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_eth[df_out_eth$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_eth[df_out_eth$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_eth[df_out_eth$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_eth[df_out_eth$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_eth[df_out_eth$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_eth$year_group=as.character(df_out_eth$year_group)
df_out_eth[df_out_eth$year_group=="(2015,2019]",]$year_group = "2016-2019"
df_out_eth[df_out_eth$year_group=="(2019,2021]",]$year_group = "2020-2021"


#Get the percentage changes
#Sex
df_supp_sex_diffs=df_supp_sex %>% 
  group_by(Condition,SEX,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_supp_sex_diffs_op=df_supp_sex_op %>% 
  group_by(Condition,SEX,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_out_sex_diff=rbind(df_supp_sex_diffs,df_supp_sex_diffs_op)
df_out_sex_diff[df_out_sex_diff$Condition=="AA_repair",]$Condition = "AA repair"
df_out_sex_diff[df_out_sex_diff$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_sex_diff[df_out_sex_diff$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_sex_diff[df_out_sex_diff$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_sex_diff[df_out_sex_diff$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_sex_diff[df_out_sex_diff$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_sex_diff[df_out_sex_diff$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_sex_diff[df_out_sex_diff$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_sex_diff[df_out_sex_diff$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_sex_diff[df_out_sex_diff$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_sex_diff$SEX=as.character(df_out_sex_diff$SEX)
df_out_sex_diff[df_out_sex_diff$SEX=="1",]$SEX = "Male"
df_out_sex_diff[df_out_sex_diff$SEX=="2",]$SEX = "Female"


#Age
df_supp_age_diffs=df_supp_age %>% 
  group_by(Condition,age_group,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_supp_age_diffs_op=df_supp_age_op %>% 
  group_by(Condition,age_group,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 


df_out_age_diff=rbind(df_supp_age_diffs,df_supp_age_diffs_op)
df_out_age_diff[df_out_age_diff$Condition=="AA_repair",]$Condition = "AA repair"
df_out_age_diff[df_out_age_diff$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_age_diff[df_out_age_diff$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_age_diff[df_out_age_diff$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_age_diff[df_out_age_diff$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_age_diff[df_out_age_diff$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_age_diff[df_out_age_diff$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_age_diff[df_out_age_diff$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_age_diff[df_out_age_diff$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_age_diff[df_out_age_diff$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_age_diff$age_group=as.character(df_out_age_diff$age_group)
df_out_age_diff[df_out_age_diff$age_group=="(-1,49]",]$age_group = "0-49"
df_out_age_diff[df_out_age_diff$age_group=="(49,59]",]$age_group = "50-59"
df_out_age_diff[df_out_age_diff$age_group=="(59,69]",]$age_group = "60-69"
df_out_age_diff[df_out_age_diff$age_group=="(69,79]",]$age_group = "70-79"
df_out_age_diff[df_out_age_diff$age_group=="(79,1e+03]",]$age_group = "80+"

#Ethnicity
df_supp_eth_diffs=df_supp_eth %>% 
  group_by(Condition,eth_readable,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_supp_eth_diffs_op=df_supp_eth_op %>% 
  group_by(Condition,eth_readable,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_out_eth_diff=rbind(df_supp_eth_diffs,df_supp_eth_diffs_op)
df_out_eth_diff[df_out_eth_diff$Condition=="AA_repair",]$Condition = "AA repair"
df_out_eth_diff[df_out_eth_diff$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_eth_diff[df_out_eth_diff$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_eth_diff[df_out_eth_diff$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_eth_diff[df_out_eth_diff$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_eth_diff[df_out_eth_diff$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_eth_diff[df_out_eth_diff$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_eth_diff[df_out_eth_diff$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_eth_diff[df_out_eth_diff$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_eth_diff[df_out_eth_diff$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"

#Charlson
df_supp_cha_diffs=df_supp_cha %>% 
  group_by(Condition,c_score_group,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_supp_cha_diffs_op=df_supp_cha_op %>% 
  group_by(Condition,c_score_group,EMERGENCY_ADMISSION) %>% 
  summarize(count_diff=average[year_group=="(2019,2021]"] - average[year_group=="(2015,2019]"],
            perc_diff=count_diff/average[year_group=="(2015,2019]"]*100) %>% 
  rename("average difference"=count_diff, 
         "percent change"= perc_diff) 

df_out_cha_diff=rbind(df_supp_cha_diffs,df_supp_cha_diffs_op)
df_out_cha_diff[df_out_cha_diff$Condition=="AA_repair",]$Condition = "AA repair"
df_out_cha_diff[df_out_cha_diff$Condition=="ACS_cayg",]$Condition = "Coronary artery bypass grafts"
df_out_cha_diff[df_out_cha_diff$Condition=="ACS_pci",]$Condition = "Percutaneous coronary intervention"
df_out_cha_diff[df_out_cha_diff$Condition=="HF_vad",]$Condition = "Ventricular assist device/ heart transplant"
df_out_cha_diff[df_out_cha_diff$Condition=="PAD_lrba",]$Condition = "Limb revascularisation, bypass or amputation "
df_out_cha_diff[df_out_cha_diff$Condition=="PAD_pla",]$Condition = "Peripheral limb angioplasty"
df_out_cha_diff[df_out_cha_diff$Condition=="stroke_cac",]$Condition = "Cerebral aneurysm coiling"
df_out_cha_diff[df_out_cha_diff$Condition=="stroke_ces",]$Condition = "Carotid endarterectomy/ stenting"
df_out_cha_diff[df_out_cha_diff$Condition=="stroke_st",]$Condition = "Stroke thrombolysis / thrombectomy"
df_out_cha_diff[df_out_cha_diff$Condition=="VTE_pae",]$Condition = "Pulmonary artery embolectomy/ embolisation"
df_out_cha_diff$c_score_group=as.character(df_out_cha_diff$c_score_group)
df_out_cha_diff[df_out_cha_diff$c_score_group=="3",]$c_score_group = "3+"


####
#Mask small results from the data

#Remove records that have <5 people
df_table[df_table$count<5,]$count=NA
df_table_avg[df_table_avg$average<5,]$average=NA

#age
df_out_age[df_out_age$count<10,]$count=NA
df_out_age[df_out_age$average<10,]$average=NA

#sex
df_out_sex[df_out_sex$count<10,]$count=NA
df_out_sex[df_out_sex$average<10,]$average=NA


#eth
df_out_eth[df_out_eth$count<10,]$count=NA
df_out_eth[df_out_eth$average<10,]$average=NA

#cha
df_out_cha[df_out_cha$count<10,]$count=NA
df_out_cha[df_out_cha$average<10,]$average=NA


#####
#Ensure data do not have missing groups (as it's a security risk)

dummy_eth=tidyr::crossing("Condition"=unique(df_out_eth$Condition),
                          "eth_readable"=unique(df_out_eth$eth_readable),
                          "EMERGENCY_ADMISSION"=unique(df_out_eth$EMERGENCY_ADMISSION),
                          "year_group"=unique(df_out_eth$year_group)) 
df_out_eth = merge(dummy_eth,df_out_eth,all=TRUE)


dummy_sex=tidyr::crossing("Condition"=unique(df_out_sex$Condition),
                          "SEX"=unique(df_out_sex$SEX),
                          "EMERGENCY_ADMISSION"=unique(df_out_sex$EMERGENCY_ADMISSION),
                          "year_group"=unique(df_out_sex$year_group)) 
df_out_sex = merge(dummy_sex,df_out_sex,all=TRUE)


dummy_cha=tidyr::crossing("Condition"=unique(df_out_cha$Condition),
                          "Charlson_score"=unique(df_out_cha$Charlson_score),
                          "EMERGENCY_ADMISSION"=unique(df_out_cha$EMERGENCY_ADMISSION),
                          "year_group"=unique(df_out_cha$year_group)) 
df_out_cha = merge(dummy_cha,df_out_cha,all=TRUE)


dummy_age=tidyr::crossing("Condition"=unique(df_out_age$Condition),
                          "age_group"=unique(df_out_age$age_group),
                          "EMERGENCY_ADMISSION"=unique(df_out_age$EMERGENCY_ADMISSION),
                          "year_group"=unique(df_out_age$year_group)) 
df_out_age = merge(dummy_age,df_out_age,all=TRUE)




#Save them all

write.csv(df_table,"../../outputs/table/df_table.csv",row.names=FALSE)
write.csv(df_table_avg,"../../outputs/table/df_table_avg.csv",row.names=FALSE)
write.csv(df_table_diffs,"../../outputs/table/df_table_diffs.csv",row.names=FALSE)

write.csv(df_out_sex_diff,"../../outputs/table/df_out_sex_diff.csv",row.names=FALSE)
write.csv(df_out_age_diff,"../../outputs/table/df_out_age_diff.csv",row.names=FALSE)
write.csv(df_out_eth_diff,"../../outputs/table/df_out_eth_diff.csv",row.names=FALSE)
write.csv(df_out_cha_diff,"../../outputs/table/df_out_cha_diff.csv",row.names=FALSE)

write.csv(df_out_sex,"../../outputs/table/df_out_sex.csv",row.names=FALSE)
write.csv(df_out_age,"../../outputs/table/df_out_age.csv",row.names=FALSE)
write.csv(df_out_eth,"../../outputs/table/df_out_eth.csv",row.names=FALSE)
write.csv(df_out_cha,"../../outputs/table/df_out_cha.csv",row.names=FALSE)

###########################
#Preprocess data for plots  

frames=list(df_dx,df_op)
names(frames)=c("Diagnoses","Procedures")


for (group_pandemic_years in c(FALSE,TRUE)){ #This is a dummy index that determines whether we want to group pandemic data together or not
  
for (i in c(1,2)){ #This index tracks the diagnoses or procedure plots

nh_final_acs_admissions_dx=frames[[i]]
admission_type=names(frames)[i]

#Change columns to lower case
colnames(nh_final_acs_admissions_dx)=tolower(colnames(nh_final_acs_admissions_dx))

#And some column names
colnames(nh_final_acs_admissions_dx)[colnames(nh_final_acs_admissions_dx)=="aa_diagnosis_any_position"]="aa_diagnosis_any"

# weekly
acs_admissions_dx_summary_age_sex_eth_admtype_wk <- nh_final_acs_admissions_dx %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) %>%
  mutate(diagnosis_any = ifelse(diagnosis_any_position == 0, 0, 1)) %>%
  mutate(diagnosis_primary = ifelse(diagnosis_primary == 0, 0, 1)) %>%
  mutate(adm_year = year(admidate), adm_month = month(admidate), adm_week = week(admidate)) %>%
  group_by(condition,emergency_admission, adm_year, adm_week, ethnic_group, gender, age_group) %>%
  summarise(total = n(), 
            adm_any = sum(diagnosis_any), 
            adm_prim = sum(diagnosis_primary)
  ) %>%
  ungroup()

#Monthly
acs_admissions_dx_summary_age_sex_eth_admtype_month <- nh_final_acs_admissions_dx %>%
  filter(!(is.na(spell_id))) %>%
  filter(admiage >= 0 & admiage < 150) %>%
  filter(sex %in% c(1,2)) %>%
  mutate(age_group = admiage) %>%
  mutate_at(vars(age_group), cut, 
            breaks = c(0, seq(50,80,10),Inf), include.lowest = T, right = F, 
            labels = c(" <50", "50-59", "60-69", "70-79", "80+")) %>%
  mutate(gender = ifelse(sex == 1, "Male", "Female")) %>%
  mutate(ethnic_group = ifelse(ethnos %in% c("A", "B", "C"), "White",
                               ifelse(ethnos %in% c("D", "E", "F", "G"), "Mixed",
                                      ifelse(ethnos %in% c("H", "J", "K", "L", "R"), "Asian",
                                             ifelse(ethnos %in% c("M", "N", "P"), "Black", "Other"))))) %>%
  #filter(aa_diagnosis_any > 0) %>%
  mutate(diagnosis_any = ifelse(diagnosis_any_position == 0, 0, 1)) %>%
  mutate(diagnosis_primary = ifelse(diagnosis_primary == 0, 0, 1)) %>%
  mutate(adm_year = year(admidate), adm_month = month(admidate), adm_week = week(admidate)) %>%
  group_by(condition,emergency_admission, adm_year, adm_month, ethnic_group, gender, age_group) %>%
  summarise(adm_total = n(), 
            adm_any = sum(diagnosis_any), 
            adm_prim = sum(diagnosis_primary)
  ) %>%
  ungroup()


#Plotting
#Take average of 2016-2019 data
df1=acs_admissions_dx_summary_age_sex_eth_admtype_month %>%
  filter(adm_year %in% c(2016,2017,2018,2019)) %>% 
  group_by(condition,emergency_admission,adm_year,adm_month) %>% 
  summarize(adm_prim=sum(adm_prim,na.rm=TRUE),
            adm_any=sum(adm_any,na.rm=TRUE),
            adm_total=sum(adm_total,na.rm=TRUE)) %>% 
  # adm_year="2016-2019") %>% 
  ungroup() %>% 
  group_by(condition,emergency_admission,adm_month) %>% 
  summarize(adm_prim=mean(adm_prim),
            adm_any=mean(adm_any),
            adm_total=mean(adm_total), 
            adm_year="2016-2019") 


#Group data by year and month 
df2=acs_admissions_dx_summary_age_sex_eth_admtype_month %>% 
  filter(adm_year %in% c(2020,2021)) %>%
  group_by(condition,emergency_admission,adm_year,adm_month) %>% 
  summarize(adm_prim=sum(adm_prim,na.rm=TRUE),
            adm_any=sum(adm_any,na.rm=TRUE),
            adm_total=sum(adm_total,na.rm=TRUE))
  
  
if (isTRUE(group_pandemic_years)){ #We want to also have plots that represent the grouped pandemic data
  
  #so modify df2 so that it now groups pandemic data together
  df2=df2 %>% 
    ungroup() %>% 
    group_by(condition,emergency_admission,adm_month) %>% 
    summarize(adm_prim=mean(adm_prim),
              adm_any=mean(adm_any),
              adm_total=mean(adm_total), 
              adm_year="2020-2021") 
}  
  

#Redefine some columns here 
df2$adm_year=as.character(df2$adm_year)

df1$emergency_admission=as.character(df1$emergency_admission)
df2$emergency_admission=as.character(df2$emergency_admission)

df1[df1$emergency_admission==0,]$emergency_admission="Elective"
df1[df1$emergency_admission==1,]$emergency_admission="Emergency"
df2[df2$emergency_admission==0,]$emergency_admission="Elective"
df2[df2$emergency_admission==1,]$emergency_admission="Emergency"



#Make any numbers <5 = 5
df1[df1$adm_prim<5,]$adm_prim=5
df1[df1$adm_any<5,]$adm_any=5
df1[df1$adm_total<5,]$adm_total=5
df2[df2$adm_prim<5,]$adm_prim=5
df2[df2$adm_any<5,]$adm_any=5
df2[df2$adm_total<5,]$adm_total=5

plot_location="../../outputs/figures"
linesize=1


#Combine dataframes and plot for all admissions - split by emergency/elective

g= rbind(df1,df2) %>% 
  ggplot(aes(x=adm_month,y=adm_any,colour=factor(adm_year)))+
  geom_line(size=linesize)+
  labs(x="Month",
       y="Number of Admissions")+
   # scale_y_continuous(breaks=seq(0,2000,250),
   #                    labels=scales::comma)+
  scale_x_continuous(breaks=seq(1,12,1))+
  #facet_grid(emergency_admission~ condition,scales="free_y")+
  facet_grid2(emergency_admission ~ condition, scales="free_y", independent="y")+
  theme(
    legend.position="bottom",
        axis.text.x=element_text(size=7))+
  ggtitle(paste0("Emergency/Elective All ",admission_type),subtitle="Wales")

if (!isTRUE(group_pandemic_years)){

  #Add scale and save
  g=g+scale_colour_discrete(name=NULL,
                        labels=c("2016-2019","2020","2021"))
  ggsave(filename=file.path(plot_location,paste0("split_any_",admission_type,".pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
  
}else{
  g=g+scale_colour_discrete(name=NULL,
                        labels=c("2016-2019","2020-2021"))
  ggsave(filename=file.path(plot_location,paste0("split_any_",admission_type,"_grouped.pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
}
  

#Combine dataframes and plot for all primary admissions - split by emergency/elective
g=rbind(df1,df2) %>% 
  ggplot(aes(x=adm_month,y=adm_prim,colour=factor(adm_year)))+
  geom_line(size=linesize)+
  labs(x="Month",
       y="Number of Admissions")+
  # scale_y_continuous(breaks=seq(0,1000,100),
  #                    labels=scales::comma)+
  scale_x_continuous(breaks=seq(1,12,1))+
  # scale_colour_discrete(name=NULL,
                        # labels=c("2016-2019","2020","2021"))+
  facet_grid(emergency_admission~condition,scales="free_y")+
  theme(
    legend.position="bottom",
        axis.text.x=element_text(size=7))+
  ggtitle(paste0("Emeregency/Elective Primary ",admission_type),subtitle="Wales")
  # ggsave(filename=file.path(plot_location,paste0("split_primary_",admission_type,".pdf")),
       # height=600/72,
       # width=900/72,
       # units="px",
       # dpi=72)

if (!isTRUE(group_pandemic_years)){ #Would make more sense to remove the ! here but I just realized theyre all wrong and a find+replace is much easier than subjecting myself to how SLOW SAIL is 
  
  #Add scale and save
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020","2021"))
  ggsave(filename=file.path(plot_location,paste0("split_primary_",admission_type,".pdf")),
         height=600/72,
         width=900/72,
         dpi=72)
  
}else{
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020-2021"))
  ggsave(filename=file.path(plot_location,paste0("split_primary_",admission_type,"_grouped.pdf")),
         height=600/72,
         width=900/72,
         dpi=72)
}

  
#Absolute numbers are much smaller for primary diagnoses

#Combine dataframes and plot for all admissions  (no emergency/elective split)
g=rbind(df1,df2) %>% 
  #First sum across the emergency/elective groups
  group_by(condition,adm_month,adm_year) %>% 
  summarize(adm_prim=sum(adm_prim),
            adm_any=sum(adm_any),
            adm_total=sum(adm_total)) %>% 
  #Then plot            
  ggplot(aes(x=adm_month,y=adm_any,colour=factor(adm_year)))+
  geom_line(size=linesize)+
  labs(x="Month",
       y="Number of Admissions")+
  # scale_y_continuous(breaks=seq(0,3000,250),
  #                    labels=scales::comma)+
  scale_x_continuous(breaks=seq(1,12,1))+
  # scale_colour_discrete(name=NULL,
  #                       labels=c("2016-2019","2020","2021"))+
  # facet_wrap(~condition,nrow=2)+
  theme(legend.position="bottom",
        axis.text.x=element_text(size=7))+
  ggtitle(paste0("All ",admission_type),subtitle="Wales")

if (i==2){ #For procedures only
  g=g+facet_wrap(~condition,scales="free_y",nrow=2)
  
  
}else{
  g=g+facet_wrap(~condition,scales="free_y",nrow=2)
}

if (!isTRUE(group_pandemic_years)){
  
  #Add scale and save
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020","2021"))
  
  ggsave(g,filename=file.path(plot_location,paste0("all_any_",admission_type,".pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
  
}else{
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020-2021"))
  ggsave(g,filename=file.path(plot_location,paste0("all_any_",admission_type,"_grouped.pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
}


#Combine dataframes and plot for all admissions  (no emergency/elective split)
g=rbind(df1,df2) %>% 
  #First sum across the emergency/elective groups
  group_by(condition,adm_month,adm_year) %>% 
  summarize(adm_prim=sum(adm_prim),
            adm_any=sum(adm_any),
            adm_total=sum(adm_total)) %>% 
  #Then plot            
  ggplot(aes(x=adm_month,y=adm_prim,colour=factor(adm_year)))+
  geom_line(size=linesize)+
  labs(x="Month",
       y="Number of Admissions")+
  # scale_y_continuous(breaks=seq(0,1000,100),
  #                    labels=scales::comma)+
  scale_x_continuous(breaks=seq(1,12,1))+
  # scale_colour_discrete(name=NULL,
  #                       labels=c("2016-2019","2020","2021"))+
  facet_wrap(~condition,nrow=2)+
  theme(legend.position="bottom",
        axis.text.x=element_text(size=7))+
  ggtitle(paste0("Primary ",admission_type),subtitle="Wales")


if (!isTRUE(group_pandemic_years)){
  
  #Add scale and save
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020","2021"))
  ggsave(g,filename=file.path(plot_location,paste0("all_primary_",admission_type,".pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
  
}else{
  g=g+scale_colour_discrete(name=NULL,
                            labels=c("2016-2019","2020-2021"))
  
  ggsave(g,filename=file.path(plot_location,paste0("all_primary_",admission_type,"_grouped.pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
}



#Create separate plots without year grouping to check averaging 
df3=acs_admissions_dx_summary_age_sex_eth_admtype_month %>% 
  group_by(condition,emergency_admission,adm_year,adm_month) %>% 
  summarize(adm_prim=sum(adm_prim,na.rm=TRUE),
            adm_any=sum(adm_any,na.rm=TRUE),
            adm_total=sum(adm_total,na.rm=TRUE)) 


df3$adm_year=as.character(df3$adm_year)


df3$emergency_admission=as.character(df3$emergency_admission)
df3[df3$emergency_admission==0,]$emergency_admission="Elective"
df3[df3$emergency_admission==1,]$emergency_admission="Emergency"


for (cond in unique(df3$condition)){
  
 df3 %>% filter(condition==cond) %>% 
    #First sum across the emergency/elective groups
    group_by(adm_month,adm_year) %>% 
    summarize(adm_prim=sum(adm_prim),
              adm_any=sum(adm_any),
              adm_total=sum(adm_total)) %>% 
    #Then plot            
    ggplot(aes(x=adm_month,y=adm_any,colour=factor(adm_year)))+
    geom_line(size=linesize)+
    labs(x="Month",
         y="Number of Admissions")+
    # scale_y_continuous(breaks=seq(0,3000,250),
    #                    labels=scales::comma)+
    scale_colour_discrete(name=NULL)+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.x=element_text(size=7))+
    ggtitle(paste0(cond," All ",admission_type),subtitle="Wales")
  # ggsave(filename=file.path(plot_location,"checks",paste0(cond,"_all_any_",admission_type,".pdf")),
  #        height=600/72,
  #        width=900/72,
  #        # units="px",
  #        dpi=72)
    
  
  
  
  df3 %>% filter(condition==cond) %>% 
    #First sum across the emergency/elective groups
    group_by(adm_month,adm_year) %>% 
    summarize(adm_prim=sum(adm_prim),
              adm_any=sum(adm_any),
              adm_total=sum(adm_total)) %>% 
    #Then plot            
    ggplot(aes(x=adm_month,y=adm_prim,colour=factor(adm_year)))+
    geom_line(size=linesize)+
    labs(x="Month",
         y="Number of Admissions")+
    # scale_y_continuous(breaks=seq(0,3000,250),
    #                    labels=scales::comma)+
    scale_colour_discrete(name=NULL)+
    scale_x_continuous(breaks=seq(1,12,1))+
    theme(axis.text.x=element_text(size=7))+
    ggtitle(paste0(cond," All ",admission_type),subtitle="Wales")
  ggsave(filename=file.path(plot_location,"checks",paste0(cond,"_all_primary_",admission_type,".pdf")),
         height=600/72,
         width=900/72,
         # units="px",
         dpi=72)
  
} 
}
}