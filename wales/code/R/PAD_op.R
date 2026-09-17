library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)

#Define data locations
condition_prefix="PAD_op"
dataspec_prefix="Peripheral arterial disease_procedures"
top_dir="../.."
eclipse_dir=file.path(top_dir,"data","eclipse","procedures")

#Get all csv files in eclipse directory
csv_files=list.files(eclipse_dir)[endsWith(list.files(eclipse_dir),".csv")]
#Keep only those prefixed correctly
csv_files=csv_files[grepl(condition_prefix,csv_files)]
#Get the most recent of these
input_file=file.path(eclipse_dir,csv_files[rev(order(file.info(file.path(eclipse_dir,csv_files))$ctime))][1])

#Define the dataspec location
dataspec_dir=file.path(top_dir,"references","data_specs")
dataspec_file=file.path(dataspec_dir,list.files(dataspec_dir)[startsWith(list.files(dataspec_dir),dataspec_prefix)])

#And the location where the output of this script should be saved 
output_file=file.path(top_dir,"data","combine",paste0(condition_prefix,".csv"))

#Read in data from Eclipse and the dataspec 
dat=read.csv(input_file)
specdat=openxlsx::read.xlsx(dataspec_file,sheet="ICD-10 Charleston index")

#Convert types
dat$ADMIS_DT=ymd(dat$ADMIS_DT)
dat$DISCH_DT=ymd(dat$DISCH_DT)

#Organize chronologically
dat = dat %>%  arrange(SPELL_NUM_E,EPI_NUM,OPER_NUM)

#Clean diagnosis codes of errors 
dat$OPER_CD=str_replace(dat$OPER_CD,"-","")

#CREATING DIAG 123 MANUALLY
dat$OPER_CD_123 = substr(dat$OPER_CD, 0, 3)

#######################################################
#Covid

#Get covid flag for operations
covid_codes=c("B342","B972","U409","U071","U072") 
dat$"POSS_COVID"=as.integer(dat$OPER_CD %in% covid_codes)


#######################################################
#Table codes

#Special flags added for the population of table1

codes=c("L54","L63","L66","L711","L16","L206","L216","L48","L49","L50","L51","L52","L53","L56","L57","L58","L59","L60","L652","L553","X09","X10","X11","X12")
names(codes)=c(rep("PAD_pla",4),rep("PAD_lrba",20))

dat$PAD_pla=0
dat$PAD_lrba=0

for (code in codes){
  if (TRUE %in% (startsWith(dat$OPER_CD,code))){ #If this code is present somewhere
    
    dat[startsWith(dat$OPER_CD,code),][names(codes)[match(code,codes)]]=1 #Then set the flag for table1 using the named vector  
    
    
  }
}

#######################################################
#######################################################
#Hierarchies


#Check operations for the first hierarchy
dat$hier_1=0

codes = c("L16",
          "L48",
          "L49",
          "L50",
          "L51",
          "L52",
          "L53",
          "L56",
          "L57",
          "L58",
          "L59",
          "L60",
          "X09",
          "X10",
          "X11",
          "X12")


for (code in codes){
  if (TRUE %in% (startsWith(dat$OPER_CD,code))){ #If this code is present somewhere
  
  dat[startsWith(dat$OPER_CD,code),]$"hier_1"=1 #Then set the hierarchy for it 
  
  }
}

#Repeat for group 2
codes=c("L54","L63","L66")

for (code in codes){
  if (TRUE %in% (startsWith(dat$OPER_CD,code))){ #If this code is present somewhere
    
    dat[startsWith(dat$OPER_CD,code),]$"hier_1"=2 #Then set the hierarchy for it 
    
  }
}

dat[dat$OPER_CD %in% c("L206", "L216", "L652", "L653"),]$"hier_1"=1
dat[dat$OPER_CD %in% c("L711"),]$"hier_1"=2

#See the amounts
# table(dat$hier_1)

#Group data by episode and get highest hierarchy for that whole episode
dat = dat %>% 
  group_by(SPELL_NUM_E,PROV_UNIT_CD,EPI_NUM) %>% 
  mutate(episode_hier_1=min(hier_1[hier_1>0]))
#             episode_hier_2=max(hier_2)) #Get hierarchies over all episodes - at diagnosis level  


#Flag up the last episode in the spell
dat = dat %>%
  group_by(SPELL_NUM_E,PROV_UNIT_CD) %>%
  mutate(lastepisodeinspell=EPI_NUM==max(EPI_NUM))

#Get the hierarchies for the operations of the last episode of the spell
dat = dat %>% 
  group_by(SPELL_NUM_E,PROV_UNIT_CD,EPI_NUM) %>%  
  mutate(last_episode_hier_1=case_when(lastepisodeinspell==TRUE ~ min(hier_1[hier_1>0]),
                                       TRUE ~ NA_real_),
         #             last_episode_hier_2=case_when(lastepisodeinspell==TRUE ~ max(hier_2),
         #                                           TRUE ~ NA_real_),
         #And also for the first diagnosis of the last episode 
         last_episode_first_oper_hier_1=case_when(lastepisodeinspell==TRUE & OPER_NUM==1 ~ hier_1,
                                                  TRUE ~ NA_real_))
#             last_episode_first_OPER_hier_2=case_when(lastepisodeinspell==TRUE & OPER_NUM==1 ~ hier_2,
#                                                      TRUE ~ NA_real_))


#Get the diagnostic positions of the hierarchies for the first two categories (ie all episodes and last episode of spell)
dat = dat %>%  
  group_by(SPELL_NUM_E,PROV_UNIT_CD,EPI_NUM) %>% 
  mutate(oper_pos_all_ep_h1=case_when(hier_1==episode_hier_1 ~ OPER_NUM,
                                      TRUE ~ NA_integer_),
         #             oper_pos_all_ep_h2=case_when(hier_2==episode_hier_2 ~ OPER_NUM,
         #                                          TRUE ~ NA_integer_),
         oper_pos_last_ep_h1=case_when(hier_1==last_episode_hier_1 ~ OPER_NUM,
                                       TRUE ~ NA_integer_))
#             oper_pos_last_ep_h2=case_when(hier_2==last_episode_hier_2 ~ OPER_NUM,
#                                         TRUE ~ NA_integer_))

#######################################################
#Charl(e)son codes 

#Clean Charlston data
specdat = specdat %>% 
  drop_na(CHARLSON_DESC) %>% 
  filter(!str_detect(CHARLSON_DESC,"^\\s*$")) #Drop na and blank descriptions

#Add in charleston scores

one_point=c("Congestive Heart Failure",
            "Peripheral Vascular Disease",
            "Dementia",
            "Cerebrovascular Disease",
            "Chronic pulmonary Disease",
            "Rheumatic Disease",
            "Peptic Ulcer Disease",
            "Mild Liver Disease",
            "Diabetes without chronic complication",
            "myocardial infarction") #This last one came from Kate and isn't in the spec 

two_point=c("Hemiplegia or paraplegia",
            "Renal Disease",
            "Diabetes with chronic complication",
            "Cancer")

three_point=c("Severe Liver Disease")

four_point=c("Metastatic Cancer",
             "HIV")

specdat$CHARLSON_DESC=trimws(specdat$CHARLSON_DESC)
specdat$score=0
specdat[tolower(specdat$CHARLSON_DESC) %in% tolower(one_point),]$score=1
specdat[tolower(specdat$CHARLSON_DESC) %in% tolower(two_point),]$score=2
specdat[tolower(specdat$CHARLSON_DESC) %in% tolower(three_point),]$score=3
specdat[tolower(specdat$CHARLSON_DESC) %in% tolower(four_point),]$score=4


#Check they're all accounted for
# table(specdat$score) #We should have no 0 scores

#This should be true - otherwise some conditions are missing. Comment out line below to see which 
all(tolower(c(one_point,two_point,three_point,four_point)) %in% tolower(unique(specdat$CHARLSON_DESC)))
#c(one_point,two_point,three_point,four_point)[!c(one_point,two_point,three_point,four_point) %in% unique(specdat$CHARLSON_DESC)]

#Set the flags at diagnostic level
dat$"CHARLSON_CANCER"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="cancer",]$ICD10_CODE))
dat$"CHARLSON_CEREBRO"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="cerebrovascular disease",]$ICD10_CODE))
dat$"CHARLSON_CPD"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="chronic pulmonary disease",]$ICD10_CODE))
dat$"CHARLSON_CONGEST_HEART_FAIL"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="congestive heart failure",]$ICD10_CODE))
dat$"CHARLSON_DEMENTIA"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="dementia",]$ICD10_CODE))
dat$"CHARLSON_DIABETES_CHRON"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="diabetes with chronic complication",]$ICD10_CODE))
dat$"CHARLSON_DIABETES_WO_CHRON"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="diabetes without chronic complication",]$ICD10_CODE))
dat$"CHARLSON_HEMIPLEGIA_PARAPLEGIA"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="hemiplegia or paraplegia",]$ICD10_CODE))
dat$"CHARLSON_METASTATIC_CANCER"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="metastatic cancer",]$ICD10_CODE))
dat$"CHARLSON_MYOCARD_INFARCT"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="myocardial infarction",]$ICD10_CODE))
dat$"CHARLSON_PEPTIC_ULCER"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="peptic ulcer disease",]$ICD10_CODE))
# dat$"CHARLSON_PERIPH_VASC"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="peripheral vascular disease",]$ICD10_CODE))
dat$"CHARLSON_PERIPH_VASC"=0 #removed from spec - see Lucy's email or AA spec
dat$"CHARLSON_RENAL"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="renal disease",]$ICD10_CODE))
dat$"CHARLSON_RHEUMATIC"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="rheumatic disease",]$ICD10_CODE))
dat$"CHARLSON_SEVERE_LIVER"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="severe liver disease",]$ICD10_CODE))
dat$"CHARLSON_HIV"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="hiv",]$ICD10_CODE))
dat$"CHARLSON_MILD_LIVER"=as.integer(dat$OPER_CD %in% unique(specdat[tolower(specdat$CHARLSON_DESC)=="mild liver disease",]$ICD10_CODE))


# #Check how many exist in each category 
# for (col in colnames(dat)[grep("CHARLSON",colnames(dat))]){
#   
#   print(col)
#   print(table(dat[col]))
#   
# }

#Save the appropriate column name for the admission data (lata on)
specdat$column=""
specdat[tolower(specdat$CHARLSON_DESC)=="cancer",]$column="CHARLSON_CANCER"
specdat[tolower(specdat$CHARLSON_DESC)=="cerebrovascular disease",]$column="CHARLSON_CEREBRO"
specdat[tolower(specdat$CHARLSON_DESC)=="chronic pulmonary disease",]$column="CHARLSON_CPD"
specdat[tolower(specdat$CHARLSON_DESC)=="congestive heart failure",]$column="CHARLSON_CONGEST_HEART_FAIL"
specdat[tolower(specdat$CHARLSON_DESC)=="dementia",]$column="CHARLSON_DEMENTIA"
specdat[tolower(specdat$CHARLSON_DESC)=="hemiplegia or paraplegia",]$column="CHARLSON_HEMIPLEGIA_PARAPLEGIA"
specdat[tolower(specdat$CHARLSON_DESC)=="metastatic cancer",]$column="CHARLSON_METASTATIC_CANCER"
specdat[tolower(specdat$CHARLSON_DESC)=="myocardial infarction",]$column="CHARLSON_MYOCARD_INFARCT"
specdat[tolower(specdat$CHARLSON_DESC)=="peptic ulcer disease",]$column="CHARLSON_PEPTIC_ULCER"
specdat[tolower(specdat$CHARLSON_DESC)=="peripheral vascular disease",]$column="CHARLSON_PERIPH_VASC"
specdat[tolower(specdat$CHARLSON_DESC)=="renal disease",]$column="CHARLSON_RENAL"
specdat[tolower(specdat$CHARLSON_DESC)=="rheumatic disease",]$column="CHARLSON_RHEUMATIC"
specdat[tolower(specdat$CHARLSON_DESC)=="severe liver disease",]$column="CHARLSON_SEVERE_LIVER"
specdat[tolower(specdat$CHARLSON_DESC)=="hiv",]$column="CHARLSON_HIV"
specdat[tolower(specdat$CHARLSON_DESC)=="mild liver disease",]$column="CHARLSON_MILD_LIVER"
specdat[tolower(specdat$CHARLSON_DESC)=="diabetes with chronic complication",]$column="CHARLSON_DIABETES_CHRON"
specdat[tolower(specdat$CHARLSON_DESC)=="diabetes without chronic complication",]$column="CHARLSON_DIABETES_WO_CHRON"

#Group by spell and get relevant metrics
dat_spell = 
  dat %>% 
  group_by(SPELL_NUM_E,PROV_UNIT_CD) %>% 
  summarize(patient_number=first(PAT_ID_E),#Patient number
            
            NUM_EPS=max(EPI_NUM), #Number of episodes
            ADMIDATE=min(ADMIS_DT), #The spell admission date - earliest episode admission date
            DISDATE=max(DISCH_DT), #Spell discharge date - latest episode discharge date
            
            ADMIS_MTHD_CD=first(ADMIS_MTHD_CD[EPI_NUM==min(EPI_NUM)]), #Get admission/discharge info  for first/last episode
            DISCH_MTHD_CD=last(DISCH_MTHD_CD[EPI_NUM==max(EPI_NUM)]), #Get admission/discharge info  for first/last episode
            
            #Get the admission speciality code
            ADMIS_SPEC_CD=first(ADMIS_SPEC_CD),
            
            #Get personal info from first episode of first spell
            SEX=first(GNDR_CD), 
            ADMIAGE=first(AGE_EPI_STR_YR),
            ETHNOS = first(ETH_GRP_DERIVED_CD),
            
            #This admission is poss_covid if any of the diagnoses are
            POSS_COVID=as.integer(any(POSS_COVID)==1), 
            
            #Get the appropriate (min/max) for first hierarchy 
            hier1=min(episode_hier_1[episode_hier_1>0]),
            #And the lowest diagnostic position of it
            hier1pos=min(oper_pos_all_ep_h1,na.rm=T),
            
            #Repeat for the second hierarchy
            #hier2=max(episode_hier_2),
            #            hier2pos=min(oper_pos_all_ep_h2,na.rm=T),
            
            #Last episode hierarchy 1
            last_ep_hier1=first(na.omit(last_episode_hier_1)),
            last_ep_hier1pos=first(na.omit(oper_pos_all_ep_h1)),
            
            #Repeat for hier 2
            #            last_ep_hier2=first(na.omit(last_episode_hier_2)),
            #           last_ep_hier2pos=first(na.omit(oper_pos_all_ep_h2)),
            
            #And the first diagnosis of the last episode
            last_ep_d1_h1=first(na.omit(last_episode_first_oper_hier_1,)),
            #            last_ep_d1_h2=first(na.omit(last_episode_first_oper_hier_2,)),
            
            #Get the ICD code for the first diagnosis of the last episode
            last_ep_d1_code=first(OPER_CD[!is.na(last_episode_first_oper_hier_1)]),
            
            #Does the patient die in any of these episodes?
            DIED_DURING_ADMISSION=any(DISCH_MTHD_CD==4),
            
            #Is the spell complete?
            COMPLETED_SPELL_CIPS=as.integer(any(DISCH_MTHD_CD) %in% c(1,2,3,4)),
            
            #Is the first episode of the spell an emergency?
            EMERGENCY_ADMISSION=as.integer(first(ADMIS_MTHD_CD) %in% c(21,22,23,24,25,27,28)),
            
            
            #Charlson info
            CHARLSON_CANCER=as.integer(any(CHARLSON_CANCER)==1),
            CHARLSON_CEREBRO=as.integer(any(CHARLSON_CEREBRO)==1),
            CHARLSON_CPD=as.integer(any(CHARLSON_CPD)==1),
            CHARLSON_CONGEST_HEART_FAIL=as.integer(any(CHARLSON_CONGEST_HEART_FAIL)==1),
            CHARLSON_DEMENTIA=as.integer(any(CHARLSON_DEMENTIA)==1),
            CHARLSON_DIABETES_CHRON=as.integer(any(CHARLSON_DIABETES_CHRON)==1),
            CHARLSON_DIABETES_WO_CHRON=as.integer(any(CHARLSON_DIABETES_WO_CHRON)==1),
            CHARLSON_HEMIPLEGIA_PARAPLEGIA=as.integer(any(CHARLSON_HEMIPLEGIA_PARAPLEGIA)==1),
            CHARLSON_METASTATIC_CANCER=as.integer(any(CHARLSON_METASTATIC_CANCER)==1),
            CHARLSON_MYOCARD_INFARCT=as.integer(any(CHARLSON_MYOCARD_INFARCT)==1),
            CHARLSON_PEPTIC_ULCER=as.integer(any(CHARLSON_PEPTIC_ULCER)==1),
            CHARLSON_PERIPH_VASC=as.integer(any(CHARLSON_PERIPH_VASC)==1),
            CHARLSON_RENAL=as.integer(any(CHARLSON_RENAL)==1),
            CHARLSON_RHEUMATIC=as.integer(any(CHARLSON_RHEUMATIC)==1),
            CHARLSON_SEVERE_LIVER=as.integer(any(CHARLSON_SEVERE_LIVER)==1),
            CHARLSON_HIV=as.integer(any(CHARLSON_HIV)==1),
            CHARLSON_MILD_LIVER=as.integer(any(CHARLSON_MILD_LIVER)==1),
            
            #Get total Charlson score
            score_sum=sum(CHARLSON_CANCER*first(specdat[specdat$column=="CHARLSON_CANCER",]$score),
                          CHARLSON_CEREBRO*first(specdat[specdat$column=="CHARLSON_CEREBRO",]$score),
                          CHARLSON_CPD*first(specdat[specdat$column=="CHARLSON_CPD",]$score),
                          CHARLSON_CONGEST_HEART_FAIL*first(specdat[specdat$column=="CHARLSON_CONGEST_HEART_FAIL",]$score),
                          CHARLSON_DEMENTIA*first(specdat[specdat$column=="CHARLSON_DEMENTIA",]$score),
                          CHARLSON_DIABETES_CHRON*first(specdat[specdat$column=="CHARLSON_DIABETES_CHRON",]$score),
                          CHARLSON_DIABETES_WO_CHRON*first(specdat[specdat$column=="CHARLSON_DIABETES_WO_CHRON",]$score),
                          CHARLSON_HEMIPLEGIA_PARAPLEGIA*first(specdat[specdat$column=="CHARLSON_HEMIPLEGIA_PARAPLEGIA",]$score),
                          CHARLSON_METASTATIC_CANCER*first(specdat[specdat$column=="CHARLSON_METASTATIC_CANCER",]$score),
                          CHARLSON_MYOCARD_INFARCT*first(specdat[specdat$column=="CHARLSON_MYOCARD_INFARCT",]$score),
                          CHARLSON_PEPTIC_ULCER*first(specdat[specdat$column=="CHARLSON_PEPTIC_ULCER",]$score),
                          CHARLSON_PERIPH_VASC*first(specdat[specdat$column=="CHARLSON_PERIPH_VASC",]$score),
                          CHARLSON_RENAL*first(specdat[specdat$column=="CHARLSON_RENAL",]$score),
                          CHARLSON_RHEUMATIC*first(specdat[specdat$column=="CHARLSON_RHEUMATIC",]$score),
                          CHARLSON_SEVERE_LIVER*first(specdat[specdat$column=="CHARLSON_SEVERE_LIVER",]$score),
                          CHARLSON_HIV*first(specdat[specdat$column=="CHARLSON_HIV",]$score),
                          CHARLSON_MILD_LIVER*first(specdat[specdat$column=="CHARLSON_MILD_LIVER",]$score)),
            #Carry through table1 info
            PAD_pla=as.integer(any(PAD_pla==1)),
            PAD_lrba=as.integer(any(PAD_lrba==1))
            
            
  )

#Quick check

if ((Inf %in% names(table(dat_spell$hier1)))==TRUE){
  print("INFS DETECTED IN HIER1")
}
if ((Inf %in% names(table(dat_spell$last_ep_hier1)))==TRUE){
  print("INFS DETECTED IN LAST_EP_HIER1")
}
if ((Inf %in% names(table(dat_spell$last_ep_d1_h1)))==TRUE){
  print("INFS DETECTED IN last_ep_d1_h1")
}




#######################################################
#Superspells

#Spell to superspell formation
superspells=
  dat_spell %>% 
  group_by(patient_number) %>%  #Group by patient
  arrange(ADMIDATE) %>%         #Sort chronologically
  mutate(timediff=ADMIDATE-lag(DISDATE), #Calculate the difference in time between the admission of one spell and the discharge date of the other
         linked_spell=case_when(timediff<=1 & ADMIS_MTHD_CD==81 ~ lag(SPELL_NUM_E)),
         top_spell=case_when(SPELL_NUM_E %in% linked_spell & is.na(linked_spell) ~ SPELL_NUM_E )) %>%    #Admission code of 81 indicates a transfer
  filter(!is.na(linked_spell) | SPELL_NUM_E %in% linked_spell) %>% #Filter only for spells that are part of superspells
  fill(top_spell) %>% #Fill this out so that we can...
  group_by(top_spell) %>%  #...group by the top spell
  summarize(SPELL_NUM_E=first(top_spell), #The spell number of the superspell is the number of the first spell comprising it
            patient_number=first(patient_number), #And take the other summary statistics - these ones are all the same over the spell so just take first
            ADMIDATE=first(ADMIDATE),
            DISDATE=last(DISDATE),
            PROV_UNIT_CD=first(PROV_UNIT_CD),
            SEX=first(SEX),
            ADMIAGE=first(ADMIAGE),
            ETHNOS=first(ETHNOS),
            
            made_of=n(),#Not needed, but good for testing
            
            #Does the patient die in any spell?
            DIED_DURING_ADMISSION=any(DISCH_MTHD_CD==4),
            
            #Is the superspell complete?
            COMPLETED_SPELL_CIPS=as.integer(last(DISCH_MTHD_CD) %in% c(1,2,3,4)),
            
            #Is the first spell an emergency?
            EMERGENCY_ADMISSION=first(EMERGENCY_ADMISSION),
            
            #Hierarchy info:
            #Get the appropriate (min/max) for first hierarchy 
            hier1=min(hier1[hier1>0]),
            #And the lowest diagnostic position of it
            hier1pos=min(hier1pos,na.rm=T),
            
            #Repeat for the second hierarchy
            # hier2=max(hier2),
            # hier2pos=min(hier2pos,na.rm=T),
            
            #Last spell hierarchy 1
            last_ep_hier1=last(hier1),
            last_ep_hier1pos=last(hier1pos),
            
            #Repeat for hier 2
            # last_ep_hier2=last(hier2),
            # last_ep_hier2pos=last(hier2pos),
            
            #And the first diagnosis of the last episode
            last_ep_d1_h1=last(last_ep_d1_h1),
            # last_ep_d1_h2=last(last_ep_d1_h2),
            last_ep_d1_code=last(last_ep_d1_code),
            
            #If these are =1 for any spell, then they =1 for the superspell
            POSS_COVID=as.integer(any(POSS_COVID)==1),
            CHARLSON_CANCER=as.integer(any(CHARLSON_CANCER)==1),
            CHARLSON_CEREBRO=as.integer(any(CHARLSON_CEREBRO)==1),
            CHARLSON_CPD=as.integer(any(CHARLSON_CPD)==1),
            CHARLSON_CONGEST_HEART_FAIL=as.integer(any(CHARLSON_CONGEST_HEART_FAIL)==1),
            CHARLSON_DEMENTIA=as.integer(any(CHARLSON_DEMENTIA)==1),
            CHARLSON_DIABETES_CHRON=as.integer(any(CHARLSON_DIABETES_CHRON)==1),
            CHARLSON_DIABETES_WO_CHRON=as.integer(any(CHARLSON_DIABETES_WO_CHRON)==1),
            CHARLSON_HEMIPLEGIA_PARAPLEGIA=as.integer(any(CHARLSON_HEMIPLEGIA_PARAPLEGIA)==1),
            CHARLSON_METASTATIC_CANCER=as.integer(any(CHARLSON_METASTATIC_CANCER)==1),
            CHARLSON_MYOCARD_INFARCT=as.integer(any(CHARLSON_MYOCARD_INFARCT)==1),
            CHARLSON_PEPTIC_ULCER=as.integer(any(CHARLSON_PEPTIC_ULCER)==1),
            CHARLSON_PERIPH_VASC=as.integer(any(CHARLSON_PERIPH_VASC)==1),
            CHARLSON_RENAL=as.integer(any(CHARLSON_RENAL)==1),
            CHARLSON_RHEUMATIC=as.integer(any(CHARLSON_RHEUMATIC)==1),
            CHARLSON_SEVERE_LIVER=as.integer(any(CHARLSON_SEVERE_LIVER)==1),
            CHARLSON_HIV=as.integer(any(CHARLSON_HIV)==1),
            CHARLSON_MILD_LIVER=as.integer(any(CHARLSON_MILD_LIVER)==1),
            
            #Get total Charlson score
            score_sum=sum(CHARLSON_CANCER*first(specdat[specdat$column=="CHARLSON_CANCER",]$score),
                          CHARLSON_CEREBRO*first(specdat[specdat$column=="CHARLSON_CEREBRO",]$score),
                          CHARLSON_CPD*first(specdat[specdat$column=="CHARLSON_CPD",]$score),
                          CHARLSON_CONGEST_HEART_FAIL*first(specdat[specdat$column=="CHARLSON_CONGEST_HEART_FAIL",]$score),
                          CHARLSON_DEMENTIA*first(specdat[specdat$column=="CHARLSON_DEMENTIA",]$score),
                          CHARLSON_DIABETES_CHRON*first(specdat[specdat$column=="CHARLSON_DIABETES_CHRON",]$score),
                          CHARLSON_DIABETES_WO_CHRON*first(specdat[specdat$column=="CHARLSON_DIABETES_WO_CHRON",]$score),
                          CHARLSON_HEMIPLEGIA_PARAPLEGIA*first(specdat[specdat$column=="CHARLSON_HEMIPLEGIA_PARAPLEGIA",]$score),
                          CHARLSON_METASTATIC_CANCER*first(specdat[specdat$column=="CHARLSON_METASTATIC_CANCER",]$score),
                          CHARLSON_MYOCARD_INFARCT*first(specdat[specdat$column=="CHARLSON_MYOCARD_INFARCT",]$score),
                          CHARLSON_PEPTIC_ULCER*first(specdat[specdat$column=="CHARLSON_PEPTIC_ULCER",]$score),
                          CHARLSON_PERIPH_VASC*first(specdat[specdat$column=="CHARLSON_PERIPH_VASC",]$score),
                          CHARLSON_RENAL*first(specdat[specdat$column=="CHARLSON_RENAL",]$score),
                          CHARLSON_RHEUMATIC*first(specdat[specdat$column=="CHARLSON_RHEUMATIC",]$score),
                          CHARLSON_SEVERE_LIVER*first(specdat[specdat$column=="CHARLSON_SEVERE_LIVER",]$score),
                          CHARLSON_HIV*first(specdat[specdat$column=="CHARLSON_HIV",]$score),
                          CHARLSON_MILD_LIVER*first(specdat[specdat$column=="CHARLSON_MILD_LIVER",]$score)),
            #Carry through table1 info
            PAD_pla=as.integer(any(PAD_pla==1)),
            PAD_lrba=as.integer(any(PAD_lrba==1))
  ) %>% 
  mutate(superspell=1) %>% 
  select(-top_spell)



#Repeat some of the above so we can get a list of the spells that are now part of superspells
#Spell to superspell formation
spells_in_superspells=
  dat_spell %>% 
  group_by(patient_number) %>%  #Group by patient
  arrange(ADMIDATE) %>%         #Sort chronologically
  mutate(timediff=ADMIDATE-lag(DISDATE), #Calculate the difference in time between the admission of one spell and the discharge date of the other
         linked_spell=case_when(timediff<=1 & ADMIS_MTHD_CD==81 ~ lag(SPELL_NUM_E)),
         top_spell=case_when(SPELL_NUM_E %in% linked_spell & is.na(linked_spell) ~ SPELL_NUM_E )) %>%    #Admission code of 81 indicates a transfer
  filter(!is.na(linked_spell) | SPELL_NUM_E %in% linked_spell) %>% #Filter only for spells that are part of superspells
  ungroup() %>% 
  select(SPELL_NUM_E)


#Get the spells which are NOT part of superspells
dat_just_spells=
  dat_spell %>%
  filter(!SPELL_NUM_E %in% spells_in_superspells$SPELL_NUM_E) %>% 
  rename(made_of=NUM_EPS) %>% 
  mutate(superspell=0) %>% 
  select(-ADMIS_MTHD_CD, #Some columns we don't need - exclude here
         -DISCH_MTHD_CD,
         -ADMIS_SPEC_CD)


#Form final admissions data, which is the spells that aren't part of superspells, plus the superspells 
dat_admissions = rbind(dat_just_spells,superspells)


#renames
colnames(dat_admissions)[colnames(dat_admissions)=="SPELL_NUM_E"]="SPELL_ID"
colnames(dat_admissions)[colnames(dat_admissions)=="PROV_UNIT_CD"]="PROCODET"
colnames(dat_admissions)[colnames(dat_admissions)=="patient_number"]="PERSON_ID"
colnames(dat_admissions)[colnames(dat_admissions)=="hier_1"]="DIAGNOSIS_ANY"
colnames(dat_admissions)[colnames(dat_admissions)=="hier1pos"]="DIAGNOSIS_ANY_POSITION"
colnames(dat_admissions)[colnames(dat_admissions)=="last_ep_hier1"]="DIAGNOSIS_DISCHARGE"
colnames(dat_admissions)[colnames(dat_admissions)=="last_ep_hier1pos"]="DIAGNOSIS_DISCHARGE_POSITION"
colnames(dat_admissions)[colnames(dat_admissions)=="last_ep_d1_h1"]="DIAGNOSIS_PRIMARY"
colnames(dat_admissions)[colnames(dat_admissions)=="last_ep_d1_code"]="PRIMARY_DISCHARGE_DIAGNOSIS_CODE"
colnames(dat_admissions)[colnames(dat_admissions)=="last_ep_d1h2"]="DIAGNOSIS_TYPE_LOCATION"


#Now calculate some other columns
#Admission duration
dat_admissions$SPELL_DUR=dat_admissions$DISDATE - dat_admissions$ADMIDATE 
#dat_admissions$ADMISSION_TO_ITU=as.integer(dat_admissions$ADMIS_SPEC_CD==192) #We don't have this for Wales (I don't think)

#Set these as blank
dat_admissions$PROVIDER_PCI_CAPABLE = NA
dat_admissions$RESIDENCE_REGION = NA
dat_admissions$PROVIDOR_WEEKLY_SUS = NA

write_csv(dat_admissions, output_file)
