# Script to extract the necessary data to compute the admission and procedure
# monthly counts and the necessary information to populate the tables.

# Load necessary packages:
library(tidyverse)
library(odbc)
library(dbplyr)
library(dplyr)
library(lubridate)
library(data.table)
library(stringr)

# Load functions:
source("~/functions.R")

# Connect to database:
con_rdb <- ""

smr01 = tbl(con_rdb, in_schema("project_2021_0102", "smr01"))

# Load all ICD10 codes that appear in SMR01 (list extracted using the script
# all_ICD10_extraction.R):
all_ICD10 = read.csv("~/CCU003_04/all_ICD10_SMR01.csv",stringsAsFactors = F)[,-1]

# Extraction of the ICD10 codes that define each CVD category. We read the codes
# from the dataspec excel file. And then we search matches of those codes
# in the all_ICD10 list. The matches are not exact, we use an "anything that
# starts with" approach.
setwd("~/CCU003_04/codelists")
ICD10_AA = readxl::read_xlsx("Aortic_aneurysm_dx_spec_250621.xlsx",sheet = 4)[1]
ICD10_code_AA = ICD10_AA$Code
a = paste(ICD10_code_AA,collapse="|")
ICD10_code_AA_final = all_ICD10[which(str_detect(all_ICD10,a))]

ICD10_IHD = readxl::read_xlsx("acute IHD dx spec_06052021.xlsx",sheet = 4)[1]
ICD10_code_IHD = ICD10_IHD$Code
ICD10_code_IHD = ICD10_code_IHD[c(which(str_detect(ICD10_code_IHD,'I21')),
                                  which(str_detect(ICD10_code_IHD,'I22')),
                                  which(str_detect(ICD10_code_IHD,'I248')),
                                  which(str_detect(ICD10_code_IHD,'I249')),
                                  which(str_detect(ICD10_code_IHD,'I200')),
                                  which(str_detect(ICD10_code_IHD,'I240')),
                                  which(str_detect(ICD10_code_IHD,'I241')))]
b = paste(ICD10_code_IHD,collapse="|")
ICD10_code_IHD_final = all_ICD10[which(str_detect(all_ICD10,b))]

ICD10_AS = readxl::read_xlsx("Acute_stroke_dx spec_19072021.xlsx",sheet = 4)[1]
ICD10_code_AS = ICD10_AS$Code
ICD10_code_AS = ICD10_code_AS[c(which(str_detect(ICD10_code_AS,'I60')),
                                which(str_detect(ICD10_code_AS,'I61')),
                                which(str_detect(ICD10_code_AS,'I63')),
                                which(str_detect(ICD10_code_AS,'I64')),
                                which(str_detect(ICD10_code_AS,'G45')))]
c = paste(ICD10_code_AS,collapse="|")
ICD10_code_AS_final = all_ICD10[which(str_detect(all_ICD10,c))]

ICD10_HF = readxl::read_xlsx("Heart failure dx spec_07062021.xlsx",sheet = 4)[1]
ICD10_code_HF = ICD10_HF$Code
ICD10_code_HF = ICD10_code_HF[c(which(str_detect(ICD10_code_HF,'I50')))]

d = paste(ICD10_code_HF,collapse="|")
ICD10_code_HF_final = all_ICD10[which(str_detect(all_ICD10,d))]

ICD10_PAD = readxl::read_xlsx("Peripheral arterial disease_dx_spec_19072021.xlsx",sheet = 4)[1]
ICD10_code_PAD = ICD10_PAD$Code
ICD10_code_PAD = ICD10_code_PAD[c(which(str_detect(ICD10_code_PAD,'I74')),
                                  which(str_detect(ICD10_code_PAD,'I70')),
                                  which(str_detect(ICD10_code_PAD,'I73')),
                                  which(str_detect(ICD10_code_PAD,'I77')))]
e = paste(ICD10_code_PAD,collapse="|")
ICD10_code_PAD_final = all_ICD10[which(str_detect(all_ICD10,e))]

ICD10_VTE = readxl::read_xlsx("VTE dx spec_17052021.xlsx",sheet = 4)[1]
ICD10_code_VTE = ICD10_VTE$Code
ICD10_code_VTE = ICD10_code_VTE[c(which(str_detect(ICD10_code_VTE,'I26')),
                                  which(str_detect(ICD10_code_VTE,'I80')),
                                  which(str_detect(ICD10_code_VTE,'I81')),
                                  which(str_detect(ICD10_code_VTE,'I82')))]
f = paste(ICD10_code_VTE,collapse="|")
ICD10_code_VTE_final = all_ICD10[which(str_detect(all_ICD10,f))]

# We have all the codes, let us proceed with the extraction.
setwd("~/CCU003_04/Jun2022/results")

weights_AA = c(2,1,1,1,1,2,1,2,6,1,1,0,2,1,3,6,1)
results_AA = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_AA_final,weights_AA)
write.csv(results_AA,"results_AA_monthly.csv")

weights_IHD = c(2,1,1,1,1,2,1,2,6,0,1,1,2,1,3,6,1)
results_IHD = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_IHD_final,weights_IHD)
write.csv(results_IHD,"results_IHD_monthly.csv")

weights_AS = c(2,0,1,1,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_AS = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_AS_final,weights_AS)
write.csv(results_AS,"results_AS_monthly.csv")

weights_HF = c(2,1,1,0,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_HF = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_HF_final,weights_HF)
write.csv(results_HF,"results_HF_monthly.csv")

weights_PAD = c(2,1,1,1,1,2,1,2,6,1,1,0,2,1,3,6,1)
results_PAD = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_PAD_final,weights_PAD)
write.csv(results_PAD,"results_PAD_monthly.csv")

weights_VTE = c(2,1,1,1,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_VTE = generate_counts_dx_v2_monthly_charlson(con_rdb,ICD10_code_VTE_final,weights_VTE)
write.csv(results_VTE,"results_VTE_monthly.csv")

############################### Procedure counts ###############################
# Extraction of the OPCS4 codes that define each CVD category. We read the codes
# from the dataspec excel file. And then we search matches of those codes
# in SMR01.
# Some of the procedure codes are split into different groupings, because
# these define different procedures (that we want to distinguish to
# populate the tables, but not for the plots).
setwd("~/CCU003_04/codelists")
OPCS4_AA = readxl::read_xlsx("Aortic_aneurysm_dx_spec_250621.xlsx",sheet = 5)[1]
OPCS4_code_AA = gsub("\\.","",OPCS4_AA$`OPCS 4.9 code`)

# Acute Stroke is a little different, since to count as an AS procedure
# we also need to have a diagnosis code.
OPCS4_AS = readxl::read_xlsx("Acute_stroke_procedures_spec_19072021.xlsx",sheet = 6)[1]
OPCS4_code_AS = gsub("\\.","",OPCS4_AS$`Code`)
OPCS4_code_AS = c(OPCS4_code_AS[c(which(str_detect(OPCS4_code_AS,'I63')),
                                  which(str_detect(OPCS4_code_AS,'O01')),
                                  which(str_detect(OPCS4_code_AS,'O02')),
                                  which(str_detect(OPCS4_code_AS,'O03')),
                                  which(str_detect(OPCS4_code_AS,'O04')),
                                  which(str_detect(OPCS4_code_AS,'Y53')),
                                  which(str_detect(OPCS4_code_AS,'Z35')))],
                  'X833','L354','L294','L295','L314')

all_ICD10 = read.csv("~/CCU003_04/all_ICD10_SMR01.csv",stringsAsFactors = F)[,-1]
ICD10_AS = readxl::read_xlsx("Acute_stroke_dx spec_19072021.xlsx",sheet = 4)[1]
ICD10_code_AS = ICD10_AS$Code
ICD10_code_AS = ICD10_code_AS[c(which(str_detect(ICD10_code_AS,'I63')))]
c = paste(ICD10_code_AS,collapse="|")
ICD10_code_AS_final = all_ICD10[which(str_detect(all_ICD10,c))]

OPCS4_IHD = readxl::read_xlsx("acute IHD revasc procedures spec_18032021.xlsx",sheet = 5)[1]
OPCS4_code_IHD = gsub("\\.","",OPCS4_IHD$`Code`)
OPCS4_code_IHD_g1 = c(OPCS4_code_IHD[c(which(str_detect(OPCS4_code_IHD,'K65')),
                                    which(str_detect(OPCS4_code_IHD,'K49')),
                                    which(str_detect(OPCS4_code_IHD,'K50')),
                                    which(str_detect(OPCS4_code_IHD,'K75')),
                                    which(str_detect(OPCS4_code_IHD,'K631')),
                                    which(str_detect(OPCS4_code_IHD,'K632')),
                                    which(str_detect(OPCS4_code_IHD,'K633')),
                                    which(str_detect(OPCS4_code_IHD,'K634')),
                                    which(str_detect(OPCS4_code_IHD,'K635')),
                                    which(str_detect(OPCS4_code_IHD,'K636')))])
OPCS4_code_IHD_g2 = c(OPCS4_code_IHD[c(which(str_detect(OPCS4_code_IHD,'K40')),
                                       which(str_detect(OPCS4_code_IHD,'K41')),
                                       which(str_detect(OPCS4_code_IHD,'K42')),
                                       which(str_detect(OPCS4_code_IHD,'K43')),
                                       which(str_detect(OPCS4_code_IHD,'K44')))],
                      'K45','K46')

OPCS4_HF = readxl::read_xlsx("Heart failure procedures spec_26032021.xlsx",sheet = 5)[1]
OPCS4_code_HF = gsub("\\.","",OPCS4_HF$`Code`)
OPCS4_code_HF_g1 = c(OPCS4_code_HF[c(which(str_detect(OPCS4_code_HF,'K60')),
                                  which(str_detect(OPCS4_code_HF,'K61')),
                                  which(str_detect(OPCS4_code_HF,'K59')))])
OPCS4_code_HF_g2 = c(OPCS4_code_HF[c(which(str_detect(OPCS4_code_HF,'K02')),
                                  which(str_detect(OPCS4_code_HF,'K01')))],
                  'K541','K542')

OPCS4_PAD = readxl::read_xlsx("Peripheral arterial disease_procedures_spec_19072021.xlsx",sheet = 5)[1]
OPCS4_code_PAD = gsub("\\.","",OPCS4_PAD$`Code`)
OPCS4_code_PAD_g1 = c(OPCS4_code_PAD[c(which(str_detect(OPCS4_code_PAD,'L54')),
                                    which(str_detect(OPCS4_code_PAD,'L63')),
                                    which(str_detect(OPCS4_code_PAD,'L66')))],
                   'L711')
OPCS4_code_PAD_g2 = c(OPCS4_code_PAD[c(which(str_detect(OPCS4_code_PAD,'L16')),
                                    which(str_detect(OPCS4_code_PAD,'L48')),
                                    which(str_detect(OPCS4_code_PAD,'L49')),
                                    which(str_detect(OPCS4_code_PAD,'L50')),
                                    which(str_detect(OPCS4_code_PAD,'L51')),
                                    which(str_detect(OPCS4_code_PAD,'L52')),
                                    which(str_detect(OPCS4_code_PAD,'L53')),
                                    which(str_detect(OPCS4_code_PAD,'L56')),
                                    which(str_detect(OPCS4_code_PAD,'L57')),
                                    which(str_detect(OPCS4_code_PAD,'L58')),
                                    which(str_detect(OPCS4_code_PAD,'L59')),
                                    which(str_detect(OPCS4_code_PAD,'L60')),
                                    which(str_detect(OPCS4_code_PAD,'X09')),
                                    which(str_detect(OPCS4_code_PAD,'X10')),
                                    which(str_detect(OPCS4_code_PAD,'X11')),
                                    which(str_detect(OPCS4_code_PAD,'X12')))],
                   'L206','L216','L652','L653')
OPCS4_VTE = readxl::read_xlsx("VTE procedures spec_17052021.xlsx",sheet = 5)[1]
OPCS4_code_VTE = gsub("\\.","",OPCS4_VTE$`Code`)
OPCS4_code_VTE = c(OPCS4_code_VTE[c(which(str_detect(OPCS4_code_VTE,'L13')))],
                   'L124','L125')

# We have all the codes, let us proceed with the extraction.
setwd("~/CCU003_04/Jun2022/results")
weights_AA = c(2,1,1,1,1,2,1,2,6,1,1,0,2,1,3,6,1)
results_AA = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_AA,weights_AA,"Repair")
write.csv(results_AA,"results_AA_proc_monthly.csv")

weights_IHD = c(2,1,1,1,1,2,1,2,6,0,1,1,2,1,3,6,1)
results_IHD_g1 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_IHD_g1,weights_IHD,"Percutaneous")
results_IHD_g2 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_IHD_g2,weights_IHD,"Bypass")
results_IHD = rbind(results_IHD_g1,results_IHD_g2)
write.csv(results_IHD,"results_IHD_proc_monthly.csv")

weights_AS = c(2,0,1,1,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_AS = generate_counts_proc_AS_monthly_charlson(con_rdb,OPCS4_code_AS,ICD10_code_AS_final,weights_AS)
write.csv(results_AS,"results_AS_proc_monthly.csv")

weights_HF = c(2,1,1,0,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_HF_g1 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_HF_g1,weights_HF,"Pacemaker")
results_HF_g2 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_HF_g2,weights_HF,"Transplant")
results_HF = rbind(results_HF_g1,results_HF_g2)
write.csv(results_HF,"results_HF_proc_monthly.csv")

weights_PAD = c(2,1,1,1,1,2,1,2,6,1,1,0,2,1,3,6,1)
results_PAD_g1 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_PAD_g1,weights_PAD,"Angioplasty")
results_PAD_g2 = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_PAD_g2,weights_PAD,"Revascular")
results_PAD = rbind(results_PAD_g1,results_PAD_g2)
write.csv(results_PAD,"results_PAD_proc_monthly.csv")

weights_VTE = c(2,1,1,1,1,2,1,2,6,1,1,1,2,1,3,6,1)
results_VTE = generate_counts_proc_monthly_charlson(con_rdb,OPCS4_code_VTE,weights_VTE,"Embolectomy")
write.csv(results_VTE,"results_VTE_proc_monthly.csv")

