# Script to generate a list of all the ICD10 codes in SMR01.
# Load packages:
library(tidyverse)
library(odbc)
library(dbplyr)
library(dplyr)
library(lubridate)
library(data.table)

# Connect to database:
con_rdb <- ""

smr01 = tbl(con_rdb, in_schema("project_2021_0102", "smr01"))

# Select all columns with ICD10 codes:
smr01_conditions = smr01 %>%
  select(main_condition,other_condition_1,other_condition_2,other_condition_3,
         other_condition_4,other_condition_5) %>%
  as.data.table()
# Put all the columns together and keep unique ICD10 codes:
all_ICD10 = smr01_conditions %>%
  gather() %>%
  select(-key) %>%
  distinct()

# Save the results:
setwd("~/CCU003_04")
write.csv(all_ICD10,"all_ICD10_SMR01.csv")
