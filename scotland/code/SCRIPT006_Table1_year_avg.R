# Script to compute the counts (and averages over the years) necessary to
# populate Table1 of the paper.
library(dplyr)

year_emergency_counts = function(df,adm_type){
  # df: data.frame from where to compute the counts
  # adm_type: either diagnosis or procedures.
  
  # Function that computes yearly counts for diagnoses or procedures, 
  # separating emergency and elective admissions.
 if(adm_type == "diagnosis"){
   df_count = df %>%
     filter(prim == 1) %>%
     filter(adm_year <= "2021" & adm_year >= "2016") %>%
     group_by(emergency_admission,adm_year) %>%
     summarize(adm_count = n()) %>%
     ungroup() 
 } else if(adm_type == "procedure"){
   df_count = df %>%
     filter(adm_year <= "2021" & adm_year >= "2016") %>%
     group_by(emergency_admission,adm_year) %>%
     summarize(adm_count = n()) %>%
     ungroup() 
 }
 return(df_count)
}
year_counts = function(df,adm_type){
  # df: data.frame from where to compute the counts
  # adm_type: either diagnosis or procedures.
  
  # Function that computes yearly counts for diagnoses or procedures, 
  # without separating emergency and elective admissions.
  if(adm_type == "diagnosis"){
    df_count = df %>%
      filter(prim == 1) %>%
      filter(adm_year <= "2021" & adm_year >= "2016") %>%
      group_by(adm_year) %>%
      summarize(adm_count = n()) %>%
      ungroup() 
  } else if(adm_type == "procedure"){
    df_count = df %>%
      filter(adm_year <= "2021" & adm_year >= "2016") %>%
      group_by(adm_year) %>%
      summarize(adm_count = n()) %>%
      ungroup() 
  }
  return(df_count)
}
year_emergency_avg = function(df){
  # df: data.frame with the yearly emergency/elective counts.
  
  # Function that computes the average count over two different set of years.
  df_avg = df %>%
    mutate(year = if_else(adm_year >= 2016 & adm_year <= 2019,'2016/19','2020/21')) %>%
    group_by(emergency_admission,year) %>%
    summarise(avg = mean(adm_count))
  return(df_avg)
}
year_avg = function(df){
  # df: data.frame with the yearly counts.
  
  # Function that computes the average count over two different set of years.
  df_avg = df %>%
    mutate(year = if_else(adm_year >= 2016 & adm_year <= 2019,'2016/19','2020/21')) %>%
    group_by(year) %>%
    summarise(avg = mean(adm_count))
  return(df_avg)
}
generate_tables_single_row = function(df,name,adm_type){
  # df: table generated using data_extraction_monthly.R
  # name: name of the CVD
  # adm_type: either diagnosis or procedures.
  
  # Function that generates a single row of table 1.
  
  # Compute yearly and average counts. Store them in different objects. 
  df_count_emergency = year_emergency_counts(df,adm_type)
  df_count_year = year_counts(df,adm_type)
  df_avg_emergency = year_emergency_avg(df_count_emergency)
  df_avg_year = year_avg(df_count_year)
  
  # Define table with elective yearly counts:
  table_elective_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_elective_count) = name
  colnames(table_elective_count) = c(2016:2021)
  table_elective_count[1,] = df_count_emergency$adm_count[which(df_count_emergency$emergency_admission == 0)]
  
  # Define table with emergency yearly counts:
  table_emergency_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_emergency_count) = name
  colnames(table_emergency_count) = c(2016:2021)
  table_emergency_count[1,] = df_count_emergency$adm_count[which(df_count_emergency$emergency_admission == 1)]
  
  # Define table with yearly counts (both elective and emergency):
  table_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_count) = name
  colnames(table_count) = c(2016:2021)
  table_count[1,] = df_count_year$adm_count
  
  # Define table with emergency and elective average counts:
  table_emergency_avg = as.data.frame(matrix(0,nrow=1,ncol=8))
  rownames(table_emergency_avg) = name
  colnames(table_emergency_avg) = c("2016/19 elective","2020/21 elective",
                                    "2016/19 emergency","2020/21 emergency",
                                    "diff elective","% change elective",
                                    "diff emergency","% change emergency")
  table_emergency_avg[1,1:4] = df_avg_emergency$avg
  table_emergency_avg[1,5] = - table_emergency_avg[1,1] + table_emergency_avg[1,2]
  table_emergency_avg[1,6] = format(round((table_emergency_avg[1,5] / table_emergency_avg[1,1]) * 100,2),nsmall = 2)
  table_emergency_avg[1,7] = - table_emergency_avg[1,3] + table_emergency_avg[1,4]
  table_emergency_avg[1,8] = format(round((table_emergency_avg[1,7] / table_emergency_avg[1,3]) * 100,2),nsmall = 2)
  
  # Define table with average counts:
  table_avg = as.data.frame(matrix(0,nrow=1,ncol=4))
  rownames(table_avg) = name
  colnames(table_avg) = c("2016/19","2020/21","diff","% change")
  table_avg[1,1:2] = df_avg_year$avg
  table_avg[1,3] = - table_avg[1,1] + table_avg[1,2]
  table_avg[1,4] = format(round((table_avg[1,3] / table_avg[1,1]) * 100,2),nsmall = 2)
  
  L = list('elect+count' = table_elective_count, 'emerg+count' = table_emergency_count,
           'count' = table_count, 'elec_emerg+avg' = table_emergency_avg, 'avg' = table_avg)
  return(L)
}
generate_tables_single_row_zero = function(df,name,adm_type){
  # Same as before but fills the tables with 0's. This is for diagnosis or
  # procedures with very few counts, making it not feasible to compute averages.
  table_elective_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_elective_count) = name
  colnames(table_elective_count) = c(2016:2021)

  table_emergency_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_emergency_count) = name
  colnames(table_emergency_count) = c(2016:2021)

  table_count = as.data.frame(matrix(0,nrow=1,ncol=6))
  rownames(table_count) = name
  colnames(table_count) = c(2016:2021)

  table_emergency_avg = as.data.frame(matrix(0,nrow=1,ncol=8))
  rownames(table_emergency_avg) = name
  colnames(table_emergency_avg) = c("2016/19 elective","2020/21 elective",
                                    "2016/19 emergency","2020/21 emergency",
                                    "diff elective","% change elective",
                                    "diff emergency","% change emergency")

  table_avg = as.data.frame(matrix(0,nrow=1,ncol=4))
  rownames(table_avg) = name
  colnames(table_avg) = c("2016/19","2020/21","diff","% change")

  L = list('elect+count' = table_elective_count, 'emerg+count' = table_emergency_count,
           'count' = table_count, 'elec_emerg+avg' = table_emergency_avg, 'avg' = table_avg)
  return(L)
}

# Call the functions defined above depending on the CVD.
setwd("~/CCU003_04/Jun2022/results")
ACS = read.csv("results_IHD_monthly.csv", stringsAsFactors = F)[,-1] 
row01 = generate_tables_single_row(ACS,"ACS","diagnosis") 

ACS_proc = read.csv("results_IHD_proc_monthly.csv", stringsAsFactors = F)[,-1]
ACS_proc1 = ACS_proc %>%
  filter(strata == "Percutaneous")
ACS_proc2 = ACS_proc %>%
  filter(strata == "Bypass")
row02 = generate_tables_single_row(ACS_proc1,"Percutaneous","procedure")
row03 = generate_tables_single_row(ACS_proc2, "Bypass","procedure")

HF = read.csv("results_HF_monthly.csv", stringsAsFactors = F)[,-1] 
row04 = generate_tables_single_row(HF,"HF","diagnosis")

HF_proc = read.csv("results_HF_proc_monthly.csv", stringsAsFactors = F)[,-1] 
levels(as.factor(HF_proc$strata))
HF_proc1 = HF_proc %>%
  filter(strata == "Pacemaker")
HF_proc2 = HF_proc %>%
  filter(strata == "Transplant")
row05 = generate_tables_single_row(HF_proc1,"Pacemaker","procedure")
row06 = generate_tables_single_row(HF_proc2, "Transplant","procedure")

AS = read.csv("results_AS_monthly.csv", stringsAsFactors = F)[,-1] 
row07 = generate_tables_single_row(AS,"Stroke","diagnosis")

AS_proc = read.csv("results_AS_proc_monthly.csv", stringsAsFactors = F)[,-1] 
levels(as.factor(AS_proc$strata))
AS_proc1 = AS_proc %>%
  filter(strata == "Thrombo")
AS_proc2 = AS_proc %>%
  filter(strata == "Carotid")
AS_proc3 = AS_proc %>%
  filter(strata == "Cerebral")
row08 = generate_tables_single_row_zero(AS_proc1,"Thrombo","procedure")
row09 = generate_tables_single_row(AS_proc2, "Carotid","procedure")
row10 = generate_tables_single_row(AS_proc3, "Cerebral","procedure")

AA = read.csv("results_AA_monthly.csv", stringsAsFactors = F)[,-1]
row11 = generate_tables_single_row(AA,"AA","diagnosis") 

AA_proc = read.csv("results_AA_proc_monthly.csv", stringsAsFactors = F)[,-1]
row12 = generate_tables_single_row(AA_proc,"AA repair","procedure") 

PAD = read.csv("results_PAD_monthly.csv", stringsAsFactors = F)[,-1]
row13 = generate_tables_single_row(PAD,"PAD","diagnosis") 

PAD_proc = read.csv("results_PAD_proc_monthly.csv", stringsAsFactors = F)[,-1]
levels(as.factor(PAD_proc$strata))
PAD_proc1 = PAD_proc %>%
  filter(strata == "Angioplasty")
PAD_proc2 = PAD_proc %>%
  filter(strata == "Revascular")
row14 = generate_tables_single_row(PAD_proc1, "Angioplasty","procedure")
row15 = generate_tables_single_row(PAD_proc2, "Revascular","procedure")

VTE = read.csv("results_VTE_monthly.csv", stringsAsFactors = F)[,-1]
row16 = generate_tables_single_row(VTE,"VTE","diagnosis") 

VTE_proc = read.csv("results_VTE_proc_monthly.csv", stringsAsFactors = F)[,-1]
row17 = generate_tables_single_row(VTE_proc,"Embolectomy","procedure") 

# Join all the rows together, by table: 
rows = c(paste("row0",1:9,sep=""),paste("row",10:17,sep=""))
N = length(rows)
elective_count = get(rows[1])[[1]]
for(i in 2:N){
  elective_count = rbind(elective_count,get(rows[i])[[1]])
}

emergency_count = get(rows[1])[[2]]
for(i in 2:N){
  emergency_count = rbind(emergency_count,get(rows[i])[[2]])
}

all_count = get(rows[1])[[3]]
for(i in 2:N){
  all_count = rbind(all_count,get(rows[i])[[3]])
}

emergency_avg = get(rows[1])[[4]]
for(i in 2:N){
  emergency_avg = rbind(emergency_avg,get(rows[i])[[4]])
}

all_avg = get(rows[1])[[5]]
for(i in 2:N){
  all_avg = rbind(all_avg,get(rows[i])[[5]])
}
# Save results:
setwd("~/CCU003_04/Jun2022/Outputs")
write.csv(elective_count,"elective_year_count.csv")
write.csv(emergency_count,"emergency_year_count.csv")
write.csv(all_count,"all_year_count.csv")
write.csv(emergency_avg,"emergency_year_avg.csv")
write.csv(all_avg,"all_year_avg.csv")