generate_counts_dx_v2_monthly_charlson = function(con_rdb,ICD10_code,w){
  # con_rbd: connection to the database to access the SMR01 table.
  # ICD10_code: list of codes to look for in the SMR01 table.
  # w: weights to compute the Charlson index.
  
  # Extract all the codes that belong to each one of the categories to compute
  # the Charslon score.
  Charlson_list = readxl::read_xlsx("~/CCU003_04/codelists/Aortic_aneurysm_dx_spec_250621.xlsx",sheet = 7) %>%
    filter(!is.na(CHARLSON_DESC)) %>%
    mutate(CHARLSON_DESC = if_else(CHARLSON_DESC == "Chronic pulmonary disease","Chronic pulmonary Disease",CHARLSON_DESC))
  
  codes_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cancer"),]$ICD10_CODE
  codes_cerebrovascular = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cerebrovascular Disease"),]$ICD10_CODE
  codes_COPD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Chronic pulmonary Disease"),]$ICD10_CODE
  codes_HF = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Congestive Heart Failure"),]$ICD10_CODE
  codes_dementia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Dementia"),]$ICD10_CODE
  codes_diabetes_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes with chronic complication"),]$ICD10_CODE
  codes_diabetes_no_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes without chronic complication"),]$ICD10_CODE
  codes_HIV = Charlson_list[which(Charlson_list$CHARLSON_DESC == "HIV"),]$ICD10_CODE
  codes_metastatic_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Metastatic Cancer"),]$ICD10_CODE
  codes_hemiplegia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Hemiplegia or paraplegia"),]$ICD10_CODE
  codes_mild_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Mild Liver Disease" ),]$ICD10_CODE
  codes_MI = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Myocardial Infarction" ),]$ICD10_CODE
  codes_ulcer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peptic Ulcer Disease" ),]$ICD10_CODE
  codes_renal = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Renal Disease" ),]$ICD10_CODE
  codes_rheumatic = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Rheumatic Disease" ),]$ICD10_CODE
  codes_severe_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Severe Liver Disease" ),]$ICD10_CODE
  codes_PVD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peripheral Vascular Disease" ),]$ICD10_CODE
  
  # Begin extraction:
  smr01 = tbl(con_rdb, in_schema("project_2021_0102", "smr01"))
  # Extracting all the records of the CVD of interest in SMR01:
  print("Extracting data from SMR01...")
  AA = smr01 %>%
    # Select the columns that we'll be using.
    select(anon_id,sex,age,date_of_birth,ethnicity_code,admission_date,admission_type,
           admission_reason,admission_transfer_from,admission_transfer_from_loc,main_condition,
           other_condition_1,other_condition_2,other_condition_3,other_condition_4,other_condition_5,
           significant_facility,discharge_date,discharge_type,discharge_transfer_to,
           length_of_stay,main_operation,other_operation_1,other_operation_2,other_operation_3,
           episode_marker,cis_marker) %>%
    # Only keep the records whose condition relates to the disease (to the codes)
    filter_at(.vars = vars(main_condition,other_condition_1,other_condition_2,other_condition_3,
                           other_condition_4,other_condition_5),
              .vars_predicate = any_vars(. %in% ICD10_code)) %>%
    # Our study analyses data from 1st of January 2016, but since some
    # admissions started on 2015, better to give 1y window. We will
    # filter later.
    filter(admission_date >= '2015-01-01') %>%
    distinct() %>%
    as.data.table()
  
  # Adding adm_year, adm_month, gender, age_group and ethnic_group columns:
  print("Adding demographic groupings...")
  AA_dem = AA %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    mutate(gender = case_when(sex == 1 ~ 'Male',
                              sex == 2 ~ 'Female')) %>%
    mutate(age_group = case_when(age < 50 ~ '<50',
                                 age >= 50 & age < 60 ~ '50-59',
                                 age >= 60 & age < 70 ~ '60-69',
                                 age >= 70 & age < 80 ~ '70-79',
                                 age >= 80 ~ '80+')) %>%
    mutate(ethnic_group = case_when(ethnicity_code %in% c('1A','1B','1C','1K','1L','1Z') ~ 'White',
                                     ethnicity_code == '2A' ~ 'Mixed',
                                     ethnicity_code %in% c('3F','3G','3H','3J','3Z') ~ 'Asian',
                                     ethnicity_code %in% c('4D','4Y','5C','5D','5Y') ~ 'Black',
                                     TRUE ~ 'Other/Unknown')) %>%
    mutate(adm_any = rep(1,n())) %>%
    mutate(prim = if_else(main_condition %in% ICD10_code,1,0)) %>%
    mutate(emergency_admission = if_else(admission_type %in% c(30,31,32,33,34,35,36,38,39),1,0))
  
  # Clean the ethnicity status before grouping. First: extract those patients
  # who have a different record in the ethnic_group variable.
  print("Cleaning misleading ethnicity records...")
  AA_eth_id = AA_dem %>%
    group_by(anon_id) %>%
    mutate(lvl = length(levels(as.factor(ethnic_group)))==1) %>%
    filter(lvl == F) %>%
    select(anon_id) %>%
    distinct()
  
  # Loop through all the identified patients and select the ethnicity that is not
  # Other/Unknown. In the strange case that a patient has been classified in 
  # two different categories not including Other/Unknown, we will pick the first
  # that appears (assuming that the input error has been made on the later admissions).
  N = dim(AA_eth_id)[1]
  eth = character(N)
  for (i in 1:N){
    id_now = as.character(AA_eth_id[i,1])
    subset = AA_dem[which(AA_dem$anon_id==id_now),]
    lvl = levels(as.factor(subset$ethnic_group))
    if(length(lvl)!=1){
      eth[i] = subset$ethnic_group[which(subset$ethnic_group != "Other/Unknown")][1]
    }
  }
  
  # Fix the ethnic_group column so every patient has the same ethnicity across their
  # episodes.
  AA_eth_fixed = as.data.frame(cbind('anon_id'=AA_eth_id$anon_id,'eth'=eth),stringsAsFactors = F)
  AA_dem_v2 = AA_dem %>%
    left_join(AA_eth_fixed,"anon_id") %>%
    mutate(ethnic_group = if_else(is.na(eth),ethnic_group,eth)) %>%
    select(-eth)
  
  # Include the indicator variables that will be used to compute the
  # Charlson comorbidity index:
  AA_charlson = AA_dem_v2 %>%
    mutate(CHA0010 = if_else(main_condition %in% codes_cancer | other_condition_1 %in% codes_cancer |
                               other_condition_2 %in% codes_cancer | other_condition_3 %in% codes_cancer |
                               other_condition_4 %in% codes_cancer | other_condition_5 %in% codes_cancer,1,0),
           CHA0020 = if_else(main_condition %in% codes_cerebrovascular | other_condition_1 %in% codes_cerebrovascular |
                               other_condition_2 %in% codes_cerebrovascular | other_condition_3 %in% codes_cerebrovascular |
                               other_condition_4 %in% codes_cerebrovascular | other_condition_5 %in% codes_cerebrovascular,1,0),
           CHA0030 = if_else(main_condition %in% codes_COPD | other_condition_1 %in% codes_COPD |
                               other_condition_2 %in% codes_COPD | other_condition_3 %in% codes_COPD |
                               other_condition_4 %in% codes_COPD | other_condition_5 %in% codes_COPD,1,0),
           CHA0040 = if_else(main_condition %in% codes_HF | other_condition_1 %in% codes_HF |
                               other_condition_2 %in% codes_HF | other_condition_3 %in% codes_HF |
                               other_condition_4 %in% codes_HF | other_condition_5 %in% codes_HF,1,0),
           CHA0050 = if_else(main_condition %in% codes_dementia | other_condition_1 %in% codes_dementia |
                               other_condition_2 %in% codes_dementia | other_condition_3 %in% codes_dementia |
                               other_condition_4 %in% codes_dementia | other_condition_5 %in% codes_dementia,1,0),
           CHA0060 = if_else(main_condition %in% codes_diabetes_complication | other_condition_1 %in% codes_diabetes_complication |
                               other_condition_2 %in% codes_diabetes_complication | other_condition_3 %in% codes_diabetes_complication |
                               other_condition_4 %in% codes_diabetes_complication | other_condition_5 %in% codes_diabetes_complication,1,0),
           CHA0070 = if_else(main_condition %in% codes_diabetes_no_complication | other_condition_1 %in% codes_diabetes_no_complication |
                               other_condition_2 %in% codes_diabetes_no_complication | other_condition_3 %in% codes_diabetes_no_complication |
                               other_condition_4 %in% codes_diabetes_no_complication | other_condition_5 %in% codes_diabetes_no_complication,1,0),
           CHA0080 = if_else(main_condition %in% codes_hemiplegia | other_condition_1 %in% codes_hemiplegia |
                               other_condition_2 %in% codes_hemiplegia | other_condition_3 %in% codes_hemiplegia |
                               other_condition_4 %in% codes_hemiplegia | other_condition_5 %in% codes_hemiplegia,1,0),
           CHA0090 = if_else(main_condition %in% codes_metastatic_cancer | other_condition_1 %in% codes_metastatic_cancer |
                               other_condition_2 %in% codes_metastatic_cancer | other_condition_3 %in% codes_metastatic_cancer |
                               other_condition_4 %in% codes_metastatic_cancer | other_condition_5 %in% codes_metastatic_cancer,1,0),
           CHA0100 = if_else(main_condition %in% codes_MI | other_condition_1 %in% codes_MI |
                               other_condition_2 %in% codes_MI | other_condition_3 %in% codes_MI |
                               other_condition_4 %in% codes_MI | other_condition_5 %in% codes_MI,1,0),
           CHA0110 = if_else(main_condition %in% codes_ulcer | other_condition_1 %in% codes_ulcer |
                               other_condition_2 %in% codes_ulcer | other_condition_3 %in% codes_ulcer |
                               other_condition_4 %in% codes_ulcer | other_condition_5 %in% codes_ulcer,1,0),
           CHA0120 = if_else(main_condition %in% codes_PVD | other_condition_1 %in% codes_PVD |
                               other_condition_2 %in% codes_PVD | other_condition_3 %in% codes_PVD |
                               other_condition_4 %in% codes_PVD | other_condition_5 %in% codes_PVD,1,0),
           CHA0130 = if_else(main_condition %in% codes_renal | other_condition_1 %in% codes_renal |
                               other_condition_2 %in% codes_renal | other_condition_3 %in% codes_renal |
                               other_condition_4 %in% codes_renal | other_condition_5 %in% codes_renal,1,0),
           CHA0140 = if_else(main_condition %in% codes_rheumatic | other_condition_1 %in% codes_rheumatic |
                               other_condition_2 %in% codes_rheumatic | other_condition_3 %in% codes_rheumatic |
                               other_condition_4 %in% codes_rheumatic | other_condition_5 %in% codes_rheumatic,1,0),
           CHA0150 = if_else(main_condition %in% codes_severe_liver | other_condition_1 %in% codes_severe_liver |
                               other_condition_2 %in% codes_severe_liver | other_condition_3 %in% codes_severe_liver |
                               other_condition_4 %in% codes_severe_liver | other_condition_5 %in% codes_severe_liver,1,0),
           CHA0160 = if_else(main_condition %in% codes_HIV | other_condition_1 %in% codes_HIV |
                               other_condition_2 %in% codes_HIV | other_condition_3 %in% codes_HIV |
                               other_condition_4 %in% codes_HIV | other_condition_5 %in% codes_HIV,1,0),
           CHA0170 = if_else(main_condition %in% codes_mild_liver | other_condition_1 %in% codes_mild_liver |
                               other_condition_2 %in% codes_mild_liver | other_condition_3 %in% codes_mild_liver |
                               other_condition_4 %in% codes_mild_liver | other_condition_5 %in% codes_mild_liver,1,0))
  
  # Define admissions: one row per patient per admssion (contributing
  # as +1 to the counts). 
  # cis_marker is the variable in SMR01 that groups episodes (multiple rows) 
  # as one admission (single row).
  # Each admission will have as admission_date the admission date of the first episode.
  # Each admission will have as discharge_date the discharge date of the last episode.
  # There are admissions in which the order of the dx change. So episodes
  # within an admission could classify as adm_prim and others don't. We use only the
  # clinical-discharge (last episode within an admission) to compute that.
  # An admission is considered adm_prim if the code of interest is the primary
  # reason for admission (main_condition column).
  # We also compute here the Charlson score.
  print("Creating one admission per patient...")
  AA_single = AA_charlson %>%
    select(anon_id,admission_date,discharge_date,cis_marker,
           gender,age_group,ethnic_group,adm_any,prim,emergency_admission,CHA0010:CHA0170) %>%
    group_by(anon_id,cis_marker) %>%
    mutate(admission_date = first(admission_date),
           discharge_date = last(discharge_date),
           prim = last(prim),
           emergency_admission = max(emergency_admission)) %>%
    mutate_at(c("CHA0010","CHA0020","CHA0030","CHA0040","CHA0050","CHA0060","CHA0070","CHA0080",
                "CHA0090","CHA0100","CHA0110","CHA0120","CHA0130","CHA0140","CHA0150","CHA0160",
                "CHA0170"),~max(.)) %>%
    ungroup() %>%
    distinct() %>%
    distinct(anon_id,cis_marker,admission_date,discharge_date,.keep_all = T) %>%
    mutate(Charlson_score = w[1]*CHA0010 + w[2]*CHA0020 + w[3]*CHA0030 + w[4]*CHA0040 + w[5]*CHA0050 + 
             w[6]*CHA0060 + w[7]*CHA0070 + w[8]*CHA0080 + w[9]*CHA0090 + w[10]*CHA0100 + w[11]*CHA0110 + w[12]*CHA0120 +
             w[13]*CHA0130 + w[14]*CHA0140 + w[15]*CHA0150 + w[16]*CHA0160 + w[17]*CHA0170)
  
  # Now  we need to exclude those admissions that occurred within 30d of other 
  # admission (for the same patient, of course). We compute the time difference
  # between admissions, and if it's less than 30, then it should be eliminated
  # from the counts.
  AA_single_lag = AA_single %>%
    group_by(anon_id) %>%
    arrange(admission_date,discharge_date) %>%
    mutate (LagDate = lag(discharge_date), Diff = admission_date - LagDate) %>%
    ungroup() %>%
    mutate(status = if_else(!is.na(Diff) & Diff >= 0 & Diff <= 30, 'Eligible', 'Not Eligible')) %>%
    arrange(anon_id,admission_date,discharge_date)
  
  # The idea now is to check if, within every patient, there are successive episodes that are
  # within those 30d.
  print("Filtering those admissions within 30d of the previous one...")
  # Extract the patients that are Eligible:
  a = as.matrix(unique(AA_single_lag[which(AA_single_lag$status=='Eligible'),1])) 
  eligible = AA_single_lag %>%
    filter(anon_id %in% a)
  
  # Create also the dataframe with the info of the patients who are not eligible. 
  not_eligible = AA_single_lag %>%
    filter(!(anon_id %in% a))
  
  eligible_count = AA_single_lag %>%
    group_by(status) %>%
    summarise(n = n()) %>%
    filter(status == "Eligible") %>%
    select(n) %>%
    as.numeric()
  
  while(eligible_count != 0){
    erase_1st_eligible = eligible %>%
      group_by(anon_id,status) %>%
      mutate(OUT = if_else(status == 'Eligible' & row_number() == 1, T, F)) %>%
      filter( OUT == F)
    
    a = as.matrix(unique(erase_1st_eligible[which(erase_1st_eligible$status=='Eligible'),1]))
    new_eligible = erase_1st_eligible %>%
      filter(anon_id %in% a)
    
    new_not_eligible = erase_1st_eligible %>%
      filter(!(anon_id %in% a)) %>%
      select(-OUT) %>%
      ungroup()
    not_eligible = rbind(not_eligible, new_not_eligible)
    
    eligible = new_eligible %>%
      group_by(anon_id) %>%
      mutate (LagDate = lag(discharge_date), Diff = admission_date - LagDate) %>%
      ungroup() %>%
      mutate(status = if_else(!is.na(Diff) & Diff >= 0 & Diff <= 30, 'Eligible', 'Not Eligible'))
    print(levels(as.factor(eligible$status)))
    if(length(levels(as.factor(eligible$status))) > 1){
      eligible_count = eligible %>%
        group_by(status) %>%
        summarise(n = n()) %>%
        filter(status == "Eligible") %>%
        select(n) %>%
        as.numeric()
    } else {
      eligible_count = 0
    }
    print(eligible_count)
  }
  
  eligible = eligible %>%
    select(-OUT)
  
  AA_semifinal = rbind(not_eligible, eligible) %>%
    arrange(anon_id)
  
  # Recompute adm_year and adm_month since we changed admission and discharges
  # dates.

  AA_final= AA_semifinal %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    select(anon_id,admission_date,discharge_date,adm_year,adm_month,adm_week,
           gender,age_group,ethnic_group,adm_any,prim,emergency_admission,cis_marker,
           Charlson_score)
  return(AA_final)
}
  
generate_counts_proc_monthly_charlson = function(con_rdb,OPCS4_code,w,strata_group){
  # con_rbd: connection to the database to access the SMR01 table.
  # OPCS4_code: list of codes to look for in the SMR01 table.
  # w: weights to compute the Charlson index.
  # strata_group: name to label the set of procedures we extract.
  
  # Extract all the codes that belong to each one of the categories to compute
  # the Charslon score.
  Charlson_list = readxl::read_xlsx("~/CCU003_04/codelists/Aortic_aneurysm_dx_spec_250621.xlsx",sheet = 7) %>%
    filter(!is.na(CHARLSON_DESC)) %>%
    mutate(CHARLSON_DESC = if_else(CHARLSON_DESC == "Chronic pulmonary disease","Chronic pulmonary Disease",CHARLSON_DESC))
  levels(as.factor(Charlson_list$CHARLSON_DESC))
  
  codes_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cancer"),]$ICD10_CODE
  codes_cerebrovascular = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cerebrovascular Disease"),]$ICD10_CODE
  codes_COPD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Chronic pulmonary Disease"),]$ICD10_CODE
  codes_HF = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Congestive Heart Failure"),]$ICD10_CODE
  codes_dementia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Dementia"),]$ICD10_CODE
  codes_diabetes_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes with chronic complication"),]$ICD10_CODE
  codes_diabetes_no_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes without chronic complication"),]$ICD10_CODE
  codes_HIV = Charlson_list[which(Charlson_list$CHARLSON_DESC == "HIV"),]$ICD10_CODE
  codes_metastatic_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Metastatic Cancer"),]$ICD10_CODE
  codes_hemiplegia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Hemiplegia or paraplegia"),]$ICD10_CODE
  codes_mild_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Mild Liver Disease" ),]$ICD10_CODE
  codes_MI = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Myocardial Infarction" ),]$ICD10_CODE
  codes_ulcer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peptic Ulcer Disease" ),]$ICD10_CODE
  codes_renal = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Renal Disease" ),]$ICD10_CODE
  codes_rheumatic = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Rheumatic Disease" ),]$ICD10_CODE
  codes_severe_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Severe Liver Disease" ),]$ICD10_CODE
  codes_PVD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peripheral Vascular Disease" ),]$ICD10_CODE
  
  smr01 = tbl(con_rdb, in_schema("project_2021_0102", "smr01"))
  # Extracting all the records of the procedure of interest in SMR01. We need to look at  
  # main_operation and other_operation_X columns. Problem: some of the entries
  # have 8 characters instead of the usual 4 characters (OPCS-4), which means that
  # two procedures were performed at the same time. We need to split in two those
  # 8-length-entries and check if the individual 4-codes are of interest.
  
  print("Extracting data from SMR01...")
  AA = smr01 %>%
    # Select the columns that we'll be using.
    select(anon_id,sex,age,date_of_birth,ethnicity_code,admission_date,admission_type,
           admission_reason,admission_transfer_from,admission_transfer_from_loc,main_condition,
           other_condition_1,other_condition_2,other_condition_3,other_condition_4,other_condition_5,
           significant_facility,discharge_date,discharge_type,discharge_transfer_to,
           length_of_stay,main_operation,other_operation_1,other_operation_2,other_operation_3,
           episode_marker,cis_marker) %>%
    # Only keep the records whose condition relates to the disease (to the codes)
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,
                           other_operation_3),
              .vars_predicate = any_vars(if_else(nchar(.) <= 4,.,str_sub(.,1,4)) %in% OPCS4_code |
                                           if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) %in% OPCS4_code)) %>%
    # Our study analyses data from 1st of January 2016, but we begin extraction
    # from before to be safe.
    filter(admission_date >= '2015-01-01') %>%
    distinct() %>%
    as.data.table()
  
  # Adding adm_year, adm_month, gender, age_group and ethnic_group columns:
  print("Adding demographic groupings...")
  AA_dem = AA %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    mutate(gender = case_when(sex == 1 ~ 'Male',
                              sex == 2 ~ 'Female')) %>%
    mutate(age_group = case_when(age < 50 ~ '<50',
                                 age >= 50 & age < 60 ~ '50-59',
                                 age >= 60 & age < 70 ~ '60-69',
                                 age >= 70 & age < 80 ~ '70-79',
                                 age >= 80 ~ '80+')) %>%
    mutate(ethnic_group = case_when(ethnicity_code %in% c('1A','1B','1C','1K','1L','1Z') ~ 'White',
                                    ethnicity_code == '2A' ~ 'Mixed',
                                    ethnicity_code %in% c('3F','3G','3H','3J','3Z') ~ 'Asian',
                                    ethnicity_code %in% c('4D','4Y','5C','5D','5Y') ~ 'Black',
                                    TRUE ~ 'Other/Unknown')) %>%
    mutate(proc = rep(1,n())) %>%
    mutate(emergency_admission = if_else(admission_type %in% c(30,31,32,33,34,35,36,38,39),1,0))
  
  # Clean the ethnicity status before grouping. First: extract those patients
  # who have a different record in the ethnic_group variable.
  print("Cleaning misleading ethnicity records...")
  AA_eth_id = AA_dem %>%
    group_by(anon_id) %>%
    mutate(lvl = length(levels(as.factor(ethnic_group)))==1) %>%
    filter(lvl == F) %>%
    select(anon_id) %>%
    distinct()
  if(dim(AA_eth_id)[1]!=0){
    # Loop through all the identified patients and select the ethnicity that is not
    # Other/Unknown. In the strange case that a patient has been clasified in "White"
    # and "Black", we'll pick the first that appears (assuming that the input error
    # has been made on the later admissions).
    N = dim(AA_eth_id)[1]
    eth = character(N)
    for (i in 1:N){
      id_now = as.character(AA_eth_id[i,1])
      subset = AA_dem[which(AA_dem$anon_id==id_now),]
      lvl = levels(as.factor(subset$ethnic_group))
      if(length(lvl)!=1){
        eth[i] = subset$ethnic_group[which(subset$ethnic_group != "Other/Unknown")][1]
      }
    }
    
    # Fix the ethnic_group column so every patient has the same ethnicity across their
    # episodes.
    AA_eth_fixed = as.data.frame(cbind('anon_id'=AA_eth_id$anon_id,'eth'=eth),stringsAsFactors = F)
    AA_dem_v2 = AA_dem %>%
      left_join(AA_eth_fixed,"anon_id") %>%
      mutate(ethnic_group = if_else(is.na(eth),ethnic_group,eth)) %>%
      select(-eth)
  } else {
    AA_dem_v2 = AA_dem
  }
  
  AA_charlson = AA_dem_v2 %>%
    mutate(CHA0010 = if_else(main_condition %in% codes_cancer | other_condition_1 %in% codes_cancer |
                               other_condition_2 %in% codes_cancer | other_condition_3 %in% codes_cancer |
                               other_condition_4 %in% codes_cancer | other_condition_5 %in% codes_cancer,1,0),
           CHA0020 = if_else(main_condition %in% codes_cerebrovascular | other_condition_1 %in% codes_cerebrovascular |
                               other_condition_2 %in% codes_cerebrovascular | other_condition_3 %in% codes_cerebrovascular |
                               other_condition_4 %in% codes_cerebrovascular | other_condition_5 %in% codes_cerebrovascular,1,0),
           CHA0030 = if_else(main_condition %in% codes_COPD | other_condition_1 %in% codes_COPD |
                               other_condition_2 %in% codes_COPD | other_condition_3 %in% codes_COPD |
                               other_condition_4 %in% codes_COPD | other_condition_5 %in% codes_COPD,1,0),
           CHA0040 = if_else(main_condition %in% codes_HF | other_condition_1 %in% codes_HF |
                               other_condition_2 %in% codes_HF | other_condition_3 %in% codes_HF |
                               other_condition_4 %in% codes_HF | other_condition_5 %in% codes_HF,1,0),
           CHA0050 = if_else(main_condition %in% codes_dementia | other_condition_1 %in% codes_dementia |
                               other_condition_2 %in% codes_dementia | other_condition_3 %in% codes_dementia |
                               other_condition_4 %in% codes_dementia | other_condition_5 %in% codes_dementia,1,0),
           CHA0060 = if_else(main_condition %in% codes_diabetes_complication | other_condition_1 %in% codes_diabetes_complication |
                               other_condition_2 %in% codes_diabetes_complication | other_condition_3 %in% codes_diabetes_complication |
                               other_condition_4 %in% codes_diabetes_complication | other_condition_5 %in% codes_diabetes_complication,1,0),
           CHA0070 = if_else(main_condition %in% codes_diabetes_no_complication | other_condition_1 %in% codes_diabetes_no_complication |
                               other_condition_2 %in% codes_diabetes_no_complication | other_condition_3 %in% codes_diabetes_no_complication |
                               other_condition_4 %in% codes_diabetes_no_complication | other_condition_5 %in% codes_diabetes_no_complication,1,0),
           CHA0080 = if_else(main_condition %in% codes_hemiplegia | other_condition_1 %in% codes_hemiplegia |
                               other_condition_2 %in% codes_hemiplegia | other_condition_3 %in% codes_hemiplegia |
                               other_condition_4 %in% codes_hemiplegia | other_condition_5 %in% codes_hemiplegia,1,0),
           CHA0090 = if_else(main_condition %in% codes_metastatic_cancer | other_condition_1 %in% codes_metastatic_cancer |
                               other_condition_2 %in% codes_metastatic_cancer | other_condition_3 %in% codes_metastatic_cancer |
                               other_condition_4 %in% codes_metastatic_cancer | other_condition_5 %in% codes_metastatic_cancer,1,0),
           CHA0100 = if_else(main_condition %in% codes_MI | other_condition_1 %in% codes_MI |
                               other_condition_2 %in% codes_MI | other_condition_3 %in% codes_MI |
                               other_condition_4 %in% codes_MI | other_condition_5 %in% codes_MI,1,0),
           CHA0110 = if_else(main_condition %in% codes_ulcer | other_condition_1 %in% codes_ulcer |
                               other_condition_2 %in% codes_ulcer | other_condition_3 %in% codes_ulcer |
                               other_condition_4 %in% codes_ulcer | other_condition_5 %in% codes_ulcer,1,0),
           CHA0120 = if_else(main_condition %in% codes_PVD | other_condition_1 %in% codes_PVD |
                               other_condition_2 %in% codes_PVD | other_condition_3 %in% codes_PVD |
                               other_condition_4 %in% codes_PVD | other_condition_5 %in% codes_PVD,1,0),
           CHA0130 = if_else(main_condition %in% codes_renal | other_condition_1 %in% codes_renal |
                               other_condition_2 %in% codes_renal | other_condition_3 %in% codes_renal |
                               other_condition_4 %in% codes_renal | other_condition_5 %in% codes_renal,1,0),
           CHA0140 = if_else(main_condition %in% codes_rheumatic | other_condition_1 %in% codes_rheumatic |
                               other_condition_2 %in% codes_rheumatic | other_condition_3 %in% codes_rheumatic |
                               other_condition_4 %in% codes_rheumatic | other_condition_5 %in% codes_rheumatic,1,0),
           CHA0150 = if_else(main_condition %in% codes_severe_liver | other_condition_1 %in% codes_severe_liver |
                               other_condition_2 %in% codes_severe_liver | other_condition_3 %in% codes_severe_liver |
                               other_condition_4 %in% codes_severe_liver | other_condition_5 %in% codes_severe_liver,1,0),
           CHA0160 = if_else(main_condition %in% codes_HIV | other_condition_1 %in% codes_HIV |
                               other_condition_2 %in% codes_HIV | other_condition_3 %in% codes_HIV |
                               other_condition_4 %in% codes_HIV | other_condition_5 %in% codes_HIV,1,0),
           CHA0170 = if_else(main_condition %in% codes_mild_liver | other_condition_1 %in% codes_mild_liver |
                               other_condition_2 %in% codes_mild_liver | other_condition_3 %in% codes_mild_liver |
                               other_condition_4 %in% codes_mild_liver | other_condition_5 %in% codes_mild_liver,1,0))
  
  # cis_marker is the variable in SMR01 that groups episodes (multiple rows) 
  # as one admission (single row).
  # Each admission will have as admission_date the admission date of the first episode.
  # Each admission will have as discharge_date the discharge date of the last episode.
  # We also compute here the Charlson score.
  # There are admissions in which the procedure appears more than once in the 
  # rows/episodes, they will be counted once (even if they are considered different)
  # procedures within the CVD.
  print("Creating one admission per patient...")
  AA_single = AA_charlson %>%
    select(anon_id,admission_date,discharge_date,cis_marker,
           gender,age_group,ethnic_group,proc,emergency_admission,CHA0010:CHA0170) %>%
    group_by(anon_id,cis_marker) %>%
    mutate(admission_date = first(admission_date),
           discharge_date = last(discharge_date),
           emergency_admission = max(emergency_admission)) %>%
    mutate_at(c("CHA0010","CHA0020","CHA0030","CHA0040","CHA0050","CHA0060","CHA0070","CHA0080",
                "CHA0090","CHA0100","CHA0110","CHA0120","CHA0130","CHA0140","CHA0150","CHA0160",
                "CHA0170"),~max(.)) %>%
    ungroup() %>%
    distinct() %>%
    distinct(anon_id,cis_marker,admission_date,discharge_date,.keep_all = T) %>%
    mutate(Charlson_score = w[1]*CHA0010 + w[2]*CHA0020 + w[3]*CHA0030 + w[4]*CHA0040 + w[5]*CHA0050 + 
             w[6]*CHA0060 + w[7]*CHA0070 + w[8]*CHA0080 + w[9]*CHA0090 + w[10]*CHA0100 + w[11]*CHA0110 + w[12]*CHA0120 +
             w[13]*CHA0130 + w[14]*CHA0140 + w[15]*CHA0150 + w[16]*CHA0160 + w[17]*CHA0170)
  
  # Recompute adm_year and adm_month since we changed admission and discharges dates
  # to match the new grouping by cis_marker.
  AA_final= AA_single %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    select(anon_id,admission_date,discharge_date,adm_year,adm_month,adm_week,
           gender,age_group,ethnic_group,proc,emergency_admission,cis_marker,
           Charlson_score) %>%
    mutate(strata = strata_group)
  return(AA_final)
}

generate_counts_proc_AS_monthly_charlson = function(con_rdb,OPCS4_code_AS,ICD10_code_AS_final,w){
  # con_rbd: connection to the database to access the SMR01 table.
  # OPCS4_code_AS: list of procedure codes to look for in the SMR01 table.
  # ICD10_code_AS_final: list of diagnosis codes to look for in the SMR01 table.
  # w: weights to compute the Charlson index.

  # Extract all the codes that belong to each one of the categories to compute
  # the Charslon score.
  Charlson_list = readxl::read_xlsx("~/CCU003_04/codelists/Aortic_aneurysm_dx_spec_250621.xlsx",sheet = 7) %>%
    filter(!is.na(CHARLSON_DESC)) %>%
    mutate(CHARLSON_DESC = if_else(CHARLSON_DESC == "Chronic pulmonary disease","Chronic pulmonary Disease",CHARLSON_DESC))
  levels(as.factor(Charlson_list$CHARLSON_DESC))
  
  codes_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cancer"),]$ICD10_CODE
  codes_cerebrovascular = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Cerebrovascular Disease"),]$ICD10_CODE
  codes_COPD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Chronic pulmonary Disease"),]$ICD10_CODE
  codes_HF = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Congestive Heart Failure"),]$ICD10_CODE
  codes_dementia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Dementia"),]$ICD10_CODE
  codes_diabetes_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes with chronic complication"),]$ICD10_CODE
  codes_diabetes_no_complication = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Diabetes without chronic complication"),]$ICD10_CODE
  codes_HIV = Charlson_list[which(Charlson_list$CHARLSON_DESC == "HIV"),]$ICD10_CODE
  codes_metastatic_cancer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Metastatic Cancer"),]$ICD10_CODE
  codes_hemiplegia = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Hemiplegia or paraplegia"),]$ICD10_CODE
  codes_mild_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Mild Liver Disease" ),]$ICD10_CODE
  codes_MI = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Myocardial Infarction" ),]$ICD10_CODE
  codes_ulcer = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peptic Ulcer Disease" ),]$ICD10_CODE
  codes_renal = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Renal Disease" ),]$ICD10_CODE
  codes_rheumatic = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Rheumatic Disease" ),]$ICD10_CODE
  codes_severe_liver = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Severe Liver Disease" ),]$ICD10_CODE
  codes_PVD = Charlson_list[which(Charlson_list$CHARLSON_DESC == "Peripheral Vascular Disease" ),]$ICD10_CODE
  
  smr01 = tbl(con_rdb, in_schema("project_2021_0102", "smr01"))
  # Extracting all the records of the procedure of interest in SMR01. We need to look at  
  # main_operation and other_operation_X columns. Problem: some of the entries
  # have 8 characters instead of the usual 4 characters (OPCS-4), which means that
  # two procedures were performed at the same time. We need to split in two those
  # 8-length-entries and check if the individual 4-codes are of interest.
  
  print("Extracting data from SMR01...")
  AS = smr01 %>%
    # Select the columns that we'll be using.
    select(anon_id,sex,age,date_of_birth,ethnicity_code,admission_date,admission_type,
           admission_reason,admission_transfer_from,admission_transfer_from_loc,main_condition,
           other_condition_1,other_condition_2,other_condition_3,other_condition_4,other_condition_5,
           significant_facility,discharge_date,discharge_type,discharge_transfer_to,
           length_of_stay,main_operation,other_operation_1,other_operation_2,other_operation_3,
           episode_marker,cis_marker) %>%
    # Only keep the records whose condition relates to the disease (to the codes)
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,
                           other_operation_3),
              #           .vars_predicate = any_vars(str_detect(.,'I71'))) %>%
              .vars_predicate = any_vars(if_else(nchar(.) <= 4,.,str_sub(.,1,4)) %in% OPCS4_code_AS |
                                           if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) %in% OPCS4_code_AS)) %>%
    # Our study analyses data from 1st of January 20215.
    filter(admission_date >= '2015-01-01') %>%
    distinct() %>%
    as.data.table() %>%
    mutate( diagnosis = if_else(main_condition %in% ICD10_code_AS_final | other_condition_1 %in% ICD10_code_AS_final | 
                                  other_condition_2 %in% ICD10_code_AS_final | other_condition_3 %in% ICD10_code_AS_final |
                                  other_condition_4 %in% ICD10_code_AS_final | other_condition_5 %in% ICD10_code_AS_final ,1,0))
  
  # For AS, extracting only the matches in the OPCS4 list is not enough. There
  # are three different definitions for an AS procedure.
  AS_1 = AS %>%
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,other_operation_3),
              .vars_predicate = any_vars((if_else(nchar(.) <= 4,.,str_sub(.,1,4)) == "X833" |
                                            if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) == "X833") & diagnosis == 1)) %>%
    mutate(strata = "Thrombo")
  AS_2 = AS %>%
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,other_operation_3),
              .vars_predicate = any_vars((if_else(nchar(.) <= 4,.,str_sub(.,1,4)) == "L354" |
                                            if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) == "L354") & diagnosis == 1)) %>%
    mutate(strata = "Thrombo")
  AS_3 = AS %>%
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,other_operation_3),
              .vars_predicate = any_vars(if_else(nchar(.) <= 4,.,str_sub(.,1,4)) %in% c("L294","L295","L314") |
                                           if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) %in% c("L294","L295","L314"))) %>%
    mutate(strata = "Carotid")
  
  OPCS_AS_O = OPCS4_code_AS[c(which(str_detect(OPCS4_code_AS,'O0')))]
  OPCS_AS_YZ = OPCS4_code_AS[c(which(str_detect(OPCS4_code_AS,'Y53')),
                               which(str_detect(OPCS4_code_AS,'Z35')))]
  AS_4 = AS %>%
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,other_operation_3),
              .vars_predicate = any_vars(if_else(nchar(.) <= 4,.,str_sub(.,1,4)) %in% OPCS_AS_O |
                                           if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) %in% OPCS_AS_O)) %>%
    filter_at(.vars = vars(main_operation, other_operation_1, other_operation_2,other_operation_3),
              .vars_predicate = any_vars(if_else(nchar(.) <= 4,.,str_sub(.,1,4)) %in% OPCS_AS_YZ |
                                           if_else(nchar(.) <= 4,.,str_sub(.,-4,-1)) %in% OPCS_AS_YZ)) %>%
    mutate(strata = "Cerebral")
  AA = rbind(AS_1,AS_2,AS_3,AS_4)
  
  # Adding adm_year, adm_month, gender, age_group and ethnic_group columns:
  print("Adding demographic groupings...")
  AA_dem = AA %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    mutate(gender = case_when(sex == 1 ~ 'Male',
                              sex == 2 ~ 'Female')) %>%
    mutate(age_group = case_when(age < 50 ~ '<50',
                                 age >= 50 & age < 60 ~ '50-59',
                                 age >= 60 & age < 70 ~ '60-69',
                                 age >= 70 & age < 80 ~ '70-79',
                                 age >= 80 ~ '80+')) %>%
    mutate(ethnic_group = case_when(ethnicity_code %in% c('1A','1B','1C','1K','1L','1Z') ~ 'White',
                                    ethnicity_code == '2A' ~ 'Mixed',
                                    ethnicity_code %in% c('3F','3G','3H','3J','3Z') ~ 'Asian',
                                    ethnicity_code %in% c('4D','4Y','5C','5D','5Y') ~ 'Black',
                                    TRUE ~ 'Other/Unknown')) %>%
    mutate(proc = rep(1,n())) %>%
    mutate(emergency_admission = if_else(admission_type %in% c(30,31,32,33,34,35,36,38,39),1,0))
  
  # Clean the ethnicity status before grouping. First: extract those patients
  # who have a different record in the ethnic_group variable.
  print("Cleaning misleading ethnicity records...")
  AA_eth_id = AA_dem %>%
    group_by(anon_id) %>%
    mutate(lvl = length(levels(as.factor(ethnic_group)))==1) %>%
    filter(lvl == F) %>%
    select(anon_id) %>%
    distinct()
  
  # Loop through all the identified patients and select the ethnicity that is not
  # Other/Unknown. In the strange case that a patient has been clasified in "White"
  # and "Black", we'll pick the first that appears (assuming that the input error
  # has been made on the later admissions).
  N = dim(AA_eth_id)[1]
  eth = character(N)
  for (i in 1:N){
    id_now = as.character(AA_eth_id[i,1])
    subset = AA_dem[which(AA_dem$anon_id==id_now),]
    lvl = levels(as.factor(subset$ethnic_group))
    if(length(lvl)!=1){
      eth[i] = subset$ethnic_group[which(subset$ethnic_group != "Other/Unknown")][1]
    }
  }
  
  # Fix the ethnic_group column so every patient has the same ethnicity across their
  # episodes.
  AA_eth_fixed = as.data.frame(cbind('anon_id'=AA_eth_id$anon_id,'eth'=eth),stringsAsFactors = F)
  AA_dem_v2 = AA_dem %>%
    left_join(AA_eth_fixed,"anon_id") %>%
    mutate(ethnic_group = if_else(is.na(eth),ethnic_group,eth)) %>%
    select(-eth)
  
  AA_charlson = AA_dem_v2 %>%
    mutate(CHA0010 = if_else(main_condition %in% codes_cancer | other_condition_1 %in% codes_cancer |
                               other_condition_2 %in% codes_cancer | other_condition_3 %in% codes_cancer |
                               other_condition_4 %in% codes_cancer | other_condition_5 %in% codes_cancer,1,0),
           CHA0020 = if_else(main_condition %in% codes_cerebrovascular | other_condition_1 %in% codes_cerebrovascular |
                               other_condition_2 %in% codes_cerebrovascular | other_condition_3 %in% codes_cerebrovascular |
                               other_condition_4 %in% codes_cerebrovascular | other_condition_5 %in% codes_cerebrovascular,1,0),
           CHA0030 = if_else(main_condition %in% codes_COPD | other_condition_1 %in% codes_COPD |
                               other_condition_2 %in% codes_COPD | other_condition_3 %in% codes_COPD |
                               other_condition_4 %in% codes_COPD | other_condition_5 %in% codes_COPD,1,0),
           CHA0040 = if_else(main_condition %in% codes_HF | other_condition_1 %in% codes_HF |
                               other_condition_2 %in% codes_HF | other_condition_3 %in% codes_HF |
                               other_condition_4 %in% codes_HF | other_condition_5 %in% codes_HF,1,0),
           CHA0050 = if_else(main_condition %in% codes_dementia | other_condition_1 %in% codes_dementia |
                               other_condition_2 %in% codes_dementia | other_condition_3 %in% codes_dementia |
                               other_condition_4 %in% codes_dementia | other_condition_5 %in% codes_dementia,1,0),
           CHA0060 = if_else(main_condition %in% codes_diabetes_complication | other_condition_1 %in% codes_diabetes_complication |
                               other_condition_2 %in% codes_diabetes_complication | other_condition_3 %in% codes_diabetes_complication |
                               other_condition_4 %in% codes_diabetes_complication | other_condition_5 %in% codes_diabetes_complication,1,0),
           CHA0070 = if_else(main_condition %in% codes_diabetes_no_complication | other_condition_1 %in% codes_diabetes_no_complication |
                               other_condition_2 %in% codes_diabetes_no_complication | other_condition_3 %in% codes_diabetes_no_complication |
                               other_condition_4 %in% codes_diabetes_no_complication | other_condition_5 %in% codes_diabetes_no_complication,1,0),
           CHA0080 = if_else(main_condition %in% codes_hemiplegia | other_condition_1 %in% codes_hemiplegia |
                               other_condition_2 %in% codes_hemiplegia | other_condition_3 %in% codes_hemiplegia |
                               other_condition_4 %in% codes_hemiplegia | other_condition_5 %in% codes_hemiplegia,1,0),
           CHA0090 = if_else(main_condition %in% codes_metastatic_cancer | other_condition_1 %in% codes_metastatic_cancer |
                               other_condition_2 %in% codes_metastatic_cancer | other_condition_3 %in% codes_metastatic_cancer |
                               other_condition_4 %in% codes_metastatic_cancer | other_condition_5 %in% codes_metastatic_cancer,1,0),
           CHA0100 = if_else(main_condition %in% codes_MI | other_condition_1 %in% codes_MI |
                               other_condition_2 %in% codes_MI | other_condition_3 %in% codes_MI |
                               other_condition_4 %in% codes_MI | other_condition_5 %in% codes_MI,1,0),
           CHA0110 = if_else(main_condition %in% codes_ulcer | other_condition_1 %in% codes_ulcer |
                               other_condition_2 %in% codes_ulcer | other_condition_3 %in% codes_ulcer |
                               other_condition_4 %in% codes_ulcer | other_condition_5 %in% codes_ulcer,1,0),
           CHA0120 = if_else(main_condition %in% codes_PVD | other_condition_1 %in% codes_PVD |
                               other_condition_2 %in% codes_PVD | other_condition_3 %in% codes_PVD |
                               other_condition_4 %in% codes_PVD | other_condition_5 %in% codes_PVD,1,0),
           CHA0130 = if_else(main_condition %in% codes_renal | other_condition_1 %in% codes_renal |
                               other_condition_2 %in% codes_renal | other_condition_3 %in% codes_renal |
                               other_condition_4 %in% codes_renal | other_condition_5 %in% codes_renal,1,0),
           CHA0140 = if_else(main_condition %in% codes_rheumatic | other_condition_1 %in% codes_rheumatic |
                               other_condition_2 %in% codes_rheumatic | other_condition_3 %in% codes_rheumatic |
                               other_condition_4 %in% codes_rheumatic | other_condition_5 %in% codes_rheumatic,1,0),
           CHA0150 = if_else(main_condition %in% codes_severe_liver | other_condition_1 %in% codes_severe_liver |
                               other_condition_2 %in% codes_severe_liver | other_condition_3 %in% codes_severe_liver |
                               other_condition_4 %in% codes_severe_liver | other_condition_5 %in% codes_severe_liver,1,0),
           CHA0160 = if_else(main_condition %in% codes_HIV | other_condition_1 %in% codes_HIV |
                               other_condition_2 %in% codes_HIV | other_condition_3 %in% codes_HIV |
                               other_condition_4 %in% codes_HIV | other_condition_5 %in% codes_HIV,1,0),
           CHA0170 = if_else(main_condition %in% codes_mild_liver | other_condition_1 %in% codes_mild_liver |
                               other_condition_2 %in% codes_mild_liver | other_condition_3 %in% codes_mild_liver |
                               other_condition_4 %in% codes_mild_liver | other_condition_5 %in% codes_mild_liver,1,0))
  
  # cis_marker is the variable in SMR01 that groups episodes (multiple rows) 
  # as one admission (single row).
  # Each admission will have as admission_date the admission date of the first episode.
  # Each admission will have as discharge_date the discharge date of the last episode.
  # We also compute here the Charlson score.
  # There are admissions in which the procedure appears more than once in the 
  # rows/episodes, they will be counted once (even if they are considered different)
  # procedures within the CVD.
  print("Creating one admission per patient...")
  AA_single = AA_charlson %>%
    select(anon_id,admission_date,discharge_date,cis_marker,
           gender,age_group,ethnic_group,proc,emergency_admission,strata,CHA0010:CHA0170) %>%
    group_by(anon_id,cis_marker) %>%
    mutate(admission_date = first(admission_date),
           discharge_date = last(discharge_date),
           emergency_admission = max(emergency_admission)) %>%
    mutate_at(c("CHA0010","CHA0020","CHA0030","CHA0040","CHA0050","CHA0060","CHA0070","CHA0080",
                "CHA0090","CHA0100","CHA0110","CHA0120","CHA0130","CHA0140","CHA0150","CHA0160",
                "CHA0170"),~max(.)) %>%
    ungroup() %>%
    distinct() %>%
    distinct(anon_id,cis_marker,admission_date,discharge_date,.keep_all = T) %>%
    mutate(Charlson_score = w[1]*CHA0010 + w[2]*CHA0020 + w[3]*CHA0030 + w[4]*CHA0040 + w[5]*CHA0050 + 
             w[6]*CHA0060 + w[7]*CHA0070 + w[8]*CHA0080 + w[9]*CHA0090 + w[10]*CHA0100 + w[11]*CHA0110 + w[12]*CHA0120 +
             w[13]*CHA0130 + w[14]*CHA0140 + w[15]*CHA0150 + w[16]*CHA0160 + w[17]*CHA0170)
  
  # Recompute adm_year and adm_month since we changed admission and discharges dates
  # to match the new grouping by cis_marker.
  AA_final= AA_single %>%
    mutate(adm_year = year(as.Date(admission_date)), adm_month = month(as.Date(admission_date)),
           adm_week = week(as.Date(admission_date))) %>%
    select(anon_id,admission_date,discharge_date,adm_year,adm_month,adm_week,
           gender,age_group,ethnic_group,proc,emergency_admission,cis_marker,strata,
           Charlson_score)
  
  return(AA_final)
}
