-- Databricks notebook source
-- MAGIC %md
-- MAGIC <h2>Extract Generator</h2>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC This workbook runs through the generation of a spec extract

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h4>Step 1 - Prep</h4>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Run both the below commands

-- COMMAND ----------

select max(DISDATE) from dars_nic_391419_j3w9t_collab.hes_apc_ext_all_years where year(DISDATE) < 2098

-- COMMAND ----------

REFRESH TABLE dars_nic_391419_j3w9t_collab.ccu003_04_base_nh

-- COMMAND ----------

select max(disdate) from dars_nic_391419_j3w9t_collab.ccu003_04_base_nh where year(DISDATE) < 2098

-- COMMAND ----------

-- MAGIC %md
-- MAGIC If the values differ, run the Prep workbook to update the base dataset so it's fully up to date

-- COMMAND ----------

-- MAGIC %python
-- MAGIC dbutils.notebook.run("./Prep", 3000)

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Now run the Charlson notebook to generate the Charlson view

-- COMMAND ----------

-- MAGIC %python
-- MAGIC dbutils.notebook.run("./Charlson", 3000)

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h4>Step 2 - Episodes</h4>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update the below cmd with the relevent selection operation or diagnosis codes & details and then run it

-- COMMAND ----------

-----------------------------------------------
-- VTE diagnosis episodes
-----------------------------------------------
-- This query returns one row per each VTE diagnosis for each episode - this means there are multiple rows per episode!!!

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_source_episodes AS

WITH extract_exploded AS
( -- need to make a CTE where the dx/op code fields are exploded into rows - then we have one row for each dx or op in each episode.
  SELECT PERSON_ID_DEID AS PERSON_ID, SUSSPELLID AS SPELL_ID, SITETRET, EPIORDER, EPIKEY, EPISTART, EPIEND, ADMIDATE, DISDATE,
  --OPS
  --posexplode(array(opertn_4_01, 	opertn_4_02, 	opertn_4_03, 	opertn_4_04, 	opertn_4_05, 	opertn_4_06, 	opertn_4_07, 	opertn_4_08, 	opertn_4_09, 	opertn_4_10, 	opertn_4_11, 	opertn_4_12, 	opertn_4_13, 	opertn_4_14, 	opertn_4_15, 	opertn_4_16, 	opertn_4_17, 	opertn_4_18, 	opertn_4_19, 	opertn_4_20, 	opertn_4_21, 	opertn_4_22, 	opertn_4_23, 	opertn_4_24)) AS (pos_code, code)
  --DIAGS
  posexplode(array(diag_01, 	diag_02, 	diag_03, 	diag_04, 	diag_05, 	diag_06, 	diag_07, 	diag_08, 	diag_09, 	diag_10, 	diag_11, 	diag_12, 	diag_13, 	diag_14, 	diag_15, 	diag_16, 	diag_17, 	diag_18, 	diag_19, 	diag_20)) AS (pos_code, code)
  FROM dars_nic_391419_j3w9t_collab.ccu003_04_base_nh
)

SELECT
*
FROM
  extract_exploded
WHERE
ADMIDATE >= '2015-01-01' AND
LEFT(code, 3) IN ('I26','I80','I81','I82')

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Run the episodes notebook

-- COMMAND ----------

-- MAGIC %md
-- MAGIC From the episodes with the condition, identfy the first episode per patient within the time window.

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_first_episodes AS-- get first episode for each person where they had the spec condition in the time window

SELECT
PERSON_ID_DEID AS PERSON_ID,
SUSSPELLID AS SPELL_ID,
EPIKEY,
CASE WHEN COALESCE(admidate, '1800-01-01') IN ('1801-01-01', '1800-01-01') THEN disdate ELSE admidate END AS Admidate,
CASE WHEN COALESCE(disdate, '1800-01-01') IN ('1801-01-01', '1800-01-01') THEN admidate ELSE disdate END AS Disdate
FROM dars_nic_391419_j3w9t_collab.ccu003_04_base_nh
WHERE epikey IN (SELECT FIRST(EPIKEY) OVER (PARTITION BY PERSON_ID ORDER BY ADMIDATE ASC, EPIORDER ASC) AS EPIKEY FROM global_temp.nh_temp_source_episodes)

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h2>Create base episodes table</h2>

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base AS

SELECT
PERSON_ID_DEID AS PERSON_ID,
SUSSPELLID AS SPELL_ID,
base.EPIKEY,
--DATEDIFF(base.Admidate, initial.Disdate) AS datediff,
initial.Disdate AS Index_Disdate,
admiage,
sex,
ethnos,
COALESCE(base.admidate, '1800-01-01') AS admidate,
COALESCE(base.disdate, '1800-01-01') AS disdate,
epistart,
epiend,
epidur,
epiorder,
admisorc,
epistat,
procodet,
sitetret,
protype,
admimeth,
lsoa11,
resgor_ons,
diag_01, 
diag_02, 
diag_03, 
diag_04, 
diag_05, 
diag_06, 
diag_07, 
diag_08, 
diag_09, 
diag_10, 
diag_11, 
diag_12, 
diag_13, 
diag_14, 
diag_15, 
diag_16, 
diag_17, 
diag_18, 
diag_19, 
diag_20,
diag_4_concat,
opertn_01,
opdate_01, 
opertn_02,
opdate_02, 
opertn_03,
opdate_03, 
opertn_04,
opdate_04, 
opertn_05,
opdate_05, 
opertn_06,
opdate_06, 
opertn_07,
opdate_07, 
opertn_08,
opdate_08, 
opertn_09,
opdate_09, 
opertn_10,
opdate_10, 
opertn_11,
opdate_11, 
opertn_12,
opdate_12, 
opertn_13,
opdate_13, 
opertn_14,
opdate_14, 
opertn_15,
opdate_15, 
opertn_16,
opdate_16, 
opertn_17,
opdate_17, 
opertn_18,
opdate_18, 
opertn_19,
opdate_19, 
opertn_20,
opdate_20, 
opertn_21,
opdate_21, 
opertn_22,
opdate_22,
opertn_23, 
opdate_23, 
opertn_24,
opdate_24,
opertn_3_concat,
opertn_4_concat, 
mainspef,
tretspef,
dismeth,
speldur,
disdest,
spelbgin,
spelend,
fde,
fae,
classpat,
IF(dismeth = 4, 1, 0) AS died
FROM dars_nic_391419_j3w9t_collab.ccu003_04_base_nh AS base INNER JOIN 
(
    SELECT DISTINCT * FROM global_temp.nh_temp_first_episodes
)
AS initial ON base.PERSON_ID_DEID = initial.PERSON_ID
WHERE
(DATEDIFF(base.Admidate, initial.Disdate) BETWEEN 0 AND 30 AND base.Admidate >= initial.Admidate AND initial.Admidate IS NOT NULL AND initial.Admidate > '1801-01-01') --capture any episode which starts within 30 days of the end of the index episode
OR base.epikey = initial.EPIKEY --capture index episode (required where index episode is longer than 1 day so not captured by the above)

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

select count(*) from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base

-- COMMAND ----------

select * from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base order by PERSON_ID, SPELL_ID, EPIKEY

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h2>Generate derived columns</h2>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Now done primarily from master notebook

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_episodes_charlson AS

WITH extract_icd_exploded AS
( -- CTE where the ICD fields are exploded into rows - one row for each diagnosis in each episode
  SELECT PERSON_ID, SPELL_ID, EPIKEY, posexplode(array(diag_01, diag_02, diag_03, diag_04, diag_05, diag_06, diag_07, diag_08, diag_09, diag_10, diag_11, diag_12, diag_13, diag_14, diag_15, diag_16, diag_17, diag_18, diag_19, diag_20)) AS (pos_icd, icd)
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base
)
  
SELECT DISTINCT --Don't double count diagnoses
  *,
  COALESCE(GROUP_SCORE, 0) AS CHARLSON_SCORE_epi,
  CASE WHEN CHARLSON_DESC = 'Cancer' THEN 1 ELSE 0 END AS CHARLSON_CANCER,
  CASE WHEN CHARLSON_DESC = 'Cerebrovascular Disease' THEN 1 ELSE 0 END AS CHARLSON_CEREBRO,
  CASE WHEN CHARLSON_DESC = 'Chronic pulmonary Disease' THEN 1 ELSE 0 END AS CHARLSON_CPD,
  CASE WHEN CHARLSON_DESC = 'Congestive Heart Failure' THEN 1 ELSE 0 END AS CHARLSON_CONGEST_HEART_FAIL,
  CASE WHEN CHARLSON_DESC = 'Dementia' THEN 1 ELSE 0 END AS CHARLSON_DEMENTIA,
  CASE WHEN CHARLSON_DESC = 'Diabetes with chronic complication' THEN 1 ELSE 0 END AS CHARLSON_DIABETES_CHRON,
  CASE WHEN CHARLSON_DESC = 'Diabetes without chronic complication' THEN 1 ELSE 0 END AS CHARLSON_DIABETES_WO_CHRON,
  CASE WHEN CHARLSON_DESC = 'Hemiplegia or paraplegia' THEN 1 ELSE 0 END AS CHARLSON_HEMIPLEGIA_PARAPLEGIA,
  CASE WHEN CHARLSON_DESC = 'Metastatic Cancer' THEN 1 ELSE 0 END  AS CHARLSON_METASTATIC_CANCER,
  CASE WHEN CHARLSON_DESC = 'Myocardial Infarction' THEN 1 ELSE 0 END AS CHARLSON_MYOCARD_INFARCT,
  CASE WHEN CHARLSON_DESC = 'Peptic Ulcer Disease' THEN 1 ELSE 0 END AS CHARLSON_PEPTIC_ULCER,
  CASE WHEN CHARLSON_DESC = 'Peripheral Vascular Disease' THEN 1 ELSE 0 END AS CHARLSON_PERIPH_VASC,
  CASE WHEN CHARLSON_DESC = 'Renal Disease' THEN 1 ELSE 0 END AS CHARLSON_RENAL,
  CASE WHEN CHARLSON_DESC = 'Rheumatic Disease' THEN 1 ELSE 0 END AS CHARLSON_RHEUMATIC,
  CASE WHEN CHARLSON_DESC = 'Severe Liver Disease' THEN 1 ELSE 0 END AS CHARLSON_SEVERE_LIVER,
  CASE WHEN CHARLSON_DESC = 'HIV' THEN 1 ELSE 0 END AS CHARLSON_HIV,
  CASE WHEN CHARLSON_DESC = 'Mild Liver Disease' THEN 1 ELSE 0 END AS CHARLSON_MILD_LIVER
FROM extract_icd_exploded
LEFT JOIN global_temp.nh_charlson_lookup AS charlson ON extract_icd_exploded.icd = charlson.ICD10_CODE

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update and run the below command to set the diagnosis grouping hierarchy<br/>
-- MAGIC Note that the procedure assumes a 1,2,3...,0 pattern to the hierarchy<br/>
-- MAGIC Also add/remove any additional grouped diagnosis fields as required

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_episodes_diagcols AS

WITH extract_icd_exploded AS
( -- CTE where the ICD fields are exploded into rows - one row for each diagnosis in each episode
  SELECT PERSON_ID, SPELL_ID, EPIKEY, posexplode(array(diag_01, diag_02, diag_03, diag_04, diag_05, diag_06, diag_07, diag_08, diag_09, diag_10, diag_11, diag_12, diag_13, diag_14, diag_15, diag_16, diag_17, diag_18, diag_19, diag_20)) AS (pos_icd, icd)
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base
)
  
SELECT 
PERSON_ID, SPELL_ID, EPIKEY,
CASE WHEN LEFT(icd, 4) IN ('B342','B972','U409','U071','U072') THEN 1 ELSE 0 END AS POSS_COVID_epi,
CASE
  WHEN LEFT(icd, 3) IN ('I26') THEN 1
  WHEN LEFT(icd, 3) IN ('I81') THEN 2
  WHEN LEFT(icd, 3) IN ('I82') THEN 3
  WHEN LEFT(icd, 3) IN ('I80') THEN 4
  ELSE 0
  END AS GROUPED_DIAG_epi,
CASE
  WHEN LEFT(icd, 3) IN ('I26') THEN pos_icd
  WHEN LEFT(icd, 3) IN ('I81') THEN pos_icd
  WHEN LEFT(icd, 3) IN ('I82') THEN pos_icd
  WHEN LEFT(icd, 3) IN ('I80') THEN pos_icd
  ELSE 0
  END AS GROUPED_DIAG_epi_pos
--Add any other grouped diagnostic fields here  

FROM extract_icd_exploded

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update and run the below command to set the procedure grouping hierarchy<br/>
-- MAGIC Note that the procedure assumes a ...3,2,1,0 pattern to the hierarchy<br/>
-- MAGIC Also add/remove any additional grouped procedure fields as required

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_episodes_opercols AS

WITH extract_opcs_exploded AS
(
--Lateral views to incorporate 2 posexplodes. Where clause to link them
  SELECT PERSON_ID, SPELL_ID, EPIKEY, admidate, pos_op, op, pos_op_date, op_date
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base
  LATERAL VIEW posexplode(array(opertn_01, opertn_02, opertn_03, opertn_04, opertn_05, opertn_06, opertn_07, opertn_08, opertn_09, opertn_10, opertn_11, opertn_12, opertn_13, opertn_14, opertn_15, opertn_16, opertn_17, opertn_18, opertn_19, opertn_20, opertn_21, opertn_22, opertn_23, opertn_24)) ops AS pos_op, op
  LATERAL VIEW posexplode(array(opdate_01, opdate_02, opdate_03, opdate_04, opdate_05, opdate_06, opdate_07, opdate_08, opdate_09, opdate_10, opdate_11, opdate_12, opdate_13, opdate_14, opdate_15, opdate_16, opdate_17, opdate_18, opdate_19, opdate_20, opdate_21, opdate_22, opdate_23, opdate_24)) dates AS pos_op_date, op_date
  WHERE ops.pos_op = dates.pos_op_date
)
  
SELECT 
PERSON_ID, SPELL_ID, EPIKEY,
CASE 
     WHEN LEFT(op, 3) = 'L13' THEN 2
     WHEN LEFT(op, 4) IN ('L124','L125') THEN 1 
     ELSE 0 
     END AS GROUPED_PROC_epi
--Add any other grouped procedural fields here  

FROM extract_opcs_exploded

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h4>Step 3 - Spells</h4>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Run the spells notebook

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h2>Generate set of spells</h2>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC First create final episodes table

-- COMMAND ----------

REFRESH TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 AS

  SELECT
  base.PERSON_ID,
  base.SPELL_ID,
  base.EPIKEY,
  base.Index_Disdate,
  base.admiage,
  base.sex,
  base.ethnos,
  base.admidate,
  base.disdate,
  base.epistart,
  base.epiend,
  base.epidur,
  base.epiorder,
  base.admisorc,
  base.epistat,
  base.procodet,
  base.sitetret,
  base.protype,
  base.admimeth,
  base.lsoa11,
  base.resgor_ons,
  base.diag_01, 
  base.diag_02, 
  base.diag_03, 
  base.diag_04, 
  base.diag_05, 
  base.diag_06, 
  base.diag_07, 
  base.diag_08, 
  base.diag_09, 
  base.diag_10, 
  base.diag_11, 
  base.diag_12, 
  base.diag_13, 
  base.diag_14, 
  base.diag_15, 
  base.diag_16, 
  base.diag_17, 
  base.diag_18, 
  base.diag_19, 
  base.diag_20,
  base.diag_4_concat,
  base.opertn_01,
  base.opdate_01, 
  base.opertn_02,
  base.opdate_02, 
  base.opertn_03,
  base.opdate_03, 
  base.opertn_04,
  base.opdate_04, 
  base.opertn_05,
  base.opdate_05, 
  base.opertn_06,
  base.opdate_06, 
  base.opertn_07,
  base.opdate_07, 
  base.opertn_08,
  base.opdate_08, 
  base.opertn_09,
  base.opdate_09, 
  base.opertn_10,
  base.opdate_10, 
  base.opertn_11,
  base.opdate_11, 
  base.opertn_12,
  base.opdate_12, 
  base.opertn_13,
  base.opdate_13, 
  base.opertn_14,
  base.opdate_14, 
  base.opertn_15,
  base.opdate_15, 
  base.opertn_16,
  base.opdate_16, 
  base.opertn_17,
  base.opdate_17, 
  base.opertn_18,
  base.opdate_18, 
  base.opertn_19,
  base.opdate_19, 
  base.opertn_20,
  base.opdate_20, 
  base.opertn_21,
  base.opdate_21, 
  base.opertn_22,
  base.opdate_22,
  base.opertn_23, 
  base.opdate_23, 
  base.opertn_24,
  base.opdate_24,
  base.opertn_3_concat,
  base.opertn_4_concat, 
  base.mainspef,
  base.tretspef,
  base.dismeth,
  base.speldur,
  base.disdest,
  base.spelbgin,
  base.spelend,
  base.fde,
  base.fae,
  base.classpat,
  base.died,
  COALESCE(derived_dx.Grouped_diag_epi, 0) AS Grouped_diag,
  COALESCE(derived_dx.Grouped_diag_epi_pos, 0) AS Grouped_diag_pos,
  COALESCE(derived_op.Grouped_proc_epi, 0) AS Grouped_proc
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_episodes_base AS base
  --derived grouped dx
  LEFT OUTER JOIN  
  (
    SELECT DISTINCT a.EPIKEY, MIN(a.Grouped_diag_epi_pos + 1) AS Grouped_diag_epi_pos, b.Grouped_diag_epi
    FROM global_temp.nh_temp_episodes_diagcols AS a INNER JOIN
    (--first find the min hierarchical code. Once found, find the minimum position for it
      SELECT EPIKEY, MIN(GROUPED_DIAG_epi) AS Grouped_diag_epi
      FROM global_temp.nh_temp_episodes_diagcols WHERE GROUPED_DIAG_epi > 0 GROUP BY EPIKEY
    ) AS b ON a.EPIKEY = b.EPIKEY AND a.Grouped_diag_epi = b.Grouped_diag_epi
    GROUP BY a.EPIKEY, b.Grouped_diag_epi
  ) AS derived_dx ON base.EPIKEY = derived_dx.EPIKEY
  --derived grouped proc
  LEFT OUTER JOIN
  (
    SELECT EPIKEY, MAX(GROUPED_PROC_epi) AS Grouped_proc_epi
    FROM global_temp.nh_temp_episodes_opercols WHERE GROUPED_PROC_epi > 0 GROUP BY EPIKEY
  ) AS derived_op ON base.EPIKEY = derived_op.EPIKEY

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

select count(*) from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Now build spells

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Take important fields from first and last episodes per spell. Derive hierarchical grouped dx and op fields (dx anywhere, discharge and primary, op anywhere)

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_spells_base9

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_spells_base9 AS

  SELECT DISTINCT
  base_spells.*,
  admission_episodes.Admidate, admission_episodes.Admimeth, admission_episodes.Procodet, admission_episodes.Admiage, admission_episodes.Sex, admission_episodes.Ethnos, admission_episodes.Resgor_ons,
  discharge_episodes.Disdate, discharge_episodes.Disdest, discharge_episodes.Dismeth,
  COALESCE(any_derived_dx.Grouped_diag_epi, 0) AS Grouped_diag_any, COALESCE(any_derived_dx.Grouped_diag_epi_pos, 0) AS Grouped_diag_any_pos,
  COALESCE(discharge_derived_dx.Grouped_diag_epi, 0) AS Grouped_diag_dis, COALESCE(discharge_derived_dx.Grouped_diag_epi_pos, 0) AS Grouped_diag_dis_pos,
  COALESCE(primary_derived_dx.Grouped_diag_epi, 0) AS Grouped_diag_pri, discharge_episodes.diag_01 AS diag_pri,
  COALESCE(any_derived_op.Grouped_proc_epi, 0) AS Grouped_proc_any
  FROM
  (--base spells
    SELECT PERSON_ID, SPELL_ID, MIN(Epikey) AS AdmissionEpikey, MAX(Epikey) AS DischargeEpikey, COUNT(*) AS EpisodeCount FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 GROUP BY PERSON_ID, SPELL_ID
  ) AS base_spells
  --admission fields
  INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 AS admission_episodes ON base_spells.AdmissionEpikey = admission_episodes.Epikey
  --discharge fields
  INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 AS discharge_episodes ON base_spells.DischargeEpikey = discharge_episodes.Epikey
  --derived grouped 'any' dx
  LEFT OUTER JOIN  
  (
    SELECT DISTINCT a.PERSON_ID, a.SPELL_ID, MIN(a.Grouped_diag_pos) AS Grouped_diag_epi_pos, b.Grouped_diag_epi
    FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 AS a INNER JOIN
    (--first find the min hierarchical code. Once found, find the minimum position for it
      SELECT PERSON_ID, SPELL_ID, MIN(Grouped_diag) AS Grouped_diag_epi
      FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 WHERE Grouped_diag > 0 GROUP BY PERSON_ID, SPELL_ID
    ) AS b ON a.PERSON_ID = b.PERSON_ID AND a.SPELL_ID = b.SPELL_ID AND a.Grouped_diag = b.Grouped_diag_epi
    GROUP BY a.PERSON_ID, a.SPELL_ID, b.Grouped_diag_epi
  ) AS any_derived_dx ON base_spells.PERSON_ID = any_derived_dx.PERSON_ID AND base_spells.SPELL_ID = any_derived_dx.SPELL_ID
  --derived grouped discharge dx
  LEFT OUTER JOIN
  (
    SELECT DISTINCT a.EPIKEY, MIN(a.Grouped_diag_pos + 1) AS Grouped_diag_epi_pos, b.Grouped_diag_epi
    FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 AS a INNER JOIN
    (--first find the min hierarchical code. Once found, find the minimum position for it
      SELECT EPIKEY, MIN(Grouped_diag) AS Grouped_diag_epi
      FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 WHERE Grouped_diag > 0 GROUP BY EPIKEY
    ) AS b ON a.EPIKEY = b.EPIKEY AND a.Grouped_diag = b.Grouped_diag_epi
    GROUP BY a.EPIKEY, b.Grouped_diag_epi
  ) AS discharge_derived_dx ON base_spells.DischargeEpikey = discharge_derived_dx.EPIKEY
  --derived grouped primary dx
  LEFT OUTER JOIN
  (--find the min hierarchical code for position = 1
    SELECT EPIKEY, MIN(Grouped_diag) AS Grouped_diag_epi
    FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 WHERE Grouped_diag > 0 AND Grouped_diag_pos = 1 GROUP BY EPIKEY
  ) AS primary_derived_dx ON base_spells.DischargeEpikey = primary_derived_dx.EPIKEY
  --derived grouped 'any' proc
  LEFT OUTER JOIN
  (
    SELECT PERSON_ID, SPELL_ID, MAX(Grouped_proc) AS Grouped_proc_epi
    FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 WHERE Grouped_proc > 0 GROUP BY PERSON_ID, SPELL_ID
  ) AS any_derived_op ON base_spells.PERSON_ID = any_derived_op.PERSON_ID AND base_spells.SPELL_ID = any_derived_op.SPELL_ID

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_spells_base9 OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

refresh table dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1

-- COMMAND ----------

refresh table dars_nic_391419_j3w9t_collab.ccu003_nh_temp_spells_base9

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update and run the below cmd to add/remove to/from the spells any additional grouped dx/op fields and Charlson conditions for score

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 AS

SELECT
--Main fields
base.PERSON_ID, base.SPELL_ID, base.AdmissionEpikey, base.DischargeEpikey, base.EpisodeCount,
base.Admidate, base.Admimeth, base.Procodet, base.Admiage, base.Sex, base.Ethnos, base.Resgor_ons, base.Disdate, base.Disdest, base.Dismeth,
DATEDIFF(base.Disdate, base.Admidate) AS SPELL_DUR,
--Grouped dx/op fields
base.Grouped_diag_any AS GROUPED_DIAGNOSIS_ANY, base.Grouped_diag_any_pos AS GROUPED_DIAGNOSIS_ANY_POSITION,
base.Grouped_diag_dis AS GROUPED_DIAGNOSIS_DISCHARGE, base.Grouped_diag_dis_pos AS GROUPED_DIAGNOSIS_DISCHARGE_POSITION,
base.Grouped_diag_pri AS GROUPED_DIAGNOSIS_PRIMARY, base.Diag_pri AS PRIMARY_DISCHARGE_DIAGNOSIS_CODE,
base.Grouped_proc_any AS GROUPED_PROCEDURE,
--Grouped cov / death
COALESCE(Poss_covid.Poss_covid_epi, 0) AS POSS_COVID,
COALESCE(Died.Died, 0) AS DIED_DURING_ADMISSION,


--Charlson
(
  Charlson.CHARLSON_CANCER*1+
  Charlson.CHARLSON_CEREBRO*1+
  Charlson.CHARLSON_CPD*1+
  Charlson.CHARLSON_CONGEST_HEART_FAIL*1+
  Charlson.CHARLSON_DEMENTIA*1+
  Charlson.CHARLSON_DIABETES_CHRON*2+
  Charlson.CHARLSON_DIABETES_WO_CHRON*1+
  Charlson.CHARLSON_HEMIPLEGIA_PARAPLEGIA*2+
  Charlson.CHARLSON_METASTATIC_CANCER*1+
  Charlson.CHARLSON_MYOCARD_INFARCT*1+ 
  Charlson.CHARLSON_PEPTIC_ULCER*1+
  Charlson.CHARLSON_PERIPH_VASC*1+
  Charlson.CHARLSON_RENAL*2+
  Charlson.CHARLSON_RHEUMATIC*1+
  Charlson.CHARLSON_SEVERE_LIVER*3+
  Charlson.CHARLSON_HIV*6+
  Charlson.CHARLSON_MILD_LIVER*1
) AS CHARLSON_SCORE,
Charlson.CHARLSON_CANCER,
Charlson.CHARLSON_CEREBRO,
Charlson.CHARLSON_CPD,
Charlson.CHARLSON_CONGEST_HEART_FAIL,
Charlson.CHARLSON_DEMENTIA,
Charlson.CHARLSON_DIABETES_CHRON,
Charlson.CHARLSON_DIABETES_WO_CHRON,
Charlson.CHARLSON_HEMIPLEGIA_PARAPLEGIA,
Charlson.CHARLSON_METASTATIC_CANCER,
Charlson.CHARLSON_MYOCARD_INFARCT,
Charlson.CHARLSON_PEPTIC_ULCER,
Charlson.CHARLSON_PERIPH_VASC,
Charlson.CHARLSON_RENAL,
Charlson.CHARLSON_RHEUMATIC,
Charlson.CHARLSON_SEVERE_LIVER,
Charlson.CHARLSON_HIV,
Charlson.CHARLSON_MILD_LIVER

FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_spells_base9 AS base 

--Grouped cov / death
LEFT OUTER JOIN
(--derived columns episodes table for covid
  SELECT PERSON_ID, SPELL_ID, MAX(POSS_COVID_epi) AS Poss_covid_epi
  FROM global_temp.nh_temp_episodes_diagcols WHERE POSS_COVID_epi > 0 GROUP BY PERSON_ID, SPELL_ID
) AS Poss_covid ON base.PERSON_ID = Poss_covid.PERSON_ID AND base.SPELL_ID = Poss_covid.SPELL_ID
LEFT OUTER JOIN
(--base episodes table for died
  SELECT PERSON_ID, SPELL_ID, MAX(died) AS Died
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 WHERE died > 0 GROUP BY PERSON_ID, SPELL_ID
) AS Died ON base.PERSON_ID = Died.PERSON_ID AND base.SPELL_ID = Died.SPELL_ID

--Other grouped dx/op fields - add or remove as applicable


--Charlson
LEFT OUTER JOIN
(
  SELECT 
    PERSON_ID,
    SPELL_ID,     
    MAX(CHARLSON_CANCER) AS CHARLSON_CANCER,
    MAX(CHARLSON_CEREBRO) AS CHARLSON_CEREBRO,
    MAX(CHARLSON_CPD) AS CHARLSON_CPD,
    MAX(CHARLSON_CONGEST_HEART_FAIL) AS CHARLSON_CONGEST_HEART_FAIL,
    MAX(CHARLSON_DEMENTIA) AS CHARLSON_DEMENTIA,
    MAX(CHARLSON_DIABETES_CHRON) AS CHARLSON_DIABETES_CHRON,
    MAX(CHARLSON_DIABETES_WO_CHRON) AS CHARLSON_DIABETES_WO_CHRON,
    MAX(CHARLSON_HEMIPLEGIA_PARAPLEGIA) AS CHARLSON_HEMIPLEGIA_PARAPLEGIA,
    MAX(CHARLSON_METASTATIC_CANCER) AS CHARLSON_METASTATIC_CANCER,
    MAX(CHARLSON_MYOCARD_INFARCT) AS CHARLSON_MYOCARD_INFARCT,
    MAX(CHARLSON_PEPTIC_ULCER) AS CHARLSON_PEPTIC_ULCER,
    MAX(CHARLSON_PERIPH_VASC) AS CHARLSON_PERIPH_VASC,
    MAX(CHARLSON_RENAL) AS CHARLSON_RENAL,
    MAX(CHARLSON_RHEUMATIC) AS CHARLSON_RHEUMATIC,
    MAX(CHARLSON_SEVERE_LIVER) AS CHARLSON_SEVERE_LIVER,
    MAX(CHARLSON_HIV) AS CHARLSON_HIV,
    MAX(CHARLSON_MILD_LIVER) AS CHARLSON_MILD_LIVER
  FROM global_temp.nh_temp_episodes_charlson
  GROUP BY PERSON_ID, SPELL_ID
) AS Charlson ON base.PERSON_ID = Charlson.PERSON_ID AND base.SPELL_ID = Charlson.SPELL_ID


-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h4>Step 4 - Superspells</h4>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Run the superspells notebook

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h2>Generate superspells</h2>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC First, create a base dataset containing all pairs of records that contain a valid superspell relationship<br/>
-- MAGIC For superspells that consist of several spells there will be several rows which will form a chain

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_superspells_base AS

WITH cte AS
(SELECT PERSON_ID, SPELL_ID, Admidate, Disdate, Disdest, Dismeth, SPELL_ID, Admidate, Disdate, Admimeth, Dismeth, PRIMARY_DISCHARGE_DIAGNOSIS_CODE, ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY Admidate, Disdate, SPELL_ID) AS RowNo FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 WHERE SPELL_ID IS NOT NULL)

SELECT TransOut.PERSON_ID, TransOut.SPELL_ID AS Out_SPELL_ID, TransOut.Admidate AS Out_Admidate, TransOut.Disdate AS Out_Disdate, TransOut.Disdest AS Out_Disdest, TransOut.Dismeth AS Out_Dismeth, TransOut.PRIMARY_DISCHARGE_DIAGNOSIS_CODE AS Out_Primary_dx, TransOut.RowNo AS Out_RowNo, TransIn.SPELL_ID AS In_SPELL_ID, TransIn.Admidate AS In_Admidate, TransIn.Disdate AS In_Disdate, TransIn.Admimeth AS In_Admimeth, TransIn.Dismeth AS In_Dismeth, TransIn.PRIMARY_DISCHARGE_DIAGNOSIS_CODE AS In_Primary_dx, TransIn.RowNo AS In_RowNo FROM
(
  SELECT * FROM cte WHERE Disdest IN (49, 50, 51, 52, 53, 87)
)  AS TransOut
INNER JOIN
(
  SELECT * FROM cte WHERE Admimeth IN ('2B', '81')
)  AS TransIn
ON TransOut.PERSON_ID = TransIn.PERSON_ID AND TransOut.RowNo = TransIn.RowNo - 1 AND DATEDIFF(TransOut.Disdate, TransIn.Admidate) BETWEEN 0 AND 1
ORDER BY TransOut.PERSON_ID, TransOut.SPELL_ID

-- COMMAND ----------

REFRESH TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base AS

SELECT *, ROW_NUMBER() OVER (PARTITION BY PERSON_ID ORDER BY Out_RowNo) AS SpellOrder FROM global_temp.nh_temp_superspells_base

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

select * from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base ORDER BY PERSON_ID, SpellOrder

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Now build the superspells lookup from the base table.<br/>
-- MAGIC Recursive queries aren't possible in Spark SQL so we'll have to find another way or do it manually

-- COMMAND ----------

-- MAGIC %md
-- MAGIC First, build table of initial spells

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS

SELECT PERSON_ID, Out_Spell_ID AS Superspell_ID, Out_Spell_ID AS Spell_ID, Out_Disdate AS Disdate, Out_Dismeth AS Dismeth, Out_Primary_dx AS Primary_dx, 1 AS SpellNo
FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base WHERE SpellOrder = 1

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

-- MAGIC %md
-- MAGIC The below is in python as it requires an iterative process to build up the superspells and spark SQL doesn't have any concept of recursion

-- COMMAND ----------

-- MAGIC %py
-- MAGIC 
-- MAGIC #df1 = spark.sql(f"SELECT * FROM dars_nic_391419_j3w9t_collab.ccu003_nh_acs_superspells_base")
-- MAGIC #print(df1.count())
-- MAGIC 
-- MAGIC #df2 = spark.sql(f"SELECT * FROM dars_nic_391419_j3w9t_collab.ccu003_nh_acs_superspells_lookup")
-- MAGIC #print(df2.count())
-- MAGIC 
-- MAGIC qry = """SELECT lookup.PERSON_ID, lookup.Superspell_ID, base.In_Spell_ID AS Spell_ID, base.In_Disdate AS Disdate, base.In_Dismeth as Dismeth, base.In_Primary_dx as Primary_dx, base.SpellOrder + 1 AS SpellNo 
-- MAGIC FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_base AS base INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS lookup ON base.Out_Spell_ID = lookup.Spell_ID AND base.PERSON_ID = lookup.Person_ID
-- MAGIC WHERE base.In_Spell_ID NOT IN (SELECT Spell_ID FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup WHERE Person_ID = lookup.PERSON_ID AND Superspell_ID = lookup.Superspell_ID)"""
-- MAGIC 
-- MAGIC rowcount = 1
-- MAGIC loopcount = 1
-- MAGIC 
-- MAGIC while rowcount > 0:
-- MAGIC   loopcount += 1
-- MAGIC   df = spark.sql(qry)  
-- MAGIC   rowcount = df.count()
-- MAGIC   print(f"Superspells with " + f'{loopcount}' + " or more spells: " + f'{df.count()}')
-- MAGIC   df.write.insertInto('dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup', overwrite = False)

-- COMMAND ----------

SELECT * FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup ORDER BY Person_ID, Superspell_ID, SpellNo

-- COMMAND ----------

select person_ID, superspell_ID, count(superspell_ID) from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup group by person_ID, superspell_ID order by count(superspell_ID) desc, person_id asc

-- COMMAND ----------

select spellcount, count(spellcount) from
(select person_ID, superspell_ID, count(superspell_ID) as spellcount from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup group by person_ID, superspell_ID)
group by spellcount
order by count(spellcount) asc

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Get items from admission spell (most items)

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_superspells_admission AS

SELECT base.Person_ID, base.Superspell_ID, spells.Spell_ID, spells.Procodet, spells.Admidate, spells.Admiage, spells.Sex, Spells.Ethnos, Spells.Resgor_ons, spells.Admimeth FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base
INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 spells on base.Person_ID = spells.Person_ID and base.Spell_ID = spells.Spell_ID
WHERE base.SpellNo = 1

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Get data items from discharge spell: Disdate and Dismeth. These have been brought through throughout the creation of the superspells so easily to hand<br/>
-- MAGIC For final items: SPELL_DUR, DISDATE, COMPLETED_SPELL_CIPS, possibly DIED_DURING_ADMISSION

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_superspells_discharge AS --maybe not needed

SELECT base.* FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base
INNER JOIN
(
  SELECT Person_ID, Superspell_ID AS Spell_ID, MAX(SpellNo) AS LastSpellNo FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup GROUP BY Person_ID, Superspell_ID
) AS dis
ON base.Person_ID = dis.Person_ID AND base.Superspell_ID = dis.Spell_ID AND base.SpellNo = dis.LastSpellNo

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Derive superspell-based DX columns

-- COMMAND ----------

CREATE OR REPLACE GLOBAL TEMP VIEW nh_temp_superspells_derived_groups AS

WITH cte AS
(  
  SELECT
  base.Person_ID, base.Superspell_ID,
  spells.GROUPED_DIAGNOSIS_ANY, spells.GROUPED_DIAGNOSIS_ANY_POSITION,
  spells.GROUPED_DIAGNOSIS_DISCHARGE, spells.GROUPED_DIAGNOSIS_DISCHARGE_POSITION,
  spells.GROUPED_DIAGNOSIS_PRIMARY, spells.PRIMARY_DISCHARGE_DIAGNOSIS_CODE,
  spells.GROUPED_PROCEDURE
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 AS spells --to provide dx info
  INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base --to provide superspell info
  ON base.Person_ID = spells.Person_ID AND base.Spell_ID = spells.Spell_ID
)

SELECT DISTINCT base.Person_ID, base.Superspell_ID,
dx_any.GROUPED_DIAGNOSIS_ANY, dx_any.GROUPED_DIAGNOSIS_ANY_POSITION,
dx_dis.GROUPED_DIAGNOSIS_DISCHARGE, dx_dis.GROUPED_DIAGNOSIS_DISCHARGE_POSITION,
dx_pri.GROUPED_DIAGNOSIS_PRIMARY,
op_any.GROUPED_PROCEDURE
FROM
--Main
cte AS base
--Grouped any
INNER JOIN
(
  SELECT 
    cte_any_pos.Person_ID,
    cte_any_pos.Superspell_ID,
    dx_any.GROUPED_DIAGNOSIS_ANY,
    MIN(cte_any_pos.GROUPED_DIAGNOSIS_ANY_POSITION) AS GROUPED_DIAGNOSIS_ANY_POSITION --take position closest to main (i.e. lowest)
  FROM cte AS cte_any_pos
  INNER JOIN
  (
    SELECT 
      Person_ID,
      Superspell_ID,
      MIN(GROUPED_DIAGNOSIS_ANY) AS GROUPED_DIAGNOSIS_ANY --take MIN due to order of hierarchy
    FROM cte
    WHERE GROUPED_DIAGNOSIS_ANY > 0
    GROUP BY Person_ID, Superspell_ID
  ) AS dx_any
  ON dx_any.Person_ID = cte_any_pos.Person_ID AND dx_any.Superspell_ID = cte_any_pos.Superspell_ID AND dx_any.GROUPED_DIAGNOSIS_ANY = cte_any_pos.GROUPED_DIAGNOSIS_ANY
  GROUP BY cte_any_pos.Person_ID, cte_any_pos.Superspell_ID, dx_any.GROUPED_DIAGNOSIS_ANY
) AS dx_any ON base.Person_ID = dx_any.Person_ID AND base.Superspell_ID = dx_any.Superspell_ID
--Grouped discharge
INNER JOIN
(
  SELECT 
    cte_dis_pos.Person_ID,
    cte_dis_pos.Superspell_ID,
    dx_dis.GROUPED_DIAGNOSIS_DISCHARGE,
    MIN(cte_dis_pos.GROUPED_DIAGNOSIS_DISCHARGE_POSITION) AS GROUPED_DIAGNOSIS_DISCHARGE_POSITION --take position closest to main (i.e. lowest)
  FROM cte AS cte_dis_pos
  INNER JOIN
  (
    SELECT 
      Person_ID,
      Superspell_ID,
      MIN(GROUPED_DIAGNOSIS_DISCHARGE) AS GROUPED_DIAGNOSIS_DISCHARGE --take MIN due to order of hierarchy
    FROM cte
    WHERE GROUPED_DIAGNOSIS_DISCHARGE > 0
    GROUP BY Person_ID, Superspell_ID
  ) AS dx_dis
  ON dx_dis.Person_ID = cte_dis_pos.Person_ID AND dx_dis.Superspell_ID = cte_dis_pos.Superspell_ID AND dx_dis.GROUPED_DIAGNOSIS_DISCHARGE = cte_dis_pos.GROUPED_DIAGNOSIS_DISCHARGE
  GROUP BY cte_dis_pos.Person_ID, cte_dis_pos.Superspell_ID, dx_dis.GROUPED_DIAGNOSIS_DISCHARGE
) AS dx_dis ON base.Person_ID = dx_dis.Person_ID AND base.Superspell_ID = dx_dis.Superspell_ID
--Grouped primary
INNER JOIN
(
  SELECT 
    Person_ID,
    Superspell_ID,
    MIN(GROUPED_DIAGNOSIS_PRIMARY) AS GROUPED_DIAGNOSIS_PRIMARY --take MIN due to order of hierarchy
  FROM cte AS cte_pri
  WHERE GROUPED_DIAGNOSIS_PRIMARY > 0
  GROUP BY Person_ID, Superspell_ID
) AS dx_pri ON base.Person_ID = dx_pri.Person_ID AND base.Superspell_ID = dx_pri.Superspell_ID
--Grouped primary
INNER JOIN
(
  SELECT 
    Person_ID,
    Superspell_ID,
    MAX(GROUPED_PROCEDURE) AS GROUPED_PROCEDURE --take MAX due to order of hierarchy
  FROM cte AS cte_op
  WHERE GROUPED_PROCEDURE > 0
  GROUP BY Person_ID, Superspell_ID
) AS op_any ON base.Person_ID = op_any.Person_ID AND base.Superspell_ID = op_any.Superspell_ID

-- COMMAND ----------

select * from dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_episodes1 where person_id = '8G9W3O906IIDB2L' order by spell_id, epiorder

-- COMMAND ----------

REFRESH TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1

-- COMMAND ----------

REFRESH TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update and run the below cmd to add/remove to/from the superspells any additional grouped dx/op fields and Charlson conditions for score

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_superspells 

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_superspells AS

SELECT
--Standard fields
admi.*,
dis.Disdate,
dis.Dismeth,
DATEDIFF(dis.Disdate, admi.Admidate) AS SPELL_DUR,
--Grouped dx/op
COALESCE(derived_groups.GROUPED_DIAGNOSIS_ANY, 0) AS GROUPED_DIAGNOSIS_ANY,
COALESCE(derived_groups.GROUPED_DIAGNOSIS_ANY_POSITION, 0) AS GROUPED_DIAGNOSIS_ANY_POSITION,
COALESCE(derived_groups.GROUPED_DIAGNOSIS_DISCHARGE, 0) AS GROUPED_DIAGNOSIS_DISCHARGE,
COALESCE(derived_groups.GROUPED_DIAGNOSIS_DISCHARGE_POSITION, 0) AS GROUPED_DIAGNOSIS_DISCHARGE_POSITION,
COALESCE(derived_groups.GROUPED_DIAGNOSIS_PRIMARY, 0) AS GROUPED_DIAGNOSIS_PRIMARY,
dis.Primary_dx AS PRIMARY_DISCHARGE_DIAGNOSIS_CODE,
COALESCE(derived_groups.GROUPED_PROCEDURE, 0) AS GROUPED_PROCEDURE,
--Grouped cov / death
POSS_COVID,
DIED_DURING_ADMISSION,
--Additional grouped dx/op fields - add or remove as applicable

--Charlson
(
  CHARLSON_CANCER*1+
  CHARLSON_CEREBRO*1+
  CHARLSON_CPD*1+
  CHARLSON_CONGEST_HEART_FAIL*1+
  CHARLSON_DEMENTIA*1+
  CHARLSON_DIABETES_CHRON*2+
  CHARLSON_DIABETES_WO_CHRON*1+
  CHARLSON_HEMIPLEGIA_PARAPLEGIA*2+
  CHARLSON_METASTATIC_CANCER*1+
  --Charlton.CHARLSON_MYOCARD_INFARCT*1+ --removed for this spec
  CHARLSON_PEPTIC_ULCER*1+
  CHARLSON_PERIPH_VASC*1+
  CHARLSON_RENAL*2+
  CHARLSON_RHEUMATIC*1+
  CHARLSON_SEVERE_LIVER*3+
  CHARLSON_HIV*6+
  CHARLSON_MILD_LIVER*1
) AS CHARLSON_SCORE,
CHARLSON_CANCER,
CHARLSON_CEREBRO,
CHARLSON_CPD,
CHARLSON_CONGEST_HEART_FAIL,
CHARLSON_DEMENTIA,
CHARLSON_DIABETES_CHRON,
CHARLSON_DIABETES_WO_CHRON,
CHARLSON_HEMIPLEGIA_PARAPLEGIA,
CHARLSON_METASTATIC_CANCER,
CHARLSON_MYOCARD_INFARCT,
CHARLSON_PEPTIC_ULCER,
CHARLSON_PERIPH_VASC,
CHARLSON_RENAL,
CHARLSON_RHEUMATIC,
CHARLSON_SEVERE_LIVER,
CHARLSON_HIV,
CHARLSON_MILD_LIVER
FROM
(--Admission spell
  SELECT base.Person_ID, base.Superspell_ID, spells.Spell_ID, spells.Procodet, spells.Admidate, spells.Admiage, spells.Sex, Spells.Ethnos, Spells.Resgor_ons, spells.Admimeth
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base
  INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 AS spells
  ON base.Person_ID = spells.Person_ID AND base.Spell_ID = spells.Spell_ID
  WHERE base.SpellNo = 1
) AS admi
INNER JOIN
(--Discharge spell
  SELECT base.Disdate, base.Dismeth, base.Primary_dx, base.Person_ID, base.SuperSpell_ID FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base --these items have been carried through into the lookup for ease of access
  INNER JOIN
  (
    SELECT Person_ID, Superspell_ID AS Spell_ID, MAX(SpellNo) AS LastSpellNo FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup GROUP BY Person_ID, Superspell_ID --assumes SpellNo is applied in chronological order
  ) AS dis
  ON base.Person_ID = dis.Person_ID AND base.Superspell_ID = dis.Spell_ID AND base.SpellNo = dis.LastSpellNo
) AS dis
ON admi.Person_ID = dis.Person_ID AND admi.Superspell_ID = dis.Superspell_ID
INNER JOIN
(--Derived columns, except grouped dx/op
  SELECT 
    base.Person_ID,
    base.Superspell_ID,  
    --Grouped dx/op
    --MIN(GROUPED_DIAGNOSIS) AS GROUPED_DIAGNOSIS, --wrong - see below join
    --MAX(GROUPED_PROCEDURE) AS GROUPED_PROCEDURE,
    --Grouped cov / death
    MAX(POSS_COVID) AS POSS_COVID,
    MAX(DIED_DURING_ADMISSION) AS DIED_DURING_ADMISSION,   
    --Additional grouped dx/op fields - add or remove as applicable

    --Charlson
    MAX(CHARLSON_CANCER) AS CHARLSON_CANCER,
    MAX(CHARLSON_CEREBRO) AS CHARLSON_CEREBRO,
    MAX(CHARLSON_CPD) AS CHARLSON_CPD,
    MAX(CHARLSON_CONGEST_HEART_FAIL) AS CHARLSON_CONGEST_HEART_FAIL,
    MAX(CHARLSON_DEMENTIA) AS CHARLSON_DEMENTIA,
    MAX(CHARLSON_DIABETES_CHRON) AS CHARLSON_DIABETES_CHRON,
    MAX(CHARLSON_DIABETES_WO_CHRON) AS CHARLSON_DIABETES_WO_CHRON,
    MAX(CHARLSON_HEMIPLEGIA_PARAPLEGIA) AS CHARLSON_HEMIPLEGIA_PARAPLEGIA,
    MAX(CHARLSON_METASTATIC_CANCER) AS CHARLSON_METASTATIC_CANCER,
    MAX(CHARLSON_MYOCARD_INFARCT) AS CHARLSON_MYOCARD_INFARCT,
    MAX(CHARLSON_PEPTIC_ULCER) AS CHARLSON_PEPTIC_ULCER,
    MAX(CHARLSON_PERIPH_VASC) AS CHARLSON_PERIPH_VASC,
    MAX(CHARLSON_RENAL) AS CHARLSON_RENAL,
    MAX(CHARLSON_RHEUMATIC) AS CHARLSON_RHEUMATIC,
    MAX(CHARLSON_SEVERE_LIVER) AS CHARLSON_SEVERE_LIVER,
    MAX(CHARLSON_HIV) AS CHARLSON_HIV,
    MAX(CHARLSON_MILD_LIVER) AS CHARLSON_MILD_LIVER
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 AS spells
  INNER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS base
  ON base.Person_ID = spells.Person_ID AND base.Spell_ID = spells.Spell_ID
  GROUP BY base.Person_ID, base.Superspell_ID
) AS derived
ON admi.Person_ID = derived.Person_ID AND admi.Superspell_ID = derived.Superspell_ID
LEFT OUTER JOIN
--Derived grouped dx/op
global_temp.nh_temp_superspells_derived_groups AS derived_groups
ON admi.Person_ID = derived_groups.Person_ID AND admi.Superspell_ID = derived_groups.Superspell_ID

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_superspells OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

-- MAGIC %md
-- MAGIC <h4>Step 5 - Generate the admissions table</h4>

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update the created table name in the cmds below to something relevant. Add/remove the additional grouped dx/op fields as required and then run the cmd.

-- COMMAND ----------

DROP TABLE IF EXISTS dars_nic_391419_j3w9t_collab.ccu003_kmc_final_vte_admissions_dx

-- COMMAND ----------

CREATE TABLE IF NOT EXISTS dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx AS 

  SELECT
  --Main fields
  base.Spell_ID AS SPELL_ID,
  base.Procodet AS PROCODET,
  CASE WHEN base.SPELL_DUR < 0 THEN -1 ELSE SPELL_DUR END AS SPELL_DUR,
  base.Person_ID AS PERSON_ID,
  COALESCE(base.Admiage, 999) AS ADMIAGE,
  base.Sex AS SEX,
  base.Ethnos AS ETHNOS,
  base.Admidate AS ADMIDATE,
  base.Disdate AS DISDATE,
  base.Resgor_ons AS RESIDENCE_REGION,
  base.Dismeth AS DISMETH,
  CASE WHEN base.Dismeth IN (1, 2, 3, 4, 5) THEN 1 ELSE 0 END AS COMPLETED_SPELL_CIPS,
  base.Admimeth AS ADMIMETH,
  CASE WHEN base.Admimeth IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D', '28') THEN 1 ELSE 0 END AS EMERGENCY_ADMISSION,
  0 AS INTERHOSPITAL_TRANSFER,
  --Grouped dx/op fields
  GROUPED_DIAGNOSIS_ANY AS VTE_DIAGNOSIS_ANY,
  GROUPED_DIAGNOSIS_ANY_POSITION AS VTE_DIAGNOSIS_ANY_POSITION,
  GROUPED_DIAGNOSIS_DISCHARGE AS VTE_DIAGNOSIS_DISCHARGE,
  GROUPED_DIAGNOSIS_DISCHARGE_POSITION AS VTE_DIAGNOSIS_DISCHARGE_POSITION,
  GROUPED_DIAGNOSIS_PRIMARY AS VTE_DIAGNOSIS_PRIMARY,
  PRIMARY_DISCHARGE_DIAGNOSIS_CODE,
  GROUPED_PROCEDURE AS PE_PROCEDURE,  
  --Grouped cov / deaths fields
  POSS_COVID,
  DIED_DURING_ADMISSION,
  --Other grouped fields - add or remove as applicable

  --Charlson
  CHARLSON_SCORE,
  CHARLSON_CANCER,
  CHARLSON_CEREBRO,
  CHARLSON_CPD,
  CHARLSON_CONGEST_HEART_FAIL,
  CHARLSON_DEMENTIA,
  CHARLSON_DIABETES_CHRON,
  CHARLSON_DIABETES_WO_CHRON,
  CHARLSON_HEMIPLEGIA_PARAPLEGIA,
  CHARLSON_METASTATIC_CANCER,
  CHARLSON_MYOCARD_INFARCT,
  CHARLSON_PEPTIC_ULCER,
  CHARLSON_PERIPH_VASC,
  CHARLSON_RENAL,
  CHARLSON_RHEUMATIC,
  CHARLSON_SEVERE_LIVER,
  CHARLSON_HIV,
  CHARLSON_MILD_LIVER
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_spells1 AS base
  FULL OUTER JOIN dars_nic_391419_j3w9t_collab.ccu003_nh_temp_superspells_lookup AS lookup ON base.Person_ID = lookup.Person_ID AND base.Spell_ID = lookup.Spell_ID
  WHERE lookup.Spell_ID IS NULL
  
  UNION ALL
  
  SELECT
  --Main fields
  Spell_ID AS SPELL_ID,
  Procodet AS PROCODET,
  CASE WHEN SPELL_DUR < 0 THEN -1 ELSE SPELL_DUR END AS SPELL_DUR,
  Person_ID AS PERSON_ID,
  COALESCE(Admiage, 999) AS ADMIAGE,
  Sex AS SEX,
  Ethnos AS ETHNOS,
  COALESCE(Admidate, '1800-01-01') AS ADMIDATE,
  Disdate AS DISDATE,
  Resgor_ons AS RESIDENCE_REGION,
  Dismeth AS DISMETH,
  CASE WHEN Dismeth IN (1, 2, 3, 4, 5) THEN 1 ELSE 0 END AS COMPLETED_SPELL_CIPS,
  Admimeth AS ADMIMETH,
  CASE WHEN Admimeth IN ('21', '22', '23', '24', '25', '2A', '2B', '2C', '2D', '28') THEN 1 ELSE 0 END AS EMERGENCY_ADMISSION,
  1 AS INTERHOSPITAL_TRANSFER,
  --Grouped dx/op fields
  GROUPED_DIAGNOSIS_ANY AS VTE_DIAGNOSIS_ANY,
  GROUPED_DIAGNOSIS_ANY_POSITION AS VTE_DIAGNOSIS_ANY_POSITION,
  GROUPED_DIAGNOSIS_DISCHARGE AS VTE_DIAGNOSIS_DISCHARGE,
  GROUPED_DIAGNOSIS_DISCHARGE_POSITION AS VTE_DIAGNOSIS_DISCHARGE_POSITION,
  GROUPED_DIAGNOSIS_PRIMARY AS VTE_DIAGNOSIS_PRIMARY,
  PRIMARY_DISCHARGE_DIAGNOSIS_CODE,
  GROUPED_PROCEDURE AS PE_PROCEDURE,  
  --Grouped cov / death fields
  POSS_COVID,
  DIED_DURING_ADMISSION,
  --Other grouped dx/op fields - add or remove as applicable
  
  --Charlson
  CHARLSON_SCORE,
  CHARLSON_CANCER,
  CHARLSON_CEREBRO,
  CHARLSON_CPD,
  CHARLSON_CONGEST_HEART_FAIL,
  CHARLSON_DEMENTIA,
  CHARLSON_DIABETES_CHRON,
  CHARLSON_DIABETES_WO_CHRON,
  CHARLSON_HEMIPLEGIA_PARAPLEGIA,
  CHARLSON_METASTATIC_CANCER,
  CHARLSON_MYOCARD_INFARCT,
  CHARLSON_PEPTIC_ULCER,
  CHARLSON_PERIPH_VASC,
  CHARLSON_RENAL,
  CHARLSON_RHEUMATIC,
  CHARLSON_SEVERE_LIVER,
  CHARLSON_HIV,
  CHARLSON_MILD_LIVER
  FROM dars_nic_391419_j3w9t_collab.ccu003_nh_temp_final_superspells 

-- COMMAND ----------

-- MAGIC %md
-- MAGIC Update and run the below command to take ownership of the new table.<br/>
-- MAGIC Change to your own username if desired - the cmd is to prevent databricks issues rather than for any security/permissions reason

-- COMMAND ----------

ALTER TABLE dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx OWNER TO `rmhikmc@ucl.ac.uk`

-- COMMAND ----------

select * from dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx

-- COMMAND ----------

select count(*) from dars_nic_391419_j3w9t_collab.ccu003_04_kmc_final_vte_admissions_dx
