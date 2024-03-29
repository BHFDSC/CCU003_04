--Find spells for which we have an Stroke diagnosis
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2 AS 
SELECT 	DISTINCT SPELL_NUM_E,
		EPI_NUM,
		PROV_UNIT_CD,
		DIAG_NUM,
		DIAG_CD,
		DIAG_CD_123
FROM SAILWWMCCV.WMCC_PEDW_DIAG
WHERE SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_CD_123 IN ('I63','O01','O02','O03','O04','Y53','Z35')
OR    SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_CD IN ('L294','L295','L314','X833','L354');


--Get diagnosis information for those spells
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.SPELL_NUM_E,
	   SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.EPI_NUM,
	   SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.PROV_UNIT_CD,
	   SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_CD,
	   SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_NUM 
FROM SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_DIAG
ON  SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_DIAG.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_DIAG.EPI_NUM
AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_DIAG.PROV_UNIT_CD;


--Now get the remaining spell information 
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2.*,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_SPEC_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.PAT_ID_E,
	   SAILWWMCCV.WMCC_PEDW_SPELL.GNDR_CD 
FROM SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_SPELL
ON  SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_SPELL.SPELL_NUM_E
--AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_SPELL.EPI_NUM
AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_FULL_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_SPELL.PROV_UNIT_CD;


--Now add on the episode start and end dates
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_EPIS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2.*,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_STR_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_END_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_CD,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_DESC,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.AGE_EPI_STR_YR
	   
FROM SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_EPISODE
ON  SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_EPISODE.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_NUM
AND	SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_FULL_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_EPISODE.PROV_UNIT_CD;

--Find spells for which we have an Stroke OPERATION
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2 AS 
SELECT 	DISTINCT SPELL_NUM_E,
		EPI_NUM,
		PROV_UNIT_CD,
		OPER_NUM,
		OPER_CD
		
FROM SAILWWMCCV.WMCC_PEDW_OPER
WHERE SAILWWMCCV.WMCC_PEDW_OPER.OPER_CD_123 IN ('I63','O01','O02','O03','O04','Y53','Z35')
OR    SAILWWMCCV.WMCC_PEDW_OPER.OPER_CD IN ('L294','L295','L314','X833','L354');


--Get operation information for those spells
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.SPELL_NUM_E,
	   SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.EPI_NUM,
	   SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.PROV_UNIT_CD,
	   SAILWWMCCV.WMCC_PEDW_OPER.OPER_CD,
	   SAILWWMCCV.WMCC_PEDW_OPER.OPER_NUM 
FROM SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_OPER
ON  SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_OPER.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_OPER.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_OPER.PROV_UNIT_CD;


--Now get the remaining spell information 
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2.*,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_SPEC_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.PAT_ID_E,
	   SAILWWMCCV.WMCC_PEDW_SPELL.GNDR_CD 
	   
FROM SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_SPELL
ON  SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_SPELL.SPELL_NUM_E
--AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_SPELL.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_FULL_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_SPELL.PROV_UNIT_CD;


--Now add on the episode start and end dates
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_EPIS_FULL_2 AS 
SELECT SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2.*,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_STR_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_END_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_CD,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_DESC,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.AGE_EPI_STR_YR
	   
FROM SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2
INNER JOIN SAILWWMCCV.WMCC_PEDW_EPISODE
ON  SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_EPISODE.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2.EPI_NUM = SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_FULL_2.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_EPISODE.PROV_UNIT_CD;


-- Export
SELECT * FROM SAILWWMCCV.CCU003_04_Stroke_DX_DIAGS_SPELLS_EPIS_FULL_2
WHERE ADMIS_DT >= '2016-01-01'
ORDER BY ADMIS_DT

SELECT * FROM SAILWWMCCV.CCU003_04_DOC_STROKE_OP_OPERS_SPELLS_EPIS_FULL_2
WHERE ADMIS_DT >= '2016-01-01'
ORDER BY ADMIS_DT
