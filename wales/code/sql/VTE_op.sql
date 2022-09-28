--Find spells for which we have an VTE OPERnosis
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS AS 
SELECT 	SPELL_NUM_E,
		EPI_NUM,
		PROV_UNIT_CD,
		OPER_NUM,
		OPER_CD
		
FROM SAILWWMCCV.WMCC_PEDW_OPER
WHERE OPER_CD = 'L124' OR
OPER_CD ='L125' OR 
OPER_CD_123 ='L13';


--Get operation information for those spells
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.SPELL_NUM_E,
	   SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.EPI_NUM,
	   SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.PROV_UNIT_CD,
	   SAILWWMCCV.WMCC_PEDW_OPER.OPER_CD,
	   SAILWWMCCV.WMCC_PEDW_OPER.OPER_NUM 
FROM SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS
INNER JOIN SAILWWMCCV.WMCC_PEDW_OPER
ON  SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_OPER.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.EPI_NUM = SAILWWMCCV.WMCC_PEDW_OPER.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_OPER.PROV_UNIT_CD;


--Now get the remaining spell information 
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL.*,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_SPEC_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.PAT_ID_E,
	   SAILWWMCCV.WMCC_PEDW_SPELL.GNDR_CD 
	   
FROM SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL
INNER JOIN SAILWWMCCV.WMCC_PEDW_SPELL
ON  SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_SPELL.SPELL_NUM_E
--AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL.EPI_NUM = SAILWWMCCV.WMCC_PEDW_SPELL.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_FULL.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_SPELL.PROV_UNIT_CD;


--Now add on the episode start and end dates
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_EPIS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL.*,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_STR_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_END_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_CD,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_DESC,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.AGE_EPI_STR_YR
	   
FROM SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL
INNER JOIN SAILWWMCCV.WMCC_PEDW_EPISODE
ON  SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_EPISODE.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL.EPI_NUM = SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_FULL.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_EPISODE.PROV_UNIT_CD;


-- Export
SELECT * FROM SAILWWMCCV.CCU003_04_DOC_VTE_OP_OPERS_SPELLS_EPIS_FULL
WHERE ADMIS_DT >= '2016-01-01'
ORDER BY ADMIS_DT