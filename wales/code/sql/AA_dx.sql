--Diagnoses and operations is a little unclear 

--Find spells for which we have an AA diagnosis
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS AS 
SELECT 	SPELL_NUM_E,
		EPI_NUM,
		PROV_UNIT_CD,
		DIAG_NUM,
		DIAG_CD
		
FROM SAILWWMCCV.WMCC_PEDW_DIAG
WHERE DIAG_CD_123 = 'I71';

--Get diagnosis information for those spells
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.SPELL_NUM_E,
	   SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.EPI_NUM,
	   SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.PROV_UNIT_CD,
	   SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_CD,
	   SAILWWMCCV.WMCC_PEDW_DIAG.DIAG_NUM 
FROM SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS
INNER JOIN SAILWWMCCV.WMCC_PEDW_DIAG
ON  SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_DIAG.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.EPI_NUM = SAILWWMCCV.WMCC_PEDW_DIAG.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_DIAG.PROV_UNIT_CD;


--Now get the remaining spell information 
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL.*,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_DT,
	   SAILWWMCCV.WMCC_PEDW_SPELL.DISCH_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_MTHD_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.ADMIS_SPEC_CD,
	   SAILWWMCCV.WMCC_PEDW_SPELL.PAT_ID_E,
	   SAILWWMCCV.WMCC_PEDW_SPELL.GNDR_CD 
	   
FROM SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL
INNER JOIN SAILWWMCCV.WMCC_PEDW_SPELL
ON  SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_SPELL.SPELL_NUM_E
--AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL.EPI_NUM = SAILWWMCCV.WMCC_PEDW_SPELL.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_FULL.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_SPELL.PROV_UNIT_CD;


--Now add on the episode start and end dates
CREATE OR REPLACE VIEW SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_EPIS_FULL AS 
SELECT SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL.*,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_STR_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_END_DT,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_CD,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.ETH_GRP_DERIVED_DESC,
	   SAILWWMCCV.WMCC_PEDW_EPISODE.AGE_EPI_STR_YR
	   
FROM SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL
INNER JOIN SAILWWMCCV.WMCC_PEDW_EPISODE
ON  SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL.SPELL_NUM_E = SAILWWMCCV.WMCC_PEDW_EPISODE.SPELL_NUM_E
AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL.EPI_NUM = SAILWWMCCV.WMCC_PEDW_EPISODE.EPI_NUM
AND	SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_FULL.PROV_UNIT_CD = SAILWWMCCV.WMCC_PEDW_EPISODE.PROV_UNIT_CD;


-- Export
SELECT * FROM SAILWWMCCV.CCU003_04_DOC_AA_DX_DIAGS_SPELLS_EPIS_FULL
WHERE ADMIS_DT >= '2016-01-01'
ORDER BY ADMIS_DT