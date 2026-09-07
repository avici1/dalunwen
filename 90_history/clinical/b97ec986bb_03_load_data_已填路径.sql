-- =============================================
-- 步骤 3：加载 CSV 数据到表（已填入您的路径）
-- 路径：f:/文章_大论文/MIMIC数据库_代码/MIMIC
-- 若 COPY 报错权限不足，请用 postgres 超级用户连接
-- =============================================

SET client_encoding TO 'utf8';

-- hosp 表
COPY mimiciv_hosp.admissions FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/admissions.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_hcpcs FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/d_hcpcs.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.diagnoses_icd FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/diagnoses_icd.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_icd_diagnoses FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/d_icd_diagnoses.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_icd_procedures FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/d_icd_procedures.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_labitems FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/d_labitems.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.drgcodes FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/drgcodes.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.emar_detail FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/emar_detail.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.emar FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/emar.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.hcpcsevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/hcpcsevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.labevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/labevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.microbiologyevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/microbiologyevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.omr FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/omr.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.patients FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/patients.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.pharmacy FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/pharmacy.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.poe_detail FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/poe_detail.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.poe FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/poe.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.prescriptions FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/prescriptions.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.procedures_icd FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/procedures_icd.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.provider FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/provider.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.services FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/services.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.transfers FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/hosp/transfers.csv' DELIMITER ',' CSV HEADER NULL '';

-- icu 表
COPY mimiciv_icu.caregiver FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/caregiver.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.chartevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/chartevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.datetimeevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/datetimeevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.d_items FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/d_items.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.icustays FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/icustays.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.ingredientevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/ingredientevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.inputevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/inputevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.outputevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/outputevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.procedureevents FROM 'f:/文章_大论文/MIMIC数据库_代码/MIMIC/icu/procedureevents.csv' DELIMITER ',' CSV HEADER NULL '';
