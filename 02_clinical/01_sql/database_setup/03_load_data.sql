-- =============================================
-- 步骤 3：加载 CSV 数据到表
-- 重要：COPY 命令要求文件在 PostgreSQL 服务器可访问的路径
-- 仅适用于本地 PostgreSQL！若连接远程数据库，请用 Navicat 导入向导
--
-- 请将下面所有路径中的 YOUR_MIMIC_PATH 替换为您的实际路径
-- 例如：f:/文章_大论文/MIMIC数据库_代码/MIMIC
-- 注意：使用正斜杠 /，不要用反斜杠
-- =============================================

SET client_encoding TO 'utf8';

-- 定义路径变量（PostgreSQL 不支持变量，此处仅作提示）
-- 请用编辑器的查找替换功能，将 YOUR_MIMIC_PATH 全部替换为实际路径

-- hosp 表
COPY mimiciv_hosp.admissions FROM 'YOUR_MIMIC_PATH/hosp/admissions.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_hcpcs FROM 'YOUR_MIMIC_PATH/hosp/d_hcpcs.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.diagnoses_icd FROM 'YOUR_MIMIC_PATH/hosp/diagnoses_icd.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_icd_diagnoses FROM 'YOUR_MIMIC_PATH/hosp/d_icd_diagnoses.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_icd_procedures FROM 'YOUR_MIMIC_PATH/hosp/d_icd_procedures.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.d_labitems FROM 'YOUR_MIMIC_PATH/hosp/d_labitems.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.drgcodes FROM 'YOUR_MIMIC_PATH/hosp/drgcodes.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.emar_detail FROM 'YOUR_MIMIC_PATH/hosp/emar_detail.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.emar FROM 'YOUR_MIMIC_PATH/hosp/emar.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.hcpcsevents FROM 'YOUR_MIMIC_PATH/hosp/hcpcsevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.labevents FROM 'YOUR_MIMIC_PATH/hosp/labevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.microbiologyevents FROM 'YOUR_MIMIC_PATH/hosp/microbiologyevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.omr FROM 'YOUR_MIMIC_PATH/hosp/omr.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.patients FROM 'YOUR_MIMIC_PATH/hosp/patients.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.pharmacy FROM 'YOUR_MIMIC_PATH/hosp/pharmacy.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.poe_detail FROM 'YOUR_MIMIC_PATH/hosp/poe_detail.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.poe FROM 'YOUR_MIMIC_PATH/hosp/poe.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.prescriptions FROM 'YOUR_MIMIC_PATH/hosp/prescriptions.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.procedures_icd FROM 'YOUR_MIMIC_PATH/hosp/procedures_icd.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.provider FROM 'YOUR_MIMIC_PATH/hosp/provider.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.services FROM 'YOUR_MIMIC_PATH/hosp/services.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_hosp.transfers FROM 'YOUR_MIMIC_PATH/hosp/transfers.csv' DELIMITER ',' CSV HEADER NULL '';

-- icu 表（chartevents 数据量大，导入可能需数小时）
COPY mimiciv_icu.caregiver FROM 'YOUR_MIMIC_PATH/icu/caregiver.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.chartevents FROM 'YOUR_MIMIC_PATH/icu/chartevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.datetimeevents FROM 'YOUR_MIMIC_PATH/icu/datetimeevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.d_items FROM 'YOUR_MIMIC_PATH/icu/d_items.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.icustays FROM 'YOUR_MIMIC_PATH/icu/icustays.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.ingredientevents FROM 'YOUR_MIMIC_PATH/icu/ingredientevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.inputevents FROM 'YOUR_MIMIC_PATH/icu/inputevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.outputevents FROM 'YOUR_MIMIC_PATH/icu/outputevents.csv' DELIMITER ',' CSV HEADER NULL '';
COPY mimiciv_icu.procedureevents FROM 'YOUR_MIMIC_PATH/icu/procedureevents.csv' DELIMITER ',' CSV HEADER NULL '';
