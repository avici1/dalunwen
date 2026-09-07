-- =============================================================================
-- 从 diagnoses_icd 筛选脑卒中患者 → mimiciv_derived.stroke_patient_0520
-- 数据库：PostgreSQL（mimiciv_hosp.diagnoses_icd）
-- =============================================================================
--
-- ICD-10 与 ICD-9 对应关系（与 I60 / I61 / I63 主亚型对齐）
-- ┌──────────┬─────────────────────┬──────────────────────────────────────────┐
-- │ ICD-10   │ 亚型                │ ICD-9（MIMIC 中无小数点，如 43491）       │
-- ├──────────┼─────────────────────┼──────────────────────────────────────────┤
-- │ I60%     │ 蛛网膜下腔出血      │ 430%  （430–4309，SUBSTR 前三位 = 430）  │
-- │ I61%     │ 脑出血              │ 431%  （431–4319）                       │
-- │ I63%     │ 脑梗死              │ 434%  （434–4349，脑动脉闭塞伴梗死）     │
-- │          │                     │ 43301,43311,43321,43331,43381,43391      │
-- │          │                     │ （颈动脉/椎基底等闭塞「伴脑梗死」）       │
-- └──────────┴─────────────────────┴──────────────────────────────────────────┘
--
-- 相关但非 I60/I61/I63 一一对应（本脚本默认不纳入，可按需取消注释）：
--   432% → I62 其他非创伤性颅内出血
--   433%（非 x1 结尾）→ I65 未导致脑梗死的动脉闭塞/狭窄
--   435% → 短暂性脑缺血（TIA）
--   436% → I64 未特指卒中
--   437% / 438% → 其他脑血管病 / 后遗症
--   Charlson 等研究常用宽口径：SUBSTR(icd_code,1,3) BETWEEN '430' AND '438'
-- =============================================================================

CREATE SCHEMA IF NOT EXISTS mimiciv_derived;

DROP TABLE IF EXISTS mimiciv_derived.stroke_patient_0520 CASCADE;

CREATE TABLE mimiciv_derived.stroke_patient_0520 AS
SELECT
    d.subject_id,
    d.hadm_id,
    d.seq_num,
    TRIM(d.icd_code)                    AS icd_code,
    d.icd_version,
    CASE
        WHEN d.icd_version = 10 AND d.icd_code LIKE 'I60%' THEN 'I60_蛛网膜下腔出血'
        WHEN d.icd_version = 10 AND d.icd_code LIKE 'I61%' THEN 'I61_脑出血'
        WHEN d.icd_version = 10 AND d.icd_code LIKE 'I63%' THEN 'I63_脑梗死'
        WHEN d.icd_version = 9  AND SUBSTR(TRIM(d.icd_code), 1, 3) = '430' THEN 'I60_蛛网膜下腔出血'
        WHEN d.icd_version = 9  AND SUBSTR(TRIM(d.icd_code), 1, 3) = '431' THEN 'I61_脑出血'
        WHEN d.icd_version = 9  AND SUBSTR(TRIM(d.icd_code), 1, 3) = '434' THEN 'I63_脑梗死'
        WHEN d.icd_version = 9  AND TRIM(d.icd_code) IN (
            '43301', '43311', '43321', '43331', '43381', '43391'
        ) THEN 'I63_脑梗死_433伴梗死'
        ELSE '未分类'
    END                                 AS stroke_subtype,
    CASE
        WHEN d.icd_version = 10 THEN 'ICD-10'
        WHEN d.icd_version = 9  THEN 'ICD-9'
        ELSE '其他'
    END                                 AS icd_system
FROM mimiciv_hosp.diagnoses_icd d
WHERE
    -- ICD-10：I60 / I61 / I63
    (
        d.icd_version = 10
        AND (
            d.icd_code LIKE 'I60%'
         OR d.icd_code LIKE 'I61%'
         OR d.icd_code LIKE 'I63%'
        )
    )
    OR
    -- ICD-9：与 I60 / I61 / I63 对应的主要编码
    (
        d.icd_version = 9
        AND (
            SUBSTR(TRIM(d.icd_code), 1, 3) IN ('430', '431', '434')
         OR TRIM(d.icd_code) IN (
                '43301', '43311', '43321', '43331', '43381', '43391'
            )
         -- 433.x1：第五位为 1 表示伴脑梗死（兼容 5 位编码写法）
         OR (
                SUBSTR(TRIM(d.icd_code), 1, 3) = '433'
            AND LENGTH(TRIM(d.icd_code)) >= 5
            AND SUBSTR(TRIM(d.icd_code), 5, 1) = '1'
            )
        )
    );

-- 索引
CREATE INDEX IF NOT EXISTS idx_stroke_patient_0520_sid
    ON mimiciv_derived.stroke_patient_0520 (subject_id);
CREATE INDEX IF NOT EXISTS idx_stroke_patient_0520_hid
    ON mimiciv_derived.stroke_patient_0520 (hadm_id);
CREATE INDEX IF NOT EXISTS idx_stroke_patient_0520_ver
    ON mimiciv_derived.stroke_patient_0520 (icd_version);

-- -----------------------------------------------------------------------------
-- 质控：各版本、各亚型记录数
-- -----------------------------------------------------------------------------
-- SELECT icd_version, stroke_subtype, COUNT(*) AS n_rows
-- FROM mimiciv_derived.stroke_patient_0520
-- GROUP BY icd_version, stroke_subtype
-- ORDER BY icd_version, stroke_subtype;

-- SELECT COUNT(DISTINCT subject_id) AS n_patients,
--        COUNT(DISTINCT hadm_id)     AS n_admissions,
--        COUNT(*)                    AS n_dx_rows
-- FROM mimiciv_derived.stroke_patient_0520;
