-- =============================================================================
-- MIMIC-IV 脑卒中队列提取（0520 版）
-- 入口：mimiciv_hosp.stroke_diagnoses_0520
-- 下游表统一后缀 _0520
-- =============================================================================

/* =========================================================
   0) 卒中诊断（ICD-9 / ICD-10：I60 / I61 / I63 及对应 ICD-9）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_diagnoses_0520;

CREATE TABLE mimiciv_hosp.stroke_diagnoses_0520 AS
SELECT
    subject_id,
    hadm_id,
    seq_num,
    icd_code,
    icd_version
FROM mimiciv_hosp.diagnoses_icd
WHERE
    seq_num < 5
    AND
    (
        (
            icd_version = 9
            AND (
                icd_code LIKE '430%'
             OR icd_code LIKE '431%'
             OR icd_code LIKE '434%'
             OR icd_code IN (
                    '43301', '43311', '43321', '43331', '43381', '43391'
                )
             OR (
                    icd_code LIKE '433%'
                AND LENGTH(TRIM(icd_code)) >= 5
                AND SUBSTR(TRIM(icd_code), 5, 1) = '1'
                )
            )
        )
        OR
        (
            icd_version = 10
            AND (
                icd_code LIKE 'I60%'
             OR icd_code LIKE 'I61%'
             OR icd_code LIKE 'I63%'
            )
        )
    );


/* =========================================================
   基础队列：来自 stroke_diagnoses_0520
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_cohort_0520;

CREATE TABLE mimiciv_hosp.stroke_cohort_0520 AS
SELECT DISTINCT subject_id, hadm_id
FROM mimiciv_hosp.stroke_diagnoses_0520
WHERE subject_id IS NOT NULL
  AND hadm_id IS NOT NULL;


/* =========================================================
   1) patients（hosp）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_patients_0520;

CREATE TABLE mimiciv_hosp.stroke_patients_0520 AS
SELECT DISTINCT
    p.subject_id,
    p.gender,
    p.anchor_age,
    p.anchor_year,
    p.dod
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_hosp.patients p
  ON c.subject_id = p.subject_id;


/* =========================================================
   2) admissions（hosp）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_admission_0520;

CREATE TABLE mimiciv_hosp.stroke_admission_0520 AS
SELECT
    a.subject_id,
    a.hadm_id,
    a.admittime,
    a.dischtime,
    a.deathtime,
    a.race,
    a.hospital_expire_flag,
    EXTRACT(EPOCH FROM (a.dischtime - a.admittime)) / 86400.0 AS hosp_los_days,
    (EXTRACT(YEAR FROM a.admittime) - p.anchor_year + p.anchor_age) AS age_at_admit,
    CASE WHEN p.dod IS NOT NULL AND p.dod <= a.admittime + INTERVAL '28 day'  THEN 1 ELSE 0 END AS death_28d,
    CASE WHEN p.dod IS NOT NULL AND p.dod <= a.admittime + INTERVAL '90 day'  THEN 1 ELSE 0 END AS death_90d,
    CASE WHEN p.dod IS NOT NULL AND p.dod <= a.admittime + INTERVAL '365 day' THEN 1 ELSE 0 END AS death_1y
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_hosp.admissions a
  ON c.subject_id = a.subject_id
 AND c.hadm_id = a.hadm_id
JOIN mimiciv_hosp.patients p
  ON a.subject_id = p.subject_id;


/* =========================================================
   3) icustays（icu）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_icu.stroke_icustays_0520;

CREATE TABLE mimiciv_icu.stroke_icustays_0520 AS
SELECT
    i.subject_id,
    i.hadm_id,
    i.stay_id,
    i.first_careunit,
    i.last_careunit,
    i.intime,
    i.outtime,
    EXTRACT(EPOCH FROM (i.outtime - i.intime)) / 86400.0 AS icu_los_days
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_icu.icustays i
  ON c.subject_id = i.subject_id
 AND c.hadm_id = i.hadm_id;


/* =========================================================
   4) chartevents（icu）→ mimiciv_hosp.stroke_chartevents_0520
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_chartevents_0520;

CREATE TABLE mimiciv_hosp.stroke_chartevents_0520 AS
SELECT
    ce.subject_id,
    ce.hadm_id,
    ce.stay_id,
    ce.charttime,
    ce.itemid,
    di.label,
    ce.value,
    ce.valuenum,
    ce.valueuom
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_icu.chartevents ce
  ON c.subject_id = ce.subject_id
 AND c.hadm_id = ce.hadm_id
JOIN mimiciv_icu.d_items di
  ON ce.itemid = di.itemid
WHERE ce.itemid IN (
    223835, 3420, 3422,
    220052, 220181, 225312, 224322,
    220739, 223900, 223901,
    220045, 220210, 223761, 223762
)
AND ce.valuenum IS NOT NULL;


/* =========================================================
   5) prescriptions（hosp）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_prescriptions_0520;

CREATE TABLE mimiciv_hosp.stroke_prescriptions_0520 AS
SELECT
    pr.subject_id,
    pr.hadm_id,
    pr.starttime,
    pr.stoptime,
    pr.drug,
    pr.drug_type,
    pr.route,
    pr.dose_val_rx,
    pr.dose_unit_rx
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_hosp.prescriptions pr
  ON c.subject_id = pr.subject_id
 AND c.hadm_id = pr.hadm_id
WHERE lower(pr.drug) LIKE ANY (ARRAY[
      '%metoprolol%','%bisoprolol%','%carvedilol%','%propranolol%',
      '%atorvastatin%','%rosuvastatin%','%simvastatin%',
      '%enalapril%','%lisinopril%','%captopril%',
      '%losartan%','%valsartan%','%irbesartan%',
      '%furosemide%','%torsemide%','%bumetanide%','%spironolactone%'
]);


/* =========================================================
   6) inputevents（icu）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_icu.stroke_inputevents_0520;

CREATE TABLE mimiciv_icu.stroke_inputevents_0520 AS
SELECT
    ie.subject_id,
    ie.hadm_id,
    ie.stay_id,
    ie.starttime,
    ie.endtime,
    ie.itemid,
    di.label,
    ie.amount,
    ie.amountuom,
    ie.rate,
    ie.rateuom
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_icu.inputevents ie
  ON c.subject_id = ie.subject_id
 AND c.hadm_id = ie.hadm_id
JOIN mimiciv_icu.d_items di
  ON ie.itemid = di.itemid
WHERE
      lower(di.label) LIKE '%norepinephrine%'
   OR lower(di.label) LIKE '%epinephrine%'
   OR lower(di.label) LIKE '%dopamine%'
   OR lower(di.label) LIKE '%dobutamine%'
   OR lower(di.label) LIKE '%vasopressin%'
   OR lower(di.label) LIKE '%phenylephrine%';


/* =========================================================
   7) labevents（hosp）→ mimiciv_hosp.stroke_labevent_0520
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_labevent_0520;

CREATE TABLE mimiciv_hosp.stroke_labevent_0520 AS
SELECT
    le.subject_id,
    le.hadm_id,
    le.charttime,
    le.itemid,
    dl.label,
    le.value,
    le.valuenum,
    le.valueuom,
    le.flag
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_hosp.labevents le
  ON c.subject_id = le.subject_id
 AND c.hadm_id = le.hadm_id
JOIN mimiciv_hosp.d_labitems dl
  ON le.itemid = dl.itemid
WHERE le.itemid IN (
    50821, 51265, 50885, 50912,
    50820, 50882, 50983, 50971, 51221, 51300, 51301
)
AND le.valuenum IS NOT NULL;


/* =========================================================
   8) diagnoses_icd 合并症（hosp）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_hosp.stroke_diagnoses_icd_0520;

CREATE TABLE mimiciv_hosp.stroke_diagnoses_icd_0520 AS
SELECT
    d.subject_id,
    d.hadm_id,
    d.seq_num,
    d.icd_code,
    d.icd_version,
    CASE
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '401%')
          OR (d.icd_version = 10 AND d.icd_code LIKE 'I10%')
        THEN 'hypertension'
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '250%')
          OR (d.icd_version = 10 AND (d.icd_code LIKE 'E10%' OR d.icd_code LIKE 'E11%' OR d.icd_code LIKE 'E13%'))
        THEN 'diabetes'
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '42731%')
          OR (d.icd_version = 10 AND d.icd_code LIKE 'I48%')
        THEN 'atrial_fibrillation'
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '410%')
          OR (d.icd_version = 10 AND (d.icd_code LIKE 'I21%' OR d.icd_code LIKE 'I22%'))
        THEN 'myocardial_infarction'
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '425%')
          OR (d.icd_version = 10 AND d.icd_code LIKE 'I42%')
        THEN 'cardiomyopathy'
        WHEN (d.icd_version = 9  AND d.icd_code LIKE '585%')
          OR (d.icd_version = 10 AND d.icd_code LIKE 'N18%')
        THEN 'ckd'
        ELSE NULL
    END AS comorbidity
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_hosp.diagnoses_icd d
  ON c.subject_id = d.subject_id
 AND c.hadm_id = d.hadm_id
WHERE
      (d.icd_version = 9  AND (d.icd_code LIKE '401%' OR d.icd_code LIKE '250%' OR d.icd_code LIKE '42731%' OR d.icd_code LIKE '410%' OR d.icd_code LIKE '425%' OR d.icd_code LIKE '585%'))
   OR (d.icd_version = 10 AND (d.icd_code LIKE 'I10%' OR d.icd_code LIKE 'E10%' OR d.icd_code LIKE 'E11%' OR d.icd_code LIKE 'E13%' OR d.icd_code LIKE 'I48%' OR d.icd_code LIKE 'I21%' OR d.icd_code LIKE 'I22%' OR d.icd_code LIKE 'I42%' OR d.icd_code LIKE 'N18%'));


/* =========================================================
   9) outputevents（icu）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_icu.stroke_outputevents_0520;

CREATE TABLE mimiciv_icu.stroke_outputevents_0520 AS
SELECT
    oe.subject_id,
    oe.hadm_id,
    oe.stay_id,
    oe.charttime,
    oe.itemid,
    di.label,
    oe.value,
    oe.valueuom
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_icu.outputevents oe
  ON c.subject_id = oe.subject_id
 AND c.hadm_id = oe.hadm_id
JOIN mimiciv_icu.d_items di
  ON oe.itemid = di.itemid
WHERE
      lower(di.label) LIKE '%urine%'
   OR lower(di.label) LIKE '%drain%';


/* =========================================================
   10) procedureevents（icu）
   ========================================================= */
DROP TABLE IF EXISTS mimiciv_icu.stroke_procedureevents_0520;

CREATE TABLE mimiciv_icu.stroke_procedureevents_0520 AS
SELECT
    pe.subject_id,
    pe.hadm_id,
    pe.stay_id,
    pe.starttime,
    pe.endtime,
    pe.itemid,
    di.label,
    pe.value,
    pe.valueuom
FROM mimiciv_hosp.stroke_cohort_0520 c
JOIN mimiciv_icu.procedureevents pe
  ON c.subject_id = pe.subject_id
 AND c.hadm_id = pe.hadm_id
JOIN mimiciv_icu.d_items di
  ON pe.itemid = di.itemid
WHERE
      lower(di.label) LIKE '%mechanical ventilation%'
   OR lower(di.label) LIKE '%ventilation%'
   OR lower(di.label) LIKE '%intubation%'
   OR lower(di.label) LIKE '%crrt%'
   OR lower(di.label) LIKE '%continuous renal replacement%';


-- =============================================================================
-- 质控（可选）
-- =============================================================================
-- SELECT COUNT(*) AS n_diagnoses    FROM mimiciv_hosp.stroke_diagnoses_0520;
-- SELECT COUNT(*) AS n_cohort       FROM mimiciv_hosp.stroke_cohort_0520;
-- SELECT COUNT(*) AS n_lab          FROM mimiciv_hosp.stroke_labevent_0520;
-- SELECT COUNT(*) AS n_chart        FROM mimiciv_hosp.stroke_chartevents_0520;
