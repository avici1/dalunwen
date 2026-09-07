-- =============================================================================
-- MIMIC-IV：脑卒中队列 → 与 0417/data_0506.csv 同结构的分析宽表
-- 数据库：PostgreSQL（schema: mimiciv_hosp / mimiciv_icu / mimiciv_derived）
-- 对应 R 流水线：0417实例数据引入.R → 0424实例数据纵向处理.R → 0506处理.R
--
-- 说明：
--   1) 随机过程（d_time/hosp_los/hosp_time/time）使用 setseed，与 R 同种子可近似复现；
--      因 RNG 实现差异，数值不会与 CSV 逐行完全一致。
--   2) 缺失值插补：SQL 用「全表列中位数」替代 missRanger，列结构一致。
--   3) 脚本末尾仅 CREATE TABLE，不含 COPY/导出。
--   4) chartevents 数据量大，建议先建好索引再运行（见 navicat/05_index.sql）。
-- =============================================================================

CREATE SCHEMA IF NOT EXISTS mimiciv_derived;

-- ---------------------------------------------------------------------------
-- 0) 参数与 itemid 清单（与 0417实例数据引入.R 一致）
-- ---------------------------------------------------------------------------
DROP TABLE IF EXISTS mimiciv_derived._params CASCADE;
CREATE TABLE mimiciv_derived._params (
    key   TEXT PRIMARY KEY,
    value TEXT NOT NULL
);
INSERT INTO mimiciv_derived._params (key, value) VALUES
    ('weibull_shape',           '1.6'),
    ('weibull_target_mean',     '12'),
    ('seed_dtime',              '20260420'),
    ('seed_hosp',               '20260421'),
    ('hosp_mean',               '13.91'),
    ('hosp_sd',                 '12.44'),
    ('icu_mean',                '4.49'),
    ('icu_sd',                  '5.10'),
    ('seed_hosp_time',          '20260506'),
    ('patient_ge3_ratio',       '0.7'),
    ('target_hosp_mean',        '13.91'),
    ('target_hosp_sd',          '12.44');

DROP TABLE IF EXISTS mimiciv_derived._itemids CASCADE;
CREATE TABLE mimiciv_derived._itemids (
    itemid      INTEGER NOT NULL,
    source      TEXT NOT NULL,   -- 'lab' | 'chart'
    use_filter  BOOLEAN NOT NULL DEFAULT TRUE,
    use_wide    BOOLEAN NOT NULL DEFAULT TRUE,
    PRIMARY KEY (itemid, source)
);

INSERT INTO mimiciv_derived._itemids (itemid, source, use_filter, use_wide) VALUES
    -- labevents（SOFA + APACHE II）
    (50821, 'lab',   TRUE, TRUE),
    (51265, 'lab',   TRUE, TRUE),
    (50885, 'lab',   TRUE, TRUE),
    (50912, 'lab',   TRUE, TRUE),
    (50820, 'lab',   TRUE, TRUE),
    (50882, 'lab',   TRUE, TRUE),
    (50983, 'lab',   TRUE, TRUE),
    (50971, 'lab',   TRUE, TRUE),
    (51221, 'lab',   TRUE, TRUE),
    (51300, 'lab',   TRUE, TRUE),
    (51301, 'lab',   TRUE, TRUE),
    -- chartevents（纳入筛选；3420/3422/223761 不进最终宽表）
    (223835, 'chart', TRUE, TRUE),
    (3420,   'chart', TRUE, FALSE),
    (3422,   'chart', TRUE, FALSE),
    (220052, 'chart', TRUE, TRUE),
    (220181, 'chart', TRUE, TRUE),
    (225312, 'chart', TRUE, TRUE),
    (224322, 'chart', TRUE, TRUE),
    (220739, 'chart', TRUE, TRUE),
    (223900, 'chart', TRUE, TRUE),
    (223901, 'chart', TRUE, TRUE),
    (220045, 'chart', TRUE, TRUE),
    (220210, 'chart', TRUE, TRUE),
    (223761, 'chart', TRUE, FALSE),
    (223762, 'chart', TRUE, TRUE);

-- =============================================================================
-- 步骤 1：ICD-10 脑卒中队列
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_dx CASCADE;
CREATE TABLE mimiciv_derived.stroke_dx AS
SELECT DISTINCT
    d.subject_id,
    d.hadm_id,
    d.icd_code,
    d.seq_num,
    d.icd_version,
    CASE
        WHEN d.icd_code LIKE 'I60%' THEN '蛛网膜下腔出血'
        WHEN d.icd_code LIKE 'I61%' THEN '脑出血'
        WHEN d.icd_code LIKE 'I63%' THEN '脑梗死'
        ELSE '其他脑血管疾病'
    END AS stroke_type
FROM mimiciv_hosp.diagnoses_icd d
WHERE d.icd_version = 10
  AND (
        d.icd_code LIKE 'I60%'
     OR d.icd_code LIKE 'I61%'
     OR d.icd_code LIKE 'I63%'
  );

CREATE INDEX IF NOT EXISTS idx_stroke_dx_sid ON mimiciv_derived.stroke_dx (subject_id);
CREATE INDEX IF NOT EXISTS idx_stroke_dx_hid ON mimiciv_derived.stroke_dx (hadm_id);

-- =============================================================================
-- 步骤 2：入院信息 + 诊断
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_admission CASCADE;
CREATE TABLE mimiciv_derived.stroke_admission AS
SELECT
    a.subject_id,
    a.hadm_id,
    a.admittime,
    a.dischtime,
    a.deathtime,
    a.admission_type,
    a.hospital_expire_flag,
    dx.icd_code,
    dx.seq_num,
    dx.icd_version,
    dx.stroke_type
FROM mimiciv_hosp.admissions a
INNER JOIN mimiciv_derived.stroke_dx dx
    ON a.hadm_id = dx.hadm_id;

-- =============================================================================
-- 步骤 3：患者基线（保留原始 dod 用于是否死亡）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_patients CASCADE;
CREATE TABLE mimiciv_derived.stroke_patients AS
SELECT DISTINCT
    p.subject_id,
    p.gender,
    p.anchor_age,
    p.anchor_year,
    p.dod                          AS dod_original,
    (p.dod IS NOT NULL)            AS died_flag_original
FROM mimiciv_hosp.patients p
WHERE p.subject_id IN (SELECT DISTINCT subject_id FROM mimiciv_derived.stroke_dx);

-- =============================================================================
-- 步骤 4：ICU 住院（用于标记 ICU 与模拟 ICU 时长）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_icu_hadm CASCADE;
CREATE TABLE mimiciv_derived.stroke_icu_hadm AS
SELECT DISTINCT
    i.subject_id,
    i.hadm_id
FROM mimiciv_icu.icustays i
WHERE i.subject_id IN (SELECT DISTINCT subject_id FROM mimiciv_derived.stroke_dx)
  AND i.hadm_id IS NOT NULL;

-- =============================================================================
-- 步骤 5：模拟 d_time / dod（仅原始 dod 非空者；seed=20260420）
-- =============================================================================
SELECT setseed((SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'seed_dtime'));

DROP TABLE IF EXISTS mimiciv_derived.stroke_patients_sim CASCADE;
CREATE TABLE mimiciv_derived.stroke_patients_sim AS
WITH pars AS (
    SELECT
        1.6::DOUBLE PRECISION AS shape_k,
        (12.0 / exp(lgamma(1.0 + 1.0 / 1.6)))::DOUBLE PRECISION AS scale_w
)
SELECT
    sp.subject_id,
    sp.gender,
    sp.anchor_age,
    sp.anchor_year,
    sp.dod_original,
    sp.died_flag_original,
    CASE WHEN sp.died_flag_original THEN dt.d_time ELSE NULL END AS d_time,
    CASE
        WHEN sp.died_flag_original THEN make_date(sp.anchor_year, 1, 1) + dt.d_time
        ELSE NULL
    END::DATE AS dod,
    make_date(sp.anchor_year, 1, 1) AS baseline_date
FROM mimiciv_derived.stroke_patients sp
LEFT JOIN LATERAL (
    SELECT GREATEST(1, ROUND(
        pars.scale_w * POWER(-LN(GREATEST(random(), 1e-12)), 1.0 / pars.shape_k)
    )::INTEGER) AS d_time
    FROM pars
) dt ON sp.died_flag_original;

-- =============================================================================
-- 步骤 6：模拟 ICU 时长 + 住院时长（seed=20260421，3 轮均值/SD 校正）
-- =============================================================================
SELECT setseed((SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'seed_hosp'));

DROP TABLE IF EXISTS mimiciv_derived.stroke_admission_sim CASCADE;
CREATE TABLE mimiciv_derived.stroke_admission_sim AS
WITH icu AS (
    SELECT
        h.subject_id,
        h.hadm_id,
        ROUND(GREATEST(
            (sqrt(-2.0 * LN(GREATEST(random(), 1e-12))) * cos(2.0 * pi() * random()))
            * (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'icu_sd')
            + (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'icu_mean'),
            0.1
        ), 2) AS icu_los_days_sim
    FROM mimiciv_derived.stroke_icu_hadm h
),
base AS (
    SELECT
        a.*,
        (i.hadm_id IS NOT NULL) AS icu_flag,
        i.icu_los_days_sim,
        GREATEST(
            (sqrt(-2.0 * LN(GREATEST(random(), 1e-12))) * cos(2.0 * pi() * random()))
            * (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_sd')
            + (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_mean'),
            0.1
        ) AS hosp_raw
    FROM mimiciv_derived.stroke_admission a
    LEFT JOIN icu i
        ON a.subject_id = i.subject_id AND a.hadm_id = i.hadm_id
),
iter AS (
    SELECT
        b.*,
        CASE WHEN b.icu_flag THEN GREATEST(b.hosp_raw, b.icu_los_days_sim) ELSE b.hosp_raw END AS hosp_v
    FROM base b
),
adj1 AS (
    SELECT
        i.*,
        GREATEST(
            0.1,
            CASE
                WHEN stddev(hosp_v) OVER () > 0 THEN
                    (hosp_v - avg(hosp_v) OVER ())
                    / stddev(hosp_v) OVER ()
                    * (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_sd')
                    + (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_mean')
                ELSE hosp_v
            END
        ) AS hosp_a1
    FROM iter i
),
adj2 AS (
    SELECT
        a1.*,
        CASE WHEN icu_flag THEN GREATEST(hosp_a1, icu_los_days_sim) ELSE hosp_a1 END AS hosp_a2
    FROM adj1 a1
),
adj3 AS (
    SELECT
        a2.*,
        GREATEST(
            0.1,
            CASE
                WHEN stddev(hosp_a2) OVER () > 0 THEN
                    (hosp_a2 - avg(hosp_a2) OVER ())
                    / stddev(hosp_a2) OVER ()
                    * (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_sd')
                    + (SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'hosp_mean')
                ELSE hosp_a2
            END
        ) AS hosp_a3
    FROM adj2 a2
)
SELECT
    subject_id,
    hadm_id,
    admittime,
    dischtime,
    deathtime,
    admission_type,
    hospital_expire_flag,
    icd_code,
    seq_num,
    icd_version,
    stroke_type,
    icu_flag,
    icu_los_days_sim,
    ROUND(
        CASE WHEN icu_flag THEN GREATEST(hosp_a3, icu_los_days_sim) ELSE hosp_a3 END,
        2
    ) AS hosp_los_days
FROM adj3;

-- 死亡约束：住院时长不超过 d_time
UPDATE mimiciv_derived.stroke_admission_sim a
SET hosp_los_days = LEAST(a.hosp_los_days, p.d_time)
FROM mimiciv_derived.stroke_patients_sim p
WHERE a.subject_id = p.subject_id
  AND p.died_flag_original
  AND p.d_time IS NOT NULL;

-- 重建 dischtime
UPDATE mimiciv_derived.stroke_admission_sim
SET dischtime = admittime + (hosp_los_days || ' days')::INTERVAL
WHERE admittime IS NOT NULL AND hosp_los_days IS NOT NULL;

-- =============================================================================
-- 步骤 7：提取 labevents / chartevents（数值型 valuenum）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_labevent_raw CASCADE;
CREATE TABLE mimiciv_derived.stroke_labevent_raw AS
SELECT
    l.subject_id,
    l.hadm_id,
    l.charttime AS charttime_raw,
    l.itemid,
    l.valuenum
FROM mimiciv_hosp.labevents l
INNER JOIN mimiciv_derived.stroke_dx s
    ON l.hadm_id = s.hadm_id
INNER JOIN mimiciv_derived._itemids it
    ON l.itemid = it.itemid AND it.source = 'lab'
WHERE l.valuenum IS NOT NULL
  AND l.hadm_id IS NOT NULL;

DROP TABLE IF EXISTS mimiciv_derived.stroke_chartevent_raw CASCADE;
CREATE TABLE mimiciv_derived.stroke_chartevent_raw AS
SELECT
    c.subject_id,
    c.hadm_id,
    c.charttime AS charttime_raw,
    c.itemid,
    c.valuenum
FROM mimiciv_icu.chartevents c
INNER JOIN mimiciv_derived.stroke_dx s
    ON c.hadm_id = s.hadm_id
INNER JOIN mimiciv_derived._itemids it
    ON c.itemid = it.itemid AND it.source = 'chart'
WHERE c.valuenum IS NOT NULL
  AND c.hadm_id IS NOT NULL;

-- =============================================================================
-- 步骤 8：按入院时间窗等间隔重建 charttime（lab + chart 同规则）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_events_long CASCADE;
CREATE TABLE mimiciv_derived.stroke_events_long AS
WITH adm_ref AS (
    SELECT subject_id, hadm_id, admittime, dischtime
    FROM mimiciv_derived.stroke_admission_sim
),
lab_ranked AS (
    SELECT
        e.subject_id,
        e.hadm_id,
        e.itemid,
        e.valuenum,
        e.charttime_raw,
        a.admittime,
        a.dischtime,
        ROW_NUMBER() OVER (
            PARTITION BY e.subject_id, e.hadm_id
            ORDER BY e.charttime_raw
        ) AS rec_idx,
        COUNT(*) OVER (PARTITION BY e.subject_id, e.hadm_id) AS n_rec
    FROM mimiciv_derived.stroke_labevent_raw e
    INNER JOIN adm_ref a
        ON e.subject_id = a.subject_id AND e.hadm_id = a.hadm_id
),
lab_time AS (
    SELECT
        subject_id,
        hadm_id,
        itemid,
        valuenum,
        CASE
            WHEN admittime IS NULL THEN NULL::TIMESTAMP
            WHEN n_rec = 1 THEN admittime
            WHEN dischtime IS NULL OR dischtime <= admittime THEN admittime
            ELSE admittime + (
                (rec_idx - 1)::DOUBLE PRECISION
                / NULLIF(n_rec - 1, 0)
            ) * (dischtime - admittime)
        END AS charttime
    FROM lab_ranked
),
chart_ranked AS (
    SELECT
        e.subject_id,
        e.hadm_id,
        e.itemid,
        e.valuenum,
        e.charttime_raw,
        a.admittime,
        a.dischtime,
        ROW_NUMBER() OVER (
            PARTITION BY e.subject_id, e.hadm_id
            ORDER BY e.charttime_raw
        ) AS rec_idx,
        COUNT(*) OVER (PARTITION BY e.subject_id, e.hadm_id) AS n_rec
    FROM mimiciv_derived.stroke_chartevent_raw e
    INNER JOIN adm_ref a
        ON e.subject_id = a.subject_id AND e.hadm_id = a.hadm_id
),
chart_time AS (
    SELECT
        subject_id,
        hadm_id,
        itemid,
        valuenum,
        CASE
            WHEN admittime IS NULL THEN NULL::TIMESTAMP
            WHEN n_rec = 1 THEN admittime
            WHEN dischtime IS NULL OR dischtime <= admittime THEN admittime
            ELSE admittime + (
                (rec_idx - 1)::DOUBLE PRECISION
                / NULLIF(n_rec - 1, 0)
            ) * (dischtime - admittime)
        END AS charttime
    FROM chart_ranked
)
SELECT * FROM lab_time
UNION ALL
SELECT * FROM chart_time;

-- =============================================================================
-- 步骤 9：日内去重（subject_id + itemid + 日期，保留最早一条）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_events_dedup CASCADE;
CREATE TABLE mimiciv_derived.stroke_events_dedup AS
SELECT DISTINCT ON (subject_id, itemid, charttime::DATE)
    subject_id,
    hadm_id,
    itemid,
    valuenum,
    charttime
FROM mimiciv_derived.stroke_events_long
WHERE charttime IS NOT NULL
ORDER BY subject_id, itemid, charttime::DATE, charttime;

-- =============================================================================
-- 步骤 10：患者纳入 —— ≥70% 监测变量各自 ≥3 次（按 subject_id 计数）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_patient_eligible CASCADE;
CREATE TABLE mimiciv_derived.stroke_patient_eligible AS
WITH n_items AS (
    SELECT COUNT(DISTINCT itemid)::INTEGER AS n_item_total
    FROM mimiciv_derived._itemids
    WHERE use_filter
),
cnt AS (
    SELECT
        e.subject_id,
        e.itemid,
        COUNT(*)::INTEGER AS n_meas
    FROM mimiciv_derived.stroke_events_dedup e
    INNER JOIN mimiciv_derived._itemids it
        ON e.itemid = it.itemid AND it.use_filter
    GROUP BY e.subject_id, e.itemid
),
ratio AS (
    SELECT
        c.subject_id,
        COUNT(DISTINCT c.itemid) FILTER (WHERE c.n_meas >= 3) AS n_item_ge3,
        ni.n_item_total
    FROM cnt c
    CROSS JOIN n_items ni
    GROUP BY c.subject_id, ni.n_item_total
)
SELECT subject_id
FROM ratio r
CROSS JOIN mimiciv_derived._params p
WHERE p.key = 'patient_ge3_ratio'
  AND r.n_item_ge3::DOUBLE PRECISION / r.n_item_total
      >= p.value::DOUBLE PRECISION;

-- =============================================================================
-- 步骤 11：筛选事件 + 生成 visit + 宽表（pivot）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.stroke_events_filtered CASCADE;
CREATE TABLE mimiciv_derived.stroke_events_filtered AS
SELECT e.*
FROM mimiciv_derived.stroke_events_dedup e
INNER JOIN mimiciv_derived.stroke_patient_eligible pe
    ON e.subject_id = pe.subject_id;

DROP TABLE IF EXISTS mimiciv_derived.stroke_events_visit CASCADE;
CREATE TABLE mimiciv_derived.stroke_events_visit AS
SELECT
    subject_id,
    hadm_id,
    itemid,
    charttime,
    valuenum,
    ROW_NUMBER() OVER (
        PARTITION BY subject_id, hadm_id, itemid
        ORDER BY charttime
    )::INTEGER AS visit
FROM mimiciv_derived.stroke_events_filtered;

DROP TABLE IF EXISTS mimiciv_derived.stroke_visit_grid CASCADE;
CREATE TABLE mimiciv_derived.stroke_visit_grid AS
SELECT
    subject_id,
    hadm_id,
    generate_series(1, MAX(visit))::INTEGER AS visit
FROM mimiciv_derived.stroke_events_visit
GROUP BY subject_id, hadm_id;

DROP TABLE IF EXISTS mimiciv_derived.data_0506_wide_raw CASCADE;
CREATE TABLE mimiciv_derived.data_0506_wide_raw AS
SELECT
    g.subject_id,
    g.hadm_id,
    g.visit,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50912)  AS item_50912,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50971)  AS item_50971,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50983)  AS item_50983,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 51221)  AS item_51221,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 51265)  AS item_51265,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 220045) AS item_220045,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 220181) AS item_220181,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 220210) AS item_220210,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 220739) AS item_220739,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 223835) AS item_223835,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 223900) AS item_223900,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 223901) AS item_223901,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50820)  AS item_50820,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50821)  AS item_50821,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50882)  AS item_50882,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 50885)  AS item_50885,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 51301)  AS item_51301,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 220052) AS item_220052,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 223762) AS item_223762,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 225312) AS item_225312,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 224322) AS item_224322,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 51300)  AS item_51300,
    MAX(v.valuenum) FILTER (WHERE v.itemid = 223761) AS item_223761
FROM mimiciv_derived.stroke_visit_grid g
LEFT JOIN mimiciv_derived.stroke_events_visit v
    ON g.subject_id = v.subject_id
   AND g.hadm_id = v.hadm_id
   AND g.visit = v.visit
GROUP BY g.subject_id, g.hadm_id, g.visit;

-- 华氏 → 摄氏（与 0424 一致）
UPDATE mimiciv_derived.data_0506_wide_raw
SET item_223762 = COALESCE(item_223762, (item_223761 - 32.0) / 1.8);

ALTER TABLE mimiciv_derived.data_0506_wide_raw DROP COLUMN IF EXISTS item_223761;

-- =============================================================================
-- 步骤 12：列中位数插补（替代 missRanger）
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.data_0506_imputed CASCADE;
CREATE TABLE mimiciv_derived.data_0506_imputed AS
SELECT
    w.*,
    COALESCE(item_50912,  med.item_50912)  AS item_50912_i,
    COALESCE(item_50971,  med.item_50971)  AS item_50971_i,
    COALESCE(item_50983,  med.item_50983)  AS item_50983_i,
    COALESCE(item_51221,  med.item_51221)  AS item_51221_i,
    COALESCE(item_51265,  med.item_51265)  AS item_51265_i,
    COALESCE(item_220045, med.item_220045) AS item_220045_i,
    COALESCE(item_220181, med.item_220181) AS item_220181_i,
    COALESCE(item_220210, med.item_220210) AS item_220210_i,
    COALESCE(item_220739, med.item_220739) AS item_220739_i,
    COALESCE(item_223835, med.item_223835) AS item_223835_i,
    COALESCE(item_223900, med.item_223900) AS item_223900_i,
    COALESCE(item_223901, med.item_223901) AS item_223901_i,
    COALESCE(item_50820,  med.item_50820)  AS item_50820_i,
    COALESCE(item_50821,  med.item_50821)  AS item_50821_i,
    COALESCE(item_50882,  med.item_50882)  AS item_50882_i,
    COALESCE(item_50885,  med.item_50885)  AS item_50885_i,
    COALESCE(item_51301,  med.item_51301)  AS item_51301_i,
    COALESCE(item_220052, med.item_220052) AS item_220052_i,
    COALESCE(item_223762, med.item_223762) AS item_223762_i,
    COALESCE(item_225312, med.item_225312) AS item_225312_i,
    COALESCE(item_224322, med.item_224322) AS item_224322_i,
    COALESCE(item_51300,  med.item_51300)  AS item_51300_i
FROM mimiciv_derived.data_0506_wide_raw w
CROSS JOIN (
    SELECT
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50912)  AS item_50912,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50971)  AS item_50971,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50983)  AS item_50983,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_51221)  AS item_51221,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_51265)  AS item_51265,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_220045) AS item_220045,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_220181) AS item_220181,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_220210) AS item_220210,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_220739) AS item_220739,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_223835) AS item_223835,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_223900) AS item_223900,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_223901) AS item_223901,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50820)  AS item_50820,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50821)  AS item_50821,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50882)  AS item_50882,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_50885)  AS item_50885,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_51301)  AS item_51301,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_220052) AS item_220052,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_223762) AS item_223762,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_225312) AS item_225312,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_224322) AS item_224322,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY item_51300)  AS item_51300
    FROM mimiciv_derived.data_0506_wide_raw
) med;

DROP TABLE IF EXISTS mimiciv_derived.data_0506_wide_imputed CASCADE;
CREATE TABLE mimiciv_derived.data_0506_wide_imputed AS
SELECT
    subject_id,
    hadm_id,
    visit,
    item_50912_i  AS item_50912,
    item_50971_i  AS item_50971,
    item_50983_i  AS item_50983,
    item_51221_i  AS item_51221,
    item_51265_i  AS item_51265,
    item_220045_i AS item_220045,
    item_220181_i AS item_220181,
    item_220210_i AS item_220210,
    item_220739_i AS item_220739,
    item_223835_i AS item_223835,
    item_223900_i AS item_223900,
    item_223901_i AS item_223901,
    item_50820_i  AS item_50820,
    item_50821_i  AS item_50821,
    item_50882_i  AS item_50882,
    item_50885_i  AS item_50885,
    item_51301_i  AS item_51301,
    item_220052_i AS item_220052,
    item_223762_i AS item_223762,
    item_225312_i AS item_225312,
    item_224322_i AS item_224322,
    item_51300_i  AS item_51300
FROM mimiciv_derived.data_0506_imputed;

-- =============================================================================
-- 步骤 13：合并 age / survival / survival_time_days
-- =============================================================================
DROP TABLE IF EXISTS mimiciv_derived.data_0506_baseline CASCADE;
CREATE TABLE mimiciv_derived.data_0506_baseline AS
SELECT
    w.*,
    p.anchor_age::DOUBLE PRECISION AS age,
    CASE
        WHEN p.dod IS NOT NULL AND p.baseline_date IS NOT NULL
        THEN (p.dod - p.baseline_date)::INTEGER::DOUBLE PRECISION
        ELSE NULL
    END AS survival_time_days,
    CASE
        WHEN p.dod IS NOT NULL AND p.baseline_date IS NOT NULL THEN 1
        ELSE 0
    END::INTEGER AS survival
FROM mimiciv_derived.data_0506_wide_imputed w
LEFT JOIN mimiciv_derived.stroke_patients_sim p
    ON w.subject_id = p.subject_id;

-- =============================================================================
-- 步骤 14：入院层面 hosp_time（0506；seed=20260506）+ 行内 time
--     使用 plpgsql 复现 R 的存活者整数平衡与 Dirichlet 型 time
-- =============================================================================
CREATE OR REPLACE FUNCTION mimiciv_derived.random_normal()
RETURNS DOUBLE PRECISION
LANGUAGE sql
VOLATILE
AS $$
    SELECT sqrt(-2.0 * ln(greatest(random(), 1e-12)))
         * cos(2.0 * pi() * random());
$$;

DROP TABLE IF EXISTS mimiciv_derived._adm_hosp CASCADE;
CREATE TABLE mimiciv_derived._adm_hosp (
    rn                  SERIAL PRIMARY KEY,
    subject_id          INTEGER NOT NULL,
    hadm_id             INTEGER NOT NULL,
    survival            INTEGER NOT NULL,
    survival_time_days  DOUBLE PRECISION,
    hosp_time           INTEGER
);

INSERT INTO mimiciv_derived._adm_hosp (subject_id, hadm_id, survival, survival_time_days)
SELECT DISTINCT subject_id, hadm_id, survival, survival_time_days
FROM mimiciv_derived.data_0506_baseline;

SELECT setseed((SELECT value::DOUBLE PRECISION FROM mimiciv_derived._params WHERE key = 'seed_hosp_time'));

DO $$
DECLARE
    v_n          INTEGER;
    v_nd         INTEGER;
    v_na         INTEGER;
    v_s_target   INTEGER;
    v_sa         INTEGER;
    v_mu         DOUBLE PRECISION := 13.91;
    v_sd         DOUBLE PRECISION := 12.44;
    v_m_a        DOUBLE PRECISION;
    v_ss_tot     DOUBLE PRECISION;
    v_ss_dead    DOUBLE PRECISION;
    v_qa         DOUBLE PRECISION;
    v_inner      DOUBLE PRECISION;
    v_c_sd       DOUBLE PRECISION;
    v_sum_z2     DOUBLE PRECISION;
    v_diff       INTEGER;
    v_need       INTEGER;
    v_spare      INTEGER;
    v_j          INTEGER;
    rec          RECORD;
    z_arr        DOUBLE PRECISION[];
    y_val        DOUBLE PRECISION;
    alive_arr    INTEGER[];
    i            INTEGER;
    j            INTEGER;
BEGIN
    SELECT COUNT(*) INTO v_n FROM mimiciv_derived._adm_hosp;
    SELECT COUNT(*) INTO v_nd FROM mimiciv_derived._adm_hosp WHERE survival = 1;
    v_na := v_n - v_nd;
    v_s_target := ROUND(v_mu * v_n)::INTEGER;

    -- 死亡者
    UPDATE mimiciv_derived._adm_hosp
    SET hosp_time = ROUND(survival_time_days)::INTEGER
    WHERE survival = 1 AND survival_time_days IS NOT NULL;

    IF v_na > 0 THEN
        SELECT v_s_target - COALESCE(SUM(hosp_time), 0)
        INTO v_sa
        FROM mimiciv_derived._adm_hosp
        WHERE survival = 1;

        IF v_na = 1 THEN
            UPDATE mimiciv_derived._adm_hosp
            SET hosp_time = v_sa
            WHERE survival = 0;
        ELSE
            v_m_a := v_sa::DOUBLE PRECISION / v_na;
            v_ss_tot := (v_n - 1) * v_sd * v_sd;
            SELECT COALESCE(SUM((hosp_time - v_mu)^2), 0) INTO v_ss_dead
            FROM mimiciv_derived._adm_hosp WHERE survival = 1;
            v_qa := v_ss_tot - v_ss_dead;
            v_inner := v_qa - v_na * (v_m_a - v_mu)^2;
            IF v_inner < 0 OR v_inner IS NULL THEN
                v_inner := 0;
            END IF;
            v_c_sd := sqrt(v_inner / (v_na - 1));

            z_arr := ARRAY[]::DOUBLE PRECISION[];
            FOR i IN 1..v_na LOOP
                z_arr := array_append(z_arr, mimiciv_derived.random_normal());
            END LOOP;
            -- 中心化并缩放
            SELECT AVG(x), SUM(x*x) INTO v_mu, v_sum_z2 FROM unnest(z_arr) AS x;
            FOR i IN 1..v_na LOOP
                z_arr[i] := z_arr[i] - v_mu;
            END LOOP;
            IF v_sum_z2 = 0 THEN
                FOR i IN 1..v_na LOOP
                    z_arr[i] := CASE WHEN i % 2 = 1 THEN -1 ELSE 1 END;
                END LOOP;
            ELSE
                FOR i IN 1..v_na LOOP
                    z_arr[i] := z_arr[i] / sqrt(v_sum_z2) * sqrt(v_na - 1);
                END LOOP;
            END IF;

            alive_arr := ARRAY[]::INTEGER[];
            i := 0;
            FOR rec IN
                SELECT rn FROM mimiciv_derived._adm_hosp WHERE survival = 0 ORDER BY rn
            LOOP
                i := i + 1;
                y_val := v_m_a + v_c_sd * z_arr[i];
                alive_arr := array_append(alive_arr, GREATEST(ROUND(y_val)::INTEGER, 1));
            END LOOP;

            -- 整数平衡
            v_diff := v_sa - (SELECT SUM(x) FROM unnest(alive_arr) AS x);
            IF v_diff > 0 THEN
                FOR j IN 1..v_diff LOOP
                    alive_arr[1 + floor(random() * v_na)::INTEGER] :=
                        alive_arr[1 + floor(random() * v_na)::INTEGER] + 1;
                END LOOP;
            ELSIF v_diff < 0 THEN
                v_need := -v_diff;
                SELECT COALESCE(SUM(a - 1), 0) INTO v_spare FROM unnest(alive_arr) AS a;
                WHILE v_need > 0 LOOP
                    FOR i IN 1..v_na LOOP
                        IF alive_arr[i] > 1 AND v_need > 0 THEN
                            IF random() < 0.5 THEN
                                alive_arr[i] := alive_arr[i] - 1;
                                v_need := v_need - 1;
                            END IF;
                        END IF;
                    END LOOP;
                    IF v_need > 0 AND v_spare = 0 THEN
                        EXIT;
                    END IF;
                END LOOP;
            END IF;

            i := 0;
            FOR rec IN
                SELECT rn FROM mimiciv_derived._adm_hosp WHERE survival = 0 ORDER BY rn
            LOOP
                i := i + 1;
                UPDATE mimiciv_derived._adm_hosp
                SET hosp_time = alive_arr[i]
                WHERE rn = rec.rn;
            END LOOP;
        END IF;
    END IF;
END $$;

-- time：按 (subject_id, hadm_id) 组内 Gamma→Dirichlet 分割（与 0506处理.R 一致）
DROP TABLE IF EXISTS mimiciv_derived._time_sim CASCADE;
CREATE TABLE mimiciv_derived._time_sim (
    subject_id  INTEGER NOT NULL,
    hadm_id     INTEGER NOT NULL,
    visit       INTEGER NOT NULL,
    time        DOUBLE PRECISION,
    PRIMARY KEY (subject_id, hadm_id, visit)
);

DO $$
DECLARE
    grec   RECORD;
    v_n    INTEGER;
    v_h    DOUBLE PRECISION;
    v_lo   DOUBLE PRECISION;
    v_span DOUBLE PRECISION;
    w      DOUBLE PRECISION[];
    wsum   DOUBLE PRECISION;
    cum    DOUBLE PRECISION;
    i      INTEGER;
    tval   DOUBLE PRECISION;
BEGIN
    FOR grec IN
        SELECT b.subject_id, b.hadm_id, h.hosp_time, MAX(b.visit) AS n_visits
        FROM mimiciv_derived.data_0506_baseline b
        INNER JOIN mimiciv_derived._adm_hosp h
            ON b.subject_id = h.subject_id AND b.hadm_id = h.hadm_id
        GROUP BY b.subject_id, b.hadm_id, h.hosp_time
    LOOP
        v_n := grec.n_visits;
        v_h := grec.hosp_time;
        IF v_n IS NULL OR v_h IS NULL OR v_h <= 0 THEN
            CONTINUE;
        END IF;
        v_lo := greatest(1e-6::DOUBLE PRECISION, v_h * 1e-9);
        v_span := v_h - v_lo - v_lo;  -- hi = H - eps, span = hi - lo
        IF v_span <= 0 THEN
            CONTINUE;
        END IF;

        IF v_n = 1 THEN
            INSERT INTO mimiciv_derived._time_sim VALUES
                (grec.subject_id, grec.hadm_id, 1, v_lo + random() * v_span);
        ELSE
            w := ARRAY[]::DOUBLE PRECISION[];
            FOR i IN 1..(v_n + 1) LOOP
                w := array_append(w, -ln(greatest(random(), 1e-12)));
            END LOOP;
            SELECT SUM(x) INTO wsum FROM unnest(w) x;
            cum := 0;
            FOR i IN 1..v_n LOOP
                cum := cum + w[i] / wsum;
                tval := v_lo + cum * v_span;
                INSERT INTO mimiciv_derived._time_sim
                    (subject_id, hadm_id, visit, time)
                VALUES (grec.subject_id, grec.hadm_id, i, tval);
            END LOOP;
        END IF;
    END LOOP;
END $$;

DROP TABLE IF EXISTS mimiciv_derived.data_0506 CASCADE;

CREATE TABLE mimiciv_derived.data_0506 AS
WITH base AS (
    SELECT
        b.*,
        h.hosp_time
    FROM mimiciv_derived.data_0506_baseline b
    INNER JOIN mimiciv_derived._adm_hosp h
        ON b.subject_id = h.subject_id AND b.hadm_id = h.hadm_id
)
SELECT
    ROW_NUMBER() OVER (ORDER BY b.subject_id, b.hadm_id, b.visit)::INTEGER AS x,
    b.subject_id,
    b.hadm_id,
    b.visit,
    b.item_50912,
    b.item_50971,
    b.item_50983,
    b.item_51221,
    b.item_51265,
    b.item_220045,
    b.item_220181,
    b.item_220210,
    b.item_220739,
    b.item_223835,
    b.item_223900,
    b.item_223901,
    b.item_50820,
    b.item_50821,
    b.item_50882,
    b.item_50885,
    b.item_51301,
    b.item_220052,
    b.item_223762,
    b.item_225312,
    b.item_224322,
    b.item_51300,
    b.age,
    b.survival_time_days,
    b.survival,
    b.hosp_time,
    t.time
FROM base b
LEFT JOIN mimiciv_derived._time_sim t
    ON b.subject_id = t.subject_id
   AND b.hadm_id = t.hadm_id
   AND b.visit = t.visit
ORDER BY b.subject_id, b.hadm_id, b.visit;

-- =============================================================================
-- 完成：查询最终结果（与 data_0506.csv 列一致，不含 CSV 行名列）
-- =============================================================================
-- SELECT * FROM mimiciv_derived.data_0506 LIMIT 20;
-- SELECT COUNT(*) AS n_rows, COUNT(DISTINCT subject_id) AS n_patients FROM mimiciv_derived.data_0506;
