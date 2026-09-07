-- 创建all_data_stroke表，合并stroke_patients_admissions和labevents数据
-- 注意：由于itemid数量较多，此脚本使用示例itemid进行透视
-- 请根据实际需要调整itemid列表

-- =============================================
-- 步骤1：确保stroke_patients_admissions表存在并包含icd_version字段
-- =============================================

-- 删除已有表（如果存在）
DROP TABLE IF EXISTS stroke_patients;
DROP TABLE IF EXISTS stroke_patients_admissions;

-- 1. 创建脑卒中患者临时表
CREATE TABLE stroke_patients AS
SELECT DISTINCT 
    d.subject_id, 
    d.hadm_id, 
    d.icd_code, 
    d.seq_num, 
    d.icd_version
FROM diagnoses_icd d
WHERE d.icd_version = 10  -- 选择ICD-10编码
  AND (d.icd_code LIKE 'I60%'  -- 蛛网膜下腔出血
       OR d.icd_code LIKE 'I61%'  -- 脑出血
       OR d.icd_code LIKE 'I63%'); -- 脑梗死

-- 2. 与admissions表合并，创建完整的stroke_patients_admissions表
CREATE TABLE stroke_patients_admissions AS
SELECT 
    a.subject_id, 
    a.hadm_id, 
    a.admittime, 
    a.dischtime, 
    a.deathtime, 
    a.admission_type, 
    a.admit_provider_id, 
    a.admission_location, 
    a.discharge_location, 
    a.insurance, 
    a.language, 
    a.marital_status, 
    a.race, 
    a.edregtime, 
    a.edouttime, 
    a.hospital_expire_flag,
    sp.icd_code, 
    sp.seq_num, 
    sp.icd_version, -- 明确包含icd_version字段
    -- 添加脑卒中类型分类
    CASE 
        WHEN sp.icd_code LIKE 'I60%' THEN '蛛网膜下腔出血'
        WHEN sp.icd_code LIKE 'I61%' THEN '脑出血'
        WHEN sp.icd_code LIKE 'I63%' THEN '脑梗死'
        ELSE '其他脑血管疾病'
    END AS stroke_type
FROM admissions a
INNER JOIN stroke_patients sp ON a.hadm_id = sp.hadm_id
ORDER BY a.hadm_id, sp.seq_num;

-- =============================================
-- 步骤2：创建基础的合并表（纵向数据结构）
-- =============================================

-- 删除已有表（如果存在）
DROP TABLE IF EXISTS all_data_stroke_long;

-- 此表保留原始的纵向结构，将stroke信息附加到每个labevent记录
CREATE TABLE all_data_stroke_long AS
SELECT 
    -- labevents表的核心字段
    l.labevent_id,
    l.subject_id,
    l.hadm_id,
    l.itemid,
    l.charttime,
    l.storetime,
    l.value,
    l.valuenum,
    l.valueuom,
    l.ref_range_lower,
    l.ref_range_upper,
    l.flag,
    
    -- stroke_patients_admissions表的字段
    s.admittime,
    s.dischtime,
    s.deathtime,
    s.admission_type,
    s.admit_provider_id,
    s.admission_location,
    s.discharge_location,
    s.insurance,
    s.language,
    s.marital_status,
    s.race,
    s.edregtime,
    s.edouttime,
    s.hospital_expire_flag,
    s.icd_code,
    s.seq_num,
    s.icd_version,
    s.stroke_type
    
FROM labevents l
INNER JOIN stroke_patients_admissions s ON l.hadm_id = s.hadm_id
ORDER BY l.subject_id, l.hadm_id, l.charttime, l.itemid;

-- =============================================
-- 步骤3：创建透视表（将itemid转换为列）
-- =============================================

-- 注意：由于itemid数量非常多，这里只选择常用的几个作为示例
-- 用户可以根据需要扩展itemid列表

DROP TABLE IF EXISTS all_data_stroke;

CREATE TABLE all_data_stroke AS
SELECT 
    subject_id,
    hadm_id,
    charttime,
    storetime,
    
    -- 基本住院信息
    admittime,
    dischtime,
    deathtime,
    admission_type,
    admission_location,
    discharge_location,
    insurance,
    language,
    marital_status,
    race,
    hospital_expire_flag,
    icd_code,
    seq_num,
    icd_version,
    stroke_type,
    
    -- 常用实验室指标（作为列）
    -- 血糖 (itemid: 50931)
    MAX(CASE WHEN itemid = 50931 THEN value END) AS glucose_value,
    MAX(CASE WHEN itemid = 50931 THEN valueuom END) AS glucose_valueuom,
    MAX(CASE WHEN itemid = 50931 THEN ref_range_lower END) AS glucose_ref_lower,
    MAX(CASE WHEN itemid = 50931 THEN ref_range_upper END) AS glucose_ref_upper,
    
    -- 肌酐 (itemid: 50912)
    MAX(CASE WHEN itemid = 50912 THEN value END) AS creatinine_value,
    MAX(CASE WHEN itemid = 50912 THEN valueuom END) AS creatinine_valueuom,
    MAX(CASE WHEN itemid = 50912 THEN ref_range_lower END) AS creatinine_ref_lower,
    MAX(CASE WHEN itemid = 50912 THEN ref_range_upper END) AS creatinine_ref_upper,
    
    -- 尿素氮 (itemid: 51006)
    MAX(CASE WHEN itemid = 51006 THEN value END) AS bun_value,
    MAX(CASE WHEN itemid = 51006 THEN valueuom END) AS bun_valueuom,
    MAX(CASE WHEN itemid = 51006 THEN ref_range_lower END) AS bun_ref_lower,
    MAX(CASE WHEN itemid = 51006 THEN ref_range_upper END) AS bun_ref_upper,
    
    -- 血红蛋白 (itemid: 51222)
    MAX(CASE WHEN itemid = 51222 THEN value END) AS hb_value,
    MAX(CASE WHEN itemid = 51222 THEN valueuom END) AS hb_valueuom,
    MAX(CASE WHEN itemid = 51222 THEN ref_range_lower END) AS hb_ref_lower,
    MAX(CASE WHEN itemid = 51222 THEN ref_range_upper END) AS hb_ref_upper,
    
    -- 白细胞计数 (itemid: 51300)
    MAX(CASE WHEN itemid = 51300 THEN value END) AS wbc_value,
    MAX(CASE WHEN itemid = 51300 THEN valueuom END) AS wbc_valueuom,
    MAX(CASE WHEN itemid = 51300 THEN ref_range_lower END) AS wbc_ref_lower,
    MAX(CASE WHEN itemid = 51300 THEN ref_range_upper END) AS wbc_ref_upper,
    
    -- 钠 (itemid: 50983)
    MAX(CASE WHEN itemid = 50983 THEN value END) AS na_value,
    MAX(CASE WHEN itemid = 50983 THEN valueuom END) AS na_valueuom,
    MAX(CASE WHEN itemid = 50983 THEN ref_range_lower END) AS na_ref_lower,
    MAX(CASE WHEN itemid = 50983 THEN ref_range_upper END) AS na_ref_upper,
    
    -- 钾 (itemid: 50971)
    MAX(CASE WHEN itemid = 50971 THEN value END) AS k_value,
    MAX(CASE WHEN itemid = 50971 THEN valueuom END) AS k_valueuom,
    MAX(CASE WHEN itemid = 50971 THEN ref_range_lower END) AS k_ref_lower,
    MAX(CASE WHEN itemid = 50971 THEN ref_range_upper END) AS k_ref_upper
    
FROM all_data_stroke_long
GROUP BY 
    subject_id,
    hadm_id,
    charttime,
    storetime,
    admittime,
    dischtime,
    deathtime,
    admission_type,
    admission_location,
    discharge_location,
    insurance,
    language,
    marital_status,
    race,
    hospital_expire_flag,
    icd_code,
    seq_num,
    icd_version,
    stroke_type
ORDER BY 
    subject_id,
    hadm_id,
    charttime;

-- =============================================
-- 步骤3：创建索引以提高查询性能
-- =============================================

-- 为常用查询字段创建索引
CREATE INDEX idx_all_data_stroke_subject_id ON all_data_stroke(subject_id);
CREATE INDEX idx_all_data_stroke_hadm_id ON all_data_stroke(hadm_id);
CREATE INDEX idx_all_data_stroke_charttime ON all_data_stroke(charttime);
CREATE INDEX idx_all_data_stroke_itemid ON all_data_stroke_long(itemid);

-- =============================================
-- 步骤4：导出结果到CSV文件
-- =============================================

-- PostgreSQL导出命令
/*
COPY all_data_stroke 
TO 'f:\文章_大论文\MIMIC数据库_代码\TREA代码\all_data_stroke.csv'
DELIMITER ',' 
CSV HEADER;
*/

-- MySQL导出命令
/*
SELECT * 
INTO OUTFILE 'f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\all_data_stroke.csv'
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
FROM all_data_stroke;
*/

-- =============================================
-- 扩展：动态生成透视查询
-- =============================================

/*
-- 获取所有唯一的itemid列表
SELECT DISTINCT itemid 
FROM labevents 
ORDER BY itemid;

-- 根据itemid列表动态生成透视查询
-- 注意：此为伪代码，需要在外部程序中执行
*/

-- =============================================
-- 查看结果示例
-- =============================================

-- 查看表结构
PRAGMA table_info('all_data_stroke');

-- 查看前10条记录
SELECT * FROM all_data_stroke LIMIT 10;

-- 统计记录数
SELECT COUNT(*) FROM all_data_stroke;

-- =============================================
-- 注意事项
-- =============================================
-- 1. 此脚本使用示例itemid进行透视，实际使用时请根据需要扩展
-- 2. 透视操作会产生大量NULL值，这是正常现象
-- 3. 建议根据实际分析需求选择相关的itemid
-- 4. 如遇性能问题，可考虑添加更多索引或分区表
