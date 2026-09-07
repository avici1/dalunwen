-- 从summary_labevent100中筛选出属于stroke_patient的记录
-- 创建summary_stroke_labevent100表保存结果

-- =============================================
-- 步骤1：创建summary_stroke_labevent100表
-- =============================================

-- 删除已有表（如果存在）
DROP TABLE IF EXISTS summary_stroke_labevent100;

-- 创建脑卒中患者的实验室检查记录表
CREATE TABLE summary_stroke_labevent100 AS
SELECT 
    l.*  -- 选择summary_labevent100表中的所有字段
FROM summary_labevent100 l
INNER JOIN stroke_patients p ON l.subject_id = p.subject_id  -- 使用INNER JOIN筛选出匹配的subject_id
ORDER BY 
    l.itemid,  -- 按itemid排序
    l.subject_id,  -- 然后按患者ID排序
    l.hadm_id,  -- 然后按住院ID排序
    l.charttime;  -- 最后按时间排序

-- 或者使用IN子查询（功能相同，性能可能略有差异）
/*
CREATE TABLE summary_stroke_labevent100 AS
SELECT 
    *
FROM summary_labevent100
WHERE subject_id IN (SELECT DISTINCT subject_id FROM stroke_patients)
ORDER BY itemid, subject_id, hadm_id, charttime;
*/

-- =============================================
-- 步骤2：验证结果
-- =============================================

-- 查看summary_stroke_labevent100表的结构
-- 请根据您使用的数据库系统选择相应的命令：

-- SQLite命令
-- PRAGMA table_info('summary_stroke_labevent100');

-- PostgreSQL命令
-- SELECT * FROM information_schema.columns WHERE table_name = 'summary_stroke_labevent100';

-- MySQL命令
DESCRIBE summary_stroke_labevent100;

-- 查看前10条记录
SELECT * FROM summary_stroke_labevent100 LIMIT 10;

-- 查看记录数量
SELECT COUNT(*) AS total_records FROM summary_stroke_labevent100;

-- 统计每个itemid的记录数量
SELECT 
    itemid,
    COUNT(*) AS record_count,
    COUNT(DISTINCT subject_id) AS patient_count,
    COUNT(DISTINCT hadm_id) AS admission_count
FROM summary_stroke_labevent100
GROUP BY itemid
ORDER BY record_count DESC;

-- 验证是否所有记录都属于脑卒中患者
SELECT 
    '总记录数：' || COUNT(*) AS total_records,
    '脑卒中患者记录数：' || COUNT(DISTINCT l.subject_id) AS stroke_patients,
    '原始脑卒中患者数：' || (SELECT COUNT(DISTINCT subject_id) FROM stroke_patients) AS original_stroke_patients
FROM summary_stroke_labevent100 l;

-- =============================================
-- 步骤3：导出结果到CSV文件
-- =============================================

-- SQLite导出命令
/*
.mode csv
.output 'f:/文章_大论文/MIMIC数据库_代码/TREA代码/summary_stroke_labevent100.csv'
SELECT * FROM summary_stroke_labevent100;
.output stdout
*/

-- PostgreSQL导出命令
/*
COPY summary_stroke_labevent100 
TO 'f:\文章_大论文\MIMIC数据库_代码\TREA代码\summary_stroke_labevent100.csv'
DELIMITER ',' 
CSV HEADER;
*/

-- MySQL导出命令
/*
SELECT * 
INTO OUTFILE 'f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\summary_stroke_labevent100.csv'
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
FROM summary_stroke_labevent100;
*/

-- =============================================
-- 扩展：创建带脑卒中类型信息的版本
-- =============================================

-- 可选：创建包含脑卒中类型信息的版本
/*
DROP TABLE IF EXISTS summary_stroke_labevent100_with_type;

CREATE TABLE summary_stroke_labevent100_with_type AS
SELECT 
    l.*,
    a.stroke_type  -- 包含脑卒中类型信息
FROM summary_labevent100 l
INNER JOIN stroke_patients_admissions a ON l.subject_id = a.subject_id AND l.hadm_id = a.hadm_id
ORDER BY 
    l.itemid,
    l.subject_id,
    l.hadm_id,
    l.charttime;

-- 查看带类型信息的版本
SELECT * FROM summary_stroke_labevent100_with_type LIMIT 10;

-- 统计不同脑卒中类型的记录数量
SELECT 
    stroke_type,
    COUNT(*) AS record_count,
    COUNT(DISTINCT subject_id) AS patient_count
FROM summary_stroke_labevent100_with_type
GROUP BY stroke_type
ORDER BY record_count DESC;
*/
