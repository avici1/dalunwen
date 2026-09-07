-- 从summary_stroke_labevent100中筛选特定列
-- 创建summary_stroke_lab100_value表保存结果

-- =============================================
-- 步骤1：创建summary_stroke_lab100_value表
-- =============================================

-- 删除已有表（如果存在）
DROP TABLE IF EXISTS summary_stroke_lab100_value;

-- 创建只包含特定列的表
-- 注意：itemid在请求中列出了两次，这里只保留一次
CREATE TABLE summary_stroke_lab100_value AS
SELECT 
    labevent_id,  -- 实验室检查事件ID
    subject_id,  -- 患者ID
    itemid,  -- 实验室检查项目ID
    charttime,  -- 检查时间
    storetime,  -- 存储时间
    value  -- 检查结果值
FROM summary_stroke_labevent100
ORDER BY 
    itemid,  -- 按itemid排序
    subject_id,  -- 然后按患者ID排序
    charttime;  -- 最后按时间排序

-- =============================================
-- 步骤2：验证结果
-- =============================================

-- 查看summary_stroke_lab100_value表的结构
-- 请根据您使用的数据库系统选择相应的命令：

-- SQLite命令
-- PRAGMA table_info('summary_stroke_lab100_value');

-- PostgreSQL命令
-- SELECT * FROM information_schema.columns WHERE table_name = 'summary_stroke_lab100_value';

-- MySQL命令
DESCRIBE summary_stroke_lab100_value;

-- 查看前10条记录
SELECT * FROM summary_stroke_lab100_value LIMIT 10;

-- 查看记录数量
SELECT COUNT(*) AS total_records FROM summary_stroke_lab100_value;

-- 统计每个itemid的记录数量
SELECT 
    itemid,
    COUNT(*) AS record_count,
    COUNT(DISTINCT subject_id) AS patient_count
FROM summary_stroke_lab100_value
GROUP BY itemid
ORDER BY record_count DESC;

-- =============================================
-- 步骤3：导出结果到CSV文件
-- =============================================

-- SQLite导出命令
/*
.mode csv
.output 'f:/文章_大论文/MIMIC数据库_代码/TREA代码/summary_stroke_lab100_value.csv'
SELECT * FROM summary_stroke_lab100_value;
.output stdout
*/

-- PostgreSQL导出命令
/*
COPY summary_stroke_lab100_value 
TO 'f:\文章_大论文\MIMIC数据库_代码\TREA代码\summary_stroke_lab100_value.csv'
DELIMITER ',' 
CSV HEADER;
*/

-- MySQL导出命令
/*
SELECT * 
INTO OUTFILE 'f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\summary_stroke_lab100_value.csv'
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
FROM summary_stroke_lab100_value;
*/

-- =============================================
-- 扩展：添加更多有用的列（可选）
-- =============================================

-- 可选：如果需要，可以添加更多有用的列，如valueuom（单位）
/*
DROP TABLE IF EXISTS summary_stroke_lab100_value_extended;

CREATE TABLE summary_stroke_lab100_value_extended AS
SELECT 
    labevent_id,
    subject_id,
    itemid,
    charttime,
    storetime,
    value,
    valueuom  -- 添加单位信息
FROM summary_stroke_labevent100
ORDER BY itemid, subject_id, charttime;

-- 查看扩展版本
SELECT * FROM summary_stroke_lab100_value_extended LIMIT 10;
*/
