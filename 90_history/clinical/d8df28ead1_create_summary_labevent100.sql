-- 从labevent中筛选出summary_lab_100_with_rank中包含的100个itemid的记录
-- 创建summary_labevent100表保存结果

-- =============================================
-- 步骤1：创建summary_labevent100表
-- =============================================

-- 删除已有表（如果存在）
DROP TABLE IF EXISTS summary_labevent100;

-- 创建筛选后的实验室检查记录表
CREATE TABLE summary_labevent100 AS
SELECT 
    l.*  -- 选择labevent表中的所有字段
FROM labevents l
INNER JOIN summary_lab_100_with_rank r ON l.itemid = r.itemid  -- 使用INNER JOIN筛选出匹配的itemid
ORDER BY 
    l.itemid,  -- 按itemid排序
    l.subject_id,  -- 然后按患者ID排序
    l.hadm_id,  -- 然后按住院ID排序
    l.charttime;  -- 最后按时间排序

-- 或者使用IN子查询（功能相同，性能可能略有差异）
/*
CREATE TABLE summary_labevent100 AS
SELECT 
    *
FROM labevents
WHERE itemid IN (SELECT itemid FROM summary_lab_100_with_rank)
ORDER BY itemid, subject_id, hadm_id, charttime;
*/

-- =============================================
-- 步骤2：验证结果
-- =============================================

-- 查看summary_labevent100表的结构
-- 请根据您使用的数据库系统选择相应的命令：

-- SQLite命令
-- PRAGMA table_info('summary_labevent100');

-- PostgreSQL命令
-- SELECT * FROM information_schema.columns WHERE table_name = 'summary_labevent100';

-- MySQL命令
DESCRIBE summary_labevent100;

-- 查看前10条记录
SELECT * FROM summary_labevent100 LIMIT 10;

-- 查看记录数量
SELECT COUNT(*) AS total_records FROM summary_labevent100;

-- 验证是否只包含100个itemid
SELECT COUNT(DISTINCT itemid) AS unique_item_count FROM summary_labevent100;

-- 查看每个itemid的记录数量
SELECT 
    itemid,
    COUNT(*) AS record_count
FROM summary_labevent100
GROUP BY itemid
ORDER BY record_count DESC;

-- =============================================
-- 步骤3：导出结果到CSV文件
-- =============================================

-- SQLite导出命令
/*
.mode csv
.output 'f:/文章_大论文/MIMIC数据库_代码/TREA代码/summary_labevent100.csv'
SELECT * FROM summary_labevent100;
.output stdout
*/

-- PostgreSQL导出命令
/*
COPY summary_labevent100 
TO 'f:\文章_大论文\MIMIC数据库_代码\TREA代码\summary_labevent100.csv'
DELIMITER ',' 
CSV HEADER;
*/

-- MySQL导出命令
/*
SELECT * 
INTO OUTFILE 'f:\\文章_大论文\\MIMIC数据库_代码\\TREA代码\\summary_labevent100.csv'
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"' 
LINES TERMINATED BY '\n'
FROM summary_labevent100;
*/

-- =============================================
-- 扩展：创建带排名信息的版本
-- =============================================

-- 可选：创建包含排名信息的版本
/*
DROP TABLE IF EXISTS summary_labevent100_with_rank;

CREATE TABLE summary_labevent100_with_rank AS
SELECT 
    l.*,
    r.rank  -- 包含原始的排名信息
FROM labevents l
INNER JOIN summary_lab_100_with_rank r ON l.itemid = r.itemid
ORDER BY 
    r.rank,  -- 按原始排名排序
    l.subject_id,
    l.hadm_id,
    l.charttime;

-- 查看带排名的版本
SELECT * FROM summary_labevent100_with_rank LIMIT 10;
*/
