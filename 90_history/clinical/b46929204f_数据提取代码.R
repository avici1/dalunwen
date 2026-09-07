# MIMIC数据库脑卒中病例纵向数据提取脚本
# 作者: [您的姓名]
# 日期: [当前日期]

# 安装并加载必要的包
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DBI")) install.packages("DBI")
if (!require("RPostgreSQL")) install.packages("RPostgreSQL")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(lubridate)

# 设置工作目录
setwd("f:/文章_大论文/MIMIC数据库_代码/TREA代码")

# 1. 连接到MIMIC数据库
# 注意：请根据您的数据库设置修改以下参数
con <- dbConnect(
  drv = PostgreSQL(),
  dbname = "mimic",
  host = "localhost",
  port = 5432,
  user = "your_username",
  password = "your_password"
)

# 2. 读取SQL查询文件
# 根据使用的数据库版本选择：MIMIC-III 或 MIMIC-IV
mimic_version <- "MIMIC-III"  # 可改为 "MIMIC-IV"
sql_file <- ifelse(mimic_version == "MIMIC-IV", 
                   "stroke_longitudinal_data_mimic4.sql",
                   "stroke_longitudinal_data.sql")
query <- readr::read_file(file.path(getwd(), sql_file))

# 3. 执行查询并获取结果
stroke_data <- dbGetQuery(con, query)

# 4. 数据处理和转换
# 将事件时间转换为日期时间格式
stroke_data <- stroke_data %>%
  mutate(event_time = as.POSIXct(event_time, format = "%Y-%m-%d %H:%M:%S"))

# 计算事件相对于入院时间的小时数
hadm_ids <- unique(stroke_data$hadm_id)
admissions_table <- ifelse(mimic_version == "MIMIC-IV", "mimiciv_hosp.admissions", "admissions")
admit_col <- ifelse(mimic_version == "MIMIC-IV", "admittime AS admitime", "admittime")
admission_query <- sprintf(
  "SELECT hadm_id, %s FROM %s WHERE hadm_id IN (%s)",
  admit_col, admissions_table, paste(hadm_ids, collapse = ",")
)
admission_times <- dbGetQuery(con, admission_query)

stroke_data <- stroke_data %>%
  left_join(admission_times, by = "hadm_id") %>%
  mutate(
    admitime = as.POSIXct(admitime, format = "%Y-%m-%d %H:%M:%S"),
    hours_from_admission = as.numeric(difftime(event_time, admitime, units = "hours"))
  ) %>%
  select(-admitime)

# 5. 数据可视化（示例）
# 绘制特定患者的心率变化
patient_example <- stroke_data %>%
  filter(subject_id == sample(unique(subject_id), 1),
         data_type == "vital",
         event_name == "Heart Rate")

if (nrow(patient_example) > 0) {
  ggplot(patient_example, aes(x = hours_from_admission, y = valuenum)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(
      title = paste("患者", patient_example$subject_id[1], "心率变化趋势"),
      x = "入院后小时数",
      y = "心率 (次/分钟)"
    ) +
    theme_minimal()
}

# 6. 数据导出（保存到 TREA代码 文件夹）
output_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码"
csv_path <- file.path(output_dir, "stroke_longitudinal_data.csv")
xlsx_path <- file.path(output_dir, "stroke_longitudinal_data.xlsx")

# 导出为CSV文件（适合纵向数据）
readr::write_csv(stroke_data, csv_path)

# 导出为Excel文件
if (!require("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)
write_xlsx(stroke_data, xlsx_path)

# 7. 关闭数据库连接
dbDisconnect(con)

# 显示数据摘要
cat("\n数据提取完成！")
cat(paste("\n提取的记录数:", nrow(stroke_data)))
cat(paste("\n涉及的患者数:", length(unique(stroke_data$subject_id))))
cat(paste("\n涉及的住院次数:", length(unique(stroke_data$hadm_id))))
cat("\n数据已保存到以下位置:")
cat("\nCSV文件:", csv_path)
cat("\nExcel文件:", xlsx_path)
