library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

# =======================
# 路径设置
# =======================
dir_result <- "F:/文章_大论文/结果整理"
input_file <- "F:/文章_大论文/结果整理/代码/RESULT_ALL2.XLSX"
# 按「数据类型」循环输出的柱状图
output_dir <- file.path(dir_result, "全局数据集")
# 全数据按模型汇总、X 轴为三指标（模型为分组）的图
output_dir_agg <- file.path(dir_result, "代码", "图像", "汇总数据集")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!dir.exists(output_dir_agg)) {
  dir.create(output_dir_agg, recursive = TRUE)
}









################




