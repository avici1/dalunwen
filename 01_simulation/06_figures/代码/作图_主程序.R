# ==============================================================================
# 读取已保存的论文核心结果表，准备作图
# ==============================================================================

library(readxl)    # 读取 Excel
library(dplyr)     # 数据处理
library(tidyr)     # 数据转换
library(ggplot2)   # 作图

# 文件路径
file_path <- "F:/文章_大论文/结果整理/论文核心结果表.xlsx"

df_censor <- read_xlsx(file_path, sheet = "Wilcoxon_删失率比较")
df_sample  <- read_xlsx(file_path, sheet = "Wilcoxon_样本量比较")
df_cortype <- read_xlsx(file_path, sheet = "Wilcoxon_相关特性比较")
df_latent  <- read_xlsx(file_path, sheet = "Wilcoxon_潜类别比较")
df_friedman_corr <- read_xlsx(file_path, sheet = "Friedman_相关性三水平")


df_censor
df_sample
df_cortype
df_latent
df_friedman_corr




