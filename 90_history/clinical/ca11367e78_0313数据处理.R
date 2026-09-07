## 0313 脑卒中患者 labevent 数据筛选脚本
## 根据 0313_stroke_diagnose.xlsx 中的 subject_id
## 从 0313_stroke_labevent.xlsx 中筛选出脑卒中患者的化验记录
## 并将结果保存为 0313_stroke_labevent1.xlsx

## 如未安装以下包，请先运行：
## install.packages(c("readxl", "writexl"))

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)


## 设置工作目录为当前脚本所在的数据处理文件夹
##（如果你是从 RStudio / VSCode 直接在该目录下打开脚本，可以不改）
setwd("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理")

## 1. 读取脑卒中患者诊断表（subject_id 列）
stroke_diag <- read_xlsx("0313_stroke_diagnose.xlsx")

## 2. 读取 labevent 全部记录表
stroke_lab <- read_xlsx("0313_stroke_labevent.xlsx")

## 3. 提取脑卒中患者的 subject_id 列（去重）
## 注意：这里假定列名为 subject_id，如实际列名不同请相应修改
stroke_ids <- unique(stroke_diag$subject_id)

## 4. 从 labevent 中筛选出 subject_id 在脑卒中患者列表中的行
stroke_lab1 <- stroke_lab[stroke_lab$subject_id %in% stroke_ids, ]

## 5. 将筛选后的结果保存为新的 Excel：0313_stroke_labevent1.xlsx
write_xlsx(stroke_lab1, "0313_stroke_labevent1.xlsx")

## 如需简单查看结果维度，可取消下面注释：
## dim(stroke_lab1)

## 6. 统计 0313_stroke_labevent1.xlsx 中独立患者数量
stroke_lab1_reloaded <- read_xlsx("0313_stroke_labevent1.xlsx")
num_patients <- length(unique(stroke_lab1_reloaded$subject_id))
num_patients










cols_to_drop <- intersect(
  names(stroke_lab1),
  c("valuenum", "valueuom", "rafrange", "ref_range", "flag",
    "priority", "comment", "comments")
)
stroke_lab1_clean <- stroke_lab1 %>%
  select(-all_of(cols_to_drop))
## 2. 按 itemid 展开为宽表
## 选择用作“行 ID”的变量（通常至少包含 subject_id / hadm_id / charttime）
id_cols <- intersect(
  names(stroke_lab1_clean),
  c("subject_id", "hadm_id", "stay_id", "specimen_id", "charttime")
)
stroke_lab1_wide <- stroke_lab1_clean %>%
  pivot_wider(
    id_cols      = all_of(id_cols),
    names_from   = itemid,
    values_from  = value,
    names_prefix = "item_"
  )

num_patients <- length(unique(stroke_lab1_wide$subject_id))





