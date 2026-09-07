library(survival)      # Surv()
library(rms)           # cph()
library(timeROC)       # timeROC()
library(dplyr)         # 基本数据处理
library(joineRML)      # mjoint()
library(readxl)        # read_excel()（如已改用 gdata 可再删）
library(writexl)
library(tidyverse)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)




# 设置输出目录并创建（如果不存在）
out_dir <- "F:/文章/大论文/程序_单一时间点/模拟数据_添加Y"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

######### 低相关 (low correlation) #########

## 500样本-变量间-30%删失
write_xlsx(sim500_30_10V_lowINTER_3c_L1, file.path(out_dir, "sim500_30_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L2, file.path(out_dir, "sim500_30_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L3, file.path(out_dir, "sim500_30_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L4, file.path(out_dir, "sim500_30_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L5, file.path(out_dir, "sim500_30_10V_lowINTER_3c_L5.xlsx"))

## 500样本-变量间-70%删失
write_xlsx(sim500_70_10V_lowINTER_3c_L1, file.path(out_dir, "sim500_70_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L2, file.path(out_dir, "sim500_70_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L3, file.path(out_dir, "sim500_70_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L4, file.path(out_dir, "sim500_70_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L5, file.path(out_dir, "sim500_70_10V_lowINTER_3c_L5.xlsx"))

## 500样本-变量&噪声-30%删失
write_xlsx(sim500_30_10V_lowBTW_3c_L1, file.path(out_dir, "sim500_30_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L2, file.path(out_dir, "sim500_30_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L3, file.path(out_dir, "sim500_30_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L4, file.path(out_dir, "sim500_30_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L5, file.path(out_dir, "sim500_30_10V_lowBTW_3c_L5.xlsx"))

## 500样本-变量&噪声-70%删失
write_xlsx(sim500_70_10V_lowBTW_3c_L1, file.path(out_dir, "sim500_70_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L2, file.path(out_dir, "sim500_70_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L3, file.path(out_dir, "sim500_70_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L4, file.path(out_dir, "sim500_70_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L5, file.path(out_dir, "sim500_70_10V_lowBTW_3c_L5.xlsx"))

## 1000样本-变量间-30%删失
write_xlsx(sim1000_30_10V_lowINTER_3c_L1, file.path(out_dir, "sim1000_30_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L2, file.path(out_dir, "sim1000_30_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L3, file.path(out_dir, "sim1000_30_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L4, file.path(out_dir, "sim1000_30_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L5, file.path(out_dir, "sim1000_30_10V_lowINTER_3c_L5.xlsx"))

## 1000样本-变量间-70%删失
write_xlsx(sim1000_70_10V_lowINTER_3c_L1, file.path(out_dir, "sim1000_70_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L2, file.path(out_dir, "sim1000_70_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L3, file.path(out_dir, "sim1000_70_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L4, file.path(out_dir, "sim1000_70_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L5, file.path(out_dir, "sim1000_70_10V_lowINTER_3c_L5.xlsx"))

## 1000样本-变量&噪声-30%删失
write_xlsx(sim1000_30_10V_lowBTW_3c_L1, file.path(out_dir, "sim1000_30_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L2, file.path(out_dir, "sim1000_30_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L3, file.path(out_dir, "sim1000_30_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L4, file.path(out_dir, "sim1000_30_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L5, file.path(out_dir, "sim1000_30_10V_lowBTW_3c_L5.xlsx"))

## 1000样本-变量&噪声-70%删失
write_xlsx(sim1000_70_10V_lowBTW_3c_L1, file.path(out_dir, "sim1000_70_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L2, file.path(out_dir, "sim1000_70_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L3, file.path(out_dir, "sim1000_70_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L4, file.path(out_dir, "sim1000_70_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L5, file.path(out_dir, "sim1000_70_10V_lowBTW_3c_L5.xlsx"))

######### 高相关 (high correlation) #########

## 500样本-变量间-30%删失
write_xlsx(sim500_30_10V_highINTER_3c_L1, file.path(out_dir, "sim500_30_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L2, file.path(out_dir, "sim500_30_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L3, file.path(out_dir, "sim500_30_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L4, file.path(out_dir, "sim500_30_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L5, file.path(out_dir, "sim500_30_10V_highINTER_3c_L5.xlsx"))

## 500样本-变量间-70%删失
write_xlsx(sim500_70_10V_highINTER_3c_L1, file.path(out_dir, "sim500_70_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L2, file.path(out_dir, "sim500_70_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L3, file.path(out_dir, "sim500_70_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L4, file.path(out_dir, "sim500_70_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L5, file.path(out_dir, "sim500_70_10V_highINTER_3c_L5.xlsx"))

## 500样本-变量&噪声-30%删失
write_xlsx(sim500_30_10V_highBTW_3c_L1, file.path(out_dir, "sim500_30_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L2, file.path(out_dir, "sim500_30_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L3, file.path(out_dir, "sim500_30_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L4, file.path(out_dir, "sim500_30_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L5, file.path(out_dir, "sim500_30_10V_highBTW_3c_L5.xlsx"))

## 500样本-变量&噪声-70%删失
write_xlsx(sim500_70_10V_highBTW_3c_L1, file.path(out_dir, "sim500_70_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L2, file.path(out_dir, "sim500_70_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L3, file.path(out_dir, "sim500_70_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L4, file.path(out_dir, "sim500_70_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L5, file.path(out_dir, "sim500_70_10V_highBTW_3c_L5.xlsx"))

## 1000样本-变量间-30%删失
write_xlsx(sim1000_30_10V_highINTER_3c_L1, file.path(out_dir, "sim1000_30_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L2, file.path(out_dir, "sim1000_30_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L3, file.path(out_dir, "sim1000_30_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L4, file.path(out_dir, "sim1000_30_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L5, file.path(out_dir, "sim1000_30_10V_highINTER_3c_L5.xlsx"))

## 1000样本-变量间-70%删失
write_xlsx(sim1000_70_10V_highINTER_3c_L1, file.path(out_dir, "sim1000_70_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L2, file.path(out_dir, "sim1000_70_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L3, file.path(out_dir, "sim1000_70_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L4, file.path(out_dir, "sim1000_70_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L5, file.path(out_dir, "sim1000_70_10V_highINTER_3c_L5.xlsx"))

## 1000样本-变量&噪声-30%删失
write_xlsx(sim1000_30_10V_highBTW_3c_L1, file.path(out_dir, "sim1000_30_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L2, file.path(out_dir, "sim1000_30_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L3, file.path(out_dir, "sim1000_30_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L4, file.path(out_dir, "sim1000_30_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L5, file.path(out_dir, "sim1000_30_10V_highBTW_3c_L5.xlsx"))

## 1000样本-变量&噪声-70%删失
write_xlsx(sim1000_70_10V_highBTW_3c_L1, file.path(out_dir, "sim1000_70_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L2, file.path(out_dir, "sim1000_70_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L3, file.path(out_dir, "sim1000_70_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L4, file.path(out_dir, "sim1000_70_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L5, file.path(out_dir, "sim1000_70_10V_highBTW_3c_L5.xlsx"))

######### 中相关 (medium correlation) #########

## 500样本-变量间-30%删失
write_xlsx(sim500_30_10V_midINTER_3c_L1, file.path(out_dir, "sim500_30_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L2, file.path(out_dir, "sim500_30_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L3, file.path(out_dir, "sim500_30_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L4, file.path(out_dir, "sim500_30_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L5, file.path(out_dir, "sim500_30_10V_midINTER_3c_L5.xlsx"))

## 500样本-变量间-70%删失
write_xlsx(sim500_70_10V_midINTER_3c_L1, file.path(out_dir, "sim500_70_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L2, file.path(out_dir, "sim500_70_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L3, file.path(out_dir, "sim500_70_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L4, file.path(out_dir, "sim500_70_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L5, file.path(out_dir, "sim500_70_10V_midINTER_3c_L5.xlsx"))

## 500样本-变量&噪声-30%删失
write_xlsx(sim500_30_10V_midBTW_3c_L1, file.path(out_dir, "sim500_30_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L2, file.path(out_dir, "sim500_30_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L3, file.path(out_dir, "sim500_30_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L4, file.path(out_dir, "sim500_30_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L5, file.path(out_dir, "sim500_30_10V_midBTW_3c_L5.xlsx"))

## 500样本-变量&噪声-70%删失
write_xlsx(sim500_70_10V_midBTW_3c_L1, file.path(out_dir, "sim500_70_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L2, file.path(out_dir, "sim500_70_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L3, file.path(out_dir, "sim500_70_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L4, file.path(out_dir, "sim500_70_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L5, file.path(out_dir, "sim500_70_10V_midBTW_3c_L5.xlsx"))

## 1000样本-变量间-30%删失
write_xlsx(sim1000_30_10V_midINTER_3c_L1, file.path(out_dir, "sim1000_30_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L2, file.path(out_dir, "sim1000_30_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L3, file.path(out_dir, "sim1000_30_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L4, file.path(out_dir, "sim1000_30_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L5, file.path(out_dir, "sim1000_30_10V_midINTER_3c_L5.xlsx"))

## 1000样本-变量间-70%删失
write_xlsx(sim1000_70_10V_midINTER_3c_L1, file.path(out_dir, "sim1000_70_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L2, file.path(out_dir, "sim1000_70_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L3, file.path(out_dir, "sim1000_70_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L4, file.path(out_dir, "sim1000_70_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L5, file.path(out_dir, "sim1000_70_10V_midINTER_3c_L5.xlsx"))

## 1000样本-变量&噪声-30%删失
write_xlsx(sim1000_30_10V_midBTW_3c_L1, file.path(out_dir, "sim1000_30_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L2, file.path(out_dir, "sim1000_30_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L3, file.path(out_dir, "sim1000_30_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L4, file.path(out_dir, "sim1000_30_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L5, file.path(out_dir, "sim1000_30_10V_midBTW_3c_L5.xlsx"))

## 1000样本-变量&噪声-70%删失
write_xlsx(sim1000_70_10V_midBTW_3c_L1, file.path(out_dir, "sim1000_70_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L2, file.path(out_dir, "sim1000_70_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L3, file.path(out_dir, "sim1000_70_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L4, file.path(out_dir, "sim1000_70_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L5, file.path(out_dir, "sim1000_70_10V_midBTW_3c_L5.xlsx"))


##################





# 设置输入目录（与之前的out_dir相同）
input_dir <- "F:/文章/大论文/程序_单一时间点/模拟数据_添加Y"

######### 低相关 (low correlation) #########

## 500样本-变量间-30%删失
sim500_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量间-70%删失
sim500_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-30%删失
sim500_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-70%删失
sim500_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-30%删失
sim1000_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-70%删失
sim1000_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-30%删失
sim1000_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-70%删失
sim1000_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

######### 高相关 (high correlation) #########

## 500样本-变量间-30%删失
sim500_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量间-70%删失
sim500_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-30%删失
sim500_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-70%删失
sim500_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-30%删失
sim1000_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-70%删失
sim1000_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-30%删失
sim1000_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-70%删失
sim1000_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))

######### 中相关 (medium correlation) #########

## 500样本-变量间-30%删失
sim500_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量间-70%删失
sim500_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-30%删失
sim500_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))

## 500样本-变量&噪声-70%删失
sim500_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-30%删失
sim1000_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量间-70%删失
sim1000_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-30%删失
sim1000_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))

## 1000样本-变量&噪声-70%删失
sim1000_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L1.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L1.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L2.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L2.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L3.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L3.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L4.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L4.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L5.xlsx")), excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L5.xlsx"))), ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))






