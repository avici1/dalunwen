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


######读取数据#####

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




######程序_RSFLC###############

cal_3_RSF <- function(data, t0 = 1) {
  #----------------#
  # 1. 数据准备（每患者一条生存数据）
  #----------------#
  surv_data <- data[!duplicated(data$ID), ]
  surv_time <- surv_data$obs_time
  surv_status <- surv_data$event
  
  # 只保留生存相关的变量（V1-V6, obs_time, event）
  rsf_data <- surv_data[, c(grep("^V[0-9]", names(surv_data), value = TRUE),
                            "obs_time", "event")]
  
  #----------------#
  # 2. 建立RSF模型
  #----------------#
  rfsrc_fit <- rfsrc(
    Surv(obs_time, event) ~ .,
    data = rsf_data,
    ntree      = 1000,
    mtry       = floor(length(grep("^V[0-9]", names(rsf_data), value = TRUE)) / 3),
    nodesize   = 10,
    importance = TRUE,
    proximity  = FALSE,
    seed       = 123
  )
  
  #----------------#
  # 3. 预测生存概率
  #----------------#
  # 预测t0时刻的生存概率
  pred <- predict(rfsrc_fit, newdata = rsf_data)
  
  # 获取t0时刻的生存概率
  time_points <- pred$time.interest
  t0_idx <- which.min(abs(time_points - t0))
  survival_probs <- pred$survival[, t0_idx]
  
  #----------------#
  # 4. 计算AUC
  #----------------#
  # 使用生存概率作为风险标记（生存概率越低，风险越高）
  risk_marker <- 1 - survival_probs
  
  roc_obj <- timeROC(
    T = surv_time,
    delta = surv_status,
    marker = risk_marker,
    cause = 1,
    times = t0
  )
  AUC <- round(roc_obj$AUC[2], 4)
  
  #----------------#
  # 5. 计算C-index
  #----------------#
  # 使用预测的生存概率计算C-index
  n <- length(surv_time)
  idx <- combn(n, 2)  # 所有pair的索引
  
  i <- idx[1, ]
  j <- idx[2, ]
  
  # 判断可比对pair（短时间的个体必须发生事件）
  comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
  comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
  comparable <- comparable_ij | comparable_ji
  
  # Concordant判断（风险标记越高，生存时间越短）
  concordant <- (comparable_ij & (risk_marker[i] > risk_marker[j])) |
    (comparable_ji & (risk_marker[j] > risk_marker[i]))
  
  tied <- (comparable & (risk_marker[i] == risk_marker[j]))
  
  n_pairs <- sum(comparable)
  n_concordant <- sum(concordant)
  n_tied <- sum(tied)
  
  Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)
  
  #----------------#
  # 6. 计算BS（Brier Score）
  #----------------#
  # 构造观测指标
  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  
  # 计算删失权重
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = rsf_data)
  
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                      ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
    return(weights)
  }
  
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  
  # 计算加权Brier Score
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  
  #----------------#
  # 7. 返回结果
  #----------------#
  result <- data.frame(
    AUC = AUC,
    BS = BS,
    Cindex = Cindex
  )
  
  return(result)
}

circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3
    tmp <- cal_3_RSF(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}







###############程序_cox###############

cal_3_cox <- function(data, t0 = 1) {
  #----------------#
  # 1. 数据准备（每患者一条生存数据）
  #----------------#
  surv_data <- data[!duplicated(data$ID), ]
  surv_time <- surv_data$obs_time
  surv_status <- surv_data$event
  
  # 只保留生存相关的变量（V1-V10, obs_time, event）
  cox_data <- surv_data[, c(grep("^V[0-9]", names(surv_data), value = TRUE),
                            "obs_time", "event")]
  
  #----------------#
  # 2. 建立COX模型
  #----------------#
  cox_fit <- coxph(
    Surv(obs_time, event) ~ ., 
    data = cox_data
  )
  
  #----------------#
  # 3. 预测风险评分
  #----------------#
  risk_scores <- predict(cox_fit, newdata = cox_data, type = "risk")
  
  #----------------#
  # 4. 计算AUC
  #----------------#
  roc_obj <- timeROC(
    T = surv_time,
    delta = surv_status,
    marker = risk_scores,
    cause = 1,
    times = t0
  )
  AUC <- round(roc_obj$AUC[2], 4)
  
  #----------------#
  # 5. 计算C-index
  #----------------#
  # 使用survConcordance计算C-index
  cindex_obj <- survConcordance(Surv(obs_time, event) ~ risk_scores, data = cox_data)
  Cindex <- round(cindex_obj$concordance, 4)
  
  #----------------#
  # 6. 计算BS（Brier Score）
  #----------------#
  # 构造观测指标
  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  
  # 使用survfit计算生存概率
  surv_fit <- survfit(cox_fit, newdata = cox_data)
  
  # 预测t0时刻的生存概率
  get_survival_prob <- function(surv_fit, time_point) {
    surv_summary <- summary(surv_fit, times = time_point)
    # 对于每个个体，找到最接近time_point的生存概率
    survival_probs <- numeric(nrow(surv_data))
    for (i in 1:nrow(surv_data)) {
      # 提取第i个个体的生存曲线
      surv_i <- summary(surv_fit, times = time_point, subset = i)
      if (length(surv_i$surv) > 0) {
        survival_probs[i] <- surv_i$surv[1]
      } else {
        # 如果time_point在生存时间之前，生存概率为1
        survival_probs[i] <- 1
      }
    }
    return(survival_probs)
  }
  
  survival_probs <- get_survival_prob(surv_fit, t0)
  
  # 计算删失权重
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = cox_data)
  
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                      ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
    return(weights)
  }
  
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  
  # 计算加权Brier Score
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  
  #----------------#
  # 7. 返回结果
  #----------------#
  result <- data.frame(
    AUC = AUC,
    BS = BS,
    Cindex = Cindex
  )
  
  return(result)
}

circle_cal_3_cox <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3_cox
    tmp <- cal_3_cox(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}















