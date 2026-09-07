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



#####程序####################


cal_3_cox <- function(data, t0 = 1) {
  
  #----------------#
  # 1. 数据准备（每患者一条）
  #----------------#
  surv_data <- data[!duplicated(data$ID), ]
  
  surv_time   <- surv_data$obs_time
  surv_status <- surv_data$event
  
  # Cox 使用的变量：V1-V10
  cox_data <- surv_data[, c(
    grep("^V[0-9]+$", names(surv_data), value = TRUE),
    "obs_time", "event"
  )]
  
  #----------------#
  # 2. 拟合 Cox 模型
  #----------------#
  cox_fit <- coxph(
    Surv(obs_time, event) ~ .,
    data = cox_data,
    x = TRUE
  )
  
  #----------------#
  # 3. 预测 t0 时刻生存概率
  #----------------#
  # Cox 的风险评分（线性预测）
  lp <- predict(cox_fit, type = "lp")
  
  # 基线生存函数
  base_surv <- survfit(cox_fit)
  
  # 找到 t0 对应的 baseline survival
  base_time <- base_surv$time
  base_S0   <- base_surv$surv
  t0_idx <- which.max(base_time[base_time <= t0])
  S0_t0 <- base_S0[t0_idx]
  
  # 个体生存概率
  survival_probs <- S0_t0 ^ exp(lp)
  
  # 风险标记（越大风险越高）
  risk_marker <- 1 - survival_probs
  
  #----------------#
  # 4. 计算 AUC（timeROC）
  #----------------#
  roc_obj <- timeROC(
    T = surv_time,
    delta = surv_status,
    marker = risk_marker,
    cause = 1,
    times = t0
  )
  
  AUC <- round(roc_obj$AUC[2], 4)
  
  #----------------#
  # 5. 计算 C-index
  #----------------#
  cindex_obj <- survConcordance(
    Surv(surv_time, surv_status) ~ risk_marker
  )
  
  Cindex <- as.numeric(cindex_obj$concordance)
  
  #----------------#
  # 6. 计算 Brier Score（IPCW）
  #----------------#
  # 观测指标
  Y_obs <- as.numeric(
    surv_time > t0 | (surv_time <= t0 & surv_status == 0)
  )
  
  # 拟合删失分布
  censoring_model <- survfit(
    Surv(obs_time, 1 - event) ~ 1,
    data = surv_data
  )
  
  get_weights <- function(time, event, censoring_model, t0) {
    G_t <- summary(censoring_model, times = t0)$surv
    G_i <- summary(censoring_model, times = pmin(time, t0))$surv
    
    w <- ifelse(
      time <= t0 & event == 1, 1 / G_i,
      ifelse(time > t0, 1 / G_t, 0)
    )
    return(w)
  }
  
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  
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
  
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  res <- lapply(seq_along(data_list), function(i) {
    
    if (verbose)
      message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    tmp <- cal_3_cox(data_list[[i]], t0 = t0)
    
    cbind(sim = paste0("sim", i), tmp)
  })
  
  do.call(rbind, res)
}


#####################

#############低相关性##########
t0 <- 1 

result_sim500_30_10V_lowINTER_3c_L1 <-circle_cal_3(sim500_30_10V_lowINTER_3c_L1,  t0 = t0)
result_sim500_30_10V_lowINTER_3c_L2 <-circle_cal_3(sim500_30_10V_lowINTER_3c_L2,  t0 = t0)
result_sim500_30_10V_lowINTER_3c_L3 <-circle_cal_3(sim500_30_10V_lowINTER_3c_L3,  t0 = t0)
result_sim500_30_10V_lowINTER_3c_L4 <-circle_cal_3(sim500_30_10V_lowINTER_3c_L4,  t0 = t0)
result_sim500_30_10V_lowINTER_3c_L5 <-circle_cal_3(sim500_30_10V_lowINTER_3c_L5,  t0 = t0)

result_sim500_30_10V_lowBTW_3c_L1 <-circle_cal_3(sim500_30_10V_lowBTW_3c_L1,  t0 = t0)
result_sim500_30_10V_lowBTW_3c_L2 <-circle_cal_3(sim500_30_10V_lowBTW_3c_L2,  t0 = t0)
result_sim500_30_10V_lowBTW_3c_L3 <-circle_cal_3(sim500_30_10V_lowBTW_3c_L3,  t0 = t0)
result_sim500_30_10V_lowBTW_3c_L4 <-circle_cal_3(sim500_30_10V_lowBTW_3c_L4,  t0 = t0)
result_sim500_30_10V_lowBTW_3c_L5 <-circle_cal_3(sim500_30_10V_lowBTW_3c_L5,  t0 = t0)

result_sim500_70_10V_lowINTER_3c_L1 <-circle_cal_3(sim500_70_10V_lowINTER_3c_L1,  t0 = t0)
result_sim500_70_10V_lowINTER_3c_L2 <-circle_cal_3(sim500_70_10V_lowINTER_3c_L2,  t0 = t0)
result_sim500_70_10V_lowINTER_3c_L3 <-circle_cal_3(sim500_70_10V_lowINTER_3c_L3,  t0 = t0)
result_sim500_70_10V_lowINTER_3c_L4 <-circle_cal_3(sim500_70_10V_lowINTER_3c_L4,  t0 = t0)
result_sim500_70_10V_lowINTER_3c_L5 <-circle_cal_3(sim500_70_10V_lowINTER_3c_L5,  t0 = t0)

result_sim500_70_10V_lowBTW_3c_L1 <-circle_cal_3(sim500_70_10V_lowBTW_3c_L1,  t0 = t0)
result_sim500_70_10V_lowBTW_3c_L2 <-circle_cal_3(sim500_70_10V_lowBTW_3c_L2,  t0 = t0)
result_sim500_70_10V_lowBTW_3c_L3 <-circle_cal_3(sim500_70_10V_lowBTW_3c_L3,  t0 = t0)
result_sim500_70_10V_lowBTW_3c_L4 <-circle_cal_3(sim500_70_10V_lowBTW_3c_L4,  t0 = t0)
result_sim500_70_10V_lowBTW_3c_L5 <-circle_cal_3(sim500_70_10V_lowBTW_3c_L5,  t0 = t0)


result_sim1000_30_10V_lowINTER_3c_L1 <-circle_cal_3(sim1000_30_10V_lowINTER_3c_L1,  t0 = t0)
result_sim1000_30_10V_lowINTER_3c_L2 <-circle_cal_3(sim1000_30_10V_lowINTER_3c_L2,  t0 = t0)
result_sim1000_30_10V_lowINTER_3c_L3 <-circle_cal_3(sim1000_30_10V_lowINTER_3c_L3,  t0 = t0)
result_sim1000_30_10V_lowINTER_3c_L4 <-circle_cal_3(sim1000_30_10V_lowINTER_3c_L4,  t0 = t0)
result_sim1000_30_10V_lowINTER_3c_L5 <-circle_cal_3(sim1000_30_10V_lowINTER_3c_L5,  t0 = t0)

result_sim1000_30_10V_lowBTW_3c_L1 <-circle_cal_3(sim1000_30_10V_lowBTW_3c_L1,  t0 = t0)
result_sim1000_30_10V_lowBTW_3c_L2 <-circle_cal_3(sim1000_30_10V_lowBTW_3c_L2,  t0 = t0)
result_sim1000_30_10V_lowBTW_3c_L3 <-circle_cal_3(sim1000_30_10V_lowBTW_3c_L3,  t0 = t0)
result_sim1000_30_10V_lowBTW_3c_L4 <-circle_cal_3(sim1000_30_10V_lowBTW_3c_L4,  t0 = t0)
result_sim1000_30_10V_lowBTW_3c_L5 <-circle_cal_3(sim1000_30_10V_lowBTW_3c_L5,  t0 = t0)

result_sim1000_70_10V_lowINTER_3c_L1 <-circle_cal_3(sim1000_70_10V_lowINTER_3c_L1,  t0 = t0)
result_sim1000_70_10V_lowINTER_3c_L2 <-circle_cal_3(sim1000_70_10V_lowINTER_3c_L2,  t0 = t0)
result_sim1000_70_10V_lowINTER_3c_L3 <-circle_cal_3(sim1000_70_10V_lowINTER_3c_L3,  t0 = t0)
result_sim1000_70_10V_lowINTER_3c_L4 <-circle_cal_3(sim1000_70_10V_lowINTER_3c_L4,  t0 = t0)
result_sim1000_70_10V_lowINTER_3c_L5 <-circle_cal_3(sim1000_70_10V_lowINTER_3c_L5,  t0 = t0)

result_sim1000_70_10V_lowBTW_3c_L1 <-circle_cal_3(sim1000_70_10V_lowBTW_3c_L1,  t0 = t0)
result_sim1000_70_10V_lowBTW_3c_L2 <-circle_cal_3(sim1000_70_10V_lowBTW_3c_L2,  t0 = t0)
result_sim1000_70_10V_lowBTW_3c_L3 <-circle_cal_3(sim1000_70_10V_lowBTW_3c_L3,  t0 = t0)
result_sim1000_70_10V_lowBTW_3c_L4 <-circle_cal_3(sim1000_70_10V_lowBTW_3c_L4,  t0 = t0)
result_sim1000_70_10V_lowBTW_3c_L5 <-circle_cal_3(sim1000_70_10V_lowBTW_3c_L5,  t0 = t0)


#############中相关性##########
t0 <- 1 

result_sim500_30_10V_midINTER_3c_L1 <-circle_cal_3(sim500_30_10V_midINTER_3c_L1,  t0 = t0)
result_sim500_30_10V_midINTER_3c_L2 <-circle_cal_3(sim500_30_10V_midINTER_3c_L2,  t0 = t0)
result_sim500_30_10V_midINTER_3c_L3 <-circle_cal_3(sim500_30_10V_midINTER_3c_L3,  t0 = t0)
result_sim500_30_10V_midINTER_3c_L4 <-circle_cal_3(sim500_30_10V_midINTER_3c_L4,  t0 = t0)
result_sim500_30_10V_midINTER_3c_L5 <-circle_cal_3(sim500_30_10V_midINTER_3c_L5,  t0 = t0)

result_sim500_30_10V_midBTW_3c_L1 <-circle_cal_3(sim500_30_10V_midBTW_3c_L1,  t0 = t0)
result_sim500_30_10V_midBTW_3c_L2 <-circle_cal_3(sim500_30_10V_midBTW_3c_L2,  t0 = t0)
result_sim500_30_10V_midBTW_3c_L3 <-circle_cal_3(sim500_30_10V_midBTW_3c_L3,  t0 = t0)
result_sim500_30_10V_midBTW_3c_L4 <-circle_cal_3(sim500_30_10V_midBTW_3c_L4,  t0 = t0)
result_sim500_30_10V_midBTW_3c_L5 <-circle_cal_3(sim500_30_10V_midBTW_3c_L5,  t0 = t0)

result_sim500_70_10V_midINTER_3c_L1 <-circle_cal_3(sim500_70_10V_midINTER_3c_L1,  t0 = t0)
result_sim500_70_10V_midINTER_3c_L2 <-circle_cal_3(sim500_70_10V_midINTER_3c_L2,  t0 = t0)
result_sim500_70_10V_midINTER_3c_L3 <-circle_cal_3(sim500_70_10V_midINTER_3c_L3,  t0 = t0)
result_sim500_70_10V_midINTER_3c_L4 <-circle_cal_3(sim500_70_10V_midINTER_3c_L4,  t0 = t0)
result_sim500_70_10V_midINTER_3c_L5 <-circle_cal_3(sim500_70_10V_midINTER_3c_L5,  t0 = t0)

result_sim500_70_10V_midBTW_3c_L1 <-circle_cal_3(sim500_70_10V_midBTW_3c_L1,  t0 = t0)
result_sim500_70_10V_midBTW_3c_L2 <-circle_cal_3(sim500_70_10V_midBTW_3c_L2,  t0 = t0)
result_sim500_70_10V_midBTW_3c_L3 <-circle_cal_3(sim500_70_10V_midBTW_3c_L3,  t0 = t0)
result_sim500_70_10V_midBTW_3c_L4 <-circle_cal_3(sim500_70_10V_midBTW_3c_L4,  t0 = t0)
result_sim500_70_10V_midBTW_3c_L5 <-circle_cal_3(sim500_70_10V_midBTW_3c_L5,  t0 = t0)


result_sim1000_30_10V_midINTER_3c_L1 <-circle_cal_3(sim1000_30_10V_midINTER_3c_L1,  t0 = t0)
result_sim1000_30_10V_midINTER_3c_L2 <-circle_cal_3(sim1000_30_10V_midINTER_3c_L2,  t0 = t0)
result_sim1000_30_10V_midINTER_3c_L3 <-circle_cal_3(sim1000_30_10V_midINTER_3c_L3,  t0 = t0)
result_sim1000_30_10V_midINTER_3c_L4 <-circle_cal_3(sim1000_30_10V_midINTER_3c_L4,  t0 = t0)
result_sim1000_30_10V_midINTER_3c_L5 <-circle_cal_3(sim1000_30_10V_midINTER_3c_L5,  t0 = t0)

result_sim1000_30_10V_midBTW_3c_L1 <-circle_cal_3(sim1000_30_10V_midBTW_3c_L1,  t0 = t0)
result_sim1000_30_10V_midBTW_3c_L2 <-circle_cal_3(sim1000_30_10V_midBTW_3c_L2,  t0 = t0)
result_sim1000_30_10V_midBTW_3c_L3 <-circle_cal_3(sim1000_30_10V_midBTW_3c_L3,  t0 = t0)
result_sim1000_30_10V_midBTW_3c_L4 <-circle_cal_3(sim1000_30_10V_midBTW_3c_L4,  t0 = t0)
result_sim1000_30_10V_midBTW_3c_L5 <-circle_cal_3(sim1000_30_10V_midBTW_3c_L5,  t0 = t0)

result_sim1000_70_10V_midINTER_3c_L1 <-circle_cal_3(sim1000_70_10V_midINTER_3c_L1,  t0 = t0)
result_sim1000_70_10V_midINTER_3c_L2 <-circle_cal_3(sim1000_70_10V_midINTER_3c_L2,  t0 = t0)
result_sim1000_70_10V_midINTER_3c_L3 <-circle_cal_3(sim1000_70_10V_midINTER_3c_L3,  t0 = t0)
result_sim1000_70_10V_midINTER_3c_L4 <-circle_cal_3(sim1000_70_10V_midINTER_3c_L4,  t0 = t0)
result_sim1000_70_10V_midINTER_3c_L5 <-circle_cal_3(sim1000_70_10V_midINTER_3c_L5,  t0 = t0)

result_sim1000_70_10V_midBTW_3c_L1 <-circle_cal_3(sim1000_70_10V_midBTW_3c_L1,  t0 = t0)
result_sim1000_70_10V_midBTW_3c_L2 <-circle_cal_3(sim1000_70_10V_midBTW_3c_L2,  t0 = t0)
result_sim1000_70_10V_midBTW_3c_L3 <-circle_cal_3(sim1000_70_10V_midBTW_3c_L3,  t0 = t0)
result_sim1000_70_10V_midBTW_3c_L4 <-circle_cal_3(sim1000_70_10V_midBTW_3c_L4,  t0 = t0)
result_sim1000_70_10V_midBTW_3c_L5 <-circle_cal_3(sim1000_70_10V_midBTW_3c_L5,  t0 = t0)



#############高相关性##########
t0 <- 1 

result_sim500_30_10V_highINTER_3c_L1 <-circle_cal_3(sim500_30_10V_highINTER_3c_L1,  t0 = t0)
result_sim500_30_10V_highINTER_3c_L2 <-circle_cal_3(sim500_30_10V_highINTER_3c_L2,  t0 = t0)
result_sim500_30_10V_highINTER_3c_L3 <-circle_cal_3(sim500_30_10V_highINTER_3c_L3,  t0 = t0)
result_sim500_30_10V_highINTER_3c_L4 <-circle_cal_3(sim500_30_10V_highINTER_3c_L4,  t0 = t0)
result_sim500_30_10V_highINTER_3c_L5 <-circle_cal_3(sim500_30_10V_highINTER_3c_L5,  t0 = t0)

result_sim500_30_10V_highBTW_3c_L1 <-circle_cal_3(sim500_30_10V_highBTW_3c_L1,  t0 = t0)
result_sim500_30_10V_highBTW_3c_L2 <-circle_cal_3(sim500_30_10V_highBTW_3c_L2,  t0 = t0)
result_sim500_30_10V_highBTW_3c_L3 <-circle_cal_3(sim500_30_10V_highBTW_3c_L3,  t0 = t0)
result_sim500_30_10V_highBTW_3c_L4 <-circle_cal_3(sim500_30_10V_highBTW_3c_L4,  t0 = t0)
result_sim500_30_10V_highBTW_3c_L5 <-circle_cal_3(sim500_30_10V_highBTW_3c_L5,  t0 = t0)

result_sim500_70_10V_highINTER_3c_L1 <-circle_cal_3(sim500_70_10V_highINTER_3c_L1,  t0 = t0)
result_sim500_70_10V_highINTER_3c_L2 <-circle_cal_3(sim500_70_10V_highINTER_3c_L2,  t0 = t0)
result_sim500_70_10V_highINTER_3c_L3 <-circle_cal_3(sim500_70_10V_highINTER_3c_L3,  t0 = t0)
result_sim500_70_10V_highINTER_3c_L4 <-circle_cal_3(sim500_70_10V_highINTER_3c_L4,  t0 = t0)
result_sim500_70_10V_highINTER_3c_L5 <-circle_cal_3(sim500_70_10V_highINTER_3c_L5,  t0 = t0)

result_sim500_70_10V_highBTW_3c_L1 <-circle_cal_3(sim500_70_10V_highBTW_3c_L1,  t0 = t0)
result_sim500_70_10V_highBTW_3c_L2 <-circle_cal_3(sim500_70_10V_highBTW_3c_L2,  t0 = t0)
result_sim500_70_10V_highBTW_3c_L3 <-circle_cal_3(sim500_70_10V_highBTW_3c_L3,  t0 = t0)
result_sim500_70_10V_highBTW_3c_L4 <-circle_cal_3(sim500_70_10V_highBTW_3c_L4,  t0 = t0)
result_sim500_70_10V_highBTW_3c_L5 <-circle_cal_3(sim500_70_10V_highBTW_3c_L5,  t0 = t0)


result_sim1000_30_10V_highINTER_3c_L1 <-circle_cal_3(sim1000_30_10V_highINTER_3c_L1,  t0 = t0)
result_sim1000_30_10V_highINTER_3c_L2 <-circle_cal_3(sim1000_30_10V_highINTER_3c_L2,  t0 = t0)
result_sim1000_30_10V_highINTER_3c_L3 <-circle_cal_3(sim1000_30_10V_highINTER_3c_L3,  t0 = t0)
result_sim1000_30_10V_highINTER_3c_L4 <-circle_cal_3(sim1000_30_10V_highINTER_3c_L4,  t0 = t0)
result_sim1000_30_10V_highINTER_3c_L5 <-circle_cal_3(sim1000_30_10V_highINTER_3c_L5,  t0 = t0)

result_sim1000_30_10V_highBTW_3c_L1 <-circle_cal_3(sim1000_30_10V_highBTW_3c_L1,  t0 = t0)
result_sim1000_30_10V_highBTW_3c_L2 <-circle_cal_3(sim1000_30_10V_highBTW_3c_L2,  t0 = t0)
result_sim1000_30_10V_highBTW_3c_L3 <-circle_cal_3(sim1000_30_10V_highBTW_3c_L3,  t0 = t0)
result_sim1000_30_10V_highBTW_3c_L4 <-circle_cal_3(sim1000_30_10V_highBTW_3c_L4,  t0 = t0)
result_sim1000_30_10V_highBTW_3c_L5 <-circle_cal_3(sim1000_30_10V_highBTW_3c_L5,  t0 = t0)

result_sim1000_70_10V_highINTER_3c_L1 <-circle_cal_3(sim1000_70_10V_highINTER_3c_L1,  t0 = t0)
result_sim1000_70_10V_highINTER_3c_L2 <-circle_cal_3(sim1000_70_10V_highINTER_3c_L2,  t0 = t0)
result_sim1000_70_10V_highINTER_3c_L3 <-circle_cal_3(sim1000_70_10V_highINTER_3c_L3,  t0 = t0)
result_sim1000_70_10V_highINTER_3c_L4 <-circle_cal_3(sim1000_70_10V_highINTER_3c_L4,  t0 = t0)
result_sim1000_70_10V_highINTER_3c_L5 <-circle_cal_3(sim1000_70_10V_highINTER_3c_L5,  t0 = t0)

result_sim1000_70_10V_highBTW_3c_L1 <-circle_cal_3(sim1000_70_10V_highBTW_3c_L1,  t0 = t0)
result_sim1000_70_10V_highBTW_3c_L2 <-circle_cal_3(sim1000_70_10V_highBTW_3c_L2,  t0 = t0)
result_sim1000_70_10V_highBTW_3c_L3 <-circle_cal_3(sim1000_70_10V_highBTW_3c_L3,  t0 = t0)
result_sim1000_70_10V_highBTW_3c_L4 <-circle_cal_3(sim1000_70_10V_highBTW_3c_L4,  t0 = t0)
result_sim1000_70_10V_highBTW_3c_L5 <-circle_cal_3(sim1000_70_10V_highBTW_3c_L5,  t0 = t0)



##################

###########保存数据#################
###########低相关###########
write_xlsx(result_sim500_30_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_lowBTW_1c_L5.xlsx")

###########中相关###########

write_xlsx(result_sim500_30_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_midBTW_1c_L5.xlsx")

###########高相关###########

write_xlsx(result_sim500_30_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_30_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim500_70_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_30_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_RSF/result_sim1000_70_10V_highBTW_1c_L5.xlsx")


















































