library(survival)      # Surv()
library(rms)           # cph()
library(timeROC)       # timeROC()
library(dplyr)         # 基本数据处理
library(joineRML)      # mjoint()
library(readxl)        # read_excel()（如已改用 gdata 可再删）
library(tidyverse)
library(writexl)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)



######读取数据#####

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

###### 使用map函数读取低相关性数据####
sim500_30_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L2.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L3.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L4.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L5.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_lowBTW_1c_L5.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L2.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L3.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L4.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L5.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_lowBTW_1c_L5.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_lowBTW_1c_L5.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_lowBTW_1c_L5.xlsx"), sheet = .x))

##### 使用map函数读取中相关性数据####
input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

sim500_30_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_10V_midINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midINTER_1c_L5.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L2.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L3.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L4.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_midBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L5.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_30_10V_midBTW_1c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_30_10V_midBTW_1c_L5.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_10V_midINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midINTER_1c_L5.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L2.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L3.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L4.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_midBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L5.xlsx")),
                                           excel_sheets(file.path(input_dir, "sim500_70_10V_midBTW_1c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(input_dir, "sim500_70_10V_midBTW_1c_L5.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midINTER_1c_L5.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_midBTW_1c_L5.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midINTER_1c_L5.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_midBTW_1c_L5.xlsx"), sheet = .x))
##### 使用map函数读取高相关性数据####
input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

sim1000_30_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L2.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L3.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L4.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L5.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highINTER_1c_L5.xlsx"), sheet = .x))


sim500_30_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_30_10V_highINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highINTER_1c_L5.xlsx"), sheet = .x))

sim500_70_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim500_70_10V_highINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highINTER_1c_L5.xlsx"), sheet = .x))

sim500_30_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim500_30_10V_highBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L2.xlsx"), sheet = .x))
sim500_30_10V_highBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L3.xlsx"), sheet = .x))
sim500_30_10V_highBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L4.xlsx"), sheet = .x))
sim500_30_10V_highBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_10V_highBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_10V_highBTW_1c_L5.xlsx"), sheet = .x))

sim500_70_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim500_70_10V_highBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L2.xlsx"), sheet = .x))
sim500_70_10V_highBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L3.xlsx"), sheet = .x))
sim500_70_10V_highBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L4.xlsx"), sheet = .x))
sim500_70_10V_highBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_10V_highBTW_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_10V_highBTW_1c_L5.xlsx"), sheet = .x))

sim1000_30_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_10V_highBTW_1c_L5.xlsx"), sheet = .x))

sim1000_70_10V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L2.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L3.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L4.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L5.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highINTER_1c_L5.xlsx"), sheet = .x))


sim1000_70_10V_highBTW_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L2.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L3.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L4.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L5.xlsx")),
                                               excel_sheets(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(input_dir, "sim1000_70_10V_highBTW_1c_L5.xlsx"), sheet = .x))


###############计算三个整合代码####################

cal_3 <- function(data, t0 = 1) {
  
  ## 数值裁剪函数（防止 exp() 爆炸）
  clip <- function(x, lim = 20) {
    pmax(pmin(x, lim), -lim)
  }
  
  tryCatch({
    
    #----------------#
    # 1. 数据清理
    #----------------#
    data_clean <- data[data$t <= data$obs_time, ]
    
    #----------------#
    # 2. 拟合 joint model
    #----------------#
    fit <- mjoint(
      formLongFixed = list(
        "Y" = Y ~ t + V1 + V2 + V3 + V4 + V5 + V6
      ),
      formLongRandom = list(
        "Y" = ~ t | ID
      ),
      formSurv = Surv(obs_time, event) ~ 1,
      data = data_clean,
      timeVar = "t"
    )
    
    #----------------#
    # 3. 生存数据（每人一条）
    #----------------#
    surv_data <- data_clean[!duplicated(data_clean$ID), ]
    surv_time <- surv_data$obs_time
    surv_status <- surv_data$event
    
    #----------------#
    # 4. 提取参数
    #----------------#
    beta <- fit$coefficients$beta
    gamma <- fit$coefficients$gamma
    random_effects <- ranef(fit)
    
    #----------------#
    # 5. 风险得分（固定 + 随机）
    #----------------#
    baseline_data <- surv_data
    baseline_data$t <- t0
    
    X_matrix <- model.matrix(
      ~ t + V1 + V2 + V3 + V4 + V5 + V6,
      data = baseline_data
    )
    
    fixed_part <- as.numeric(X_matrix %*% beta)
    fixed_part <- clip(fixed_part)
    
    random_part <- random_effects[, 1] + random_effects[, 2] * t0
    random_part <- clip(random_part)
    
    comprehensive_risk <- as.numeric(gamma) * (fixed_part + random_part)
    
    #----------------#
    # 6. AUC
    #----------------#
    roc_obj <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = comprehensive_risk,
      cause = 1,
      times = t0
    )
    AUC <- round(roc_obj$AUC[2], 4)
    
    #----------------#
    # 7. C-index
    #----------------#
    n <- length(surv_time)
    idx <- combn(n, 2)
    
    i <- idx[1, ]
    j <- idx[2, ]
    
    comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
    comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
    comparable <- comparable_ij | comparable_ji
    
    concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
      (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))
    
    tied <- comparable & (comprehensive_risk[i] == comprehensive_risk[j])
    
    n_pairs <- sum(comparable)
    n_concordant <- sum(concordant)
    n_tied <- sum(tied)
    
    Cindex <- ifelse(
      n_pairs > 0,
      (n_concordant + 0.5 * n_tied) / n_pairs,
      NA
    )
    
    #----------------#
    # 8. Brier Score
    #----------------#
    lp_long_full <- model.matrix(
      ~ t + V1 + V2 + V3 + V4 + V5 + V6,
      data = data_clean
    ) %*% beta
    
    lp_long_full <- clip(as.numeric(lp_long_full))
    
    S0_t0 <- summary(
      survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)),
      times = t0
    )$surv
    
    S_pred <- S0_t0 ^ exp(as.numeric(gamma) * lp_long_full)
    
    censoring_model <- survfit(
      Surv(obs_time, 1 - event) ~ 1,
      data = data_clean
    )
    
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- summary(
        censoring_model,
        times = pmin(time, t0)
      )$surv
      
      weights <- ifelse(
        time <= t0 & event == 1,
        1 / cens_probs,
        ifelse(
          time > t0,
          1 / summary(censoring_model, times = t0)$surv,
          0
        )
      )
      weights
    }
    
    weights <- get_weights(
      data_clean$obs_time,
      data_clean$event,
      censoring_model,
      t0
    )
    
    Y_obs <- as.numeric(
      data_clean$obs_time > t0 |
        (data_clean$obs_time <= t0 & data_clean$event == 0)
    )
    
    BS <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)
    
    #----------------#
    # 9. 返回结果
    #----------------#
    data.frame(
      AUC = AUC,
      BS = BS,
      Cindex = Cindex
    )
    
  }, error = function(e) {
    
    message("⚠️ cal_3 failed: ", e$message)
    
    # 保证 circle_cal_3 不会炸
    data.frame(
      AUC = NA,
      BS = NA,
      Cindex = NA
    )
  })
}

#循环输出
circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3
    tmp <- cal_3(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}

###########################


#############低相关性##########
t0 <- 1 

result_sim500_30_10V_lowINTER_1c_L1 <-circle_cal_3(sim500_30_10V_lowINTER_1c_L1,  t0 = t0)
result_sim500_30_10V_lowINTER_1c_L2 <-circle_cal_3(sim500_30_10V_lowINTER_1c_L2,  t0 = t0)
result_sim500_30_10V_lowINTER_1c_L3 <-circle_cal_3(sim500_30_10V_lowINTER_1c_L3,  t0 = t0)
result_sim500_30_10V_lowINTER_1c_L4 <-circle_cal_3(sim500_30_10V_lowINTER_1c_L4,  t0 = t0)
result_sim500_30_10V_lowINTER_1c_L5 <-circle_cal_3(sim500_30_10V_lowINTER_1c_L5,  t0 = t0)

result_sim500_30_10V_lowBTW_1c_L1 <-circle_cal_3(sim500_30_10V_lowBTW_1c_L1,  t0 = t0)
result_sim500_30_10V_lowBTW_1c_L2 <-circle_cal_3(sim500_30_10V_lowBTW_1c_L2,  t0 = t0)
result_sim500_30_10V_lowBTW_1c_L3 <-circle_cal_3(sim500_30_10V_lowBTW_1c_L3,  t0 = t0)
result_sim500_30_10V_lowBTW_1c_L4 <-circle_cal_3(sim500_30_10V_lowBTW_1c_L4,  t0 = t0)
result_sim500_30_10V_lowBTW_1c_L5 <-circle_cal_3(sim500_30_10V_lowBTW_1c_L5,  t0 = t0)

result_sim500_70_10V_lowINTER_1c_L1 <-circle_cal_3(sim500_70_10V_lowINTER_1c_L1,  t0 = t0)
result_sim500_70_10V_lowINTER_1c_L2 <-circle_cal_3(sim500_70_10V_lowINTER_1c_L2,  t0 = t0)
result_sim500_70_10V_lowINTER_1c_L3 <-circle_cal_3(sim500_70_10V_lowINTER_1c_L3,  t0 = t0)
result_sim500_70_10V_lowINTER_1c_L4 <-circle_cal_3(sim500_70_10V_lowINTER_1c_L4,  t0 = t0)
result_sim500_70_10V_lowINTER_1c_L5 <-circle_cal_3(sim500_70_10V_lowINTER_1c_L5,  t0 = t0)

result_sim500_70_10V_lowBTW_1c_L1 <-circle_cal_3(sim500_70_10V_lowBTW_1c_L1,  t0 = t0)
result_sim500_70_10V_lowBTW_1c_L2 <-circle_cal_3(sim500_70_10V_lowBTW_1c_L2,  t0 = t0)
result_sim500_70_10V_lowBTW_1c_L3 <-circle_cal_3(sim500_70_10V_lowBTW_1c_L3,  t0 = t0)
result_sim500_70_10V_lowBTW_1c_L4 <-circle_cal_3(sim500_70_10V_lowBTW_1c_L4,  t0 = t0)
result_sim500_70_10V_lowBTW_1c_L5 <-circle_cal_3(sim500_70_10V_lowBTW_1c_L5,  t0 = t0)


result_sim1000_30_10V_lowINTER_1c_L1 <-circle_cal_3(sim1000_30_10V_lowINTER_1c_L1,  t0 = t0)
result_sim1000_30_10V_lowINTER_1c_L2 <-circle_cal_3(sim1000_30_10V_lowINTER_1c_L2,  t0 = t0)
result_sim1000_30_10V_lowINTER_1c_L3 <-circle_cal_3(sim1000_30_10V_lowINTER_1c_L3,  t0 = t0)
result_sim1000_30_10V_lowINTER_1c_L4 <-circle_cal_3(sim1000_30_10V_lowINTER_1c_L4,  t0 = t0)
result_sim1000_30_10V_lowINTER_1c_L5 <-circle_cal_3(sim1000_30_10V_lowINTER_1c_L5,  t0 = t0)

result_sim1000_30_10V_lowBTW_1c_L1 <-circle_cal_3(sim1000_30_10V_lowBTW_1c_L1,  t0 = t0)
result_sim1000_30_10V_lowBTW_1c_L2 <-circle_cal_3(sim1000_30_10V_lowBTW_1c_L2,  t0 = t0)
result_sim1000_30_10V_lowBTW_1c_L3 <-circle_cal_3(sim1000_30_10V_lowBTW_1c_L3,  t0 = t0)
result_sim1000_30_10V_lowBTW_1c_L4 <-circle_cal_3(sim1000_30_10V_lowBTW_1c_L4,  t0 = t0)
result_sim1000_30_10V_lowBTW_1c_L5 <-circle_cal_3(sim1000_30_10V_lowBTW_1c_L5,  t0 = t0)

result_sim1000_70_10V_lowINTER_1c_L1 <-circle_cal_3(sim1000_70_10V_lowINTER_1c_L1,  t0 = t0)
result_sim1000_70_10V_lowINTER_1c_L2 <-circle_cal_3(sim1000_70_10V_lowINTER_1c_L2,  t0 = t0)
result_sim1000_70_10V_lowINTER_1c_L3 <-circle_cal_3(sim1000_70_10V_lowINTER_1c_L3,  t0 = t0)
result_sim1000_70_10V_lowINTER_1c_L4 <-circle_cal_3(sim1000_70_10V_lowINTER_1c_L4,  t0 = t0)
result_sim1000_70_10V_lowINTER_1c_L5 <-circle_cal_3(sim1000_70_10V_lowINTER_1c_L5,  t0 = t0)

result_sim1000_70_10V_lowBTW_1c_L1 <-circle_cal_3(sim1000_70_10V_lowBTW_1c_L1,  t0 = t0)
result_sim1000_70_10V_lowBTW_1c_L2 <-circle_cal_3(sim1000_70_10V_lowBTW_1c_L2,  t0 = t0)
result_sim1000_70_10V_lowBTW_1c_L3 <-circle_cal_3(sim1000_70_10V_lowBTW_1c_L3,  t0 = t0)
result_sim1000_70_10V_lowBTW_1c_L4 <-circle_cal_3(sim1000_70_10V_lowBTW_1c_L4,  t0 = t0)
result_sim1000_70_10V_lowBTW_1c_L5 <-circle_cal_3(sim1000_70_10V_lowBTW_1c_L5,  t0 = t0)


#############中相关性##########
t0 <- 1 

result_sim500_30_10V_midINTER_1c_L1 <-circle_cal_3(sim500_30_10V_midINTER_1c_L1,  t0 = t0)
result_sim500_30_10V_midINTER_1c_L2 <-circle_cal_3(sim500_30_10V_midINTER_1c_L2,  t0 = t0)
result_sim500_30_10V_midINTER_1c_L3 <-circle_cal_3(sim500_30_10V_midINTER_1c_L3,  t0 = t0)
result_sim500_30_10V_midINTER_1c_L4 <-circle_cal_3(sim500_30_10V_midINTER_1c_L4,  t0 = t0)
result_sim500_30_10V_midINTER_1c_L5 <-circle_cal_3(sim500_30_10V_midINTER_1c_L5,  t0 = t0)

result_sim500_30_10V_midBTW_1c_L1 <-circle_cal_3(sim500_30_10V_midBTW_1c_L1,  t0 = t0)
result_sim500_30_10V_midBTW_1c_L2 <-circle_cal_3(sim500_30_10V_midBTW_1c_L2,  t0 = t0)
result_sim500_30_10V_midBTW_1c_L3 <-circle_cal_3(sim500_30_10V_midBTW_1c_L3,  t0 = t0)
result_sim500_30_10V_midBTW_1c_L4 <-circle_cal_3(sim500_30_10V_midBTW_1c_L4,  t0 = t0)
result_sim500_30_10V_midBTW_1c_L5 <-circle_cal_3(sim500_30_10V_midBTW_1c_L5,  t0 = t0)

result_sim500_70_10V_midINTER_1c_L1 <-circle_cal_3(sim500_70_10V_midINTER_1c_L1,  t0 = t0)
result_sim500_70_10V_midINTER_1c_L2 <-circle_cal_3(sim500_70_10V_midINTER_1c_L2,  t0 = t0)
result_sim500_70_10V_midINTER_1c_L3 <-circle_cal_3(sim500_70_10V_midINTER_1c_L3,  t0 = t0)
result_sim500_70_10V_midINTER_1c_L4 <-circle_cal_3(sim500_70_10V_midINTER_1c_L4,  t0 = t0)
result_sim500_70_10V_midINTER_1c_L5 <-circle_cal_3(sim500_70_10V_midINTER_1c_L5,  t0 = t0)

result_sim500_70_10V_midBTW_1c_L1 <-circle_cal_3(sim500_70_10V_midBTW_1c_L1,  t0 = t0)
result_sim500_70_10V_midBTW_1c_L2 <-circle_cal_3(sim500_70_10V_midBTW_1c_L2,  t0 = t0)
result_sim500_70_10V_midBTW_1c_L3 <-circle_cal_3(sim500_70_10V_midBTW_1c_L3,  t0 = t0)
result_sim500_70_10V_midBTW_1c_L4 <-circle_cal_3(sim500_70_10V_midBTW_1c_L4,  t0 = t0)
result_sim500_70_10V_midBTW_1c_L5 <-circle_cal_3(sim500_70_10V_midBTW_1c_L5,  t0 = t0)


result_sim1000_30_10V_midINTER_1c_L1 <-circle_cal_3(sim1000_30_10V_midINTER_1c_L1,  t0 = t0)
result_sim1000_30_10V_midINTER_1c_L2 <-circle_cal_3(sim1000_30_10V_midINTER_1c_L2,  t0 = t0)
result_sim1000_30_10V_midINTER_1c_L3 <-circle_cal_3(sim1000_30_10V_midINTER_1c_L3,  t0 = t0)
result_sim1000_30_10V_midINTER_1c_L4 <-circle_cal_3(sim1000_30_10V_midINTER_1c_L4,  t0 = t0)
result_sim1000_30_10V_midINTER_1c_L5 <-circle_cal_3(sim1000_30_10V_midINTER_1c_L5,  t0 = t0)

result_sim1000_30_10V_midBTW_1c_L1 <-circle_cal_3(sim1000_30_10V_midBTW_1c_L1,  t0 = t0)
result_sim1000_30_10V_midBTW_1c_L2 <-circle_cal_3(sim1000_30_10V_midBTW_1c_L2,  t0 = t0)
result_sim1000_30_10V_midBTW_1c_L3 <-circle_cal_3(sim1000_30_10V_midBTW_1c_L3,  t0 = t0)
result_sim1000_30_10V_midBTW_1c_L4 <-circle_cal_3(sim1000_30_10V_midBTW_1c_L4,  t0 = t0)
result_sim1000_30_10V_midBTW_1c_L5 <-circle_cal_3(sim1000_30_10V_midBTW_1c_L5,  t0 = t0)

result_sim1000_70_10V_midINTER_1c_L1 <-circle_cal_3(sim1000_70_10V_midINTER_1c_L1,  t0 = t0)
result_sim1000_70_10V_midINTER_1c_L2 <-circle_cal_3(sim1000_70_10V_midINTER_1c_L2,  t0 = t0)
result_sim1000_70_10V_midINTER_1c_L3 <-circle_cal_3(sim1000_70_10V_midINTER_1c_L3,  t0 = t0)
result_sim1000_70_10V_midINTER_1c_L4 <-circle_cal_3(sim1000_70_10V_midINTER_1c_L4,  t0 = t0)
result_sim1000_70_10V_midINTER_1c_L5 <-circle_cal_3(sim1000_70_10V_midINTER_1c_L5,  t0 = t0)

result_sim1000_70_10V_midBTW_1c_L1 <-circle_cal_3(sim1000_70_10V_midBTW_1c_L1,  t0 = t0)
result_sim1000_70_10V_midBTW_1c_L2 <-circle_cal_3(sim1000_70_10V_midBTW_1c_L2,  t0 = t0)
result_sim1000_70_10V_midBTW_1c_L3 <-circle_cal_3(sim1000_70_10V_midBTW_1c_L3,  t0 = t0)
result_sim1000_70_10V_midBTW_1c_L4 <-circle_cal_3(sim1000_70_10V_midBTW_1c_L4,  t0 = t0)
result_sim1000_70_10V_midBTW_1c_L5 <-circle_cal_3(sim1000_70_10V_midBTW_1c_L5,  t0 = t0)



#############高相关性##########
t0 <- 1 

result_sim500_30_10V_highINTER_1c_L1 <-circle_cal_3(sim500_30_10V_highINTER_1c_L1,  t0 = t0)
result_sim500_30_10V_highINTER_1c_L2 <-circle_cal_3(sim500_30_10V_highINTER_1c_L2,  t0 = t0)
result_sim500_30_10V_highINTER_1c_L3 <-circle_cal_3(sim500_30_10V_highINTER_1c_L3,  t0 = t0)
result_sim500_30_10V_highINTER_1c_L4 <-circle_cal_3(sim500_30_10V_highINTER_1c_L4,  t0 = t0)
result_sim500_30_10V_highINTER_1c_L5 <-circle_cal_3(sim500_30_10V_highINTER_1c_L5,  t0 = t0)

result_sim500_30_10V_highBTW_1c_L1 <-circle_cal_3(sim500_30_10V_highBTW_1c_L1,  t0 = t0)
result_sim500_30_10V_highBTW_1c_L2 <-circle_cal_3(sim500_30_10V_highBTW_1c_L2,  t0 = t0)
result_sim500_30_10V_highBTW_1c_L3 <-circle_cal_3(sim500_30_10V_highBTW_1c_L3,  t0 = t0)
result_sim500_30_10V_highBTW_1c_L4 <-circle_cal_3(sim500_30_10V_highBTW_1c_L4,  t0 = t0)
result_sim500_30_10V_highBTW_1c_L5 <-circle_cal_3(sim500_30_10V_highBTW_1c_L5,  t0 = t0)

result_sim500_70_10V_highINTER_1c_L1 <-circle_cal_3(sim500_70_10V_highINTER_1c_L1,  t0 = t0)
result_sim500_70_10V_highINTER_1c_L2 <-circle_cal_3(sim500_70_10V_highINTER_1c_L2,  t0 = t0)
result_sim500_70_10V_highINTER_1c_L3 <-circle_cal_3(sim500_70_10V_highINTER_1c_L3,  t0 = t0)
result_sim500_70_10V_highINTER_1c_L4 <-circle_cal_3(sim500_70_10V_highINTER_1c_L4,  t0 = t0)
result_sim500_70_10V_highINTER_1c_L5 <-circle_cal_3(sim500_70_10V_highINTER_1c_L5,  t0 = t0)

result_sim500_70_10V_highBTW_1c_L1 <-circle_cal_3(sim500_70_10V_highBTW_1c_L1,  t0 = t0)
result_sim500_70_10V_highBTW_1c_L2 <-circle_cal_3(sim500_70_10V_highBTW_1c_L2,  t0 = t0)
result_sim500_70_10V_highBTW_1c_L3 <-circle_cal_3(sim500_70_10V_highBTW_1c_L3,  t0 = t0)
result_sim500_70_10V_highBTW_1c_L4 <-circle_cal_3(sim500_70_10V_highBTW_1c_L4,  t0 = t0)
result_sim500_70_10V_highBTW_1c_L5 <-circle_cal_3(sim500_70_10V_highBTW_1c_L5,  t0 = t0)


result_sim1000_30_10V_highINTER_1c_L1 <-circle_cal_3(sim1000_30_10V_highINTER_1c_L1,  t0 = t0)
result_sim1000_30_10V_highINTER_1c_L2 <-circle_cal_3(sim1000_30_10V_highINTER_1c_L2,  t0 = t0)
result_sim1000_30_10V_highINTER_1c_L3 <-circle_cal_3(sim1000_30_10V_highINTER_1c_L3,  t0 = t0)
result_sim1000_30_10V_highINTER_1c_L4 <-circle_cal_3(sim1000_30_10V_highINTER_1c_L4,  t0 = t0)
result_sim1000_30_10V_highINTER_1c_L5 <-circle_cal_3(sim1000_30_10V_highINTER_1c_L5,  t0 = t0)

result_sim1000_30_10V_highBTW_1c_L1 <-circle_cal_3(sim1000_30_10V_highBTW_1c_L1,  t0 = t0)
result_sim1000_30_10V_highBTW_1c_L2 <-circle_cal_3(sim1000_30_10V_highBTW_1c_L2,  t0 = t0)
result_sim1000_30_10V_highBTW_1c_L3 <-circle_cal_3(sim1000_30_10V_highBTW_1c_L3,  t0 = t0)
result_sim1000_30_10V_highBTW_1c_L4 <-circle_cal_3(sim1000_30_10V_highBTW_1c_L4,  t0 = t0)
result_sim1000_30_10V_highBTW_1c_L5 <-circle_cal_3(sim1000_30_10V_highBTW_1c_L5,  t0 = t0)

result_sim1000_70_10V_highINTER_1c_L1 <-circle_cal_3(sim1000_70_10V_highINTER_1c_L1,  t0 = t0)
result_sim1000_70_10V_highINTER_1c_L2 <-circle_cal_3(sim1000_70_10V_highINTER_1c_L2,  t0 = t0)
result_sim1000_70_10V_highINTER_1c_L3 <-circle_cal_3(sim1000_70_10V_highINTER_1c_L3,  t0 = t0)
result_sim1000_70_10V_highINTER_1c_L4 <-circle_cal_3(sim1000_70_10V_highINTER_1c_L4,  t0 = t0)
result_sim1000_70_10V_highINTER_1c_L5 <-circle_cal_3(sim1000_70_10V_highINTER_1c_L5,  t0 = t0)

result_sim1000_70_10V_highBTW_1c_L1 <-circle_cal_3(sim1000_70_10V_highBTW_1c_L1,  t0 = t0)
result_sim1000_70_10V_highBTW_1c_L2 <-circle_cal_3(sim1000_70_10V_highBTW_1c_L2,  t0 = t0)
result_sim1000_70_10V_highBTW_1c_L3 <-circle_cal_3(sim1000_70_10V_highBTW_1c_L3,  t0 = t0)
result_sim1000_70_10V_highBTW_1c_L4 <-circle_cal_3(sim1000_70_10V_highBTW_1c_L4,  t0 = t0)
result_sim1000_70_10V_highBTW_1c_L5 <-circle_cal_3(sim1000_70_10V_highBTW_1c_L5,  t0 = t0)






###########保存数据#################
write_xlsx(result_sim500_30_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_lowBTW_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_midBTW_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim500_30_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_30_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim500_70_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_30_10V_highBTW_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highBTW_1c_L1, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highBTW_1c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L2, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highBTW_1c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L3, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highBTW_1c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L4, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highBTW_1c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_1c_L5, "F:/文章/大论文/程序Trae/数据_result_C1_JM/result_sim1000_70_10V_highBTW_1c_L5.xlsx")









