library(survival)
library(rms)
library(timeROC)
library(dplyr)
library(joineRML)
library(readxl)
library(writexl)
library(tidyverse)
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



#####程序 —— 5折交叉验证版本####################


cal_3_cox <- function(data, t0 = 1, K = 5) {

  #----------------#
  # 1. 数据准备（每患者一条）
  #----------------#
  surv_data <- data[!duplicated(data$ID), ]
  n <- nrow(surv_data)

  cox_vars <- grep("^V[0-9]+$", names(surv_data), value = TRUE)

  #----------------#
  # 2. 创建 K 折索引
  #----------------#
  set.seed(123)
  fold_ids <- sample(rep(1:K, length.out = n))

  #----------------#
  # 3. 逐折训练 + 预测，收集 OOF 预测
  #----------------#
  oof_risk_marker    <- numeric(n)
  oof_survival_probs <- numeric(n)

  for (k in 1:K) {
    train_idx <- which(fold_ids != k)
    test_idx  <- which(fold_ids == k)

    train_data <- surv_data[train_idx, c(cox_vars, "obs_time", "event")]
    test_data  <- surv_data[test_idx,  c(cox_vars, "obs_time", "event")]

    cox_fit <- coxph(
      Surv(obs_time, event) ~ .,
      data = train_data,
      x = TRUE
    )

    lp_test <- predict(cox_fit, newdata = test_data, type = "lp")

    base_surv <- survfit(cox_fit)
    base_time <- base_surv$time
    base_S0   <- base_surv$surv
    t0_idx <- which.max(base_time[base_time <= t0])
    S0_t0  <- base_S0[t0_idx]

    surv_probs_test <- S0_t0 ^ exp(lp_test)
    risk_marker_test <- 1 - surv_probs_test

    oof_survival_probs[test_idx] <- surv_probs_test
    oof_risk_marker[test_idx]    <- risk_marker_test
  }

  #----------------#
  # 4. 在全部 OOF 预测上计算指标
  #----------------#
  surv_time   <- surv_data$obs_time
  surv_status <- surv_data$event

  # AUC（timeROC）
  roc_obj <- timeROC(
    T = surv_time,
    delta = surv_status,
    marker = oof_risk_marker,
    cause = 1,
    times = t0
  )
  AUC <- round(roc_obj$AUC[2], 4)

  # C-index
  cindex_obj <- survConcordance(
    Surv(surv_time, surv_status) ~ oof_risk_marker
  )
  Cindex <- as.numeric(cindex_obj$concordance)

  # Brier Score（IPCW）
  Y_obs <- as.numeric(
    surv_time > t0 | (surv_time <= t0 & surv_status == 0)
  )

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
  BS <- mean(weights * (oof_survival_probs - Y_obs)^2, na.rm = TRUE)

  #----------------#
  # 5. 返回结果
  #----------------#
  result <- data.frame(
    AUC = AUC,
    BS = BS,
    Cindex = Cindex
  )

  return(result)
}


circle_cal_3 <- function(data_list, t0 = 1, K = 5, verbose = TRUE) {

  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")

  res <- lapply(seq_along(data_list), function(i) {

    if (verbose)
      message(sprintf("【%s】Processing sim%g (5-fold CV) ...", Sys.time(), i))

    tmp <- cal_3_cox(data_list[[i]], t0 = t0, K = K)

    cbind(sim = paste0("sim", i), tmp)
  })

  do.call(rbind, res)
}

#####################

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



##################

###########保存数据#################
output_dir <- "F:/文章_大论文/0319大改/结果"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

###########低相关###########
write_xlsx(result_sim500_30_10V_lowINTER_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_lowINTER_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_lowINTER_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_lowINTER_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_lowINTER_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_30_10V_lowBTW_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_lowBTW_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_lowBTW_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_lowBTW_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_lowBTW_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_lowBTW_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_lowINTER_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_lowINTER_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_lowINTER_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_lowINTER_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_lowINTER_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_lowBTW_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_lowBTW_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_lowBTW_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_lowBTW_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_lowBTW_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_lowBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_lowINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_lowINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_lowBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_lowBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_lowBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_lowINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_lowINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_lowBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_lowBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_lowBTW_1c_L5.xlsx"))

###########中相关###########

write_xlsx(result_sim500_30_10V_midINTER_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_midINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_midINTER_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_midINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_midINTER_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_midINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_midINTER_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_midINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_midINTER_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_midINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_30_10V_midBTW_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_midBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_midBTW_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_midBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_midBTW_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_midBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_midBTW_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_midBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_midBTW_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_midBTW_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_midINTER_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_midINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_midINTER_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_midINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_midINTER_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_midINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_midINTER_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_midINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_midINTER_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_midINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_midBTW_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_midBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_midBTW_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_midBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_midBTW_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_midBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_midBTW_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_midBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_midBTW_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_midBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_midINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_midINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_midINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_midINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_midINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_midINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_midINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_midINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_midINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_midINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_midBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_midBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_midBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_midBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_midBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_midBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_midBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_midBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_midBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_midBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_midINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_midINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_midINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_midINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_midINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_midINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_midINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_midINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_midINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_midINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_midBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_midBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_midBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_midBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_midBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_midBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_midBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_midBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_midBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_midBTW_1c_L5.xlsx"))

###########高相关###########

write_xlsx(result_sim500_30_10V_highINTER_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_highINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_highINTER_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_highINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_highINTER_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_highINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_highINTER_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_highINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_highINTER_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_highINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_30_10V_highBTW_1c_L1, file.path(output_dir, "result_COX_sim500_30_10V_highBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_30_10V_highBTW_1c_L2, file.path(output_dir, "result_COX_sim500_30_10V_highBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_30_10V_highBTW_1c_L3, file.path(output_dir, "result_COX_sim500_30_10V_highBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_30_10V_highBTW_1c_L4, file.path(output_dir, "result_COX_sim500_30_10V_highBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_30_10V_highBTW_1c_L5, file.path(output_dir, "result_COX_sim500_30_10V_highBTW_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_highINTER_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_highINTER_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_highINTER_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_highINTER_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_highINTER_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_highINTER_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_highINTER_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_highINTER_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_highINTER_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_highINTER_1c_L5.xlsx"))

write_xlsx(result_sim500_70_10V_highBTW_1c_L1, file.path(output_dir, "result_COX_sim500_70_10V_highBTW_1c_L1.xlsx"))
write_xlsx(result_sim500_70_10V_highBTW_1c_L2, file.path(output_dir, "result_COX_sim500_70_10V_highBTW_1c_L2.xlsx"))
write_xlsx(result_sim500_70_10V_highBTW_1c_L3, file.path(output_dir, "result_COX_sim500_70_10V_highBTW_1c_L3.xlsx"))
write_xlsx(result_sim500_70_10V_highBTW_1c_L4, file.path(output_dir, "result_COX_sim500_70_10V_highBTW_1c_L4.xlsx"))
write_xlsx(result_sim500_70_10V_highBTW_1c_L5, file.path(output_dir, "result_COX_sim500_70_10V_highBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_highINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_highINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_highINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_highINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_highINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_highINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_highINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_highINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_highINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_highINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_30_10V_highBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_30_10V_highBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_30_10V_highBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_30_10V_highBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_30_10V_highBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_30_10V_highBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_30_10V_highBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_30_10V_highBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_30_10V_highBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_30_10V_highBTW_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_highINTER_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_highINTER_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_highINTER_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_highINTER_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_highINTER_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_highINTER_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_highINTER_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_highINTER_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_highINTER_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_highINTER_1c_L5.xlsx"))

write_xlsx(result_sim1000_70_10V_highBTW_1c_L1, file.path(output_dir, "result_COX_sim1000_70_10V_highBTW_1c_L1.xlsx"))
write_xlsx(result_sim1000_70_10V_highBTW_1c_L2, file.path(output_dir, "result_COX_sim1000_70_10V_highBTW_1c_L2.xlsx"))
write_xlsx(result_sim1000_70_10V_highBTW_1c_L3, file.path(output_dir, "result_COX_sim1000_70_10V_highBTW_1c_L3.xlsx"))
write_xlsx(result_sim1000_70_10V_highBTW_1c_L4, file.path(output_dir, "result_COX_sim1000_70_10V_highBTW_1c_L4.xlsx"))
write_xlsx(result_sim1000_70_10V_highBTW_1c_L5, file.path(output_dir, "result_COX_sim1000_70_10V_highBTW_1c_L5.xlsx"))
