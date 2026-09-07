
library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


######读取数据####

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"

# 使用map函数读取低相关性数据
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

###### 使用map函数读取中相关性数据#########
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

