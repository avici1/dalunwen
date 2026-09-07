library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

###########读取数据#########

# 设置输入目录
input_dir <- "F:/文章/大论文/程序Trae/模拟数据_生存"

###### 使用map函数读取低相关性数据#####
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

###### 使用map函数读取中相关性数据#####
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

##### 使用map函数读取高相关性数据#####
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
##########程序##################

add_Y <- function(sim_df, k, time_effects, id_col = "ID") {
  
  ## ---------------------------
  ## 0. 参数检查
  ## ---------------------------
  if (!is.numeric(k) || length(k) != 10)
    stop("k 必须是长度为 10 的数值向量")
  
  if (!is.numeric(time_effects) || length(time_effects) != 4)
    stop("time_effects 必须是长度为 4 的数值向量")
  
  if (!"t" %in% names(sim_df))
    stop("数据中必须包含时间变量 't'")
  
  if (!id_col %in% names(sim_df))
    stop(paste0("数据中必须包含个体 ID 变量: ", id_col))
  
  ## ---------------------------
  ## 1. X (V) 固定效应
  ## ---------------------------
  v_cols <- grep("^V\\d+", names(sim_df), value = TRUE)
  if (length(v_cols) != 10)
    stop("必须存在 10 个 V (X) 变量")
  
  k_vec <- setNames(k, v_cols)
  V_mat <- sim_df[, v_cols] |> as.matrix()
  X_effect <- as.numeric(V_mat %*% k_vec)
  
  ## ---------------------------
  ## 2. 拆解时间混合效应参数
  ## ---------------------------
  beta0 <- time_effects[1]   # 固定截距
  beta1 <- time_effects[2]   # 固定斜率
  sd_b0 <- time_effects[3]   # 随机截距 SD
  sd_b1 <- time_effects[4]   # 随机斜率 SD
  
  ## ---------------------------
  ## 3. 生成个体层随机效应
  ## ---------------------------
  id_vec <- unique(sim_df[[id_col]])
  
  re_df <- data.frame(
    ID  = id_vec,
    b0i = rnorm(length(id_vec), 0, sd_b0),
    b1i = rnorm(length(id_vec), 0, sd_b1)
  )
  
  sim_df <- dplyr::left_join(
    sim_df,
    re_df,
    by = setNames("ID", id_col)
  )
  
  ## ---------------------------
  ## 4. 生成 Y
  ## ---------------------------
  sim_df <- sim_df %>%
    dplyr::mutate(
      Y = X_effect +
        (beta0 + b0i) +
        (beta1 + b1i) * t +
        rnorm(n(), mean = 0, sd = 0.5)
    )
  
  ## ---------------------------
  ## 5. 保存真值参数
  ## ---------------------------
  attr(sim_df, "true_params") <- list(
    beta_x = k_vec,
    beta0  = beta0,
    beta1  = beta1,
    sd_b0  = sd_b0,
    sd_b1  = sd_b1
  )
  
  sim_df
}

## 辅助函数：批量处理列表
add_Y_to_list <- function(data_list, k, time_effects, base_seed = 123) {
  lapply(seq_along(data_list), function(i) {
    set.seed(base_seed + i)
    add_Y(
      sim_df = data_list[[i]],
      k = k,
      time_effects = time_effects
    )
  })
}

k <- c(runif(6, 0.8, 1.2), rep(0.025, 4))

time_effects <- c(
  beta0 = 0.3,
  beta1 = 1.0,
  sd_b0 = 0.6,
  sd_b1 = 0.4
)

sim_df <- add_Y(
  sim_df = sim500_30_10V_lowINTER_1c_L1[[1]],
  k = k,
  time_effects = time_effects
)
sim_df



###########添加Y###############

######低相关性######
# 500样本，30时间点
sim500_30_10V_lowINTER_1c_L1 <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_lowINTER_1c_L2 <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_lowINTER_1c_L3 <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_lowINTER_1c_L4 <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_lowINTER_1c_L5 <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L5, k = k, time_effects = time_effects)

sim500_30_10V_lowBTW_1c_L1 <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_lowBTW_1c_L2 <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_lowBTW_1c_L3 <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_lowBTW_1c_L4 <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_lowBTW_1c_L5 <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L5, k = k, time_effects = time_effects)

# 500样本，70时间点
sim500_70_10V_lowINTER_1c_L1 <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_lowINTER_1c_L2 <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_lowINTER_1c_L3 <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_lowINTER_1c_L4 <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_lowINTER_1c_L5 <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L5, k = k, time_effects = time_effects)

sim500_70_10V_lowBTW_1c_L1 <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_lowBTW_1c_L2 <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_lowBTW_1c_L3 <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_lowBTW_1c_L4 <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_lowBTW_1c_L5 <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L5, k = k, time_effects = time_effects)

# 1000样本，30时间点
sim1000_30_10V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L5, k = k, time_effects = time_effects)

sim1000_30_10V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L5, k = k, time_effects = time_effects)

# 1000样本，70时间点
sim1000_70_10V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L5, k = k, time_effects = time_effects)

sim1000_70_10V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L5, k = k, time_effects = time_effects)

#######中相关性#####
# 500样本，30时间点
sim500_30_10V_midINTER_1c_L1 <- add_Y_to_list(sim500_30_10V_midINTER_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_midINTER_1c_L2 <- add_Y_to_list(sim500_30_10V_midINTER_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_midINTER_1c_L3 <- add_Y_to_list(sim500_30_10V_midINTER_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_midINTER_1c_L4 <- add_Y_to_list(sim500_30_10V_midINTER_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_midINTER_1c_L5 <- add_Y_to_list(sim500_30_10V_midINTER_1c_L5, k = k, time_effects = time_effects)

sim500_30_10V_midBTW_1c_L1 <- add_Y_to_list(sim500_30_10V_midBTW_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_midBTW_1c_L2 <- add_Y_to_list(sim500_30_10V_midBTW_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_midBTW_1c_L3 <- add_Y_to_list(sim500_30_10V_midBTW_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_midBTW_1c_L4 <- add_Y_to_list(sim500_30_10V_midBTW_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_midBTW_1c_L5 <- add_Y_to_list(sim500_30_10V_midBTW_1c_L5, k = k, time_effects = time_effects)

# 500样本，70时间点
sim500_70_10V_midINTER_1c_L1 <- add_Y_to_list(sim500_70_10V_midINTER_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_midINTER_1c_L2 <- add_Y_to_list(sim500_70_10V_midINTER_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_midINTER_1c_L3 <- add_Y_to_list(sim500_70_10V_midINTER_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_midINTER_1c_L4 <- add_Y_to_list(sim500_70_10V_midINTER_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_midINTER_1c_L5 <- add_Y_to_list(sim500_70_10V_midINTER_1c_L5, k = k, time_effects = time_effects)

sim500_70_10V_midBTW_1c_L1 <- add_Y_to_list(sim500_70_10V_midBTW_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_midBTW_1c_L2 <- add_Y_to_list(sim500_70_10V_midBTW_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_midBTW_1c_L3 <- add_Y_to_list(sim500_70_10V_midBTW_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_midBTW_1c_L4 <- add_Y_to_list(sim500_70_10V_midBTW_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_midBTW_1c_L5 <- add_Y_to_list(sim500_70_10V_midBTW_1c_L5, k = k, time_effects = time_effects)

# 1000样本，30时间点
sim1000_30_10V_midINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_midINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_midINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_midINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_midINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L5, k = k, time_effects = time_effects)

sim1000_30_10V_midBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_midBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_midBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_midBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_midBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L5, k = k, time_effects = time_effects)

# 1000样本，70时间点
sim1000_70_10V_midINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_midINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_midINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_midINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_midINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L5, k = k, time_effects = time_effects)

sim1000_70_10V_midBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_midBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_midBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_midBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_midBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L5, k = k, time_effects = time_effects)

#######高相关性#####
# 样本量500，变量30
sim500_30_10V_highINTER_1c_L1 <- add_Y_to_list(sim500_30_10V_highINTER_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_highINTER_1c_L2 <- add_Y_to_list(sim500_30_10V_highINTER_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_highINTER_1c_L3 <- add_Y_to_list(sim500_30_10V_highINTER_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_highINTER_1c_L4 <- add_Y_to_list(sim500_30_10V_highINTER_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_highINTER_1c_L5 <- add_Y_to_list(sim500_30_10V_highINTER_1c_L5, k = k, time_effects = time_effects)
sim500_30_10V_highBTW_1c_L1 <- add_Y_to_list(sim500_30_10V_highBTW_1c_L1, k = k, time_effects = time_effects)
sim500_30_10V_highBTW_1c_L2 <- add_Y_to_list(sim500_30_10V_highBTW_1c_L2, k = k, time_effects = time_effects)
sim500_30_10V_highBTW_1c_L3 <- add_Y_to_list(sim500_30_10V_highBTW_1c_L3, k = k, time_effects = time_effects)
sim500_30_10V_highBTW_1c_L4 <- add_Y_to_list(sim500_30_10V_highBTW_1c_L4, k = k, time_effects = time_effects)
sim500_30_10V_highBTW_1c_L5 <- add_Y_to_list(sim500_30_10V_highBTW_1c_L5, k = k, time_effects = time_effects)

# 样本量500，变量70
sim500_70_10V_highINTER_1c_L1 <- add_Y_to_list(sim500_70_10V_highINTER_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_highINTER_1c_L2 <- add_Y_to_list(sim500_70_10V_highINTER_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_highINTER_1c_L3 <- add_Y_to_list(sim500_70_10V_highINTER_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_highINTER_1c_L4 <- add_Y_to_list(sim500_70_10V_highINTER_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_highINTER_1c_L5 <- add_Y_to_list(sim500_70_10V_highINTER_1c_L5, k = k, time_effects = time_effects)
sim500_70_10V_highBTW_1c_L1 <- add_Y_to_list(sim500_70_10V_highBTW_1c_L1, k = k, time_effects = time_effects)
sim500_70_10V_highBTW_1c_L2 <- add_Y_to_list(sim500_70_10V_highBTW_1c_L2, k = k, time_effects = time_effects)
sim500_70_10V_highBTW_1c_L3 <- add_Y_to_list(sim500_70_10V_highBTW_1c_L3, k = k, time_effects = time_effects)
sim500_70_10V_highBTW_1c_L4 <- add_Y_to_list(sim500_70_10V_highBTW_1c_L4, k = k, time_effects = time_effects)
sim500_70_10V_highBTW_1c_L5 <- add_Y_to_list(sim500_70_10V_highBTW_1c_L5, k = k, time_effects = time_effects)

# 样本量1000，变量30
sim1000_30_10V_highINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_highINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_highINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_highINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_highINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L5, k = k, time_effects = time_effects)
sim1000_30_10V_highBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_30_10V_highBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_30_10V_highBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_30_10V_highBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_30_10V_highBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L5, k = k, time_effects = time_effects)

# 样本量1000，变量70
sim1000_70_10V_highINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_highINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_highINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_highINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_highINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L5, k = k, time_effects = time_effects)
sim1000_70_10V_highBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L1, k = k, time_effects = time_effects)
sim1000_70_10V_highBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L2, k = k, time_effects = time_effects)
sim1000_70_10V_highBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L3, k = k, time_effects = time_effects)
sim1000_70_10V_highBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L4, k = k, time_effects = time_effects)
sim1000_70_10V_highBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L5, k = k, time_effects = time_effects)

################保存数据##################

# 设置输出目录
out_dir <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"


# 确保目录存在
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

################## 低相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_lowINTER_1c_L1, path = file.path(out_dir, "sim500_30_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_1c_L2, path = file.path(out_dir, "sim500_30_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_1c_L3, path = file.path(out_dir, "sim500_30_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_1c_L4, path = file.path(out_dir, "sim500_30_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_1c_L5, path = file.path(out_dir, "sim500_30_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim500_30_10V_lowBTW_1c_L1, path = file.path(out_dir, "sim500_30_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_1c_L2, path = file.path(out_dir, "sim500_30_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_1c_L3, path = file.path(out_dir, "sim500_30_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_1c_L4, path = file.path(out_dir, "sim500_30_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_1c_L5, path = file.path(out_dir, "sim500_30_10V_lowBTW_1c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_lowINTER_1c_L1, path = file.path(out_dir, "sim500_70_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_1c_L2, path = file.path(out_dir, "sim500_70_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_1c_L3, path = file.path(out_dir, "sim500_70_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_1c_L4, path = file.path(out_dir, "sim500_70_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_1c_L5, path = file.path(out_dir, "sim500_70_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim500_70_10V_lowBTW_1c_L1, path = file.path(out_dir, "sim500_70_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_1c_L2, path = file.path(out_dir, "sim500_70_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_1c_L3, path = file.path(out_dir, "sim500_70_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_1c_L4, path = file.path(out_dir, "sim500_70_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_1c_L5, path = file.path(out_dir, "sim500_70_10V_lowBTW_1c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_lowINTER_1c_L1, path = file.path(out_dir, "sim1000_30_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_1c_L2, path = file.path(out_dir, "sim1000_30_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_1c_L3, path = file.path(out_dir, "sim1000_30_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_1c_L4, path = file.path(out_dir, "sim1000_30_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_1c_L5, path = file.path(out_dir, "sim1000_30_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim1000_30_10V_lowBTW_1c_L1, path = file.path(out_dir, "sim1000_30_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_1c_L2, path = file.path(out_dir, "sim1000_30_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_1c_L3, path = file.path(out_dir, "sim1000_30_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_1c_L4, path = file.path(out_dir, "sim1000_30_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_1c_L5, path = file.path(out_dir, "sim1000_30_10V_lowBTW_1c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_lowINTER_1c_L1, path = file.path(out_dir, "sim1000_70_10V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_1c_L2, path = file.path(out_dir, "sim1000_70_10V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_1c_L3, path = file.path(out_dir, "sim1000_70_10V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_1c_L4, path = file.path(out_dir, "sim1000_70_10V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_1c_L5, path = file.path(out_dir, "sim1000_70_10V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim1000_70_10V_lowBTW_1c_L1, path = file.path(out_dir, "sim1000_70_10V_lowBTW_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_1c_L2, path = file.path(out_dir, "sim1000_70_10V_lowBTW_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_1c_L3, path = file.path(out_dir, "sim1000_70_10V_lowBTW_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_1c_L4, path = file.path(out_dir, "sim1000_70_10V_lowBTW_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_1c_L5, path = file.path(out_dir, "sim1000_70_10V_lowBTW_1c_L5.xlsx"))

################## 中相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_midINTER_1c_L1, path = file.path(out_dir, "sim500_30_10V_midINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_midINTER_1c_L2, path = file.path(out_dir, "sim500_30_10V_midINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_midINTER_1c_L3, path = file.path(out_dir, "sim500_30_10V_midINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_midINTER_1c_L4, path = file.path(out_dir, "sim500_30_10V_midINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_midINTER_1c_L5, path = file.path(out_dir, "sim500_30_10V_midINTER_1c_L5.xlsx"))

write_xlsx(sim500_30_10V_midBTW_1c_L1, path = file.path(out_dir, "sim500_30_10V_midBTW_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_midBTW_1c_L2, path = file.path(out_dir, "sim500_30_10V_midBTW_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_midBTW_1c_L3, path = file.path(out_dir, "sim500_30_10V_midBTW_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_midBTW_1c_L4, path = file.path(out_dir, "sim500_30_10V_midBTW_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_midBTW_1c_L5, path = file.path(out_dir, "sim500_30_10V_midBTW_1c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_midINTER_1c_L1, path = file.path(out_dir, "sim500_70_10V_midINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_midINTER_1c_L2, path = file.path(out_dir, "sim500_70_10V_midINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_midINTER_1c_L3, path = file.path(out_dir, "sim500_70_10V_midINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_midINTER_1c_L4, path = file.path(out_dir, "sim500_70_10V_midINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_midINTER_1c_L5, path = file.path(out_dir, "sim500_70_10V_midINTER_1c_L5.xlsx"))

write_xlsx(sim500_70_10V_midBTW_1c_L1, path = file.path(out_dir, "sim500_70_10V_midBTW_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_midBTW_1c_L2, path = file.path(out_dir, "sim500_70_10V_midBTW_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_midBTW_1c_L3, path = file.path(out_dir, "sim500_70_10V_midBTW_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_midBTW_1c_L4, path = file.path(out_dir, "sim500_70_10V_midBTW_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_midBTW_1c_L5, path = file.path(out_dir, "sim500_70_10V_midBTW_1c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_midINTER_1c_L1, path = file.path(out_dir, "sim1000_30_10V_midINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_1c_L2, path = file.path(out_dir, "sim1000_30_10V_midINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_1c_L3, path = file.path(out_dir, "sim1000_30_10V_midINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_1c_L4, path = file.path(out_dir, "sim1000_30_10V_midINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_1c_L5, path = file.path(out_dir, "sim1000_30_10V_midINTER_1c_L5.xlsx"))

write_xlsx(sim1000_30_10V_midBTW_1c_L1, path = file.path(out_dir, "sim1000_30_10V_midBTW_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_1c_L2, path = file.path(out_dir, "sim1000_30_10V_midBTW_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_1c_L3, path = file.path(out_dir, "sim1000_30_10V_midBTW_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_1c_L4, path = file.path(out_dir, "sim1000_30_10V_midBTW_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_1c_L5, path = file.path(out_dir, "sim1000_30_10V_midBTW_1c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_midINTER_1c_L1, path = file.path(out_dir, "sim1000_70_10V_midINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_1c_L2, path = file.path(out_dir, "sim1000_70_10V_midINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_1c_L3, path = file.path(out_dir, "sim1000_70_10V_midINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_1c_L4, path = file.path(out_dir, "sim1000_70_10V_midINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_1c_L5, path = file.path(out_dir, "sim1000_70_10V_midINTER_1c_L5.xlsx"))

write_xlsx(sim1000_70_10V_midBTW_1c_L1, path = file.path(out_dir, "sim1000_70_10V_midBTW_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_1c_L2, path = file.path(out_dir, "sim1000_70_10V_midBTW_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_1c_L3, path = file.path(out_dir, "sim1000_70_10V_midBTW_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_1c_L4, path = file.path(out_dir, "sim1000_70_10V_midBTW_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_1c_L5, path = file.path(out_dir, "sim1000_70_10V_midBTW_1c_L5.xlsx"))

################## 高相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_highINTER_1c_L1, path = file.path(out_dir, "sim500_30_10V_highINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_highINTER_1c_L2, path = file.path(out_dir, "sim500_30_10V_highINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_highINTER_1c_L3, path = file.path(out_dir, "sim500_30_10V_highINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_highINTER_1c_L4, path = file.path(out_dir, "sim500_30_10V_highINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_highINTER_1c_L5, path = file.path(out_dir, "sim500_30_10V_highINTER_1c_L5.xlsx"))
write_xlsx(sim500_30_10V_highBTW_1c_L1, path = file.path(out_dir, "sim500_30_10V_highBTW_1c_L1.xlsx"))
write_xlsx(sim500_30_10V_highBTW_1c_L2, path = file.path(out_dir, "sim500_30_10V_highBTW_1c_L2.xlsx"))
write_xlsx(sim500_30_10V_highBTW_1c_L3, path = file.path(out_dir, "sim500_30_10V_highBTW_1c_L3.xlsx"))
write_xlsx(sim500_30_10V_highBTW_1c_L4, path = file.path(out_dir, "sim500_30_10V_highBTW_1c_L4.xlsx"))
write_xlsx(sim500_30_10V_highBTW_1c_L5, path = file.path(out_dir, "sim500_30_10V_highBTW_1c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_highINTER_1c_L1, path = file.path(out_dir, "sim500_70_10V_highINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_highINTER_1c_L2, path = file.path(out_dir, "sim500_70_10V_highINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_highINTER_1c_L3, path = file.path(out_dir, "sim500_70_10V_highINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_highINTER_1c_L4, path = file.path(out_dir, "sim500_70_10V_highINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_highINTER_1c_L5, path = file.path(out_dir, "sim500_70_10V_highINTER_1c_L5.xlsx"))
write_xlsx(sim500_70_10V_highBTW_1c_L1, path = file.path(out_dir, "sim500_70_10V_highBTW_1c_L1.xlsx"))
write_xlsx(sim500_70_10V_highBTW_1c_L2, path = file.path(out_dir, "sim500_70_10V_highBTW_1c_L2.xlsx"))
write_xlsx(sim500_70_10V_highBTW_1c_L3, path = file.path(out_dir, "sim500_70_10V_highBTW_1c_L3.xlsx"))
write_xlsx(sim500_70_10V_highBTW_1c_L4, path = file.path(out_dir, "sim500_70_10V_highBTW_1c_L4.xlsx"))
write_xlsx(sim500_70_10V_highBTW_1c_L5, path = file.path(out_dir, "sim500_70_10V_highBTW_1c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_highINTER_1c_L1, path = file.path(out_dir, "sim1000_30_10V_highINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_1c_L2, path = file.path(out_dir, "sim1000_30_10V_highINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_1c_L3, path = file.path(out_dir, "sim1000_30_10V_highINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_1c_L4, path = file.path(out_dir, "sim1000_30_10V_highINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_1c_L5, path = file.path(out_dir, "sim1000_30_10V_highINTER_1c_L5.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_1c_L1, path = file.path(out_dir, "sim1000_30_10V_highBTW_1c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_1c_L2, path = file.path(out_dir, "sim1000_30_10V_highBTW_1c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_1c_L3, path = file.path(out_dir, "sim1000_30_10V_highBTW_1c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_1c_L4, path = file.path(out_dir, "sim1000_30_10V_highBTW_1c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_1c_L5, path = file.path(out_dir, "sim1000_30_10V_highBTW_1c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_highINTER_1c_L1, path = file.path(out_dir, "sim1000_70_10V_highINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_1c_L2, path = file.path(out_dir, "sim1000_70_10V_highINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_1c_L3, path = file.path(out_dir, "sim1000_70_10V_highINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_1c_L4, path = file.path(out_dir, "sim1000_70_10V_highINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_1c_L5, path = file.path(out_dir, "sim1000_70_10V_highINTER_1c_L5.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_1c_L1, path = file.path(out_dir, "sim1000_70_10V_highBTW_1c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_1c_L2, path = file.path(out_dir, "sim1000_70_10V_highBTW_1c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_1c_L3, path = file.path(out_dir, "sim1000_70_10V_highBTW_1c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_1c_L4, path = file.path(out_dir, "sim1000_70_10V_highBTW_1c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_1c_L5, path = file.path(out_dir, "sim1000_70_10V_highBTW_1c_L5.xlsx"))

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
###############





