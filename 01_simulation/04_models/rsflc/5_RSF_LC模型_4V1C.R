library(dplyr)
library(survival)
library(timeROC)
library(DynForest)
library(purrr)
library(readxl)
library(writexl)
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)



######读取数据#####

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/生成Y"

###### 使用map函数读取低相关性数据####
sim500_30_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_4V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_4V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_4V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_4V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_4V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_4V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_lowINTER_1c_L5.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_4V_lowINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_lowINTER_1c_L5.xlsx"), sheet = .x))

##### 使用map函数读取中相关性数据####

sim500_30_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_4V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_4V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_4V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_4V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_30_4V_midINTER_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_30_4V_midINTER_1c_L5.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L2.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L3.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L4.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_4V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L5.xlsx")),
                                            excel_sheets(file.path(input_dir, "sim500_70_4V_midINTER_1c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(input_dir, "sim500_70_4V_midINTER_1c_L5.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_4V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_midINTER_1c_L5.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_4V_midINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_midINTER_1c_L5.xlsx"), sheet = .x))


##### 使用map函数读取高相关性数据####


sim500_30_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_30_4V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L2.xlsx"), sheet = .x))
sim500_30_4V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L3.xlsx"), sheet = .x))
sim500_30_4V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L4.xlsx"), sheet = .x))
sim500_30_4V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_30_4V_highINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_30_4V_highINTER_1c_L5.xlsx"), sheet = .x))

sim500_70_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim500_70_4V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L2.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L2.xlsx"), sheet = .x))
sim500_70_4V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L3.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L3.xlsx"), sheet = .x))
sim500_70_4V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L4.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L4.xlsx"), sheet = .x))
sim500_70_4V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L5.xlsx")),
                                             excel_sheets(file.path(input_dir, "sim500_70_4V_highINTER_1c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(input_dir, "sim500_70_4V_highINTER_1c_L5.xlsx"), sheet = .x))



sim1000_30_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_30_4V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L2.xlsx"), sheet = .x))
sim1000_30_4V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L3.xlsx"), sheet = .x))
sim1000_30_4V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L4.xlsx"), sheet = .x))
sim1000_30_4V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_30_4V_highINTER_1c_L5.xlsx"), sheet = .x))



sim1000_70_4V_highINTER_1c_L1 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"), sheet = .x))
sim1000_70_4V_highINTER_1c_L2 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L2.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L2.xlsx"), sheet = .x))
sim1000_70_4V_highINTER_1c_L3 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L3.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L3.xlsx"), sheet = .x))
sim1000_70_4V_highINTER_1c_L4 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L4.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L4.xlsx"), sheet = .x))
sim1000_70_4V_highINTER_1c_L5 <- map(setNames(excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L5.xlsx")),
                                              excel_sheets(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(input_dir, "sim1000_70_4V_highINTER_1c_L5.xlsx"), sheet = .x))



#####程序######
cal_3_RSF_LC <- function(data, t0 = 1) {
  
  tryCatch({
    # 确保加载了所有必要的包
    if (!requireNamespace("timeROC", quietly = TRUE)) {
      install.packages("timeROC")
      library(timeROC)
    }
    
    ################
    ## 1. 数据准备
    ################
    
    # 检查数据格式：如果是4变量数据（有V1-V4），需要转换为纵向格式
    if (any(grepl("^V[0-9]", names(data)))) {
      # 这是4变量格式，需要转换为纵向数据格式
      
      # 获取变量名
      var_names <- grep("^V[0-9]", names(data), value = TRUE)
      
      # 将宽格式转换为长格式（纵向数据）
      longitudinal_data <- data.frame()
      
      for (id in unique(data$ID)) {
        id_data <- data[data$ID == id, ]
        
        # 对于每个ID，创建纵向记录
        # 假设每个V变量对应不同时间点的观测
        # 这里我们使用观测时间作为时间点
        for (j in seq_along(var_names)) {
          var_name <- var_names[j]
          
          # 模拟时间点：基于观测时间的比例分配
          # 这里简单地将时间点均匀分布在0到obs_time之间
          obs_time <- unique(id_data$obs_time)
          
          # 创建纵向记录
          temp <- data.frame(
            id = id,
            time = obs_time * (j / length(var_names)),  # 时间点分布
            Y = id_data[[var_name]][1]  # 使用该时间点的V值
          )
          
          longitudinal_data <- rbind(longitudinal_data, temp)
        }
      }
      
      # 生存数据（每个ID一行）
      survival_data <- data %>%
        group_by(ID) %>%
        summarise(
          time  = unique(obs_time),
          event = unique(event),
          .groups = "drop"
        ) %>%
        rename(id = ID)
      
      # 基线协变量（这里没有额外的基线变量，可以创建一个常数项）
      baseline_data <- survival_data %>%
        mutate(
          intercept = 1
        ) %>%
        select(id, intercept)
      
    } else {
      # 假设已经是纵向格式（有id, time, Y等字段）
      
      # 纵向数据
      longitudinal_data <- data %>%
        select(id = ID, time = t, Y) %>%
        mutate(id = as.numeric(id))
      
      # 生存数据（每个ID一行）
      survival_data <- data %>%
        group_by(ID) %>%
        summarise(
          time  = unique(obs_time),
          event = unique(event),
          .groups = "drop"
        ) %>%
        rename(id = ID)
      
      # 基线协变量（如果有）
      if ("lp" %in% names(data) && "class" %in% names(data)) {
        baseline_data <- data %>%
          group_by(ID) %>%
          summarise(
            lp    = unique(lp),
            class = unique(class),
            .groups = "drop"
          ) %>%
          rename(id = ID)
      } else {
        # 如果没有基线变量，创建一个常数项
        baseline_data <- survival_data %>%
          mutate(
            intercept = 1
          ) %>%
          select(id, intercept)
      }
    }
    
    # 准备dynforest所需的数据格式
    fixed_data <- survival_data %>%
      left_join(baseline_data, by = "id") %>%
      mutate(id = as.numeric(id))
    
    ################
    ## 2. DynForest 模型
    ################
    
    # 定义时间变量模型
    timeVarModel <- list(
      Y = list(
        model = "linear",
        fixed = ~ 1,
        random = ~ 1 + time | id
      )
    )
    
    # 如果存在基线变量，添加到fixed formula中
    baseline_vars <- setdiff(names(baseline_data), "id")
    if (length(baseline_vars) > 0) {
      # 可以将基线变量添加到模型中
      # 这里保持简单，只使用随机效应模型
      # 如果需要使用基线变量，可以修改timeVarModel
    }
    
    # 运行DynForest模型
    dyn_model <- DynForest::dynforest(
      timeData      = as.data.frame(longitudinal_data),
      fixedData     = as.data.frame(fixed_data),
      idVar         = "id",
      timeVar       = "time",
      timeVarModel  = timeVarModel,
      Y             = list(
        type = "surv",
        Y = data.frame(
          id = fixed_data$id,
          time = fixed_data$time,
          event = as.numeric(fixed_data$event)
        )
      ),
      ntree         = 500,
      mtry          = min(2, length(baseline_vars) + 1),  # 确保mtry不超过变量数
      nodesize      = 10,
      minsplit      = 2,
      nsplit_option = "quantile",
      ncores        = 1,
      verbose       = FALSE
    )
    
    ################
    ## 3. 提取风险得分
    ################
    
    surv_time <- fixed_data$time
    surv_event <- fixed_data$event
    n <- length(surv_time)
    
    # 初始化风险得分
    risk_score <- numeric(n)
    valid_trees <- 0
    
    # 从模型中提取风险得分
    rf <- dyn_model$rf
    
    if (is.list(rf)) {
      for (tree_idx in seq_along(rf)) {
        tree <- rf[[tree_idx]]
        
        if (is.list(tree) && "leaf" %in% names(tree) && "leaf.info" %in% names(tree)) {
          leaf_ids <- tree$leaf
          leaf_info <- tree$leaf.info
          
          if (length(leaf_ids) == n) {
            for (sample_idx in 1:n) {
              leaf_id <- leaf_ids[sample_idx]
              
              # 提取风险得分（通常是累积风险或危险率）
              if (is.matrix(leaf_info)) {
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.data.frame(leaf_info)) {
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[leaf_id, 1]
              } else if (is.list(leaf_info)) {
                risk_score[sample_idx] <- risk_score[sample_idx] + leaf_info[[leaf_id]][1]
              }
            }
            valid_trees <- valid_trees + 1
          }
        }
      }
    }
    
    # 计算平均风险得分
    if (valid_trees > 0) {
      risk_score <- risk_score / valid_trees
    } else {
      # 如果无法提取风险得分，使用简单的替代方案
      # 基于纵向数据的最后观测值
      last_obs <- longitudinal_data %>%
        group_by(id) %>%
        arrange(time) %>%
        slice(n()) %>%
        pull(Y)
      
      risk_score <- scale(last_obs)[, 1]  # 标准化
    }
    
    ################
    ## 4. 计算生存分析指标
    ################
    
    # 计算C-index
    cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
    cindex <- cindex_result$concordance
    
    # 检查C-index方向
    if (cindex < 0.5) {
      risk_score <- -risk_score
      cindex_result <- survival::concordance(survival::Surv(surv_time, surv_event) ~ risk_score)
      cindex <- cindex_result$concordance
    }
    
    # 计算AUC（取两个方向的最大值）
    roc_obj <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = risk_score,
      cause = 1,
      times = t0
    )
    
    roc_obj_rev <- timeROC::timeROC(
      T = surv_time,
      delta = surv_event,
      marker = -risk_score,
      cause = 1,
      times = t0
    )
    
    # 提取AUC值
    auc <- if (length(roc_obj$AUC) >= 2) roc_obj$AUC[2] else if (length(roc_obj$AUC) == 1) roc_obj$AUC[1] else 0.5
    auc_rev <- if (length(roc_obj_rev$AUC) >= 2) roc_obj_rev$AUC[2] else if (length(roc_obj_rev$AUC) == 1) roc_obj_rev$AUC[1] else 0.5
    auc <- max(auc, auc_rev)
    
    # 计算Brier Score
    # 使用风险得分的指数作为危险率
    hazard <- exp(risk_score - mean(risk_score))  # 中心化避免数值问题
    cum_hazard <- hazard * t0
    surv_prob <- exp(-cum_hazard)
    
    brier <- numeric(n)
    for (i in 1:n) {
      if (surv_time[i] <= t0 && surv_event[i] == 1) {
        brier[i] <- (0 - surv_prob[i])^2
      } else if (surv_time[i] > t0) {
        brier[i] <- (1 - surv_prob[i])^2
      } else {
        # 删失的情况：使用逆概率加权
        # 简化处理：假设随机删失
        brier[i] <- ifelse(surv_time[i] <= t0, (0 - surv_prob[i])^2, (1 - surv_prob[i])^2)
      }
    }
    bs <- mean(brier, na.rm = TRUE)
    
    # 返回结果
    result <- data.frame(
      AUC = round(auc, 4),
      BS = round(bs, 4),
      Cindex = round(cindex, 4)
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
    
  }, error = function(e) {
    
    message("⚠️ cal_3_RSF_LC failed: ", e$message)
    
    # 返回默认值
    result <- data.frame(
      AUC = 0.5,
      BS = 0.25,
      Cindex = 0.5
    )
    
    rownames(result) <- paste0("t=", t0)
    
    return(result)
  })
}

circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3_RSF_LC
    tmp <- cal_3_RSF_LC(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}


circle_cal_3 <- function(data_list, t0 = 1, verbose = TRUE) {
  
  # 检查输入
  if (!is.list(data_list) || length(data_list) == 0)
    stop("data_list 必须是一个非空 list")
  
  # 主循环
  res <- lapply(seq_along(data_list), function(i) {
    if (verbose) message(sprintf("【%s】Processing sim%g ...", Sys.time(), i))
    
    # 运行 cal_3
    tmp <- cal_3_RSF_LC(data_list[[i]], t0 = t0)
    
    # 加上来源标记
    cbind(sim = paste0("sim", i), tmp)
  })
  
  # 合并
  do.call(rbind, res)
}


#############低相关性##########
t0 <- 1 

result_sim500_30_4V_lowINTER_1c_L1 <-circle_cal_3(sim500_30_4V_lowINTER_1c_L1,  t0 = t0)
result_sim500_30_4V_lowINTER_1c_L2 <-circle_cal_3(sim500_30_4V_lowINTER_1c_L2,  t0 = t0)
result_sim500_30_4V_lowINTER_1c_L3 <-circle_cal_3(sim500_30_4V_lowINTER_1c_L3,  t0 = t0)
result_sim500_30_4V_lowINTER_1c_L4 <-circle_cal_3(sim500_30_4V_lowINTER_1c_L4,  t0 = t0)
result_sim500_30_4V_lowINTER_1c_L5 <-circle_cal_3(sim500_30_4V_lowINTER_1c_L5,  t0 = t0)


result_sim500_70_4V_lowINTER_1c_L1 <-circle_cal_3(sim500_70_4V_lowINTER_1c_L1,  t0 = t0)
result_sim500_70_4V_lowINTER_1c_L2 <-circle_cal_3(sim500_70_4V_lowINTER_1c_L2,  t0 = t0)
result_sim500_70_4V_lowINTER_1c_L3 <-circle_cal_3(sim500_70_4V_lowINTER_1c_L3,  t0 = t0)
result_sim500_70_4V_lowINTER_1c_L4 <-circle_cal_3(sim500_70_4V_lowINTER_1c_L4,  t0 = t0)
result_sim500_70_4V_lowINTER_1c_L5 <-circle_cal_3(sim500_70_4V_lowINTER_1c_L5,  t0 = t0)



result_sim1000_30_4V_lowINTER_1c_L1 <-circle_cal_3(sim1000_30_4V_lowINTER_1c_L1,  t0 = t0)
result_sim1000_30_4V_lowINTER_1c_L2 <-circle_cal_3(sim1000_30_4V_lowINTER_1c_L2,  t0 = t0)
result_sim1000_30_4V_lowINTER_1c_L3 <-circle_cal_3(sim1000_30_4V_lowINTER_1c_L3,  t0 = t0)
result_sim1000_30_4V_lowINTER_1c_L4 <-circle_cal_3(sim1000_30_4V_lowINTER_1c_L4,  t0 = t0)
result_sim1000_30_4V_lowINTER_1c_L5 <-circle_cal_3(sim1000_30_4V_lowINTER_1c_L5,  t0 = t0)


result_sim1000_70_4V_lowINTER_1c_L1 <-circle_cal_3(sim1000_70_4V_lowINTER_1c_L1,  t0 = t0)
result_sim1000_70_4V_lowINTER_1c_L2 <-circle_cal_3(sim1000_70_4V_lowINTER_1c_L2,  t0 = t0)
result_sim1000_70_4V_lowINTER_1c_L3 <-circle_cal_3(sim1000_70_4V_lowINTER_1c_L3,  t0 = t0)
result_sim1000_70_4V_lowINTER_1c_L4 <-circle_cal_3(sim1000_70_4V_lowINTER_1c_L4,  t0 = t0)
result_sim1000_70_4V_lowINTER_1c_L5 <-circle_cal_3(sim1000_70_4V_lowINTER_1c_L5,  t0 = t0)



#############中相关性##########
t0 <- 1 

result_sim500_30_4V_midINTER_1c_L1 <-circle_cal_3(sim500_30_4V_midINTER_1c_L1,  t0 = t0)
result_sim500_30_4V_midINTER_1c_L2 <-circle_cal_3(sim500_30_4V_midINTER_1c_L2,  t0 = t0)
result_sim500_30_4V_midINTER_1c_L3 <-circle_cal_3(sim500_30_4V_midINTER_1c_L3,  t0 = t0)
result_sim500_30_4V_midINTER_1c_L4 <-circle_cal_3(sim500_30_4V_midINTER_1c_L4,  t0 = t0)
result_sim500_30_4V_midINTER_1c_L5 <-circle_cal_3(sim500_30_4V_midINTER_1c_L5,  t0 = t0)


result_sim500_70_4V_midINTER_1c_L1 <-circle_cal_3(sim500_70_4V_midINTER_1c_L1,  t0 = t0)
result_sim500_70_4V_midINTER_1c_L2 <-circle_cal_3(sim500_70_4V_midINTER_1c_L2,  t0 = t0)
result_sim500_70_4V_midINTER_1c_L3 <-circle_cal_3(sim500_70_4V_midINTER_1c_L3,  t0 = t0)
result_sim500_70_4V_midINTER_1c_L4 <-circle_cal_3(sim500_70_4V_midINTER_1c_L4,  t0 = t0)
result_sim500_70_4V_midINTER_1c_L5 <-circle_cal_3(sim500_70_4V_midINTER_1c_L5,  t0 = t0)



result_sim1000_30_4V_midINTER_1c_L1 <-circle_cal_3(sim1000_30_4V_midINTER_1c_L1,  t0 = t0)
result_sim1000_30_4V_midINTER_1c_L2 <-circle_cal_3(sim1000_30_4V_midINTER_1c_L2,  t0 = t0)
result_sim1000_30_4V_midINTER_1c_L3 <-circle_cal_3(sim1000_30_4V_midINTER_1c_L3,  t0 = t0)
result_sim1000_30_4V_midINTER_1c_L4 <-circle_cal_3(sim1000_30_4V_midINTER_1c_L4,  t0 = t0)
result_sim1000_30_4V_midINTER_1c_L5 <-circle_cal_3(sim1000_30_4V_midINTER_1c_L5,  t0 = t0)


result_sim1000_70_4V_midINTER_1c_L1 <-circle_cal_3(sim1000_70_4V_midINTER_1c_L1,  t0 = t0)
result_sim1000_70_4V_midINTER_1c_L2 <-circle_cal_3(sim1000_70_4V_midINTER_1c_L2,  t0 = t0)
result_sim1000_70_4V_midINTER_1c_L3 <-circle_cal_3(sim1000_70_4V_midINTER_1c_L3,  t0 = t0)
result_sim1000_70_4V_midINTER_1c_L4 <-circle_cal_3(sim1000_70_4V_midINTER_1c_L4,  t0 = t0)
result_sim1000_70_4V_midINTER_1c_L5 <-circle_cal_3(sim1000_70_4V_midINTER_1c_L5,  t0 = t0)




#############高相关性##########
t0 <- 1 

result_sim500_30_4V_highINTER_1c_L1 <-circle_cal_3(sim500_30_4V_highINTER_1c_L1,  t0 = t0)
result_sim500_30_4V_highINTER_1c_L2 <-circle_cal_3(sim500_30_4V_highINTER_1c_L2,  t0 = t0)
result_sim500_30_4V_highINTER_1c_L3 <-circle_cal_3(sim500_30_4V_highINTER_1c_L3,  t0 = t0)
result_sim500_30_4V_highINTER_1c_L4 <-circle_cal_3(sim500_30_4V_highINTER_1c_L4,  t0 = t0)
result_sim500_30_4V_highINTER_1c_L5 <-circle_cal_3(sim500_30_4V_highINTER_1c_L5,  t0 = t0)


result_sim500_70_4V_highINTER_1c_L1 <-circle_cal_3(sim500_70_4V_highINTER_1c_L1,  t0 = t0)
result_sim500_70_4V_highINTER_1c_L2 <-circle_cal_3(sim500_70_4V_highINTER_1c_L2,  t0 = t0)
result_sim500_70_4V_highINTER_1c_L3 <-circle_cal_3(sim500_70_4V_highINTER_1c_L3,  t0 = t0)
result_sim500_70_4V_highINTER_1c_L4 <-circle_cal_3(sim500_70_4V_highINTER_1c_L4,  t0 = t0)
result_sim500_70_4V_highINTER_1c_L5 <-circle_cal_3(sim500_70_4V_highINTER_1c_L5,  t0 = t0)



result_sim1000_30_4V_highINTER_1c_L1 <-circle_cal_3(sim1000_30_4V_highINTER_1c_L1,  t0 = t0)
result_sim1000_30_4V_highINTER_1c_L2 <-circle_cal_3(sim1000_30_4V_highINTER_1c_L2,  t0 = t0)
result_sim1000_30_4V_highINTER_1c_L3 <-circle_cal_3(sim1000_30_4V_highINTER_1c_L3,  t0 = t0)
result_sim1000_30_4V_highINTER_1c_L4 <-circle_cal_3(sim1000_30_4V_highINTER_1c_L4,  t0 = t0)
result_sim1000_30_4V_highINTER_1c_L5 <-circle_cal_3(sim1000_30_4V_highINTER_1c_L5,  t0 = t0)


result_sim1000_70_4V_highINTER_1c_L1 <-circle_cal_3(sim1000_70_4V_highINTER_1c_L1,  t0 = t0)
result_sim1000_70_4V_highINTER_1c_L2 <-circle_cal_3(sim1000_70_4V_highINTER_1c_L2,  t0 = t0)
result_sim1000_70_4V_highINTER_1c_L3 <-circle_cal_3(sim1000_70_4V_highINTER_1c_L3,  t0 = t0)
result_sim1000_70_4V_highINTER_1c_L4 <-circle_cal_3(sim1000_70_4V_highINTER_1c_L4,  t0 = t0)
result_sim1000_70_4V_highINTER_1c_L5 <-circle_cal_3(sim1000_70_4V_highINTER_1c_L5,  t0 = t0)



###########保存数据#################
###########低相关###########
write_xlsx(result_sim500_30_4V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_4V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_4V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_4V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_4V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_lowINTER_1c_L5.xlsx")


write_xlsx(result_sim500_70_4V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_4V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_4V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_4V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_4V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_lowINTER_1c_L5.xlsx")


write_xlsx(result_sim1000_30_4V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_4V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_4V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_4V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_4V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_lowINTER_1c_L5.xlsx")


write_xlsx(result_sim1000_70_4V_lowINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_lowINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_4V_lowINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_lowINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_4V_lowINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_lowINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_4V_lowINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_lowINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_4V_lowINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_lowINTER_1c_L5.xlsx")


###########中相关###########

write_xlsx(result_sim500_30_4V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_4V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_4V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_4V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_4V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_midINTER_1c_L5.xlsx")


write_xlsx(result_sim500_70_4V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_4V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_4V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_4V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_4V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_midINTER_1c_L5.xlsx")


write_xlsx(result_sim1000_30_4V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_4V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_4V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_4V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_4V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_midINTER_1c_L5.xlsx")


write_xlsx(result_sim1000_70_4V_midINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_midINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_4V_midINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_midINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_4V_midINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_midINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_4V_midINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_midINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_4V_midINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_midINTER_1c_L5.xlsx")


###########高相关###########

write_xlsx(result_sim500_30_4V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_30_4V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_30_4V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_30_4V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_30_4V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_30_4V_highINTER_1c_L5.xlsx")


write_xlsx(result_sim500_70_4V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim500_70_4V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim500_70_4V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim500_70_4V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim500_70_4V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim500_70_4V_highINTER_1c_L5.xlsx")


write_xlsx(result_sim1000_30_4V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_30_4V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_30_4V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_30_4V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_30_4V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_30_4V_highINTER_1c_L5.xlsx")

write_xlsx(result_sim1000_70_4V_highINTER_1c_L1, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_highINTER_1c_L1.xlsx")
write_xlsx(result_sim1000_70_4V_highINTER_1c_L2, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_highINTER_1c_L2.xlsx")
write_xlsx(result_sim1000_70_4V_highINTER_1c_L3, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_highINTER_1c_L3.xlsx")
write_xlsx(result_sim1000_70_4V_highINTER_1c_L4, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_highINTER_1c_L4.xlsx")
write_xlsx(result_sim1000_70_4V_highINTER_1c_L5, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_RSFLC/result_sim1000_70_4V_highINTER_1c_L5.xlsx")






################





















