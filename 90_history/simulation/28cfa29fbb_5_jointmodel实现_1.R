library(survival)      # Surv()
library(rms)           # cph()
library(timeROC)       # timeROC()
library(dplyr)         # 基本数据处理
library(joineRML)      # mjoint()
library(readxl)        # read_excel()（如已改用 gdata 可再删）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


################数据输入尝试#############
simdata3<-read_xlsx("F:/文章/大论文/程序/模拟数据/simdata3_1.xlsx")
data("heart.valve")
head(heart.valve)

clean_data <- function(data) {
  data[data$t <= data$obs_time, ]
}

simdata3_clean <- clean_data(simdata3)

file_path <- "F:/文章/大论文/程序/模拟数据/500_30删失_10V_HIGH(变量间)_3类/sim500_30_10V_highINTER_3c_1.xlsx"
simdata <- read_excel(file_path)
print(simdata3, width = Inf)



hvd <- heart.valve[!is.na(heart.valve$grad) & !is.na(heart.valve$lvmi), ]
hvd <- hvd[hvd$num <= 50, ]
head(hvd)

##########读取数据#################
t0 <- 1 


# 加载必要的包
library(readxl)
library(writexl)

# 设置数据目录路径
data_dir <- "F:/文章/大论文/程序/模拟数据/数据3_withY"

# 设置各相关性级别的子目录
out_dir_low <- file.path(data_dir, "低相关")
out_dir_mid <- file.path(data_dir, "中相关")  
out_dir_high <- file.path(data_dir, "高相关")

##################  低相关性数据导入 ##################
library(readxl)
library(purrr)

# 样本量500，变量30
sim500_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

################## 中相关性数据导入 ##################
library(readxl)
library(purrr)
# 样本量500，变量30
sim500_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))



################## 高相关性数据导入 ##################
library(readxl)
library(purrr)

# 样本量500，变量30
sim500_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))

###############计算三个整合代码####################

cal_3 <- function(data, t0 = 1) {
  #----------------#
  # 1. 统一数据清理
  #----------------#
  data_clean <- data[data$t <= data$obs_time, ]
  
  #----------------#
  # 2. 统一建立mjoint模型
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
  # 3. 提取生存数据（每患者一条）
  #----------------#
  surv_data <- data_clean[!duplicated(data_clean$ID), ]
  surv_time <- surv_data$obs_time
  surv_status <- surv_data$event
  
  #----------------#
  # 4. 提取模型参数
  #----------------#
  beta <- fit$coefficients$beta   # 纵向固定效应
  gamma <- fit$coefficients$gamma # 关联参数
  random_effects <- ranef(fit)    # 随机效应
  
  #----------------#
  # 5. 计算完整的风险得分（固定效应 + 随机效应）
  #----------------#
  # 获取每个个体的基线数据
  baseline_data <- data_clean[!duplicated(data_clean$ID), ]
  baseline_data$t <- t0  # 设置时间为t0
  
  # 计算固定效应部分
  X_matrix <- model.matrix(~ t + V1 + V2 + V3 + V4 + V5 + V6, 
                           data = baseline_data)
  fixed_part <- as.numeric(X_matrix %*% beta)
  
  # 计算随机效应部分（截距 + 斜率×t0）
  random_part <- random_effects[, 1] + random_effects[, 2] * t0
  
  # 完整风险得分 = γ × (固定效应 + 随机效应)
  comprehensive_risk <- as.numeric(gamma) * (fixed_part + random_part)
  
  #----------------#
  # 6. 计算AUC（使用完整风险得分）
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
  # 7. 计算C-index（使用完整风险得分）
  #----------------#
  n <- length(surv_time)
  idx <- combn(n, 2)  # 所有pair的索引
  
  i <- idx[1, ]
  j <- idx[2, ]
  
  # 判断可比对pair（短时间的个体必须发生事件）
  comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
  comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
  comparable <- comparable_ij | comparable_ji
  
  # Concordant判断
  concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
    (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))
  
  tied <- (comparable & (comprehensive_risk[i] == comprehensive_risk[j]))
  
  n_pairs <- sum(comparable)
  n_concordant <- sum(concordant)
  n_tied <- sum(tied)
  
  Cindex <- ifelse(n_pairs > 0, (n_concordant + 0.5 * n_tied) / n_pairs, NA)
  
  #----------------#
  # 8. 计算BS（保持原有方法）
  #----------------#
  # 线性预测子（纵向部分）- 使用完整数据
  lp_long_full <- model.matrix(~ t + V1 + V2 + V3 + V4 + V5 + V6, 
                               data = data_clean) %*% beta
  
  # 基线生存函数估计
  S0_t0 <- summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)), 
                   times = t0)$surv
  
  # 个体预测生存概率
  S_pred <- S0_t0 ^ exp(as.numeric(gamma) * as.numeric(lp_long_full))
  
  # 计算删失权重
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = data_clean)
  
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                      ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
    return(weights)
  }
  
  weights <- get_weights(data_clean$obs_time, data_clean$event, censoring_model, t0)
  
  # 构造观测指标
  Y_obs <- as.numeric(data_clean$obs_time > t0 | 
                        (data_clean$obs_time <= t0 & data_clean$event == 0))
  
  # 计算加权Brier Score
  BS <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)
  
  #----------------#
  # 9. 返回结果
  #----------------#
  result <- data.frame(
    AUC = AUC,
    BS = BS,
    Cindex = Cindex
  )
  
  return(result)
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

#############建模并输出3结果###########################################



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




#######









#########输出结果#######
library(writexl)




#############输出低相关性#######
write_xlsx(result_sim500_30_10V_lowINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowINTER_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowINTER_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowINTER_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowINTER_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowINTER_3c_L5.xlsx")

write_xlsx(result_sim500_30_10V_lowBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowBTW_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowBTW_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowBTW_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowBTW_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_lowBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_lowBTW_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowINTER_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowINTER_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowINTER_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowINTER_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowINTER_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_lowBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowBTW_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowBTW_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowBTW_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowBTW_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_lowBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_lowBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_lowBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_lowBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_lowBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_lowBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_lowBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_lowBTW_3c_L5.xlsx")

#############输出中相关性#######
write_xlsx(result_sim500_30_10V_midINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midINTER_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midINTER_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midINTER_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midINTER_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midINTER_3c_L5.xlsx")

write_xlsx(result_sim500_30_10V_midBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midBTW_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midBTW_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midBTW_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midBTW_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_midBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_midBTW_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midINTER_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midINTER_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midINTER_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midINTER_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midINTER_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_midBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midBTW_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midBTW_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midBTW_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midBTW_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_midBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_midBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_midBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_midBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_midBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_midBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_midBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_midBTW_3c_L5.xlsx")

#############输出高相关性#######
write_xlsx(result_sim500_30_10V_highINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highINTER_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highINTER_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highINTER_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highINTER_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highINTER_3c_L5.xlsx")

write_xlsx(result_sim500_30_10V_highBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highBTW_3c_L1.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highBTW_3c_L2.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highBTW_3c_L3.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highBTW_3c_L4.xlsx")
write_xlsx(result_sim500_30_10V_highBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_30_10V_highBTW_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highINTER_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highINTER_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highINTER_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highINTER_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highINTER_3c_L5.xlsx")

write_xlsx(result_sim500_70_10V_highBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highBTW_3c_L1.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highBTW_3c_L2.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highBTW_3c_L3.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highBTW_3c_L4.xlsx")
write_xlsx(result_sim500_70_10V_highBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim500_70_10V_highBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_30_10V_highBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_30_10V_highBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_30_10V_highBTW_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highINTER_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highINTER_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highINTER_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highINTER_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highINTER_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highINTER_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highINTER_3c_L5.xlsx")

write_xlsx(result_sim1000_70_10V_highBTW_3c_L1, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highBTW_3c_L1.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_3c_L2, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highBTW_3c_L2.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_3c_L3, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highBTW_3c_L3.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_3c_L4, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highBTW_3c_L4.xlsx")
write_xlsx(result_sim1000_70_10V_highBTW_3c_L5, "F:/文章/大论文/程序/模拟数据/数据_result/result_sim1000_70_10V_highBTW_3c_L5.xlsx")
#######




















####################jointmodel建模################

#模板建模
fit <- mjoint(
  formLongFixed = list("grad" = log.grad ~ time + sex + hs, 
                       "lvmi" = log.lvmi ~ time + sex),
  formLongRandom = list("grad" = ~ 1 | num,
                        "lvmi" = ~ time | num),
  formSurv = Surv(fuyrs, status) ~ age,
  data = list(hvd, hvd),
  inits = list("gamma" = c(0.11, 1.51, 0.80)),
  timeVar = "time")

###模拟数据建模
#一组高影响变量间 建模
fit_simdata <- mjoint(
  formLongFixed = list(
    "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30
  ),
  formLongRandom = list(
    "Y" = ~ t | ID     # 每个个体随机截距，你也可以换成 ~ t | ID 加随机斜率
  ),
  formSurv = Surv(obs_time, event) ~ 1,  # 生存模型，这里先不加协变量
  data = simdata3_clean,
  timeVar = "t"
)
#两组高影响变量建模
fit_simdata_all <- mjoint(
  formLongFixed = list(
    "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70
  ),
  formLongRandom = list(
    "Y" = ~ t | ID     # 每个个体随机截距，你也可以换成 ~ t | ID 加随机斜率
  ),
  formSurv = Surv(obs_time, event) ~ 1,  # 生存模型，这里先不加协变量
  data = simdata3_clean,
  timeVar = "t"
)
#全部变量建模
fit_simdata_10 <- mjoint(
  formLongFixed = list(
    "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70 + V7_0.40 + V8_0.95 + V9_0.15 + V10_0.60
  ),
  formLongRandom = list(
    "Y" = ~ t | ID     # 每个个体随机截距，你也可以换成 ~ t | ID 加随机斜率
  ),
  formSurv = Surv(obs_time, event) ~ 1,  # 生存模型，这里先不加协变量
  data = simdata3_clean,
  timeVar = "t"
)

summary(fit)
summary(fit_simdata)
summary(fit_simdata_all)
summary(fit_simdata_10)

plot(fit, params = "gamma")


##############COX模型的结果输出
# 定义生存对象
surv_obj <- Surv(time = hvd$fuyrs, event = hvd$status)

# 确保数据准备正确（使用您的数据框 hvd）
# 设置分布参数（可选，用于校准）
ddist <- datadist(hvd)
options(datadist = "ddist")

# 构建CPH模型
model <- cph(Surv(time, status) ~ log.lvmi + sex, data = hvd)

# 输出模型摘要
print(model)

###########输出ROC曲线###
hvd$lp <- predict(model, newdata = hvd, type = "lp")


# 计算在3年时的 time-dependent AUC
roc_3yr <- timeROC(T = hvd$time,        # 观测到的生存时间
                   delta = hvd$status,   # 事件状态 (1=发生事件，0=删失)
                   marker = hvd$lp,      # 模型预测的风险评分
                   cause = 1,            # 我们关心的事件类型 (如果是竞争风险模型则需要指定，这里就是1)
                   weighting = "marginal", # 权重方法
                   times = 3,            # 感兴趣的时间点 (3年)
                   iid = FALSE)          # 是否计算置信区间，为简单起见先设为FALSE

# 计算在5年时的 time-dependent AUC
roc_5yr <- timeROC(T = hvd$time,
                   delta = hvd$status,
                   marker = hvd$lp,
                   cause = 1,
                   weighting = "marginal",
                   times = 5,
                   iid = FALSE)

# 输出AUC值及其置信区间
print(paste("3-Year AUC:", round(roc_3yr$AUC[2], 3))) # AUC值存储在结果的AUC向量中
print(paste("5-Year AUC:", round(roc_5yr$AUC[2], 3)))


######合并出图
plot(roc_3yr, time = 3, col = "blue", title = FALSE)
# 添加第二条ROC曲线 (5年)
lines(roc_5yr$FP[, 2], roc_5yr$TP[, 2], col = "red", type = "l")

# 添加对角线 (参考线)
abline(0, 1, lty = 2)

# 添加图例
legend("bottomright",
       c(paste("3 Years (AUC = ", round(roc_3yr$AUC[2], 3), ")", sep=""),
         paste("5 Years (AUC = ", round(roc_5yr$AUC[2], 3), ")", sep="")),
       col = c("blue", "red"),
       lty = 1,
       bty = "n") # ‘bty = "n"‘ 表示去掉图例的边框


##############JOINT模型结果输出
#模板代码
roc_3yr <- timeROC(T = hvd$time,        # 观测到的生存时间
                   delta = hvd$status,   # 事件状态 (1=发生事件，0=删失)
                   marker = hvd$lp,      # 模型预测的风险评分
                   cause = 1,            # 我们关心的事件类型 (如果是竞争风险模型则需要指定，这里就是1)
                   weighting = "marginal", # 权重方法
                   times = 3,            # 感兴趣的时间点 (3年)
                   iid = FALSE)          # 是否计算置信区间，为简单起见先设为FALSE
#失败的提取lp
#hvd$lp <- predict(fit, newdata = hvd, type = "lp")


###############计算AUC值(成功)
# 基本数据提取
surv_data <- simdata3_clean[!duplicated(simdata3_clean$ID), ]
surv_time <- surv_data$obs_time
surv_status <- surv_data$event

# 获取随机效应
random_effects <- ranef(fit_simdata_all)

# 简单使用随机截距作为风险得分
risk_scores <- random_effects[, 1]

# 确保顺序匹配
if(length(risk_scores) == length(surv_time)) {
  # 计算t=1时的AUC
  roc_result <- timeROC(
    T = surv_time,
    delta = surv_status,
    marker = risk_scores,
    cause = 1,
    times = 1  # 直接使用t=1
  )
  
  cat("t=1时的AUC:", round(roc_result$AUC[2], 4), "\n")
} else {
  cat("风险得分与生存数据长度不匹配\n")
  cat("风险得分长度:", length(risk_scores), "\n")
  cat("生存数据长度:", length(surv_time), "\n")
}














cal_auc <- function(data, t0 = 1) {
  #----------------#
  # 1. 清理数据
  #----------------#
  data_clean <- data[data$t <= data$obs_time, ]
  
  #----------------#
  # 2. 建立 mjoint 模型
  #----------------#
  fit <- mjoint(
    formLongFixed = list(
      "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70
    ),
    formLongRandom = list(
      "Y" = ~ t | ID
    ),
    formSurv = Surv(obs_time, event) ~ 1,
    data = data_clean,
    timeVar = "t"
  )
  
  #----------------#
  # 3. 提取生存数据（每个ID一条）
  #----------------#
  surv_data   <- data_clean[!duplicated(data_clean$ID), ]
  surv_time   <- surv_data$obs_time
  surv_status <- surv_data$event
  
  #----------------#
  # 4. 获取随机效应作为风险评分
  #----------------#
  random_effects <- ranef(fit)
  risk_scores    <- random_effects[, 1]  # 截距项当作 marker
  
  #----------------#
  # 5. 计算 AUC
  #----------------#
  if (length(risk_scores) == length(surv_time)) {
    roc_result <- timeROC(
      T = surv_time,
      delta = surv_status,
      marker = risk_scores,
      cause = 1,
      times = t0
    )
    auc_value <- roc_result$AUC[2]  # 第2列对应 times = t0
  } else {
    stop("风险得分与生存数据长度不匹配")
  }
  
  return(auc_value)
}

auc_1 <- cal_auc(simdata3, t0 = 1)
print(auc_1)


#######KIMI

cal_AUC <- function(data, t0 = 1) {
  
  ## 1. 数据清洗
  dat_clean <- data[data$t <= data$obs_time, ]
  
  ## 2. 拟合联合模型
  fit <- joineRML::mjoint(
    formLongFixed = list(
      "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70
    ),
    formLongRandom = list("Y" = ~ t | ID),
    formSurv       = Surv(obs_time, event) ~ 1,
    data           = dat_clean,
    timeVar        = "t"
  )
  
  ## 3. 提取随机效应（风险得分）
  rand_eff <- joineRML::ranef(fit)  # 数据框：n_subj × 随机项
  risk_scores <- rand_eff[, 1]      # 第一列：随机截距
  
  ## 4. 提取生存数据（每患者一条）
  surv_data <- dat_clean[!duplicated(dat_clean$ID), ]
  surv_time  <- surv_data$obs_time
  surv_status <- surv_data$event
  
  ## 5. 计算 t0 时刻 AUC
  roc_obj <- timeROC::timeROC(
    T      = surv_time,
    delta  = surv_status,
    marker = risk_scores,
    cause  = 1,
    times  = t0
  )
  
  return(roc_obj$AUC[2])  # 第 2 个元素对应 times = t0
}




##############ROC曲线
plot(roc_result, 
     time = 1,                    # 指定时间点
     col = "blue",                # 曲线颜色
     lwd = 2,                     # 线宽
     title = FALSE)               # 不显示默认标题

# 添加对角线（随机猜测线）
abline(0, 1, lty = 2, col = "gray")

# 添加图例和标题
legend("bottomright", 
       legend = c(paste("ROC曲线 (AUC =", round(roc_result$AUC[2], 4), ")"),
                  "随机猜测线"),
       col = c("blue", "gray"),
       lty = c(1, 2),
       lwd = c(2, 1),
       bty = "n")

title(main = "时间点 t=1 的ROC曲线",
      xlab = "1 - 特异度 (假阳性率)",
      ylab = "敏感度 (真阳性率)")

# 添加网格线使图形更易读
grid()



##############计算BS CINDEX BRIER原始代码########################

cal_BS <- function(data, t0 = 1) {
  #----------------#
  # 1. 清理数据
  #----------------#
  data_clean <- data[data$t <= data$obs_time, ]
  
  #----------------#
  # 2. 建立mjoint模型
  #----------------#
  fit <- mjoint(
    formLongFixed = list(
      "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70
    ),
    formLongRandom = list(
      "Y" = ~ t | ID
    ),
    formSurv = Surv(obs_time, event) ~ 1,
    data = data_clean,
    timeVar = "t"
  )
  
  #----------------#
  # 3. 提取参数
  #----------------#
  beta <- fit$coefficients$beta   # 纵向固定效应
  gamma <- fit$coefficients$gamma # 关联参数
  
  # 线性预测子（纵向部分）
  lp_long <- model.matrix(~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70, 
                          data = data_clean) %*% beta
  
  #----------------#
  # 4. 基线生存函数估计
  #----------------#
  S0_t0 <- summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = data_clean)), 
                   times = t0)$surv
  
  # 个体预测生存概率
  S_pred <- S0_t0 ^ exp(as.numeric(gamma) * lp_long)
  
  #----------------#
  # 5. 计算删失权重
  #----------------#
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = data_clean)
  
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                      ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
    return(weights)
  }
  
  weights <- get_weights(data_clean$obs_time, data_clean$event, censoring_model, t0)
  
  #----------------#
  # 6. 构造观测指标 (Y_obs)
  #----------------#
  Y_obs <- as.numeric(data_clean$obs_time > t0 | 
                        (data_clean$obs_time <= t0 & data_clean$event == 0))
  
  #----------------#
  # 7. 计算加权Brier Score
  #----------------#
  BS_t0_weighted <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)
  
  return(BS_t0_weighted)
}

bs_1 <- cal_BS(simdata3, t0 = 1)
print(bs_1)



##########计算C_INDEX
cal_cindex <- function(data, t0 = 1) {
  #----------------#
  # 1. 清理数据
  #----------------#
  data_clean <- data[data$t <= data$obs_time, ]
  
  #----------------#
  # 2. 建立 mjoint 模型
  #----------------#
  fit <- mjoint(
    formLongFixed = list(
      "Y" = Y ~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70
    ),
    formLongRandom = list(
      "Y" = ~ t | ID
    ),
    formSurv = Surv(obs_time, event) ~ 1,
    data = data_clean,
    timeVar = "t"
  )
  
  #----------------#
  # 3. 提取参数
  #----------------#
  beta  <- fit$coefficients$beta   # 纵向固定效应
  gamma <- fit$coefficients$gamma  # 关联参数
  
  # 线性预测子
  lp_long <- model.matrix(~ t + V1_0.55 + V2_0.80 + V3_0.30 + 
                            V4_0.90 + V5_0.25 + V6_0.70, 
                          data = data_clean) %*% beta
  
  # 风险分数
  risk_score <- as.numeric(gamma) * lp_long
  
  #----------------#
  # 4. 构造所有 pair (i, j)，矢量化比较
  #----------------#
  time <- data_clean$obs_time
  event <- data_clean$event
  
  n <- length(time)
  idx <- combn(n, 2)  # 所有 pair 的索引
  
  i <- idx[1, ]
  j <- idx[2, ]
  
  # 判断可比对 pair（短时间的个体必须发生事件）
  comparable_ij <- (event[i] == 1 & time[i] < time[j])
  comparable_ji <- (event[j] == 1 & time[j] < time[i])
  
  comparable <- comparable_ij | comparable_ji
  
  # Concordant 判断
  concordant <- (comparable_ij & (risk_score[i] > risk_score[j])) |
    (comparable_ji & (risk_score[j] > risk_score[i]))
  
  tied <- (comparable & (risk_score[i] == risk_score[j]))
  
  n_pairs <- sum(comparable)
  n_concordant <- sum(concordant)
  n_tied <- sum(tied)
  
  cindex <- (n_concordant + 0.5 * n_tied) / n_pairs
  
  return(cindex)
}

cindex_1 <- cal_cindex(simdata3, t0 = 1)
print(cindex_1)




###尝试####
#测试1
result<-cal_3(sim1000_30_10V_highINTER_3c[[1]],t0 = 1)


#正式应用
t0 <- 1 
result_500_30_10V_lowINTER_3c <- circle_cal_3(sim500_30_10V_lowINTER_3c,t0 = t0 )
result_500_30_10V_midINTER_3c  <- circle_cal_3(sim500_30_10V_midINTER_3c,  t0 = t0)
result_500_30_10V_highINTER_3c <- circle_cal_3(sim500_30_10V_highINTER_3c, t0 = t0)

result_500_70_10V_lowINTER_3c  <- circle_cal_3(sim500_70_10V_lowINTER_3c,  t0 = t0)
result_500_70_10V_midINTER_3c  <- circle_cal_3(sim500_70_10V_midINTER_3c,  t0 = t0)
result_500_70_10V_highINTER_3c <- circle_cal_3(sim500_70_10V_highINTER_3c, t0 = t0)

result_1000_30_10V_lowINTER_3c  <- circle_cal_3(sim1000_30_10V_lowINTER_3c,  t0 = t0)
result_1000_30_10V_midINTER_3c  <- circle_cal_3(sim1000_30_10V_midINTER_3c,  t0 = t0)
result_1000_30_10V_highINTER_3c <- circle_cal_3(sim1000_30_10V_highINTER_3c, t0 = t0)

result_1000_70_10V_lowINTER_3c  <- circle_cal_3(sim1000_70_10V_lowINTER_3c,  t0 = t0)
result_1000_70_10V_midINTER_3c  <- circle_cal_3(sim1000_70_10V_midINTER_3c,  t0 = t0)
result_1000_70_10V_highINTER_3c <- circle_cal_3(sim1000_70_10V_highINTER_3c, t0 = t0)











 










