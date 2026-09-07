library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

in_dir<- "F:/文章_大论文/结果/数据_cox_10V3C/"

read_result <- function(path) {
  read_xlsx(path) |>
    dplyr::mutate(
      dplyr::across(c(AUC, BS, Cindex), as.numeric)
    )
}




########### 低相关 ###########

result_sim500_30_10V_lowINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowINTER_3c_L1.xlsx")
result_sim500_30_10V_lowINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowINTER_3c_L2.xlsx")
result_sim500_30_10V_lowINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowINTER_3c_L3.xlsx")
result_sim500_30_10V_lowINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowINTER_3c_L4.xlsx")
result_sim500_30_10V_lowINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowINTER_3c_L5.xlsx")

result_sim500_70_10V_lowINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowINTER_3c_L1.xlsx")
result_sim500_70_10V_lowINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowINTER_3c_L2.xlsx")
result_sim500_70_10V_lowINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowINTER_3c_L3.xlsx")
result_sim500_70_10V_lowINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowINTER_3c_L4.xlsx")
result_sim500_70_10V_lowINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowINTER_3c_L5.xlsx")

result_sim1000_30_10V_lowINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowINTER_3c_L1.xlsx")
result_sim1000_30_10V_lowINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowINTER_3c_L2.xlsx")
result_sim1000_30_10V_lowINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowINTER_3c_L3.xlsx")
result_sim1000_30_10V_lowINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowINTER_3c_L4.xlsx")
result_sim1000_30_10V_lowINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowINTER_3c_L5.xlsx")

result_sim1000_70_10V_lowINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowINTER_3c_L1.xlsx")
result_sim1000_70_10V_lowINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowINTER_3c_L2.xlsx")
result_sim1000_70_10V_lowINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowINTER_3c_L3.xlsx")
result_sim1000_70_10V_lowINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowINTER_3c_L4.xlsx")
result_sim1000_70_10V_lowINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowINTER_3c_L5.xlsx")



result_sim500_30_10V_lowBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowBTW_3c_L1.xlsx")
result_sim500_30_10V_lowBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowBTW_3c_L2.xlsx")
result_sim500_30_10V_lowBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowBTW_3c_L3.xlsx")
result_sim500_30_10V_lowBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowBTW_3c_L4.xlsx")
result_sim500_30_10V_lowBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_lowBTW_3c_L5.xlsx")

result_sim500_70_10V_lowBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowBTW_3c_L1.xlsx")
result_sim500_70_10V_lowBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowBTW_3c_L2.xlsx")
result_sim500_70_10V_lowBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowBTW_3c_L3.xlsx")
result_sim500_70_10V_lowBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowBTW_3c_L4.xlsx")
result_sim500_70_10V_lowBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_lowBTW_3c_L5.xlsx")

result_sim1000_30_10V_lowBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowBTW_3c_L1.xlsx")
result_sim1000_30_10V_lowBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowBTW_3c_L2.xlsx")
result_sim1000_30_10V_lowBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowBTW_3c_L3.xlsx")
result_sim1000_30_10V_lowBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowBTW_3c_L4.xlsx")
result_sim1000_30_10V_lowBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_lowBTW_3c_L5.xlsx")

result_sim1000_70_10V_lowBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowBTW_3c_L1.xlsx")
result_sim1000_70_10V_lowBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowBTW_3c_L2.xlsx")
result_sim1000_70_10V_lowBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowBTW_3c_L3.xlsx")
result_sim1000_70_10V_lowBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowBTW_3c_L4.xlsx")
result_sim1000_70_10V_lowBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_lowBTW_3c_L5.xlsx")



########### 中相关 ###########

result_sim500_30_10V_midINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midINTER_3c_L1.xlsx")
result_sim500_30_10V_midINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midINTER_3c_L2.xlsx")
result_sim500_30_10V_midINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midINTER_3c_L3.xlsx")
result_sim500_30_10V_midINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midINTER_3c_L4.xlsx")
result_sim500_30_10V_midINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midINTER_3c_L5.xlsx")

result_sim500_70_10V_midINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midINTER_3c_L1.xlsx")
result_sim500_70_10V_midINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midINTER_3c_L2.xlsx")
result_sim500_70_10V_midINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midINTER_3c_L3.xlsx")
result_sim500_70_10V_midINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midINTER_3c_L4.xlsx")
result_sim500_70_10V_midINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midINTER_3c_L5.xlsx")

result_sim1000_30_10V_midINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midINTER_3c_L1.xlsx")
result_sim1000_30_10V_midINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midINTER_3c_L2.xlsx")
result_sim1000_30_10V_midINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midINTER_3c_L3.xlsx")
result_sim1000_30_10V_midINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midINTER_3c_L4.xlsx")
result_sim1000_30_10V_midINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midINTER_3c_L5.xlsx")

result_sim1000_70_10V_midINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midINTER_3c_L1.xlsx")
result_sim1000_70_10V_midINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midINTER_3c_L2.xlsx")
result_sim1000_70_10V_midINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midINTER_3c_L3.xlsx")
result_sim1000_70_10V_midINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midINTER_3c_L4.xlsx")
result_sim1000_70_10V_midINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midINTER_3c_L5.xlsx")


result_sim500_30_10V_midBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midBTW_3c_L1.xlsx")
result_sim500_30_10V_midBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midBTW_3c_L2.xlsx")
result_sim500_30_10V_midBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midBTW_3c_L3.xlsx")
result_sim500_30_10V_midBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midBTW_3c_L4.xlsx")
result_sim500_30_10V_midBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_midBTW_3c_L5.xlsx")

result_sim500_70_10V_midBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midBTW_3c_L1.xlsx")
result_sim500_70_10V_midBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midBTW_3c_L2.xlsx")
result_sim500_70_10V_midBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midBTW_3c_L3.xlsx")
result_sim500_70_10V_midBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midBTW_3c_L4.xlsx")
result_sim500_70_10V_midBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_midBTW_3c_L5.xlsx")

result_sim1000_30_10V_midBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midBTW_3c_L1.xlsx")
result_sim1000_30_10V_midBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midBTW_3c_L2.xlsx")
result_sim1000_30_10V_midBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midBTW_3c_L3.xlsx")
result_sim1000_30_10V_midBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midBTW_3c_L4.xlsx")
result_sim1000_30_10V_midBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_midBTW_3c_L5.xlsx")

result_sim1000_70_10V_midBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midBTW_3c_L1.xlsx")
result_sim1000_70_10V_midBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midBTW_3c_L2.xlsx")
result_sim1000_70_10V_midBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midBTW_3c_L3.xlsx")
result_sim1000_70_10V_midBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midBTW_3c_L4.xlsx")
result_sim1000_70_10V_midBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_midBTW_3c_L5.xlsx")

########### 高相关 ###########

result_sim500_30_10V_highINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highINTER_3c_L1.xlsx")
result_sim500_30_10V_highINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highINTER_3c_L2.xlsx")
result_sim500_30_10V_highINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highINTER_3c_L3.xlsx")
result_sim500_30_10V_highINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highINTER_3c_L4.xlsx")
result_sim500_30_10V_highINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highINTER_3c_L5.xlsx")

result_sim500_70_10V_highINTER_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highINTER_3c_L1.xlsx")
result_sim500_70_10V_highINTER_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highINTER_3c_L2.xlsx")
result_sim500_70_10V_highINTER_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highINTER_3c_L3.xlsx")
result_sim500_70_10V_highINTER_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highINTER_3c_L4.xlsx")
result_sim500_70_10V_highINTER_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highINTER_3c_L5.xlsx")

result_sim1000_30_10V_highINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highINTER_3c_L1.xlsx")
result_sim1000_30_10V_highINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highINTER_3c_L2.xlsx")
result_sim1000_30_10V_highINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highINTER_3c_L3.xlsx")
result_sim1000_30_10V_highINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highINTER_3c_L4.xlsx")
result_sim1000_30_10V_highINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highINTER_3c_L5.xlsx")

result_sim1000_70_10V_highINTER_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highINTER_3c_L1.xlsx")
result_sim1000_70_10V_highINTER_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highINTER_3c_L2.xlsx")
result_sim1000_70_10V_highINTER_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highINTER_3c_L3.xlsx")
result_sim1000_70_10V_highINTER_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highINTER_3c_L4.xlsx")
result_sim1000_70_10V_highINTER_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highINTER_3c_L5.xlsx")

result_sim500_30_10V_highBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highBTW_3c_L1.xlsx")
result_sim500_30_10V_highBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highBTW_3c_L2.xlsx")
result_sim500_30_10V_highBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highBTW_3c_L3.xlsx")
result_sim500_30_10V_highBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highBTW_3c_L4.xlsx")
result_sim500_30_10V_highBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_30_10V_highBTW_3c_L5.xlsx")

result_sim500_70_10V_highBTW_3c_L1  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highBTW_3c_L1.xlsx")
result_sim500_70_10V_highBTW_3c_L2  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highBTW_3c_L2.xlsx")
result_sim500_70_10V_highBTW_3c_L3  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highBTW_3c_L3.xlsx")
result_sim500_70_10V_highBTW_3c_L4  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highBTW_3c_L4.xlsx")
result_sim500_70_10V_highBTW_3c_L5  <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim500_70_10V_highBTW_3c_L5.xlsx")

result_sim1000_30_10V_highBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highBTW_3c_L1.xlsx")
result_sim1000_30_10V_highBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highBTW_3c_L2.xlsx")
result_sim1000_30_10V_highBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highBTW_3c_L3.xlsx")
result_sim1000_30_10V_highBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highBTW_3c_L4.xlsx")
result_sim1000_30_10V_highBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_30_10V_highBTW_3c_L5.xlsx")

result_sim1000_70_10V_highBTW_3c_L1 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highBTW_3c_L1.xlsx")
result_sim1000_70_10V_highBTW_3c_L2 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highBTW_3c_L2.xlsx")
result_sim1000_70_10V_highBTW_3c_L3 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highBTW_3c_L3.xlsx")
result_sim1000_70_10V_highBTW_3c_L4 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highBTW_3c_L4.xlsx")
result_sim1000_70_10V_highBTW_3c_L5 <- read_result("F:/文章_大论文/结果/数据_cox_10V3c/result_sim1000_70_10V_highBTW_3c_L5.xlsx")










##########合并每5个##############

#########sim500_30_10V_lowINTER_3c#########
result_sim500_30_10V_lowINTER_3c <- rbind(result_sim500_30_10V_lowINTER_3c_L1,result_sim500_30_10V_lowINTER_3c_L2,
                                          result_sim500_30_10V_lowINTER_3c_L3,result_sim500_30_10V_lowINTER_3c_L4,
                                          result_sim500_30_10V_lowINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_lowINTER_3c <- data.frame(
  var = "sim500_30_10V_lowINTER_3c",
  AUC_mean = mean(result_sim500_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_lowINTER_3c#########
result_sim500_70_10V_lowINTER_3c <- rbind(result_sim500_70_10V_lowINTER_3c_L1,result_sim500_70_10V_lowINTER_3c_L2,
                                          result_sim500_70_10V_lowINTER_3c_L3,result_sim500_70_10V_lowINTER_3c_L4,
                                          result_sim500_70_10V_lowINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_lowINTER_3c <- data.frame(
  var = "sim500_70_10V_lowINTER_3c",
  AUC_mean = mean(result_sim500_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_lowINTER_3c#########
result_sim1000_30_10V_lowINTER_3c <- rbind(result_sim1000_30_10V_lowINTER_3c_L1,result_sim1000_30_10V_lowINTER_3c_L2,
                                           result_sim1000_30_10V_lowINTER_3c_L3,result_sim1000_30_10V_lowINTER_3c_L4,
                                           result_sim1000_30_10V_lowINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_lowINTER_3c <- data.frame(
  var = "sim1000_30_10V_lowINTER_3c",
  AUC_mean = mean(result_sim1000_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_lowINTER_3c#########
result_sim1000_70_10V_lowINTER_3c <- rbind(result_sim1000_70_10V_lowINTER_3c_L1,result_sim1000_70_10V_lowINTER_3c_L2,
                                           result_sim1000_70_10V_lowINTER_3c_L3,result_sim1000_70_10V_lowINTER_3c_L4,
                                           result_sim1000_70_10V_lowINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_lowINTER_3c <- data.frame(
  var = "sim1000_70_10V_lowINTER_3c",
  AUC_mean = mean(result_sim1000_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)

#########sim500_30_10V_midINTER_3c#########
result_sim500_30_10V_midINTER_3c <- rbind(result_sim500_30_10V_midINTER_3c_L1,result_sim500_30_10V_midINTER_3c_L2,
                                          result_sim500_30_10V_midINTER_3c_L3,result_sim500_30_10V_midINTER_3c_L4,
                                          result_sim500_30_10V_midINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_midINTER_3c <- data.frame(
  var = "sim500_30_10V_midINTER_3c",
  AUC_mean = mean(result_sim500_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_midINTER_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_midINTER_3c#########
result_sim500_70_10V_midINTER_3c <- rbind(result_sim500_70_10V_midINTER_3c_L1,result_sim500_70_10V_midINTER_3c_L2,
                                          result_sim500_70_10V_midINTER_3c_L3,result_sim500_70_10V_midINTER_3c_L4,
                                          result_sim500_70_10V_midINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_midINTER_3c <- data.frame(
  var = "sim500_70_10V_midINTER_3c",
  AUC_mean = mean(result_sim500_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_midINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_midINTER_3c#########
result_sim1000_30_10V_midINTER_3c <- rbind(result_sim1000_30_10V_midINTER_3c_L1,result_sim1000_30_10V_midINTER_3c_L2,
                                           result_sim1000_30_10V_midINTER_3c_L3,result_sim1000_30_10V_midINTER_3c_L4,
                                           result_sim1000_30_10V_midINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_midINTER_3c <- data.frame(
  var = "sim1000_30_10V_midINTER_3c",
  AUC_mean = mean(result_sim1000_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_midINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_midINTER_3c#########
result_sim1000_70_10V_midINTER_3c <- rbind(result_sim1000_70_10V_midINTER_3c_L1,result_sim1000_70_10V_midINTER_3c_L2,
                                           result_sim1000_70_10V_midINTER_3c_L3,result_sim1000_70_10V_midINTER_3c_L4,
                                           result_sim1000_70_10V_midINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_midINTER_3c <- data.frame(
  var = "sim1000_70_10V_midINTER_3c",
  AUC_mean = mean(result_sim1000_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_midINTER_3c$Cindex, na.rm = TRUE)
)

#########sim500_30_10V_highINTER_3c#########
result_sim500_30_10V_highINTER_3c <- rbind(result_sim500_30_10V_highINTER_3c_L1,result_sim500_30_10V_highINTER_3c_L2,
                                           result_sim500_30_10V_highINTER_3c_L3,result_sim500_30_10V_highINTER_3c_L4,
                                           result_sim500_30_10V_highINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_highINTER_3c <- data.frame(
  var = "sim500_30_10V_highINTER_3c",
  AUC_mean = mean(result_sim500_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_highINTER_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_highINTER_3c#########
result_sim500_70_10V_highINTER_3c <- rbind(result_sim500_70_10V_highINTER_3c_L1,result_sim500_70_10V_highINTER_3c_L2,
                                           result_sim500_70_10V_highINTER_3c_L3,result_sim500_70_10V_highINTER_3c_L4,
                                           result_sim500_70_10V_highINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_highINTER_3c <- data.frame(
  var = "sim500_70_10V_highINTER_3c",
  AUC_mean = mean(result_sim500_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_highINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_highINTER_3c#########
result_sim1000_30_10V_highINTER_3c <- rbind(result_sim1000_30_10V_highINTER_3c_L1,result_sim1000_30_10V_highINTER_3c_L2,
                                            result_sim1000_30_10V_highINTER_3c_L3,result_sim1000_30_10V_highINTER_3c_L4,
                                            result_sim1000_30_10V_highINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_highINTER_3c <- data.frame(
  var = "sim1000_30_10V_highINTER_3c",
  AUC_mean = mean(result_sim1000_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_highINTER_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_highINTER_3c#########
result_sim1000_70_10V_highINTER_3c <- rbind(result_sim1000_70_10V_highINTER_3c_L1,result_sim1000_70_10V_highINTER_3c_L2,
                                            result_sim1000_70_10V_highINTER_3c_L3,result_sim1000_70_10V_highINTER_3c_L4,
                                            result_sim1000_70_10V_highINTER_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_highINTER_3c <- data.frame(
  var = "sim1000_70_10V_highINTER_3c",
  AUC_mean = mean(result_sim1000_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_highINTER_3c$Cindex, na.rm = TRUE)
)




#########sim500_30_10V_lowBTW_3c#########
result_sim500_30_10V_lowBTW_3c <- rbind(result_sim500_30_10V_lowBTW_3c_L1,result_sim500_30_10V_lowBTW_3c_L2,
                                        result_sim500_30_10V_lowBTW_3c_L3,result_sim500_30_10V_lowBTW_3c_L4,
                                        result_sim500_30_10V_lowBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_lowBTW_3c <- data.frame(
  var = "sim500_30_10V_lowBTW_3c",
  AUC_mean = mean(result_sim500_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_lowBTW_3c#########
result_sim500_70_10V_lowBTW_3c <- rbind(result_sim500_70_10V_lowBTW_3c_L1,result_sim500_70_10V_lowBTW_3c_L2,
                                        result_sim500_70_10V_lowBTW_3c_L3,result_sim500_70_10V_lowBTW_3c_L4,
                                        result_sim500_70_10V_lowBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_lowBTW_3c <- data.frame(
  var = "sim500_70_10V_lowBTW_3c",
  AUC_mean = mean(result_sim500_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_lowBTW_3c#########
result_sim1000_30_10V_lowBTW_3c <- rbind(result_sim1000_30_10V_lowBTW_3c_L1,result_sim1000_30_10V_lowBTW_3c_L2,
                                         result_sim1000_30_10V_lowBTW_3c_L3,result_sim1000_30_10V_lowBTW_3c_L4,
                                         result_sim1000_30_10V_lowBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_lowBTW_3c <- data.frame(
  var = "sim1000_30_10V_lowBTW_3c",
  AUC_mean = mean(result_sim1000_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_lowBTW_3c#########
result_sim1000_70_10V_lowBTW_3c <- rbind(result_sim1000_70_10V_lowBTW_3c_L1,result_sim1000_70_10V_lowBTW_3c_L2,
                                         result_sim1000_70_10V_lowBTW_3c_L3,result_sim1000_70_10V_lowBTW_3c_L4,
                                         result_sim1000_70_10V_lowBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_lowBTW_3c <- data.frame(
  var = "sim1000_70_10V_lowBTW_3c",
  AUC_mean = mean(result_sim1000_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)

#########sim500_30_10V_midBTW_3c#########
result_sim500_30_10V_midBTW_3c <- rbind(result_sim500_30_10V_midBTW_3c_L1,result_sim500_30_10V_midBTW_3c_L2,
                                        result_sim500_30_10V_midBTW_3c_L3,result_sim500_30_10V_midBTW_3c_L4,
                                        result_sim500_30_10V_midBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_midBTW_3c <- data.frame(
  var = "sim500_30_10V_midBTW_3c",
  AUC_mean = mean(result_sim500_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_midBTW_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_midBTW_3c#########
result_sim500_70_10V_midBTW_3c <- rbind(result_sim500_70_10V_midBTW_3c_L1,result_sim500_70_10V_midBTW_3c_L2,
                                        result_sim500_70_10V_midBTW_3c_L3,result_sim500_70_10V_midBTW_3c_L4,
                                        result_sim500_70_10V_midBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_midBTW_3c <- data.frame(
  var = "sim500_70_10V_midBTW_3c",
  AUC_mean = mean(result_sim500_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_midBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_midBTW_3c#########
result_sim1000_30_10V_midBTW_3c <- rbind(result_sim1000_30_10V_midBTW_3c_L1,result_sim1000_30_10V_midBTW_3c_L2,
                                         result_sim1000_30_10V_midBTW_3c_L3,result_sim1000_30_10V_midBTW_3c_L4,
                                         result_sim1000_30_10V_midBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_midBTW_3c <- data.frame(
  var = "sim1000_30_10V_midBTW_3c",
  AUC_mean = mean(result_sim1000_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_midBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_midBTW_3c#########
result_sim1000_70_10V_midBTW_3c <- rbind(result_sim1000_70_10V_midBTW_3c_L1,result_sim1000_70_10V_midBTW_3c_L2,
                                         result_sim1000_70_10V_midBTW_3c_L3,result_sim1000_70_10V_midBTW_3c_L4,
                                         result_sim1000_70_10V_midBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_midBTW_3c <- data.frame(
  var = "sim1000_70_10V_midBTW_3c",
  AUC_mean = mean(result_sim1000_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_midBTW_3c$Cindex, na.rm = TRUE)
)

#########sim500_30_10V_highBTW_3c#########
result_sim500_30_10V_highBTW_3c <- rbind(result_sim500_30_10V_highBTW_3c_L1,result_sim500_30_10V_highBTW_3c_L2,
                                         result_sim500_30_10V_highBTW_3c_L3,result_sim500_30_10V_highBTW_3c_L4,
                                         result_sim500_30_10V_highBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_30_10V_highBTW_3c <- data.frame(
  var = "sim500_30_10V_highBTW_3c",
  AUC_mean = mean(result_sim500_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_10V_highBTW_3c$Cindex, na.rm = TRUE)
)

#########sim500_70_10V_highBTW_3c#########
result_sim500_70_10V_highBTW_3c <- rbind(result_sim500_70_10V_highBTW_3c_L1,result_sim500_70_10V_highBTW_3c_L2,
                                         result_sim500_70_10V_highBTW_3c_L3,result_sim500_70_10V_highBTW_3c_L4,
                                         result_sim500_70_10V_highBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim500_70_10V_highBTW_3c <- data.frame(
  var = "sim500_70_10V_highBTW_3c",
  AUC_mean = mean(result_sim500_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_10V_highBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_30_10V_highBTW_3c#########
result_sim1000_30_10V_highBTW_3c <- rbind(result_sim1000_30_10V_highBTW_3c_L1,result_sim1000_30_10V_highBTW_3c_L2,
                                          result_sim1000_30_10V_highBTW_3c_L3,result_sim1000_30_10V_highBTW_3c_L4,
                                          result_sim1000_30_10V_highBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_30_10V_highBTW_3c <- data.frame(
  var = "sim1000_30_10V_highBTW_3c",
  AUC_mean = mean(result_sim1000_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_10V_highBTW_3c$Cindex, na.rm = TRUE)
)

#########sim1000_70_10V_highBTW_3c#########
result_sim1000_70_10V_highBTW_3c <- rbind(result_sim1000_70_10V_highBTW_3c_L1,result_sim1000_70_10V_highBTW_3c_L2,
                                          result_sim1000_70_10V_highBTW_3c_L3,result_sim1000_70_10V_highBTW_3c_L4,
                                          result_sim1000_70_10V_highBTW_3c_L5)
# 计算三列数据的均数和方差
RESULT__sim1000_70_10V_highBTW_3c <- data.frame(
  var = "sim1000_70_10V_highBTW_3c",
  AUC_mean = mean(result_sim1000_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_10V_highBTW_3c$Cindex, na.rm = TRUE)
)



#################合并12##########
# 将12个结果数据框合并为一个
RESULT_ALL <- rbind(RESULT__sim500_30_10V_lowINTER_3c,
                    RESULT__sim500_70_10V_lowINTER_3c,
                    RESULT__sim1000_30_10V_lowINTER_3c,
                    RESULT__sim1000_70_10V_lowINTER_3c,
                    RESULT__sim500_30_10V_midINTER_3c,
                    RESULT__sim500_70_10V_midINTER_3c,
                    RESULT__sim1000_30_10V_midINTER_3c,
                    RESULT__sim1000_70_10V_midINTER_3c,
                    RESULT__sim500_30_10V_highINTER_3c,
                    RESULT__sim500_70_10V_highINTER_3c,
                    RESULT__sim1000_30_10V_highINTER_3c,
                    RESULT__sim1000_70_10V_highINTER_3c,
                    RESULT__sim500_30_10V_lowBTW_3c,
                    RESULT__sim500_70_10V_lowBTW_3c,
                    RESULT__sim1000_30_10V_lowBTW_3c,
                    RESULT__sim1000_70_10V_lowBTW_3c,
                    RESULT__sim500_30_10V_midBTW_3c,
                    RESULT__sim500_70_10V_midBTW_3c,
                    RESULT__sim1000_30_10V_midBTW_3c,
                    RESULT__sim1000_70_10V_midBTW_3c,
                    RESULT__sim500_30_10V_highBTW_3c,
                    RESULT__sim500_70_10V_highBTW_3c,
                    RESULT__sim1000_30_10V_highBTW_3c,
                    RESULT__sim1000_70_10V_highBTW_3c)

write_xlsx(RESULT_ALL,"F:/文章_大论文/结果/数据_cox_10V3C/RESULT_ALL_RSFLC.xlsx")













