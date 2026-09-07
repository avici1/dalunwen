library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


in_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/结果_JM/"

######读取数据######
read_result <- function(path) {
  readxl::read_xlsx(path) |>
    dplyr::mutate(
      dplyr::across(c(AUC, BS, Cindex), as.numeric)
    )
}

#### low INTER ####
result_sim500_30_4V_lowINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_30_4V_lowINTER_1c_L1.xlsx"))
result_sim500_30_4V_lowINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_30_4V_lowINTER_1c_L2.xlsx"))
result_sim500_30_4V_lowINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_30_4V_lowINTER_1c_L3.xlsx"))
result_sim500_30_4V_lowINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_30_4V_lowINTER_1c_L4.xlsx"))
result_sim500_30_4V_lowINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_30_4V_lowINTER_1c_L5.xlsx"))

result_sim500_70_4V_lowINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_70_4V_lowINTER_1c_L1.xlsx"))
result_sim500_70_4V_lowINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_70_4V_lowINTER_1c_L2.xlsx"))
result_sim500_70_4V_lowINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_70_4V_lowINTER_1c_L3.xlsx"))
result_sim500_70_4V_lowINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_70_4V_lowINTER_1c_L4.xlsx"))
result_sim500_70_4V_lowINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_70_4V_lowINTER_1c_L5.xlsx"))

result_sim1000_30_4V_lowINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_30_4V_lowINTER_1c_L1.xlsx"))
result_sim1000_30_4V_lowINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_30_4V_lowINTER_1c_L2.xlsx"))
result_sim1000_30_4V_lowINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_30_4V_lowINTER_1c_L3.xlsx"))
result_sim1000_30_4V_lowINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_30_4V_lowINTER_1c_L4.xlsx"))
result_sim1000_30_4V_lowINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_30_4V_lowINTER_1c_L5.xlsx"))

result_sim1000_70_4V_lowINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_70_4V_lowINTER_1c_L1.xlsx"))
result_sim1000_70_4V_lowINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_70_4V_lowINTER_1c_L2.xlsx"))
result_sim1000_70_4V_lowINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_70_4V_lowINTER_1c_L3.xlsx"))
result_sim1000_70_4V_lowINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_70_4V_lowINTER_1c_L4.xlsx"))
result_sim1000_70_4V_lowINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_70_4V_lowINTER_1c_L5.xlsx"))

#### mid INTER ####
result_sim500_30_4V_midINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_30_4V_midINTER_1c_L1.xlsx"))
result_sim500_30_4V_midINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_30_4V_midINTER_1c_L2.xlsx"))
result_sim500_30_4V_midINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_30_4V_midINTER_1c_L3.xlsx"))
result_sim500_30_4V_midINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_30_4V_midINTER_1c_L4.xlsx"))
result_sim500_30_4V_midINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_30_4V_midINTER_1c_L5.xlsx"))

result_sim500_70_4V_midINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_70_4V_midINTER_1c_L1.xlsx"))
result_sim500_70_4V_midINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_70_4V_midINTER_1c_L2.xlsx"))
result_sim500_70_4V_midINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_70_4V_midINTER_1c_L3.xlsx"))
result_sim500_70_4V_midINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_70_4V_midINTER_1c_L4.xlsx"))
result_sim500_70_4V_midINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_70_4V_midINTER_1c_L5.xlsx"))

result_sim1000_30_4V_midINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_30_4V_midINTER_1c_L1.xlsx"))
result_sim1000_30_4V_midINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_30_4V_midINTER_1c_L2.xlsx"))
result_sim1000_30_4V_midINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_30_4V_midINTER_1c_L3.xlsx"))
result_sim1000_30_4V_midINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_30_4V_midINTER_1c_L4.xlsx"))
result_sim1000_30_4V_midINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_30_4V_midINTER_1c_L5.xlsx"))

result_sim1000_70_4V_midINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_70_4V_midINTER_1c_L1.xlsx"))
result_sim1000_70_4V_midINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_70_4V_midINTER_1c_L2.xlsx"))
result_sim1000_70_4V_midINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_70_4V_midINTER_1c_L3.xlsx"))
result_sim1000_70_4V_midINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_70_4V_midINTER_1c_L4.xlsx"))
result_sim1000_70_4V_midINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_70_4V_midINTER_1c_L5.xlsx"))

#### high INTER ####
result_sim500_30_4V_highINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_30_4V_highINTER_1c_L1.xlsx"))
result_sim500_30_4V_highINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_30_4V_highINTER_1c_L2.xlsx"))
result_sim500_30_4V_highINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_30_4V_highINTER_1c_L3.xlsx"))
result_sim500_30_4V_highINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_30_4V_highINTER_1c_L4.xlsx"))
result_sim500_30_4V_highINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_30_4V_highINTER_1c_L5.xlsx"))

result_sim500_70_4V_highINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim500_70_4V_highINTER_1c_L1.xlsx"))
result_sim500_70_4V_highINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim500_70_4V_highINTER_1c_L2.xlsx"))
result_sim500_70_4V_highINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim500_70_4V_highINTER_1c_L3.xlsx"))
result_sim500_70_4V_highINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim500_70_4V_highINTER_1c_L4.xlsx"))
result_sim500_70_4V_highINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim500_70_4V_highINTER_1c_L5.xlsx"))

result_sim1000_30_4V_highINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_30_4V_highINTER_1c_L1.xlsx"))
result_sim1000_30_4V_highINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_30_4V_highINTER_1c_L2.xlsx"))
result_sim1000_30_4V_highINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_30_4V_highINTER_1c_L3.xlsx"))
result_sim1000_30_4V_highINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_30_4V_highINTER_1c_L4.xlsx"))
result_sim1000_30_4V_highINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_30_4V_highINTER_1c_L5.xlsx"))

result_sim1000_70_4V_highINTER_1c_L1 <- read_result(paste0(in_dir, "result_sim1000_70_4V_highINTER_1c_L1.xlsx"))
result_sim1000_70_4V_highINTER_1c_L2 <- read_result(paste0(in_dir, "result_sim1000_70_4V_highINTER_1c_L2.xlsx"))
result_sim1000_70_4V_highINTER_1c_L3 <- read_result(paste0(in_dir, "result_sim1000_70_4V_highINTER_1c_L3.xlsx"))
result_sim1000_70_4V_highINTER_1c_L4 <- read_result(paste0(in_dir, "result_sim1000_70_4V_highINTER_1c_L4.xlsx"))
result_sim1000_70_4V_highINTER_1c_L5 <- read_result(paste0(in_dir, "result_sim1000_70_4V_highINTER_1c_L5.xlsx"))



#######合并#################

######### 合并并计算统计量 - 4V数据 #########

#### low INTER ####
# 样本量500，时间点30
result_sim500_30_4V_lowINTER_1c <- rbind(result_sim500_30_4V_lowINTER_1c_L1, result_sim500_30_4V_lowINTER_1c_L2,
                                         result_sim500_30_4V_lowINTER_1c_L3, result_sim500_30_4V_lowINTER_1c_L4,
                                         result_sim500_30_4V_lowINTER_1c_L5)
RESULT_sim500_30_4V_lowINTER_1c <- data.frame(
  var = "sim500_30_4V_lowINTER_1c",
  AUC_mean = mean(result_sim500_30_4V_lowINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_4V_lowINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_4V_lowINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_4V_lowINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_4V_lowINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_4V_lowINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量500，时间点70
result_sim500_70_4V_lowINTER_1c <- rbind(result_sim500_70_4V_lowINTER_1c_L1, result_sim500_70_4V_lowINTER_1c_L2,
                                         result_sim500_70_4V_lowINTER_1c_L3, result_sim500_70_4V_lowINTER_1c_L4,
                                         result_sim500_70_4V_lowINTER_1c_L5)
RESULT_sim500_70_4V_lowINTER_1c <- data.frame(
  var = "sim500_70_4V_lowINTER_1c",
  AUC_mean = mean(result_sim500_70_4V_lowINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_4V_lowINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_4V_lowINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_4V_lowINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_4V_lowINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_4V_lowINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点30
result_sim1000_30_4V_lowINTER_1c <- rbind(result_sim1000_30_4V_lowINTER_1c_L1, result_sim1000_30_4V_lowINTER_1c_L2,
                                          result_sim1000_30_4V_lowINTER_1c_L3, result_sim1000_30_4V_lowINTER_1c_L4,
                                          result_sim1000_30_4V_lowINTER_1c_L5)
RESULT_sim1000_30_4V_lowINTER_1c <- data.frame(
  var = "sim1000_30_4V_lowINTER_1c",
  AUC_mean = mean(result_sim1000_30_4V_lowINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_4V_lowINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_4V_lowINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_4V_lowINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_4V_lowINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_4V_lowINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点70
result_sim1000_70_4V_lowINTER_1c <- rbind(result_sim1000_70_4V_lowINTER_1c_L1, result_sim1000_70_4V_lowINTER_1c_L2,
                                          result_sim1000_70_4V_lowINTER_1c_L3, result_sim1000_70_4V_lowINTER_1c_L4,
                                          result_sim1000_70_4V_lowINTER_1c_L5)
RESULT_sim1000_70_4V_lowINTER_1c <- data.frame(
  var = "sim1000_70_4V_lowINTER_1c",
  AUC_mean = mean(result_sim1000_70_4V_lowINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_4V_lowINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_4V_lowINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_4V_lowINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_4V_lowINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_4V_lowINTER_1c$Cindex, na.rm = TRUE)
)

#### mid INTER ####
# 样本量500，时间点30
result_sim500_30_4V_midINTER_1c <- rbind(result_sim500_30_4V_midINTER_1c_L1, result_sim500_30_4V_midINTER_1c_L2,
                                         result_sim500_30_4V_midINTER_1c_L3, result_sim500_30_4V_midINTER_1c_L4,
                                         result_sim500_30_4V_midINTER_1c_L5)
RESULT_sim500_30_4V_midINTER_1c <- data.frame(
  var = "sim500_30_4V_midINTER_1c",
  AUC_mean = mean(result_sim500_30_4V_midINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_4V_midINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_4V_midINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_4V_midINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_4V_midINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_4V_midINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量500，时间点70
result_sim500_70_4V_midINTER_1c <- rbind(result_sim500_70_4V_midINTER_1c_L1, result_sim500_70_4V_midINTER_1c_L2,
                                         result_sim500_70_4V_midINTER_1c_L3, result_sim500_70_4V_midINTER_1c_L4,
                                         result_sim500_70_4V_midINTER_1c_L5)
RESULT_sim500_70_4V_midINTER_1c <- data.frame(
  var = "sim500_70_4V_midINTER_1c",
  AUC_mean = mean(result_sim500_70_4V_midINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_4V_midINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_4V_midINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_4V_midINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_4V_midINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_4V_midINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点30
result_sim1000_30_4V_midINTER_1c <- rbind(result_sim1000_30_4V_midINTER_1c_L1, result_sim1000_30_4V_midINTER_1c_L2,
                                          result_sim1000_30_4V_midINTER_1c_L3, result_sim1000_30_4V_midINTER_1c_L4,
                                          result_sim1000_30_4V_midINTER_1c_L5)
RESULT_sim1000_30_4V_midINTER_1c <- data.frame(
  var = "sim1000_30_4V_midINTER_1c",
  AUC_mean = mean(result_sim1000_30_4V_midINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_4V_midINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_4V_midINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_4V_midINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_4V_midINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_4V_midINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点70
result_sim1000_70_4V_midINTER_1c <- rbind(result_sim1000_70_4V_midINTER_1c_L1, result_sim1000_70_4V_midINTER_1c_L2,
                                          result_sim1000_70_4V_midINTER_1c_L3, result_sim1000_70_4V_midINTER_1c_L4,
                                          result_sim1000_70_4V_midINTER_1c_L5)
RESULT_sim1000_70_4V_midINTER_1c <- data.frame(
  var = "sim1000_70_4V_midINTER_1c",
  AUC_mean = mean(result_sim1000_70_4V_midINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_4V_midINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_4V_midINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_4V_midINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_4V_midINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_4V_midINTER_1c$Cindex, na.rm = TRUE)
)

#### high INTER ####
# 样本量500，时间点30
result_sim500_30_4V_highINTER_1c <- rbind(result_sim500_30_4V_highINTER_1c_L1, result_sim500_30_4V_highINTER_1c_L2,
                                          result_sim500_30_4V_highINTER_1c_L3, result_sim500_30_4V_highINTER_1c_L4,
                                          result_sim500_30_4V_highINTER_1c_L5)
RESULT_sim500_30_4V_highINTER_1c <- data.frame(
  var = "sim500_30_4V_highINTER_1c",
  AUC_mean = mean(result_sim500_30_4V_highINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_30_4V_highINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_30_4V_highINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_30_4V_highINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_30_4V_highINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_30_4V_highINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量500，时间点70
result_sim500_70_4V_highINTER_1c <- rbind(result_sim500_70_4V_highINTER_1c_L1, result_sim500_70_4V_highINTER_1c_L2,
                                          result_sim500_70_4V_highINTER_1c_L3, result_sim500_70_4V_highINTER_1c_L4,
                                          result_sim500_70_4V_highINTER_1c_L5)
RESULT_sim500_70_4V_highINTER_1c <- data.frame(
  var = "sim500_70_4V_highINTER_1c",
  AUC_mean = mean(result_sim500_70_4V_highINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim500_70_4V_highINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim500_70_4V_highINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim500_70_4V_highINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim500_70_4V_highINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim500_70_4V_highINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点30
result_sim1000_30_4V_highINTER_1c <- rbind(result_sim1000_30_4V_highINTER_1c_L1, result_sim1000_30_4V_highINTER_1c_L2,
                                           result_sim1000_30_4V_highINTER_1c_L3, result_sim1000_30_4V_highINTER_1c_L4,
                                           result_sim1000_30_4V_highINTER_1c_L5)
RESULT_sim1000_30_4V_highINTER_1c <- data.frame(
  var = "sim1000_30_4V_highINTER_1c",
  AUC_mean = mean(result_sim1000_30_4V_highINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_30_4V_highINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_30_4V_highINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_30_4V_highINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_30_4V_highINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_30_4V_highINTER_1c$Cindex, na.rm = TRUE)
)

# 样本量1000，时间点70
result_sim1000_70_4V_highINTER_1c <- rbind(result_sim1000_70_4V_highINTER_1c_L1, result_sim1000_70_4V_highINTER_1c_L2,
                                           result_sim1000_70_4V_highINTER_1c_L3, result_sim1000_70_4V_highINTER_1c_L4,
                                           result_sim1000_70_4V_highINTER_1c_L5)
RESULT_sim1000_70_4V_highINTER_1c <- data.frame(
  var = "sim1000_70_4V_highINTER_1c",
  AUC_mean = mean(result_sim1000_70_4V_highINTER_1c$AUC, na.rm = TRUE),
  AUC_var = var(result_sim1000_70_4V_highINTER_1c$AUC, na.rm = TRUE),
  BS_mean = mean(result_sim1000_70_4V_highINTER_1c$BS, na.rm = TRUE),
  BS_var = var(result_sim1000_70_4V_highINTER_1c$BS, na.rm = TRUE),
  Cindex_mean = mean(result_sim1000_70_4V_highINTER_1c$Cindex, na.rm = TRUE),
  Cindex_var = var(result_sim1000_70_4V_highINTER_1c$Cindex, na.rm = TRUE)
)

######### 合并所有结果到一个数据框 #########
all_results_4V <- rbind(
  RESULT_sim500_30_4V_lowINTER_1c,
  RESULT_sim500_70_4V_lowINTER_1c,
  RESULT_sim1000_30_4V_lowINTER_1c,
  RESULT_sim1000_70_4V_lowINTER_1c,
  RESULT_sim500_30_4V_midINTER_1c,
  RESULT_sim500_70_4V_midINTER_1c,
  RESULT_sim1000_30_4V_midINTER_1c,
  RESULT_sim1000_70_4V_midINTER_1c,
  RESULT_sim500_30_4V_highINTER_1c,
  RESULT_sim500_70_4V_highINTER_1c,
  RESULT_sim1000_30_4V_highINTER_1c,
  RESULT_sim1000_70_4V_highINTER_1c
)

# 查看合并后的结果
print(all_results_4V)

# 如果需要保存汇总结果
write_xlsx(all_results_4V, "F:/文章/大论文/程序Trae/模拟数据_4V/结果_JM/summary_results_4V.xlsx")




###############










