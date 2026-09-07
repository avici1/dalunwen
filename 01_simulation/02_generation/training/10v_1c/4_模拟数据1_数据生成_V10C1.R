library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(writexl)
library(readxl)

options(scipen = 999)


##############生成程序#################

# 单类别版本的模拟函数

sim_single_class <- function(n=200,
                            n_time =5,
                            param_list,
                            Sigma = diag(20),
                            seed = 123) {
  
  ## 0. 环境准备 ----------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  
  ## 1. 基本框架 ----------------------------------------------------------
  # 单类别模型，所有个体都属于同一个类别
  class_assign <- rep("c1", n)  # 所有人属于类别1
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  
  ## 2. 随机效应 ----------------------------------------------------------
  # 生成随机效应
  re <- MASS::mvrnorm(n, mu = rep(0, 20), Sigma = Sigma)
  
  var_names <- sapply(param_list, `[[`, "varname")
  colnames(re) <- c(rbind(paste0("int_", var_names),
                          paste0("slp_", var_names)))
  
  rand_long        <- as.data.frame(re)
  rand_long$ID     <- 1:n
  rand_long$class  <- class_assign
  num_cols         <- setdiff(names(rand_long), c("ID", "class"))
  rand_long[num_cols] <- round(rand_long[num_cols], 2)
  
  ## 3. 固定效应系数矩阵 --------------------------------------------------
  int_mu <- sapply(param_list, `[[`, "mean_intercept")
  slp_mu <- sapply(param_list, `[[`, "mean_slope")
  
  fixed_mu <- as.vector(rbind(int_mu, slp_mu))
  
  fixed_eff <- matrix(rep(fixed_mu, each = n), nrow = n, byrow = TRUE)
  colnames(fixed_eff) <- colnames(re)
  
  fixed_long        <- as.data.frame(fixed_eff)
  fixed_long$ID     <- 1:n
  fixed_long$class  <- class_assign
  
  ## 4. 合成最终值 --------------------------------------------------------
  noise_sd <- sapply(param_list, `[[`, "noise_sd")   # 提取每个变量的噪声标准差
  n_id     <- nrow(rand_long)
  vars     <- var_names
  
  # 4.1 随机时间矩阵
  t_mat <- matrix(NA, nrow = n_id, ncol = n_time)
  # V1 固定为 0
  t_mat[, 1] <- 0  
  # V2–V5 按区间生成
  t_mat[, 2] <- runif(n_id, 0, 0.25)
  t_mat[, 3] <- runif(n_id, 0.25, 0.5)
  t_mat[, 4] <- runif(n_id, 0.5, 0.75)
  t_mat[, 5] <- runif(n_id, 0.75, 1)
  # 确保每一行时间递增
  t_mat <- t(apply(t_mat, 1, sort))
  
  # 4.2 长格式辅助向量
  ID_vec       <- rep(rand_long$ID, each = n_time)
  class_vec    <- rep(rand_long$class, each = n_time)
  time_vec_seq <- rep(1:n_time, times = n_id)
  t_vec_long   <- c(t(t_mat))
  
  # 4.3 预分配结果矩阵
  Y_mat <- matrix(NA, nrow = n_id * n_time, ncol = length(vars))
  colnames(Y_mat) <- vars
  
  # 4.4 按变量循环生成 Y
  for (i in seq_along(vars)) {
    vn <- vars[i]
    rand_int <- rand_long[[paste0("int_", vn)]]
    rand_slp <- rand_long[[paste0("slp_", vn)]]
    fix_int  <- fixed_long[[paste0("int_", vn)]]
    fix_slp  <- fixed_long[[paste0("slp_", vn)]]
    
    sys <- fix_int + rand_int + (fix_slp + rand_slp) * t_mat
    sys_long <- as.vector(t(sys))   # <-- 关键：按 ID 展开矩阵
    
    Y_mat[, i] <- sys_long + rnorm(n_id * n_time, 0, noise_sd[i])
  }
  
  # 4.5 打包返回
  Y_df <- data.frame(Y_mat,
                     ID = ID_vec,
                     class = class_vec,
                     time = time_vec_seq,
                     t = t_vec_long)
  return(Y_df)
}

put_in_list <- function(n_datasets = 200,
                       n = 500,
                       n_time = 5,
                       param_list = param_list_1,
                       Sigma = cov_matrix_low,
                       base_seed = 123,
                       param_set = "L1",
                       corr_level = "low") {
  
  # 创建空的列表来存储所有数据集
  result_list <- list()
  
  # 循环生成数据集并直接存入列表
  for (i in 1:n_datasets) {
    # 生成数据集
    dataset <- sim_single_class(
      n = n,
      n_time = n_time,
      param_list = param_list,
      Sigma = Sigma,
      seed = base_seed + i
    )
    
    # 将数据集添加到列表中
    list_name <- paste0("sim_data_", n, "_", corr_level, "_", param_set, "_", i)
    result_list[[list_name]] <- dataset
  }
  
  return(result_list)
}
###############数据参数list######

param_list_1 <- list(
  list(varname = "V1",
       mean_intercept = 0.55, mean_slope = 0.35,
       noise_sd = 0.03),
  
  list(varname = "V2",
       mean_intercept = 0.70, mean_slope = -0.25,
       noise_sd = 0.12),
  
  list(varname = "V3",
       mean_intercept = 0.30, mean_slope = 0.60,
       noise_sd = 0.04),
  
  list(varname = "V4",
       mean_intercept = 0.85, mean_slope = -0.15,
       noise_sd = 0.115),
  
  list(varname = "V5",
       mean_intercept = 0.45, mean_slope = -0.45,
       noise_sd = 0.025),
  
  list(varname = "V6",
       mean_intercept = 0.65, mean_slope = 0.20,
       noise_sd = 0.11),
  
  list(varname = "V7",
       mean_intercept = 0.75, mean_slope = -0.30,
       noise_sd = 0.035),
  
  list(varname = "V8",
       mean_intercept = 0.90, mean_slope = 0.40,
       noise_sd = 0.045),
  
  list(varname = "V9",
       mean_intercept = 0.25, mean_slope = -0.20,
       noise_sd = 0.022),
  
  list(varname = "V10",
       mean_intercept = 0.50, mean_slope = 0.25,
       noise_sd = 0.128)
)

param_list_2 <- list(
  list(varname = "V1",
       mean_intercept = 0.60, mean_slope = 0.30,
       noise_sd = 0.035),
  
  list(varname = "V2",
       mean_intercept = 0.65, mean_slope = -0.20,
       noise_sd = 0.11),
  
  list(varname = "V3",
       mean_intercept = 0.35, mean_slope = 0.55,
       noise_sd = 0.045),
  
  list(varname = "V4",
       mean_intercept = 0.80, mean_slope = -0.12,
       noise_sd = 0.105),
  
  list(varname = "V5",
       mean_intercept = 0.50, mean_slope = -0.40,
       noise_sd = 0.03),
  
  list(varname = "V6",
       mean_intercept = 0.70, mean_slope = 0.15,
       noise_sd = 0.095),
  
  list(varname = "V7",
       mean_intercept = 0.80, mean_slope = -0.25,
       noise_sd = 0.04),
  
  list(varname = "V8",
       mean_intercept = 0.85, mean_slope = 0.35,
       noise_sd = 0.05),
  
  list(varname = "V9",
       mean_intercept = 0.30, mean_slope = -0.18,
       noise_sd = 0.025),
  
  list(varname = "V10",
       mean_intercept = 0.55, mean_slope = 0.20,
       noise_sd = 0.115)
)

param_list_3 <- list(
  list(varname = "V1",
       mean_intercept = 0.50, mean_slope = 0.40,
       noise_sd = 0.028),
  
  list(varname = "V2",
       mean_intercept = 0.75, mean_slope = -0.30,
       noise_sd = 0.125),
  
  list(varname = "V3",
       mean_intercept = 0.25, mean_slope = 0.65,
       noise_sd = 0.038),
  
  list(varname = "V4",
       mean_intercept = 0.90, mean_slope = -0.18,
       noise_sd = 0.098),
  
  list(varname = "V5",
       mean_intercept = 0.40, mean_slope = -0.50,
       noise_sd = 0.022),
  
  list(varname = "V6",
       mean_intercept = 0.60, mean_slope = 0.25,
       noise_sd = 0.108),
  
  list(varname = "V7",
       mean_intercept = 0.70, mean_slope = -0.35,
       noise_sd = 0.042),
  
  list(varname = "V8",
       mean_intercept = 0.95, mean_slope = 0.30,
       noise_sd = 0.055),
  
  list(varname = "V9",
       mean_intercept = 0.20, mean_slope = -0.22,
       noise_sd = 0.018),
  
  list(varname = "V10",
       mean_intercept = 0.45, mean_slope = 0.30,
       noise_sd = 0.122)
)

param_list_4 <- list(
  list(varname = "V1",
       mean_intercept = 0.65, mean_slope = 0.25,
       noise_sd = 0.032),
  
  list(varname = "V2",
       mean_intercept = 0.60, mean_slope = -0.35,
       noise_sd = 0.118),
  
  list(varname = "V3",
       mean_intercept = 0.40, mean_slope = 0.50,
       noise_sd = 0.042),
  
  list(varname = "V4",
       mean_intercept = 0.75, mean_slope = -0.20,
       noise_sd = 0.112),
  
  list(varname = "V5",
       mean_intercept = 0.35, mean_slope = -0.45,
       noise_sd = 0.028),
  
  list(varname = "V6",
       mean_intercept = 0.75, mean_slope = 0.18,
       noise_sd = 0.102),
  
  list(varname = "V7",
       mean_intercept = 0.85, mean_slope = -0.28,
       noise_sd = 0.038),
  
  list(varname = "V8",
       mean_intercept = 0.80, mean_slope = 0.45,
       noise_sd = 0.048),
  
  list(varname = "V9",
       mean_intercept = 0.35, mean_slope = -0.15,
       noise_sd = 0.03),
  
  list(varname = "V10",
       mean_intercept = 0.60, mean_slope = 0.22,
       noise_sd = 0.135)
)

param_list_5 <- list(
  list(varname = "V1",
       mean_intercept = 0.45, mean_slope = 0.45,
       noise_sd = 0.026),
  
  list(varname = "V2",
       mean_intercept = 0.80, mean_slope = -0.22,
       noise_sd = 0.108),
  
  list(varname = "V3",
       mean_intercept = 0.32, mean_slope = 0.58,
       noise_sd = 0.036),
  
  list(varname = "V4",
       mean_intercept = 0.88, mean_slope = -0.14,
       noise_sd = 0.092),
  
  list(varname = "V5",
       mean_intercept = 0.42, mean_slope = -0.48,
       noise_sd = 0.02),
  
  list(varname = "V6",
       mean_intercept = 0.68, mean_slope = 0.22,
       noise_sd = 0.098),
  
  list(varname = "V7",
       mean_intercept = 0.78, mean_slope = -0.32,
       noise_sd = 0.032),
  
  list(varname = "V8",
       mean_intercept = 0.92, mean_slope = 0.38,
       noise_sd = 0.052),
  
  list(varname = "V9",
       mean_intercept = 0.28, mean_slope = -0.25,
       noise_sd = 0.015),
  
  list(varname = "V10",
       mean_intercept = 0.52, mean_slope = 0.28,
       noise_sd = 0.118)
)

###############协方差矩阵/相关系数矩阵############################
path <- "F:/文章/大论文/程序/模拟数据/相关系数矩阵/"

# 对于单类别模型，我们只需要一个协方差矩阵
cov_matrix_low    <- read_excel(paste0(path, "cov_matrix_low1.xlsx"))
cov_matrix_medium <- read_excel(paste0(path, "cov_matrix_medium1.xlsx"))
cov_matrix_high   <- read_excel(paste0(path, "cov_matrix_high1.xlsx"))

###############生成数据集############################

# 定义输出目录
out_dir <- "F:/文章/大论文/程序Trae/模拟数据_协变量"

# 确保输出目录存在
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 为了保持兼容性，仍然定义三个目录变量，但都指向同一个目录
out_dir_low <- out_dir
out_dir_mid <- out_dir
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"

############数据生成############
##############低相关性#####
sim_data_500_low_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_low,
  base_seed = 123,
  param_set = "L1",
  corr_level = "low"
)
sim_data_500_low_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_low,
  base_seed = 124,
  param_set = "L2",
  corr_level = "low"
)
sim_data_500_low_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_low,
  base_seed = 125,
  param_set = "L3",
  corr_level = "low"
)
sim_data_500_low_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_low,
  base_seed = 126,
  param_set = "L4",
  corr_level = "low"
)
sim_data_500_low_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_low,
  base_seed = 127,
  param_set = "L5",
  corr_level = "low"
)

sim_data_1000_low_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_low,
  base_seed = 138,
  param_set = "L1",
  corr_level = "low"
)
sim_data_1000_low_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_low,
  base_seed = 139,
  param_set = "L2",
  corr_level = "low"
)
sim_data_1000_low_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_low,
  base_seed = 140,
  param_set = "L3",
  corr_level = "low"
)
sim_data_1000_low_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_low,
  base_seed = 141,
  param_set = "L4",
  corr_level = "low"
)
sim_data_1000_low_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_low,
  base_seed = 142,
  param_set = "L5",
  corr_level = "low"
)

cat("low")

##############中相关性##############
sim_data_500_medium_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_medium,
  base_seed = 128,
  param_set = "L1",
  corr_level = "medium"
)
sim_data_500_medium_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_medium,
  base_seed = 129,
  param_set = "L2",
  corr_level = "medium"
)
sim_data_500_medium_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_medium,
  base_seed = 130,
  param_set = "L3",
  corr_level = "medium"
)
sim_data_500_medium_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_medium,
  base_seed = 131,
  param_set = "L4",
  corr_level = "medium"
)
sim_data_500_medium_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_medium,
  base_seed = 132,
  param_set = "L5",
  corr_level = "medium"
)

sim_data_1000_medium_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_medium,
  base_seed = 143,
  param_set = "L1",
  corr_level = "medium"
)
sim_data_1000_medium_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_medium,
  base_seed = 144,
  param_set = "L2",
  corr_level = "medium"
)
sim_data_1000_medium_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_medium,
  base_seed = 145,
  param_set = "L3",
  corr_level = "medium"
)
sim_data_1000_medium_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_medium,
  base_seed = 146,
  param_set = "L4",
  corr_level = "medium"
)
sim_data_1000_medium_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_medium,
  base_seed = 147,
  param_set = "L5",
  corr_level = "medium"
)
cat("medium")

##############高相关性##############
sim_data_500_high_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_high,
  base_seed = 133,
  param_set = "L1",
  corr_level = "high"
)
sim_data_500_high_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_high,
  base_seed = 134,
  param_set = "L2",
  corr_level = "high"
)
sim_data_500_high_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_high,
  base_seed = 135,
  param_set = "L3",
  corr_level = "high"
)
sim_data_500_high_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_high,
  base_seed = 136,
  param_set = "L4",
  corr_level = "high"
)
sim_data_500_high_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_high,
  base_seed = 137,
  param_set = "L5",
  corr_level = "high"
)

sim_data_1000_high_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma = cov_matrix_high,
  base_seed = 148,
  param_set = "L1",
  corr_level = "high"
)
sim_data_1000_high_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma = cov_matrix_high,
  base_seed = 149,
  param_set = "L2",
  corr_level = "high"
)
sim_data_1000_high_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma = cov_matrix_high,
  base_seed = 150,
  param_set = "L3",
  corr_level = "high"
)
sim_data_1000_high_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma = cov_matrix_high,
  base_seed = 151,
  param_set = "L4",
  corr_level = "high"
)
sim_data_1000_high_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma = cov_matrix_high,
  base_seed = 152,
  param_set = "L5",
  corr_level = "high"
)

cat("high")

################循环导出#########

#保存数据

out_dir <- "F:/文章/大论文/程序Trae/模拟数据_协变量"

## 低相关
write_xlsx(sim_data_500_low_L1,  path = file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"))
write_xlsx(sim_data_500_low_L2,  path = file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"))
write_xlsx(sim_data_500_low_L3,  path = file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"))
write_xlsx(sim_data_500_low_L4,  path = file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"))
write_xlsx(sim_data_500_low_L5,  path = file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"))
write_xlsx(sim_data_1000_low_L1,  path = file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"))
write_xlsx(sim_data_1000_low_L2,  path = file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"))
write_xlsx(sim_data_1000_low_L3,  path = file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"))
write_xlsx(sim_data_1000_low_L4,  path = file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"))
write_xlsx(sim_data_1000_low_L5,  path = file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"))

## 中相关
write_xlsx(sim_data_500_medium_L1, path = file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"))
write_xlsx(sim_data_500_medium_L2, path = file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"))
write_xlsx(sim_data_500_medium_L3, path = file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"))
write_xlsx(sim_data_500_medium_L4, path = file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"))
write_xlsx(sim_data_500_medium_L5, path = file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"))
write_xlsx(sim_data_1000_medium_L1, path = file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"))
write_xlsx(sim_data_1000_medium_L2, path = file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"))
write_xlsx(sim_data_1000_medium_L3, path = file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"))
write_xlsx(sim_data_1000_medium_L4, path = file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"))
write_xlsx(sim_data_1000_medium_L5, path = file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"))

## 高相关
write_xlsx(sim_data_500_high_L1, path = file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"))
write_xlsx(sim_data_500_high_L2, path = file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"))
write_xlsx(sim_data_500_high_L3, path = file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"))
write_xlsx(sim_data_500_high_L4, path = file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"))
write_xlsx(sim_data_500_high_L5, path = file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"))
write_xlsx(sim_data_1000_high_L1, path = file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"))
write_xlsx(sim_data_1000_high_L2, path = file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"))
write_xlsx(sim_data_1000_high_L3, path = file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx")) 
write_xlsx(sim_data_1000_high_L4, path = file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"))
write_xlsx(sim_data_1000_high_L5, path = file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"))





#############数据导入###########

data<-sim_data_500_low_c1_L1[[1]]

library(readxl)
library(purrr) 

out_dir <- "F:/文章/大论文/程序Trae/模拟数据_协变量"

## 低相关
sim_data_500_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"), sheet = .x))

sim_data_500_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"), sheet = .x))

sim_data_500_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"), sheet = .x))

sim_data_500_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"), sheet = .x))

sim_data_500_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"), sheet = .x))

sim_data_1000_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"), sheet = .x))

sim_data_1000_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"), sheet = .x))

sim_data_1000_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"), sheet = .x))

sim_data_1000_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"), sheet = .x))

sim_data_1000_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"), sheet = .x))

## 中相关
sim_data_500_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"), sheet = .x))

sim_data_500_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"), sheet = .x))

sim_data_500_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"), sheet = .x))

sim_data_500_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"), sheet = .x))

sim_data_500_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"), sheet = .x))

## 高相关
sim_data_500_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"), sheet = .x))

sim_data_500_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"), sheet = .x))

sim_data_500_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"), sheet = .x))

sim_data_500_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"), sheet = .x))

sim_data_500_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"), sheet = .x))

sim_data_1000_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"), sheet = .x))

sim_data_1000_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"), sheet = .x))

sim_data_1000_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"), sheet = .x))

sim_data_1000_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"), sheet = .x))

sim_data_1000_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"), sheet = .x))




