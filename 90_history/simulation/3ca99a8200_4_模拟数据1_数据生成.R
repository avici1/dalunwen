library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(writexl)
library(readxl)

options(scipen = 999)


#保存数据
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"








##############生成程序#################

sim_multi_class1 <- function(n = 200,
                             n_time = 5,
                             param_list,
                             Sigma1 = diag(20),
                             Sigma2 = diag(20),
                             Sigma3 = diag(20),
                             seed = 123) {
  
  ## 0. 环境准备 ----------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  
  ## 1. 基本框架 ----------------------------------------------------------
  cls_idx      <- rep(1:3, length.out = n)
  class_assign <- paste0("c", cls_idx)
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  
  ## 2. 随机效应 ----------------------------------------------------------
  n1 <- sum(cls_idx == 1); n2 <- sum(cls_idx == 2); n3 <- sum(cls_idx == 3)
  re1 <- mvrnorm(n1, mu = rep(0, 20), Sigma = Sigma1)
  re2 <- mvrnorm(n2, mu = rep(0, 20), Sigma = Sigma2)
  re3 <- mvrnorm(n3, mu = rep(0, 20), Sigma = Sigma3)
  re_list <- list(re1, re2, re3)
  
  rand_eff <- do.call(rbind, lapply(1:n, function(i) {
    prev_count <- sum(cls_idx[1:i] == cls_idx[i])
    re_list[[cls_idx[i]]][prev_count, , drop = FALSE]
  }))
  
  var_names <- sapply(param_list, `[[`, "varname")
  colnames(rand_eff) <- c(rbind(paste0("int_", var_names),
                                paste0("slp_", var_names)))
  
  rand_long        <- as.data.frame(rand_eff)
  rand_long$ID     <- 1:n
  rand_long$class  <- class_assign
  num_cols         <- setdiff(names(rand_long), c("ID", "class"))
  rand_long[num_cols] <- round(rand_long[num_cols], 2)
  
  ## 3. 固定效应系数矩阵 --------------------------------------------------
  int_mu <- sapply(param_list, `[[`, "mean_intercept")
  slp_mu <- sapply(param_list, `[[`, "mean_slope")
  int_v  <- sapply(param_list, `[[`, "v_intercept")
  slp_v  <- sapply(param_list, `[[`, "v_slope")
  
  fixed_mu <- as.vector(rbind(int_mu, slp_mu))
  fixed_v  <- as.vector(rbind(int_v, slp_v))
  
  mu_mat <- rbind(fixed_mu - fixed_v,   # class 1
                  fixed_mu,             # class 2
                  fixed_mu + fixed_v)   # class 3
  
  fixed_eff <- mu_mat[cls_idx, ]
  colnames(fixed_eff) <- colnames(rand_eff)
  
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
                        Sigma1 = cov_matrix_low1,
                        Sigma2 = cov_matrix_low2,
                        Sigma3 = cov_matrix_low3,
                        base_seed = 123,
                        param_set = "L1") {
  
  # 创建空的列表来存储所有数据集
  result_list <- list()
  
  # 循环生成数据集并直接存入列表
  for (i in 1:n_datasets) {
    # 生成数据集
    dataset <- sim_multi_class1(
      n = n,
      n_time = n_time,
      param_list = param_list,
      Sigma1 = Sigma1,
      Sigma2 = Sigma2,
      Sigma3 = Sigma3,
      seed = base_seed + i
    )
    
    # 将数据集添加到列表中
    list_name <- paste0("sim_data_", n, "_low_", param_set, "_", i)
    result_list[[list_name]] <- dataset
  }
  
  return(result_list)
}


###############数据参数list######

param_list_1 <- list(
  list(varname = "V1",
       mean_intercept = 0.55, mean_slope = 0.35,
       v_intercept = 0.15, v_slope = 0.10,
       noise_sd = 0.03),
  
  list(varname = "V2",
       mean_intercept = 0.70, mean_slope = -0.25,
       v_intercept = 0.20, v_slope = 0.15,
       noise_sd = 0.12),
  
  list(varname = "V3",
       mean_intercept = 0.30, mean_slope = 0.60,
       v_intercept = 0.10, v_slope = 0.20,
       noise_sd = 0.04),
  
  list(varname = "V4",
       mean_intercept = 0.85, mean_slope = -0.15,
       v_intercept = 0.25, v_slope = -0.18,
       noise_sd = 0.115),
  
  list(varname = "V5",
       mean_intercept = 0.45, mean_slope = -0.45,
       v_intercept = 0.15, v_slope = -0.25,
       noise_sd = 0.025),
  
  list(varname = "V6",
       mean_intercept = 0.65, mean_slope = 0.20,
       v_intercept = 0.12, v_slope = -0.05,
       noise_sd = 0.11),
  
  list(varname = "V7",
       mean_intercept = 0.75, mean_slope = -0.30,
       v_intercept = 0.22, v_slope = 0.18,
       noise_sd = 0.035),
  
  list(varname = "V8",
       mean_intercept = 0.90, mean_slope = 0.40,
       v_intercept = 0.28, v_slope = 0.12,
       noise_sd = 0.045),
  
  list(varname = "V9",
       mean_intercept = 0.25, mean_slope = -0.20,
       v_intercept = 0.08, v_slope = 0.10,
       noise_sd = 0.022),
  
  list(varname = "V10",
       mean_intercept = 0.50, mean_slope = 0.25,
       v_intercept = 0.18, v_slope = 0.15,
       noise_sd = 0.128)
)

param_list_2 <- list(
  list(varname = "V1",
       mean_intercept = 0.60, mean_slope = 0.30,
       v_intercept = 0.18, v_slope = 0.12,
       noise_sd = 0.035),
  
  list(varname = "V2",
       mean_intercept = 0.65, mean_slope = -0.20,
       v_intercept = 0.22, v_slope = 0.12,
       noise_sd = 0.11),
  
  list(varname = "V3",
       mean_intercept = 0.35, mean_slope = 0.55,
       v_intercept = 0.12, v_slope = 0.18,
       noise_sd = 0.045),
  
  list(varname = "V4",
       mean_intercept = 0.80, mean_slope = -0.12,
       v_intercept = 0.28, v_slope = -0.15,
       noise_sd = 0.105),
  
  list(varname = "V5",
       mean_intercept = 0.50, mean_slope = -0.40,
       v_intercept = 0.18, v_slope = -0.22,
       noise_sd = 0.03),
  
  list(varname = "V6",
       mean_intercept = 0.70, mean_slope = 0.15,
       v_intercept = 0.15, v_slope = -0.08,
       noise_sd = 0.095),
  
  list(varname = "V7",
       mean_intercept = 0.80, mean_slope = -0.25,
       v_intercept = 0.25, v_slope = 0.15,
       noise_sd = 0.04),
  
  list(varname = "V8",
       mean_intercept = 0.85, mean_slope = 0.35,
       v_intercept = 0.32, v_slope = 0.10,
       noise_sd = 0.05),
  
  list(varname = "V9",
       mean_intercept = 0.30, mean_slope = -0.18,
       v_intercept = 0.10, v_slope = 0.08,
       noise_sd = 0.025),
  
  list(varname = "V10",
       mean_intercept = 0.55, mean_slope = 0.20,
       v_intercept = 0.20, v_slope = 0.12,
       noise_sd = 0.115)
)

param_list_3 <- list(
  list(varname = "V1",
       mean_intercept = 0.50, mean_slope = 0.40,
       v_intercept = 0.16, v_slope = 0.08,
       noise_sd = 0.028),
  
  list(varname = "V2",
       mean_intercept = 0.75, mean_slope = -0.30,
       v_intercept = 0.18, v_slope = 0.18,
       noise_sd = 0.125),
  
  list(varname = "V3",
       mean_intercept = 0.25, mean_slope = 0.65,
       v_intercept = 0.08, v_slope = 0.22,
       noise_sd = 0.038),
  
  list(varname = "V4",
       mean_intercept = 0.90, mean_slope = -0.18,
       v_intercept = 0.30, v_slope = -0.12,
       noise_sd = 0.098),
  
  list(varname = "V5",
       mean_intercept = 0.40, mean_slope = -0.50,
       v_intercept = 0.12, v_slope = -0.28,
       noise_sd = 0.022),
  
  list(varname = "V6",
       mean_intercept = 0.60, mean_slope = 0.25,
       v_intercept = 0.14, v_slope = -0.03,
       noise_sd = 0.108),
  
  list(varname = "V7",
       mean_intercept = 0.70, mean_slope = -0.35,
       v_intercept = 0.20, v_slope = 0.20,
       noise_sd = 0.042),
  
  list(varname = "V8",
       mean_intercept = 0.95, mean_slope = 0.30,
       v_intercept = 0.35, v_slope = 0.08,
       noise_sd = 0.055),
  
  list(varname = "V9",
       mean_intercept = 0.20, mean_slope = -0.22,
       v_intercept = 0.06, v_slope = 0.12,
       noise_sd = 0.018),
  
  list(varname = "V10",
       mean_intercept = 0.45, mean_slope = 0.30,
       v_intercept = 0.16, v_slope = 0.18,
       noise_sd = 0.122)
)

param_list_4 <- list(
  list(varname = "V1",
       mean_intercept = 0.65, mean_slope = 0.25,
       v_intercept = 0.20, v_slope = 0.15,
       noise_sd = 0.032),
  
  list(varname = "V2",
       mean_intercept = 0.60, mean_slope = -0.35,
       v_intercept = 0.25, v_slope = 0.10,
       noise_sd = 0.118),
  
  list(varname = "V3",
       mean_intercept = 0.40, mean_slope = 0.50,
       v_intercept = 0.14, v_slope = 0.16,
       noise_sd = 0.042),
  
  list(varname = "V4",
       mean_intercept = 0.75, mean_slope = -0.20,
       v_intercept = 0.22, v_slope = -0.20,
       noise_sd = 0.112),
  
  list(varname = "V5",
       mean_intercept = 0.35, mean_slope = -0.45,
       v_intercept = 0.10, v_slope = -0.30,
       noise_sd = 0.028),
  
  list(varname = "V6",
       mean_intercept = 0.75, mean_slope = 0.18,
       v_intercept = 0.18, v_slope = -0.10,
       noise_sd = 0.102),
  
  list(varname = "V7",
       mean_intercept = 0.85, mean_slope = -0.28,
       v_intercept = 0.28, v_slope = 0.12,
       noise_sd = 0.038),
  
  list(varname = "V8",
       mean_intercept = 0.80, mean_slope = 0.45,
       v_intercept = 0.30, v_slope = 0.15,
       noise_sd = 0.048),
  
  list(varname = "V9",
       mean_intercept = 0.35, mean_slope = -0.15,
       v_intercept = 0.12, v_slope = 0.06,
       noise_sd = 0.03),
  
  list(varname = "V10",
       mean_intercept = 0.60, mean_slope = 0.22,
       v_intercept = 0.22, v_slope = 0.14,
       noise_sd = 0.135)
)

param_list_5 <- list(
  list(varname = "V1",
       mean_intercept = 0.45, mean_slope = 0.45,
       v_intercept = 0.14, v_slope = 0.09,
       noise_sd = 0.026),
  
  list(varname = "V2",
       mean_intercept = 0.80, mean_slope = -0.22,
       v_intercept = 0.15, v_slope = 0.16,
       noise_sd = 0.108),
  
  list(varname = "V3",
       mean_intercept = 0.32, mean_slope = 0.58,
       v_intercept = 0.09, v_slope = 0.24,
       noise_sd = 0.036),
  
  list(varname = "V4",
       mean_intercept = 0.88, mean_slope = -0.14,
       v_intercept = 0.26, v_slope = -0.16,
       noise_sd = 0.092),
  
  list(varname = "V5",
       mean_intercept = 0.42, mean_slope = -0.48,
       v_intercept = 0.13, v_slope = -0.26,
       noise_sd = 0.02),
  
  list(varname = "V6",
       mean_intercept = 0.68, mean_slope = 0.22,
       v_intercept = 0.16, v_slope = -0.06,
       noise_sd = 0.098),
  
  list(varname = "V7",
       mean_intercept = 0.78, mean_slope = -0.32,
       v_intercept = 0.24, v_slope = 0.22,
       noise_sd = 0.032),
  
  list(varname = "V8",
       mean_intercept = 0.92, mean_slope = 0.38,
       v_intercept = 0.33, v_slope = 0.11,
       noise_sd = 0.052),
  
  list(varname = "V9",
       mean_intercept = 0.28, mean_slope = -0.25,
       v_intercept = 0.07, v_slope = 0.14,
       noise_sd = 0.015),
  
  list(varname = "V10",
       mean_intercept = 0.52, mean_slope = 0.28,
       v_intercept = 0.19, v_slope = 0.16,
       noise_sd = 0.118)
)
###############协方差矩阵/相关系数矩阵############################
path <- "F:/文章/大论文/程序/模拟数据/相关系数矩阵/"
#低相关
cov_matrix_low1    <- read_excel(paste0(path, "cov_matrix_low1.xlsx"))
cov_matrix_low2    <- read_excel(paste0(path, "cov_matrix_low2.xlsx"))
cov_matrix_low3    <- read_excel(paste0(path, "cov_matrix_low3.xlsx"))
# 中相关
cov_matrix_medium1 <- read_excel(paste0(path, "cov_matrix_medium1.xlsx"))
cov_matrix_medium2 <- read_excel(paste0(path, "cov_matrix_medium2.xlsx"))
cov_matrix_medium3 <- read_excel(paste0(path, "cov_matrix_medium3.xlsx"))

# 高相关
cov_matrix_high1   <- read_excel(paste0(path, "cov_matrix_high1.xlsx"))
cov_matrix_high2   <- read_excel(paste0(path, "cov_matrix_high2.xlsx"))
cov_matrix_high3   <- read_excel(paste0(path, "cov_matrix_high3.xlsx"))


############数据生成############
##############低相关性#####
sim_data_500_low_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_500_low_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_500_low_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_500_low_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_500_low_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 923,
  param_set = "L5"
)

sim_data_1000_low_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_1000_low_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_1000_low_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_1000_low_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_1000_low_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  base_seed = 923,
  param_set = "L5"
)

cat("low")

##############中相关性##############
sim_data_500_medium_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_500_medium_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_500_medium_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_500_medium_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_500_medium_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 923,
  param_set = "L5"
)

sim_data_1000_medium_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_1000_medium_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_1000_medium_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_1000_medium_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_1000_medium_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_medium1,
  Sigma2 = cov_matrix_medium2,
  Sigma3 = cov_matrix_medium3,
  base_seed = 923,
  param_set = "L5"
)
cat("medium")

##############高相关性##############
sim_data_500_high_L1 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_500_high_L2 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_500_high_L3 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_500_high_L4 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_500_high_L5 <- put_in_list(
  n_datasets = 200,
  n = 500,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 923,
  param_set = "L5"
)

sim_data_1000_high_L1 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_1,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 123,
  param_set = "L1"
)
sim_data_1000_high_L2 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_2,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 323,
  param_set = "L2"
)
sim_data_1000_high_L3 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_3,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 523,
  param_set = "L3"
)
sim_data_1000_high_L4 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_4,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 723,
  param_set = "L4"
)
sim_data_1000_high_L5 <- put_in_list(
  n_datasets = 200,
  n = 1000,
  n_time = 5,
  param_list = param_list_5,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  base_seed = 923,
  param_set = "L5"
)

cat("high")
################循环导出#########

#保存数据
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"


##  500 样本

## 低相关
write_xlsx(sim_data_500_low_L1,  path = file.path(out_dir_low, "sim_data_500_low_L1.xlsx"))
write_xlsx(sim_data_500_low_L2,  path = file.path(out_dir_low, "sim_data_500_low_L2.xlsx"))
write_xlsx(sim_data_500_low_L3,  path = file.path(out_dir_low, "sim_data_500_low_L3.xlsx"))
write_xlsx(sim_data_500_low_L4,  path = file.path(out_dir_low, "sim_data_500_low_L4.xlsx"))
write_xlsx(sim_data_500_low_L5,  path = file.path(out_dir_low, "sim_data_500_low_L5.xlsx"))
write_xlsx(sim_data_1000_low_L1,  path = file.path(out_dir_low, "sim_data_1000_low_L1.xlsx"))
write_xlsx(sim_data_1000_low_L2,  path = file.path(out_dir_low, "sim_data_1000_low_L2.xlsx"))
write_xlsx(sim_data_1000_low_L3,  path = file.path(out_dir_low, "sim_data_1000_low_L3.xlsx"))
write_xlsx(sim_data_1000_low_L4,  path = file.path(out_dir_low, "sim_data_1000_low_L4.xlsx"))
write_xlsx(sim_data_1000_low_L5,  path = file.path(out_dir_low, "sim_data_1000_low_L5.xlsx"))

## 中相关
write_xlsx(sim_data_500_medium_L1, path = file.path(out_dir_mid, "sim_data_500_medium_L1.xlsx"))
write_xlsx(sim_data_500_medium_L2, path = file.path(out_dir_mid, "sim_data_500_medium_L2.xlsx"))
write_xlsx(sim_data_500_medium_L3, path = file.path(out_dir_mid, "sim_data_500_medium_L3.xlsx"))
write_xlsx(sim_data_500_medium_L4, path = file.path(out_dir_mid, "sim_data_500_medium_L4.xlsx"))
write_xlsx(sim_data_500_medium_L5, path = file.path(out_dir_mid, "sim_data_500_medium_L5.xlsx"))
write_xlsx(sim_data_1000_medium_L1, path = file.path(out_dir_mid, "sim_data_1000_medium_L1.xlsx"))
write_xlsx(sim_data_1000_medium_L2, path = file.path(out_dir_mid, "sim_data_1000_medium_L2.xlsx"))
write_xlsx(sim_data_1000_medium_L3, path = file.path(out_dir_mid, "sim_data_1000_medium_L3.xlsx"))
write_xlsx(sim_data_1000_medium_L4, path = file.path(out_dir_mid, "sim_data_1000_medium_L4.xlsx"))
write_xlsx(sim_data_1000_medium_L5, path = file.path(out_dir_mid, "sim_data_1000_medium_L5.xlsx"))

## 高相关
write_xlsx(sim_data_500_high_L1, path = file.path(out_dir_high, "sim_data_500_high_L1.xlsx"))
write_xlsx(sim_data_500_high_L2, path = file.path(out_dir_high, "sim_data_500_high_L2.xlsx"))
write_xlsx(sim_data_500_high_L3, path = file.path(out_dir_high, "sim_data_500_high_L3.xlsx"))
write_xlsx(sim_data_500_high_L4, path = file.path(out_dir_high, "sim_data_500_high_L4.xlsx"))
write_xlsx(sim_data_500_high_L5, path = file.path(out_dir_high, "sim_data_500_high_L5.xlsx"))
write_xlsx(sim_data_1000_high_L1, path = file.path(out_dir_high, "sim_data_1000_high_L1.xlsx"))
write_xlsx(sim_data_1000_high_L2, path = file.path(out_dir_high, "sim_data_1000_high_L2.xlsx"))
write_xlsx(sim_data_1000_high_L3, path = file.path(out_dir_high, "sim_data_1000_high_L3.xlsx"))
write_xlsx(sim_data_1000_high_L4, path = file.path(out_dir_high, "sim_data_1000_high_L4.xlsx"))
write_xlsx(sim_data_1000_high_L5, path = file.path(out_dir_high, "sim_data_1000_high_L5.xlsx"))



########作图观察######

#500
ggplot(sim_data_500_low_L1[[75]], aes(x = t, y = V2_0.80, color = class)) +
  geom_smooth(se = TRUE, method = "loess", size = 1.5, alpha = 0.3) +
  labs(x = "Time (t)", y = "V2_0.80 value",
       title = "Smoothed Trajectories by Class (Dataset 1)") +
  theme_minimal(base_size = 14)

ggplot(sim_data500_high[[5]], aes(x = t, y = V4_0.90, color = class)) +
  geom_smooth(se = TRUE, method = "loess", size = 1.5, alpha = 0.3) +
  labs(x = "Time (t)", y = "V4_0.90 value",
       title = "Smoothed Trajectories by Class (Dataset 2)") +
  theme_minimal(base_size = 14)

ggplot(sim_data500_low1, aes(x = t, y = V1_0.55, color = class, fill = class)) +
  geom_smooth(method = "loess", se = TRUE, size = 1.2, alpha = 0.3) +
  labs(x = "Time (t)", y = "V1_0.55 value",
       title = "Smoothed Trajectories with 95% CI by Class") +
  theme_minimal(base_size = 14)

#

# 创建图表
p <- ggplot(sim_data500_low[[75]], aes(x = t, y = V2_0.80, group = ID, color = class)) +
  # 绘制所有个体的轨迹线（较细，半透明）
  geom_line(alpha = 0.3, linewidth = 0.1) +
  
  # 计算每个class在每个时间点的平均值
  # 添加平均轨迹线（较粗，不透明）
  stat_summary(
    aes(group = class, color = class),
    fun = mean, geom = "line", linewidth = 0.5,
    linetype = "solid"
  ) +
  
  # 设置颜色和主题
  scale_color_manual(values = c("c1" = "red", "c2" = "green", "c3" = "blue")) +
  theme_minimal() +
  
  # 添加标签和标题
  labs(
    title = "个体轨迹与平均轨迹 (按类别)",
    x = "时间 (t)",
    y = "V2_0.80",
    color = "类别"
  )

# 显示图表
print(p)





#1000
ggplot(sim_data1000_low[[75]], aes(x = t, y = V2_0.80, color = class)) +
  geom_smooth(se = TRUE, method = "loess", size = 1.5, alpha = 0.3) +
  labs(x = "Time (t)", y = "V2_0.80 value",
       title = "Smoothed Trajectories by Class (Dataset 1)") +
  theme_minimal(base_size = 14)

ggplot(sim_data1000_high[[5]], aes(x = t, y = V4_0.90, color = class)) +
  geom_smooth(se = TRUE, method = "loess", size = 1.5, alpha = 0.3) +
  labs(x = "Time (t)", y = "V4_0.90 value",
       title = "Smoothed Trajectories by Class (Dataset 2)") +
  theme_minimal(base_size = 14)



#

# 创建图表
p <- ggplot(sim_data1000_low[[75]], aes(x = t, y = V2_0.80, group = ID, color = class)) +
  # 绘制所有个体的轨迹线（较细，半透明）
  geom_line(alpha = 0.3, linewidth = 0.1) +
  
  # 计算每个class在每个时间点的平均值
  # 添加平均轨迹线（较粗，不透明）
  stat_summary(
    aes(group = class, color = class),
    fun = mean, geom = "line", linewidth = 0.5,
    linetype = "solid"
  ) +
  
  # 设置颜色和主题
  scale_color_manual(values = c("c1" = "red", "c2" = "green", "c3" = "blue")) +
  theme_minimal() +
  
  # 添加标签和标题
  labs(
    title = "个体轨迹与平均轨迹 (按类别)",
    x = "时间 (t)",
    y = "V2_0.80",
    color = "类别"
  )

# 显示图表
print(p)



#########读取数据######
library(readxl)
library(purrr) 
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"

# =====================================================================
#                            低相关数据

##  500样本
sim_data_500_low_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_500_low_L1.xlsx")),
                                    excel_sheets(file.path(out_dir_low, "sim_data_500_low_L1.xlsx"))),
                           ~ read_xlsx(file.path(out_dir_low, "sim_data_500_low_L1.xlsx"), sheet = .x))

sim_data_500_low_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_500_low_L2.xlsx")),
                                    excel_sheets(file.path(out_dir_low, "sim_data_500_low_L2.xlsx"))),
                           ~ read_xlsx(file.path(out_dir_low, "sim_data_500_low_L2.xlsx"), sheet = .x))

sim_data_500_low_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_500_low_L3.xlsx")),
                                    excel_sheets(file.path(out_dir_low, "sim_data_500_low_L3.xlsx"))),
                           ~ read_xlsx(file.path(out_dir_low, "sim_data_500_low_L3.xlsx"), sheet = .x))

sim_data_500_low_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_500_low_L4.xlsx")),
                                    excel_sheets(file.path(out_dir_low, "sim_data_500_low_L4.xlsx"))),
                           ~ read_xlsx(file.path(out_dir_low, "sim_data_500_low_L4.xlsx"), sheet = .x))

sim_data_500_low_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_500_low_L5.xlsx")),
                                    excel_sheets(file.path(out_dir_low, "sim_data_500_low_L5.xlsx"))),
                           ~ read_xlsx(file.path(out_dir_low, "sim_data_500_low_L5.xlsx"), sheet = .x))

## ===== 1000样本 
sim_data_1000_low_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L1.xlsx")),
                                     excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L1.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_low, "sim_data_1000_low_L1.xlsx"), sheet = .x))

sim_data_1000_low_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L2.xlsx")),
                                     excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L2.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_low, "sim_data_1000_low_L2.xlsx"), sheet = .x))

sim_data_1000_low_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L3.xlsx")),
                                     excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L3.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_low, "sim_data_1000_low_L3.xlsx"), sheet = .x))

sim_data_1000_low_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L4.xlsx")),
                                     excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L4.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_low, "sim_data_1000_low_L4.xlsx"), sheet = .x))

sim_data_1000_low_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L5.xlsx")),
                                     excel_sheets(file.path(out_dir_low, "sim_data_1000_low_L5.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_low, "sim_data_1000_low_L5.xlsx"), sheet = .x))

# =====================================================================
#                            中相关数据

## ===== 500样本 
sim_data_500_medium_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L1.xlsx")),
                                       excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L1.xlsx"))),
                              ~ read_xlsx(file.path(out_dir_mid, "sim_data_500_medium_L1.xlsx"), sheet = .x))

sim_data_500_medium_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L2.xlsx")),
                                       excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L2.xlsx"))),
                              ~ read_xlsx(file.path(out_dir_mid, "sim_data_500_medium_L2.xlsx"), sheet = .x))

sim_data_500_medium_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L3.xlsx")),
                                       excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L3.xlsx"))),
                              ~ read_xlsx(file.path(out_dir_mid, "sim_data_500_medium_L3.xlsx"), sheet = .x))

sim_data_500_medium_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L4.xlsx")),
                                       excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L4.xlsx"))),
                              ~ read_xlsx(file.path(out_dir_mid, "sim_data_500_medium_L4.xlsx"), sheet = .x))

sim_data_500_medium_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L5.xlsx")),
                                       excel_sheets(file.path(out_dir_mid, "sim_data_500_medium_L5.xlsx"))),
                              ~ read_xlsx(file.path(out_dir_mid, "sim_data_500_medium_L5.xlsx"), sheet = .x))

## ===== 1000样本 
sim_data_1000_medium_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L1.xlsx")),
                                        excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir_mid, "sim_data_1000_medium_L1.xlsx"), sheet = .x))

sim_data_1000_medium_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L2.xlsx")),
                                        excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir_mid, "sim_data_1000_medium_L2.xlsx"), sheet = .x))

sim_data_1000_medium_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L3.xlsx")),
                                        excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir_mid, "sim_data_1000_medium_L3.xlsx"), sheet = .x))

sim_data_1000_medium_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L4.xlsx")),
                                        excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir_mid, "sim_data_1000_medium_L4.xlsx"), sheet = .x))

sim_data_1000_medium_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L5.xlsx")),
                                        excel_sheets(file.path(out_dir_mid, "sim_data_1000_medium_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir_mid, "sim_data_1000_medium_L5.xlsx"), sheet = .x))

# =====================================================================
#                            高相关数据

## ===== 500样本 
sim_data_500_high_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_500_high_L1.xlsx")),
                                     excel_sheets(file.path(out_dir_high, "sim_data_500_high_L1.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_high, "sim_data_500_high_L1.xlsx"), sheet = .x))

sim_data_500_high_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_500_high_L2.xlsx")),
                                     excel_sheets(file.path(out_dir_high, "sim_data_500_high_L2.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_high, "sim_data_500_high_L2.xlsx"), sheet = .x))

sim_data_500_high_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_500_high_L3.xlsx")),
                                     excel_sheets(file.path(out_dir_high, "sim_data_500_high_L3.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_high, "sim_data_500_high_L3.xlsx"), sheet = .x))

sim_data_500_high_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_500_high_L4.xlsx")),
                                     excel_sheets(file.path(out_dir_high, "sim_data_500_high_L4.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_high, "sim_data_500_high_L4.xlsx"), sheet = .x))

sim_data_500_high_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_500_high_L5.xlsx")),
                                     excel_sheets(file.path(out_dir_high, "sim_data_500_high_L5.xlsx"))),
                            ~ read_xlsx(file.path(out_dir_high, "sim_data_500_high_L5.xlsx"), sheet = .x))

## ===== 1000样本 
sim_data_1000_high_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L1.xlsx")),
                                      excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L1.xlsx"))),
                             ~ read_xlsx(file.path(out_dir_high, "sim_data_1000_high_L1.xlsx"), sheet = .x))

sim_data_1000_high_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L2.xlsx")),
                                      excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L2.xlsx"))),
                             ~ read_xlsx(file.path(out_dir_high, "sim_data_1000_high_L2.xlsx"), sheet = .x))

sim_data_1000_high_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L3.xlsx")),
                                      excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L3.xlsx"))),
                             ~ read_xlsx(file.path(out_dir_high, "sim_data_1000_high_L3.xlsx"), sheet = .x))

sim_data_1000_high_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L4.xlsx")),
                                      excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L4.xlsx"))),
                             ~ read_xlsx(file.path(out_dir_high, "sim_data_1000_high_L4.xlsx"), sheet = .x))

sim_data_1000_high_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L5.xlsx")),
                                      excel_sheets(file.path(out_dir_high, "sim_data_1000_high_L5.xlsx"))),
                             ~ read_xlsx(file.path(out_dir_high, "sim_data_1000_high_L5.xlsx"), sheet = .x))


# 低相关性
sim_data500_low <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_low, "/sim_data500_low", i, ".xlsx")
  read_xlsx(file_name)
})
# 中相关性
sim_data500_mid <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_mid, "/sim_data500_mid", i, ".xlsx")
  read_xlsx(file_name)
})
# 高相关性
sim_data500_high <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_high, "/sim_data500_high", i, ".xlsx")
  read_xlsx(file_name)
})


sim_data1000_low <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_low, "/sim_data1000_low", i, ".xlsx")
  read_xlsx(file_name)
})


# 中相关性
sim_data1000_mid <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_mid, "/sim_data1000_mid", i, ".xlsx")
  read_xlsx(file_name)
})

# 高相关性
sim_data1000_high <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_high, "/sim_data1000_high", i, ".xlsx")
  read_xlsx(file_name)
})


########################




