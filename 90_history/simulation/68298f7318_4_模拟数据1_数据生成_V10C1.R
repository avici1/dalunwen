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
sim_single_class <- function(n,
                             n_time,
                             param_list,
                             Sigma,
                             seed = NULL) {
  
  # 如果提供了种子，则设置随机种子
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # 获取变量数量
  n_vars <- length(param_list)
  
  # 检查Sigma的维度是否正确（应该是2*n_vars x 2*n_vars）
  expected_dim <- 2 * n_vars
  if (nrow(Sigma) != expected_dim || ncol(Sigma) != expected_dim) {
    stop(paste0("Sigma must be a ", expected_dim, "x", expected_dim, " matrix"))
  }
  
  # 初始化结果数据框
  result_df <- data.frame()
  
  # 为每个变量生成数据
  for (var_idx in 1:n_vars) {
    var_params <- param_list[[var_idx]]
    
    # 提取参数
    varname <- var_params$varname
    mean_intercept <- var_params$mean_intercept
    mean_slope <- var_params$mean_slope
    noise_sd <- var_params$noise_sd
    
    # 为当前变量提取对应的2x2协方差子矩阵
    # 每个变量占据Sigma中的两行两列：第(2*var_idx-1)和第(2*var_idx)行/列
    row_idx <- c(2 * var_idx - 1, 2 * var_idx)
    var_Sigma <- Sigma[row_idx, row_idx]
    
    # 生成随机效应（对于单类别模型）
    re <- MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = var_Sigma)
    
    # 生成时间变量
    time <- rep(0:(n_time - 1), n)
    
    # 生成个体标识符
    id <- rep(1:n, each = n_time)
    
    # 计算线性预测器（包含随机效应）
    linear_predictor <- (mean_intercept + re[, 1]) + (mean_slope + re[, 2]) * time
    
    # 添加噪声并应用逆链接函数（这里使用恒等链接）
    observed_values <- linear_predictor + rnorm(n * n_time, mean = 0, sd = noise_sd)
    
    # 创建临时数据框
    temp_df <- data.frame(
      id = id,
      time = time,
      observed_values = observed_values
    )
    
    # 设置列名
    colnames(temp_df)[3] <- varname
    
    # 合并到结果数据框
    if (var_idx == 1) {
      result_df <- temp_df
    } else {
      result_df <- merge(result_df, temp_df, by = c("id", "time"), all = TRUE)
    }
  }
  
  # 按照id和time排序
  result_df <- result_df[order(result_df$id, result_df$time), ]
  
  return(result_df)
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
out_dir_high <- out_dir

# 生成低相关数据集 (500样本)
sim_data_500_low_L1 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                   param_list = param_list_1, Sigma = cov_matrix_low, 
                                   base_seed = 123, param_set = "L1", corr_level = "low")

sim_data_500_low_L2 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                   param_list = param_list_2, Sigma = cov_matrix_low, 
                                   base_seed = 124, param_set = "L2", corr_level = "low")

sim_data_500_low_L3 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                   param_list = param_list_3, Sigma = cov_matrix_low, 
                                   base_seed = 125, param_set = "L3", corr_level = "low")

sim_data_500_low_L4 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                   param_list = param_list_4, Sigma = cov_matrix_low, 
                                   base_seed = 126, param_set = "L4", corr_level = "low")

sim_data_500_low_L5 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                   param_list = param_list_5, Sigma = cov_matrix_low, 
                                   base_seed = 127, param_set = "L5", corr_level = "low")

# 生成中等相关数据集 (500样本)
sim_data_500_medium_L1 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                      param_list = param_list_1, Sigma = cov_matrix_medium, 
                                      base_seed = 128, param_set = "L1", corr_level = "medium")

sim_data_500_medium_L2 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                      param_list = param_list_2, Sigma = cov_matrix_medium, 
                                      base_seed = 129, param_set = "L2", corr_level = "medium")

sim_data_500_medium_L3 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                      param_list = param_list_3, Sigma = cov_matrix_medium, 
                                      base_seed = 130, param_set = "L3", corr_level = "medium")

sim_data_500_medium_L4 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                      param_list = param_list_4, Sigma = cov_matrix_medium, 
                                      base_seed = 131, param_set = "L4", corr_level = "medium")

sim_data_500_medium_L5 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                      param_list = param_list_5, Sigma = cov_matrix_medium, 
                                      base_seed = 132, param_set = "L5", corr_level = "medium")

# 生成高相关数据集 (500样本)
sim_data_500_high_L1 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                    param_list = param_list_1, Sigma = cov_matrix_high, 
                                    base_seed = 133, param_set = "L1", corr_level = "high")

sim_data_500_high_L2 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                    param_list = param_list_2, Sigma = cov_matrix_high, 
                                    base_seed = 134, param_set = "L2", corr_level = "high")

sim_data_500_high_L3 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                    param_list = param_list_3, Sigma = cov_matrix_high, 
                                    base_seed = 135, param_set = "L3", corr_level = "high")

sim_data_500_high_L4 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                    param_list = param_list_4, Sigma = cov_matrix_high, 
                                    base_seed = 136, param_set = "L4", corr_level = "high")

sim_data_500_high_L5 <- put_in_list(n_datasets = 200, n = 500, n_time = 5, 
                                    param_list = param_list_5, Sigma = cov_matrix_high, 
                                    base_seed = 137, param_set = "L5", corr_level = "high")

# 保存数据集到Excel文件

# 保存低相关数据集 (500样本)
for(i in 1:length(sim_data_500_low_L1)) {
  write_xlsx(sim_data_500_low_L1[i], 
             paste0(out_dir_low, "/sim_data_500_low_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_low_L2)) {
  write_xlsx(sim_data_500_low_L2[i], 
             paste0(out_dir_low, "/sim_data_500_low_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_low_L3)) {
  write_xlsx(sim_data_500_low_L3[i], 
             paste0(out_dir_low, "/sim_data_500_low_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_low_L4)) {
  write_xlsx(sim_data_500_low_L4[i], 
             paste0(out_dir_low, "/sim_data_500_low_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_low_L5)) {
  write_xlsx(sim_data_500_low_L5[i], 
             paste0(out_dir_low, "/sim_data_500_low_L5_", i, ".xlsx"))
}

# 保存中等相关数据集 (500样本)
for(i in 1:length(sim_data_500_medium_L1)) {
  write_xlsx(sim_data_500_medium_L1[i], 
             paste0(out_dir_mid, "/sim_data_500_medium_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_medium_L2)) {
  write_xlsx(sim_data_500_medium_L2[i], 
             paste0(out_dir_mid, "/sim_data_500_medium_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_medium_L3)) {
  write_xlsx(sim_data_500_medium_L3[i], 
             paste0(out_dir_mid, "/sim_data_500_medium_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_medium_L4)) {
  write_xlsx(sim_data_500_medium_L4[i], 
             paste0(out_dir_mid, "/sim_data_500_medium_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_medium_L5)) {
  write_xlsx(sim_data_500_medium_L5[i], 
             paste0(out_dir_mid, "/sim_data_500_medium_L5_", i, ".xlsx"))
}

# 保存高相关数据集 (500样本)
for(i in 1:length(sim_data_500_high_L1)) {
  write_xlsx(sim_data_500_high_L1[i], 
             paste0(out_dir_high, "/sim_data_500_high_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_high_L2)) {
  write_xlsx(sim_data_500_high_L2[i], 
             paste0(out_dir_high, "/sim_data_500_high_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_high_L3)) {
  write_xlsx(sim_data_500_high_L3[i], 
             paste0(out_dir_high, "/sim_data_500_high_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_high_L4)) {
  write_xlsx(sim_data_500_high_L4[i], 
             paste0(out_dir_high, "/sim_data_500_high_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_500_high_L5)) {
  write_xlsx(sim_data_500_high_L5[i], 
             paste0(out_dir_high, "/sim_data_500_high_L5_", i, ".xlsx"))
}

# 生成1000样本的数据集
# 生成低相关数据集 (1000样本)
sim_data_1000_low_L1 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                    param_list = param_list_1, Sigma = cov_matrix_low, 
                                    base_seed = 138, param_set = "L1", corr_level = "low")

sim_data_1000_low_L2 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                    param_list = param_list_2, Sigma = cov_matrix_low, 
                                    base_seed = 139, param_set = "L2", corr_level = "low")

sim_data_1000_low_L3 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                    param_list = param_list_3, Sigma = cov_matrix_low, 
                                    base_seed = 140, param_set = "L3", corr_level = "low")

sim_data_1000_low_L4 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                    param_list = param_list_4, Sigma = cov_matrix_low, 
                                    base_seed = 141, param_set = "L4", corr_level = "low")

sim_data_1000_low_L5 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                    param_list = param_list_5, Sigma = cov_matrix_low, 
                                    base_seed = 142, param_set = "L5", corr_level = "low")

# 生成中等相关数据集 (1000样本)
sim_data_1000_medium_L1 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                       param_list = param_list_1, Sigma = cov_matrix_medium, 
                                       base_seed = 143, param_set = "L1", corr_level = "medium")

sim_data_1000_medium_L2 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                       param_list = param_list_2, Sigma = cov_matrix_medium, 
                                       base_seed = 144, param_set = "L2", corr_level = "medium")

sim_data_1000_medium_L3 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                       param_list = param_list_3, Sigma = cov_matrix_medium, 
                                       base_seed = 145, param_set = "L3", corr_level = "medium")

sim_data_1000_medium_L4 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                       param_list = param_list_4, Sigma = cov_matrix_medium, 
                                       base_seed = 146, param_set = "L4", corr_level = "medium")

sim_data_1000_medium_L5 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                       param_list = param_list_5, Sigma = cov_matrix_medium, 
                                       base_seed = 147, param_set = "L5", corr_level = "medium")

# 生成高相关数据集 (1000样本)
sim_data_1000_high_L1 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                     param_list = param_list_1, Sigma = cov_matrix_high, 
                                     base_seed = 148, param_set = "L1", corr_level = "high")

sim_data_1000_high_L2 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                     param_list = param_list_2, Sigma = cov_matrix_high, 
                                     base_seed = 149, param_set = "L2", corr_level = "high")

sim_data_1000_high_L3 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                     param_list = param_list_3, Sigma = cov_matrix_high, 
                                     base_seed = 150, param_set = "L3", corr_level = "high")

sim_data_1000_high_L4 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                     param_list = param_list_4, Sigma = cov_matrix_high, 
                                     base_seed = 151, param_set = "L4", corr_level = "high")

sim_data_1000_high_L5 <- put_in_list(n_datasets = 200, n = 1000, n_time = 5, 
                                     param_list = param_list_5, Sigma = cov_matrix_high, 
                                     base_seed = 152, param_set = "L5", corr_level = "high")

# 保存1000样本的数据集到Excel文件

# 保存低相关数据集 (1000样本)
for(i in 1:length(sim_data_1000_low_L1)) {
  write_xlsx(sim_data_1000_low_L1[i], 
             paste0(out_dir_low, "/sim_data_1000_low_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_low_L2)) {
  write_xlsx(sim_data_1000_low_L2[i], 
             paste0(out_dir_low, "/sim_data_1000_low_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_low_L3)) {
  write_xlsx(sim_data_1000_low_L3[i], 
             paste0(out_dir_low, "/sim_data_1000_low_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_low_L4)) {
  write_xlsx(sim_data_1000_low_L4[i], 
             paste0(out_dir_low, "/sim_data_1000_low_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_low_L5)) {
  write_xlsx(sim_data_1000_low_L5[i], 
             paste0(out_dir_low, "/sim_data_1000_low_L5_", i, ".xlsx"))
}

# 保存中等相关数据集 (1000样本)
for(i in 1:length(sim_data_1000_medium_L1)) {
  write_xlsx(sim_data_1000_medium_L1[i], 
             paste0(out_dir_mid, "/sim_data_1000_medium_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_medium_L2)) {
  write_xlsx(sim_data_1000_medium_L2[i], 
             paste0(out_dir_mid, "/sim_data_1000_medium_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_medium_L3)) {
  write_xlsx(sim_data_1000_medium_L3[i], 
             paste0(out_dir_mid, "/sim_data_1000_medium_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_medium_L4)) {
  write_xlsx(sim_data_1000_medium_L4[i], 
             paste0(out_dir_mid, "/sim_data_1000_medium_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_medium_L5)) {
  write_xlsx(sim_data_1000_medium_L5[i], 
             paste0(out_dir_mid, "/sim_data_1000_medium_L5_", i, ".xlsx"))
}

# 保存高相关数据集 (1000样本)
for(i in 1:length(sim_data_1000_high_L1)) {
  write_xlsx(sim_data_1000_high_L1[i], 
             paste0(out_dir_high, "/sim_data_1000_high_L1_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_high_L2)) {
  write_xlsx(sim_data_1000_high_L2[i], 
             paste0(out_dir_high, "/sim_data_1000_high_L2_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_high_L3)) {
  write_xlsx(sim_data_1000_high_L3[i], 
             paste0(out_dir_high, "/sim_data_1000_high_L3_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_high_L4)) {
  write_xlsx(sim_data_1000_high_L4[i], 
             paste0(out_dir_high, "/sim_data_1000_high_L4_", i, ".xlsx"))
}

for(i in 1:length(sim_data_1000_high_L5)) {
  write_xlsx(sim_data_1000_high_L5[i], 
             paste0(out_dir_high, "/sim_data_1000_high_L5_", i, ".xlsx"))
}


