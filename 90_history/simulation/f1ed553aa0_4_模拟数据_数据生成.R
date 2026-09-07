library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(MASS)
library(writexl)
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
  t_mat <- matrix(runif(n_id * n_time), nrow = n_id, ncol = n_time)
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


###############数据参数list######

param_list_custom <- list(
  list(varname = "V1_0.55",
       mean_intercept = 0.55, mean_slope = 0.35,
       v_intercept = 0.165, v_slope = 0.105,
       noise_sd = 0.031),
  
  list(varname = "V2_0.80",
       mean_intercept = 0.80, mean_slope = 0.45,
       v_intercept = 0.24,  v_slope = 0.135,
       noise_sd = 0.012),
  
  list(varname = "V3_0.30",
       mean_intercept = 0.30, mean_slope = 0.65,
       v_intercept = 0.09,  v_slope = 0.195,
       noise_sd = 0.046),
  
  list(varname = "V4_0.90",
       mean_intercept = 0.90, mean_slope = 0.85,
       v_intercept = 0.27,  v_slope = 0.255,
       noise_sd = 0.020),
  
  list(varname = "V5_0.25",
       mean_intercept = 0.25, mean_slope = 0.40,
       v_intercept = 0.075, v_slope = 0.120,
       noise_sd = 0.005),
  
  list(varname = "V6_0.70",
       mean_intercept = 0.70, mean_slope = 0.55,
       v_intercept = 0.21,  v_slope = 0.165,
       noise_sd = 0.041),
  
  list(varname = "V7_0.40",
       mean_intercept = 0.40, mean_slope = 0.95,
       v_intercept = 0.12,  v_slope = 0.285,
       noise_sd = 0.018),
  
  list(varname = "V8_0.95",
       mean_intercept = 0.95, mean_slope = 0.35,
       v_intercept = 0.285, v_slope = 0.105,
       noise_sd = 0.049),
  
  list(varname = "V9_0.15",
       mean_intercept = 0.15, mean_slope = 0.30,
       v_intercept = 0.045, v_slope = 0.090,
       noise_sd = 0.027),
  
  list(varname = "V10_0.60",
       mean_intercept = 0.60, mean_slope = 0.50,
       v_intercept = 0.18,  v_slope = 0.150,
       noise_sd = 0.038)
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
#######500数据#######
#低相关性
for (i in 1:100) {
  assign(paste0("sim_data500_low", i), 
         sim_multi_class1(
           n = 500, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_low1,
           Sigma2 = cov_matrix_low2,
           Sigma3 = cov_matrix_low3,
           seed = 123 + i   # 从123开始，每次+1
         ))
}
#中相关性
for (i in 1:100) {
  assign(paste0("sim_data500_mid", i), 
         sim_multi_class1(
           n = 500, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_medium1,
           Sigma2 = cov_matrix_medium2,
           Sigma3 = cov_matrix_medium3,
           seed = 123 + i   # 从123开始，每次+1
         ))
}
#高相关性
for (i in 1:100) {
  assign(paste0("sim_data500_high", i), 
         sim_multi_class1(
           n = 500, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_high1,
           Sigma2 = cov_matrix_high2,
           Sigma3 = cov_matrix_high3,
           seed = 123 + i   # 从123开始，每次+1
         ))
}
######1000数据#######
for (i in 1:100) {
  assign(paste0("sim_data1000_low", i), 
         sim_multi_class1(
           n = 1000, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_low1,
           Sigma2 = cov_matrix_low2,
           Sigma3 = cov_matrix_low3,
           seed = 423 + i   # 从123开始，每次+1
         ))
}
#中相关性
for (i in 1:100) {
  assign(paste0("sim_data1000_mid", i), 
         sim_multi_class1(
           n = 1000, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_medium1,
           Sigma2 = cov_matrix_medium2,
           Sigma3 = cov_matrix_medium3,
           seed = 423 + i   # 从123开始，每次+1
         ))
}
#高相关性
for (i in 1:100) {
  assign(paste0("sim_data1000_high", i), 
         sim_multi_class1(
           n = 1000, 
           n_time = 5, 
           param_list = param_list_custom,
           Sigma1 = cov_matrix_high1,
           Sigma2 = cov_matrix_high2,
           Sigma3 = cov_matrix_high3,
           seed = 423 + i   # 从123开始，每次+1
         ))
}
cat("1")









################循环导出#########
for (i in 1:100) {
  df_name <- paste0("sim_data500_low", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_low, "/sim_data500_low", i, ".xlsx")
  write_xlsx(df, path = file_name)
}

for (i in 1:100) {
  df_name <- paste0("sim_data500_mid", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_mid, "/sim_data500_mid", i, ".xlsx")
  write_xlsx(df, path = file_name)
}

for (i in 1:100) {
  df_name <- paste0("sim_data500_high", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_high, "/sim_data500_high", i, ".xlsx")
  write_xlsx(df, path = file_name)
}
for (i in 1:100) {
  df_name <- paste0("sim_data1000_low", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_low, "/sim_data1000_low", i, ".xlsx")
  write_xlsx(df, path = file_name)
}

for (i in 1:100) {
  df_name <- paste0("sim_data1000_mid", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_mid, "/sim_data1000_mid", i, ".xlsx")
  write_xlsx(df, path = file_name)
}

for (i in 1:100) {
  df_name <- paste0("sim_data1000_high", i)
  df <- get(df_name)   # 取出对应的数据框
  
  file_name <- paste0(out_dir_high, "/sim_data1000_high", i, ".xlsx")
  write_xlsx(df, path = file_name)
}
cat("1")



########作图观察######

#500
ggplot(sim_data500_low[[75]], aes(x = t, y = V2_0.80, color = class)) +
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
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"

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







