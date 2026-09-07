library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
options(scipen = 999)

########list######
param_list_custom <- list(
  list(varname = "V1_0.55",
       mean_intercept = 16.5, mean_slope = 10.5,
       v_intercept = 4.95, v_slope = 3.15,
       noise_sd = 0.031),
  
  list(varname = "V2_0.80",
       mean_intercept = 24.0, mean_slope = 13.5,
       v_intercept = 7.20,  v_slope = 4.05,
       noise_sd = 0.012),
  
  list(varname = "V3_0.30",
       mean_intercept = 9.0, mean_slope = 19.5,
       v_intercept = 2.70,  v_slope = 5.85,
       noise_sd = 0.046),
  
  list(varname = "V4_0.90",
       mean_intercept = 27.0, mean_slope = 25.5,
       v_intercept = 8.10,  v_slope = 7.65,
       noise_sd = 0.020),
  
  list(varname = "V5_0.25",
       mean_intercept = 7.5, mean_slope = 12.0,
       v_intercept = 2.25, v_slope = 3.60,
       noise_sd = 0.005),
  
  list(varname = "V6_0.70",
       mean_intercept = 21.0, mean_slope = 16.5,
       v_intercept = 6.30,  v_slope = 4.95,
       noise_sd = 0.041),
  
  list(varname = "V7_0.40",
       mean_intercept = 12.0, mean_slope = 28.5,
       v_intercept = 3.60,  v_slope = 8.55,
       noise_sd = 0.018),
  
  list(varname = "V8_0.95",
       mean_intercept = 28.5, mean_slope = 10.5,
       v_intercept = 8.55, v_slope = 3.15,
       noise_sd = 0.049),
  
  list(varname = "V9_0.15",
       mean_intercept = 4.5, mean_slope = 9.0,
       v_intercept = 1.35, v_slope = 2.70,
       noise_sd = 0.027),
  
  list(varname = "V10_0.60",
       mean_intercept = 18.0, mean_slope = 15.0,
       v_intercept = 5.40,  v_slope = 4.50,
       noise_sd = 0.038)
)
###########################
sim_multi_class_optimized3 <- function(n = 200, n_time = 5, param_list,
                                       Sigma1 = diag(20), Sigma2 = diag(20),
                                       Sigma3 = diag(20), seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  ## 1. 基本框架 ----------------------------------------------------------
  cls_idx      <- rep(1:3, length.out = n)
  class_assign <- paste0("c", cls_idx)
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  df           <- data.frame(ID = ID, time = time, class = class)
  
  ## 2. 随机效应 ----------------------------------------------------------
  n1 <- sum(cls_idx == 1); n2 <- sum(cls_idx == 2); n3 <- sum(cls_idx == 3)
  re1 <- MASS::mvrnorm(n1, mu = rep(0, 20), Sigma = Sigma1)
  re2 <- MASS::mvrnorm(n2, mu = rep(0, 20), Sigma = Sigma2)
  re3 <- MASS::mvrnorm(n3, mu = rep(0, 20), Sigma = Sigma3)
  re_list <- list(re1, re2, re3)
  
  rand_eff <- do.call(rbind, lapply(1:n, function(i) {
    prev_count <- sum(cls_idx[1:i] == cls_idx[i])
    re_list[[cls_idx[i]]][prev_count, , drop = FALSE]
  }))
  colnames(rand_eff) <- c(
    paste0("int_", sapply(param_list, `[[`, "varname")),
    paste0("slp_", sapply(param_list, `[[`, "varname"))
  )
  
  rand_long        <- as.data.frame(rand_eff)
  rand_long$ID     <- 1:n
  rand_long$class  <- class_assign
  df               <- merge(df, rand_long, by = c("ID", "class"))
  
  ## 3. 固定效应系数矩阵（仅生成一次） ------------------------------------
  for (i in seq_along(param_list)) {
    par <- param_list[[i]]
    vn <- par$varname
    
    df$class <- as.character(df$class)  # 保证是字符型
    df[df$class == "c1", paste0("fix_int_", vn)] <- par$mean_intercept_c1
    df[df$class == "c2", paste0("fix_int_", vn)] <- par$mean_intercept_c2
    df[df$class == "c1", paste0("fix_slp_", vn)] <- par$mean_slope_c1
    df[df$class == "c2", paste0("fix_slp_", vn)] <- par$mean_slope_c2
  }
  
  ## 4. 合成最终值 ---------------------------------------------------------
  for (par in param_list) {
    vn <- par$varname
    int_rand_name <- paste0("int_", vn)
    slp_rand_name <- paste0("slp_", vn)
    fix_int_name  <- paste0("fix_int_", vn)
    fix_slp_name  <- paste0("fix_slp_", vn)
    
    noise_i <- rnorm(nrow(df), 0, par$noise_i_sd)
    
    # ---- 标准化固定效应（全局减均值除以标准差）----
    fix_int_std <- as.numeric(scale(df[[fix_int_name]]))
    fix_slp_std <- as.numeric(scale(df[[fix_slp_name]]))
    
    # ---- 合成 ----
    df[[vn]] <- (fix_int_std + df[[int_rand_name]]) +
      (fix_slp_std + df[[slp_rand_name]]) * df$time +
      noise_i
  }
  
  ## 5. 清理并返回 ---------------------------------------------------------
  df <- df %>% 
    dplyr::select(-dplyr::starts_with("int_"), 
                  -dplyr::starts_with("slp_"), 
                  -dplyr::starts_with("fix_")) %>% 
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))
  
  df  }


sim_data <- sim_multi_class_optimized3(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  seed = 123
)

head(sim_data)







#######作图######

class_mean <- sim_data %>%
  group_by(class, time) %>%
  summarise(glu_mean = mean(glu), .groups = "drop")

# 作图
ggplot() +
  # 个体轨迹
  geom_line(data = sim_data, 
            aes(x = time, y = glu, group = ID, color = class),
            alpha = 0.3) +
  # class 平均轨迹
  geom_line(data = class_mean, 
            aes(x = time, y = glu_mean, color = class, group = class),
            size = 1.5) +
  labs(x = "Time", y = "Glucose (glu)",
       title = "Trajectories of glu with class mean") +
  theme_minimal()

################################################################################


############
#1_随机效应赋值，矩阵顺序有误#
#2_初始list值赋值#
#3_noise在list中定义#
#4_程序还没封装#
############




cls_idx      <- rep(1:3, length.out = 200)
class_assign <- paste0("c", cls_idx)
ID           <- rep(1:200, each = 5)
time         <- rep(1:5, times = 200)
class        <- rep(class_assign, each = 5)
df           <- data.frame(ID = ID, time = time, class = class)



## 2. 随机效应 ----------------------------------------------------------
n1 <- sum(cls_idx == 1); n2 <- sum(cls_idx == 2); n3 <- sum(cls_idx == 3)
re1 <- MASS::mvrnorm(n1, mu = rep(0, 20), Sigma = cov_matrix_low1)
re2 <- MASS::mvrnorm(n2, mu = rep(0, 20), Sigma = cov_matrix_low2)
re3 <- MASS::mvrnorm(n3, mu = rep(0, 20), Sigma = cov_matrix_low3)
re_list <- list(re1, re2, re3)

rand_eff <- do.call(rbind, lapply(1:200, function(i) {
  prev_count <- sum(cls_idx[1:i] == cls_idx[i])
  re_list[[cls_idx[i]]][prev_count, , drop = FALSE]
}))
var_names <- sapply(param_list_custom, `[[`, "varname")

# 交替命名
colnames(rand_eff) <- c(rbind(
  paste0("int_", var_names),
  paste0("slp_", var_names)
))

rand_long        <- as.data.frame(rand_eff)
rand_long$ID     <- 1:200
rand_long$class  <- class_assign

num_cols <- setdiff(names(rand_long), c("ID", "class"))  # 只处理数值列

## 3. 固定效应展开 ----------------------------------------------------------

int_mu <- sapply(param_list_custom, `[[`, "mean_intercept")
slp_mu <- sapply(param_list_custom, `[[`, "mean_slope")
fixed_mu <- as.vector(rbind(int_mu, slp_mu))
int_v<-sapply(param_list_custom, `[[`, "v_intercept")
slp_v<-sapply(param_list_custom, `[[`, "v_slope")
fixed_v <- as.vector(rbind(int_v, slp_v))

mu_mat <- rbind(
  fixed_mu - fixed_v,  # class 1
  fixed_mu,            # class 2
  fixed_mu + fixed_v   # class 3
)

fixed_eff <- mu_mat[cls_idx, ]
colnames(fixed_eff) <- colnames(rand_eff)

fixed_long        <- as.data.frame(fixed_eff)
fixed_long$ID     <- 1:200
fixed_long$class  <- class_assign


## 4. 合成最终值 ------------------------------------
## 0. 参数 ---------------------------------------------------------------
n_time <- 5
n_id   <- nrow(rand_long)
vars   <- sapply(param_list_custom, `[[`, "varname")
noise_sd <- 0.02
set.seed(123)                                      # 可重复

## 1. 随机时间矩阵（n_id×5）----------------------------------------------
t_mat <- matrix(runif(n_id * n_time), nrow = n_id, ncol = n_time)
t_mat <- t(apply(t_mat, 1, sort))

## 2. 长格式辅助向量 -----------------------------------------------------
ID_vec      <- rep(rand_long$ID, each = n_time)
class_vec   <- rep(rand_long$class, each = n_time)
time_vec_seq <- rep(1:n_time, times = n_id)        # 测量序号 1-5
t_vec_long  <- c(t(t_mat))                         # 拉成 1000×1 的随机 t

## 3. 预分配 1000×10 结果矩阵 -------------------------------------------
Y_mat <- matrix(NA, nrow = n_id * n_time, ncol = length(vars))
colnames(Y_mat) <- vars

## 4. 按变量循环生成 Y ---------------------------------------------------
for (i in seq_along(vars)) {
  vn <- vars[i]
  rand_int <- rand_long[[paste0("int_", vn)]]
  rand_slp <- rand_long[[paste0("slp_", vn)]]
  fix_int  <- fixed_long[[paste0("int_", vn)]]
  fix_slp  <- fixed_long[[paste0("slp_", vn)]]
  
  sys <- fix_int + rand_int + (fix_slp + rand_slp) * t_mat
  sys_long <- as.vector(t(sys))   # <-- 关键：按 ID 展开矩阵
  Y_mat[, i] <- sys_long + rnorm(1000, 0, 0.02)
}

## 5. 打包 1000×13 数据框 ------------------------------------------------
Y_df <- data.frame(Y_mat, ID = ID_vec, class = class_vec, t = t_vec_long)

head(Y_df)
########################################################











###############################



#########

head(rand_long)



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
    Y_mat[, i] <- c(sys) + rnorm(n_id * n_time, 0, noise_sd[i])
    
    }
  
  # 4.5 打包返回
  Y_df <- data.frame(Y_mat,
                     ID = ID_vec,
                     class = class_vec,
                     time = time_vec_seq,
                     t = t_vec_long)
  return(Y_df)
}



sim_data <- sim_multi_class1(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  seed = 123
)








