
library(readxl)
library(ggplot2)
library(dplyr)
library(MASS)
options(scipen = 999)

###############list######
param_list_custom <- list(
  list(varname = "V1_0.55",
       mean_intercept = 0.55, mean_slope = 0.10,
       v_intercept = 0.165, v_slope = 0.03,
       noise_sd = 0.031),
  
  list(varname = "V2_0.80",
       mean_intercept = 0.80, mean_slope = 0.20,
       v_intercept = 0.24, v_slope = 0.06,
       noise_sd = 0.012),
  
  list(varname = "V3_0.30",
       mean_intercept = 0.30, mean_slope = 0.40,
       v_intercept = 0.09, v_slope = 0.12,
       noise_sd = 0.046),
  
  list(varname = "V4_0.90",
       mean_intercept = 0.90, mean_slope = 0.60,
       v_intercept = 0.27, v_slope = 0.18,
       noise_sd = 0.020),
  
  list(varname = "V5_0.25",
       mean_intercept = 0.25, mean_slope = 0.15,
       v_intercept = 0.075, v_slope = 0.045,
       noise_sd = 0.005),
  
  list(varname = "V6_0.70",
       mean_intercept = 0.70, mean_slope = 0.30,
       v_intercept = 0.21, v_slope = 0.09,
       noise_sd = 0.041),
  
  list(varname = "V7_0.40",
       mean_intercept = 0.40, mean_slope = 0.80,
       v_intercept = 0.12, v_slope = 0.24,
       noise_sd = 0.018),
  
  list(varname = "V8_0.95",
       mean_intercept = 0.95, mean_slope = 0.10,
       v_intercept = 0.285, v_slope = 0.03,
       noise_sd = 0.049),
  
  list(varname = "V9_0.15",
       mean_intercept = 0.15, mean_slope = 0.05,
       v_intercept = 0.045, v_slope = 0.015,
       noise_sd = 0.027),
  
  list(varname = "V10_0.60",
       mean_intercept = 0.60, mean_slope = 0.25,
       v_intercept = 0.18, v_slope = 0.075,
       noise_sd = 0.038)
)











###############function############
sim_multi_class_optimized2 <- function(n = 200, n_time = 5, param_list, 
                                       Sigma1 = diag(20), Sigma2 = diag(20), Sigma3 = diag(20), 
                                       seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 基本框架
  cls_idx <- rep(1:3, length.out = n)
  class_assign <- paste0("c", cls_idx)
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  df <- data.frame(ID = ID, time = time, class = class)
  
  # 使用三个不同的协方差矩阵生成随机效应
  n1 <- sum(cls_idx == 1)
  n2 <- sum(cls_idx == 2)
  n3 <- sum(cls_idx == 3)
  
  rand_eff1 <- MASS::mvrnorm(n1, mu = rep(0, 20), Sigma = Sigma1)
  rand_eff2 <- MASS::mvrnorm(n2, mu = rep(0, 20), Sigma = Sigma2)
  rand_eff3 <- MASS::mvrnorm(n3, mu = rep(0, 20), Sigma = Sigma3)
  
  # 创建随机效应列表
  re_list <- list(rand_eff1, rand_eff2, rand_eff3)
  
  # 按照类别顺序构建随机效应矩阵
  rand_eff <- do.call(rbind, lapply(1:n, function(i) {
    class_idx <- cls_idx[i]
    # 找到当前类别在当前ID之前的出现次数
    prev_count <- sum(cls_idx[1:i] == class_idx)
    re_list[[class_idx]][prev_count, , drop = FALSE]
  }))
  
  # 设置列名
  colnames(rand_eff) <- c(
    paste0("int_", sapply(param_list, `[[`, "varname")),
    paste0("slp_", sapply(param_list, `[[`, "varname"))
  )
  
  # 创建随机效应数据框
  rand_long <- as.data.frame(rand_eff)
  rand_long$ID <- 1:n
  rand_long$class <- class_assign
  
  # 合并随机效应到主数据框
  df <- merge(df, rand_long, by = c("ID", "class"))
  
  # 预计算固定效应矩阵（避免在循环中重复计算）
  fix_effects <- matrix(0, nrow = n * n_time, ncol = 2 * length(param_list))
  colnames(fix_effects) <- c(
    paste0("fix_int_", sapply(param_list, `[[`, "varname")),
    paste0("fix_slp_", sapply(param_list, `[[`, "varname"))
  )
  
  for (i in seq_along(param_list)) {
    par <- param_list[[i]]
    vn <- par$varname
    
    # 计算固定效应
    fix_effects[, paste0("fix_int_", vn)] <- with(df, 
                                                  ifelse(class == "c1", par$mean_intercept + par$v_intercept,
                                                         ifelse(class == "c2", par$mean_intercept + par$v_intercept,
                                                                par$mean_intercept - par$v_intercept))
    )
    
    fix_effects[, paste0("fix_slp_", vn)] <- with(df,
                                                  ifelse(class == "c1", par$mean_slope + par$v_slope,
                                                         ifelse(class == "c2", par$mean_slope - par$v_slope,
                                                                par$mean_slope - par$v_slope))
    )
  }
  
  # 将固定效应添加到数据框
  df <- cbind(df, fix_effects)
  
  # 计算最终值
  for (par in param_list) {
    vn <- par$varname
    int_rand_name <- paste0("int_", vn)
    slp_rand_name <- paste0("slp_", vn)
    fix_int_name <- paste0("fix_int_", vn)
    fix_slp_name <- paste0("fix_slp_", vn)
    
    # 测量误差
    noise_i <- rnorm(nrow(df), 0, par$noise_i_sd)
    
    # 合成最终值
    df[[vn]] <- (df[[fix_int_name]] + df[[int_rand_name]]) +
      (df[[fix_slp_name]] + df[[slp_rand_name]]) * df$time +
      noise_i
  }
  
  # 清理临时列并返回结果
  df %>% 
    dplyr::select(-dplyr::starts_with("int_"), 
                  -dplyr::starts_with("slp_"),
                  -dplyr::starts_with("fix_")) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
}




#录入矩阵
cov_matrix_low1<- read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low1.xlsx")
cov_matrix_low2<- read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low2.xlsx")
cov_matrix_low3<- read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low3.xlsx")


sim_data <- sim_multi_class_optimized2(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom,
  Sigma1 = cov_matrix_low1,
  Sigma2 = cov_matrix_low2,
  Sigma3 = cov_matrix_low3,
  seed = 123
)


#############作图####
class_mean <- raw_multi_class4 %>%
  group_by(class, time) %>%
  summarise(glu_mean = mean(glu), .groups = "drop")

# 作图
ggplot() +
  # 个体轨迹
  geom_line(data = raw_multi_class4, 
            aes(x = time, y = glu, group = ID, color = class),
            alpha = 0.3) +
  # class 平均轨迹
  geom_line(data = class_mean, 
            aes(x = time, y = glu_mean, color = class, group = class),
            size = 1.5) +
  labs(x = "Time", y = "Glucose (glu)",
       title = "Trajectories of glu with class mean") +
  theme_minimal()




class_mean <- raw_multi_class %>%
  group_by(class, time) %>%
  summarise(hb_mean = mean(hb), .groups = "drop")

# 作图
ggplot() +
  # 个体轨迹
  geom_line(data = raw_multi_class, 
            aes(x = time, y = hb, group = ID, color = class),
            alpha = 0.3) +
  # class 平均轨迹
  geom_line(data = class_mean, 
            aes(x = time, y = hb_mean, color = class, group = class),
            size = 1.5) +
  labs(x = "Time", y = "Hemoglobin (hb)",
       title = "Trajectories of Hemoglobin (hb) with class mean") +
  theme_minimal()














###################生成随机效应程序##############


sim_rand <- function(n, param_list, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  p <- length(param_list)  # 变量个数
  ri_names <- paste0("ri_", sapply(param_list, function(x) x$varname))
  rs_names <- paste0("rs_", sapply(param_list, function(x) x$varname))
  
  # 构建均值向量：所有随机效应的均值为0
  mu <- rep(0, 2 * p)
  
  # 构建协方差矩阵（可根据需要自定义）
  Sigma <- diag(2 * p) * 0.01  # 初始化为对角阵，可根据需要修改
  
  # 使用 mvrnorm 生成随机效应
  rand_effects <- MASS::mvrnorm(n, mu, Sigma)
  colnames(rand_effects) <- c(ri_names, rs_names)
  
  return(as.data.frame(rand_effects))
}

rand_effects <- sim_rand(n = 100, param_list = param_list_custom, seed = 123)


diag(2 * 5)


###################尝试1############################


sim_multi_class1 <- function(n = 200,
                            n_time = 5,
                            param_list,
                            Sigma = diag(20),   # ← 你自己想改的协方差矩阵
                            seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  ## ---------- 1. 基本框架（你原来就有） ----------
  class_assign <- rep(1:3, length.out = n)
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  df           <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  ## ---------- 2. 一次性抽随机效应 ----------
  # 每个个体一行，20 列：前 10 列是 intercept_rand，后 10 列是 slope_rand
  rand_eff <- mvrnorm(n, mu = rep(0, 20), Sigma = Sigma)
  colnames(rand_eff) <- c(paste0("int_", sapply(param_list, `[[`, "varname")),
                          paste0("slp_", sapply(param_list, `[[`, "varname")))
  
  ## ---------- 3. 把随机效应展开到长格式 ----------
  rand_long <- data.frame(ID = 1:n, rand_eff)
  df <- merge(df, rand_long, by = "ID")   # 现在 df 里多了 int_glu … slp_bmi 等列
  
  ## ---------- 4. 按变量循环计算观测值 ----------
  for (par in param_list) {
    vn <- par$varname
    int_rand_name <- paste0("int_", vn)
    slp_rand_name <- paste0("slp_", vn)
    
    ## 固定部分（你原来就有）
    fix_int  <- with(df, ifelse(class == "c1", par$mean_intercept + par$v_intercept,
                                ifelse(class == "c2", par$mean_intercept + par$v_intercept,
                                       par$mean_intercept - par$v_intercept)))
    fix_slp  <- with(df, ifelse(class == "c1", par$mean_slope + par$v_slope,
                                ifelse(class == "c2", par$mean_slope - par$v_slope,
                                       par$mean_slope - par$v_slope)))
    
    ## 随机部分
    rand_int <- df[[int_rand_name]]
    rand_slp <- df[[slp_rand_name]]
    
    ## 测量误差
    noise_i  <- rnorm(nrow(df), 0, par$noise_i_sd)
    
    ## 合成最终值
    df[[vn]] <- (fix_int + rand_int) +
      (fix_slp + rand_slp) * df$time +
      noise_i
  }
  
  ## ---------- 5. 返回干净数据 ----------
  df %>% dplyr::select(-starts_with("int_"), -starts_with("slp_")) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
}

## 运行示例 -------------------------------------------------
raw_multi_class2 <- sim_multi_class1(
  n          = 200,
  n_time     = 5,
  param_list = param_list_custom,
  Sigma      = diag(20)   # 这里你可以换成任何 20×20 矩阵
)


##############################



sim_multi_class3 <- function(n = 200, n_time = 5, param_list, cov_matrix = NULL, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 如果没有提供协方差矩阵，使用默认的20x20单位矩阵
  n_vars <- length(param_list)
  if (is.null(cov_matrix)) {
    cov_matrix <- diag(2 * n_vars)  # 每个变量有截距和斜率两个随机效应
  }
  
  # 生成多元随机效应
  library(MASS)
  random_effects <- mvrnorm(n, mu = rep(0, 2 * n_vars), Sigma = cov_matrix)
  
  # 个体分配到3组轨迹
  class_assign <- rep(1:3, length.out = n)
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  
  df <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  for (j in seq_along(param_list)) {
    par <- param_list[[j]]
    values <- numeric(n * n_time)
    
    # 提取当前变量的随机效应
    # 假设随机效应矩阵的前n_vars列是截距，后n_vars列是斜率
    rand_intercepts <- random_effects[, j]  # 第j个变量的随机截距
    rand_slopes <- random_effects[, n_vars + j]  # 第j个变量的随机斜率
    
    # 为每个类别生成数据
    for (k in 1:3) {
      class_ids <- which(class_assign == k)
      idx <- which(df$class == paste0("c", k))
      n_class <- length(class_ids)
      
      # 定义3组轨迹的固定效应
      if (k == 1) {
        mu_int <- par$mean_intercept + par$v_intercept
        mu_slope <- par$mean_slope + par$v_slope
      } else if (k == 2) {
        mu_int <- par$mean_intercept + par$v_intercept
        mu_slope <- par$mean_slope - par$v_slope
      } else {
        mu_int <- par$mean_intercept - par$v_intercept
        mu_slope <- par$mean_slope - par$v_slope
      }
      
      # 扩展随机效应到时间序列格式
      rand_int_long <- rep(rand_intercepts[class_ids], each = n_time)
      rand_slope_long <- rep(rand_slopes[class_ids], each = n_time)
      
      # 固定效应部分
      intercept_long <- rep(mu_int, n_class * n_time)
      slope_long <- rep(mu_slope, n_class * n_time)
      
      # 个体噪声
      noise_i <- rnorm(n_class * n_time, mean = 0, sd = par$noise_i_sd)
      
      # 时间值
      time_values <- rep(time[1:n_time], times = n_class)
      
      # 计算当前类别的值（加入随机效应）
      values[idx] <- (intercept_long + rand_int_long) + 
        (slope_long + rand_slope_long) * time_values + 
        noise_i
    }
    
    df[[par$varname]] <- values
  }
  
  return(df)
}


raw_multi_class3 <- sim_multi_class3(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom
) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))


##########################







sim_multi_class_optimized1 <- function(n = 200,
                                      n_time = 5,
                                      param_list,
                                      Sigma = diag(20),
                                      seed = 123) {
  
  ## 0. 环境准备
  if (!is.null(seed)) set.seed(seed)
  
  ## 1. 骨架表（与原版完全相同）
  class_assign <- rep(1:3, length.out = n)
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  df           <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  ## 2. 把 Sigma 升级成“每类一个”的列表（向后兼容）
  if (!is.list(Sigma)) {
    Sigma <- list(c1 = Sigma, c2 = Sigma, c3 = Sigma)
  } else {
    # 如果用户传了 list，但只给 1~2 个，也自动补齐
    if (is.null(Sigma$c1)) Sigma$c1 <- Sigma[[1]]
    if (is.null(Sigma$c2)) Sigma$c2 <- Sigma[[1]]
    if (is.null(Sigma$c3)) Sigma$c3 <- Sigma[[1]]
  }
  
  ## 3. 按类分别采样随机效应，再按 ID 顺序拼回去
  rand_eff <- do.call(rbind,
                      lapply(1:3, function(k) {
                        idx <- which(class_assign == k)
                        MASS::mvrnorm(n = length(idx),
                                      mu  = rep(0, 20),
                                      Sigma = Sigma[[paste0("c", k)]])
                      }))
  rownames(rand_eff) <- NULL
  colnames(rand_eff) <- c(
    paste0("int_",  sapply(param_list, `[[`, "varname")),
    paste0("slp_",  sapply(param_list, `[[`, "varname"))
  )
  
  ## 4. 把随机效应展开到长格式（与原版相同）
  rand_long <- as.data.frame(rand_eff)
  rand_long$ID <- 1:n
  df <- merge(df, rand_long, by = "ID")
  
  ## 5. 预计算固定效应矩阵（与原版相同）
  fix_effects <- matrix(0,
                        nrow  = n * n_time,
                        ncol = 2 * length(param_list))
  colnames(fix_effects) <- c(
    paste0("fix_int_", sapply(param_list, `[[`, "varname")),
    paste0("fix_slp_", sapply(param_list, `[[`, "varname"))
  )
  
  for (i in seq_along(param_list)) {
    par <- param_list[[i]]
    vn  <- par$varname
    
    fix_effects[, paste0("fix_int_", vn)] <-
      with(df, ifelse(class == "c1", par$mean_intercept + par$v_intercept,
                      ifelse(class == "c2", par$mean_intercept + par$v_intercept,
                             par$mean_intercept - par$v_intercept)))
    
    fix_effects[, paste0("fix_slp_", vn)] <-
      with(df, ifelse(class == "c1", par$mean_slope + par$v_slope,
                      ifelse(class == "c2", par$mean_slope - par$v_slope,
                             par$mean_slope - par$v_slope)))
  }
  
  df <- cbind(df, fix_effects)
  
  ## 6. 合成最终观测值（与原版相同）
  for (par in param_list) {
    vn <- par$varname
    int_rand_name <- paste0("int_", vn)
    slp_rand_name <- paste0("slp_", vn)
    fix_int_name  <- paste0("fix_int_", vn)
    fix_slp_name  <- paste0("fix_slp_", vn)
    
    noise_i <- rnorm(nrow(df), 0, par$noise_i_sd)
    
    df[[vn]] <- (df[[fix_int_name]] + df[[int_rand_name]]) +
      (df[[fix_slp_name]] + df[[slp_rand_name]]) * df$time +
      noise_i
  }
  
  ## 7. 清理并返回
  df %>%
    dplyr::select(-dplyr::starts_with("int_"),
                  -dplyr::starts_with("slp_"),
                  -dplyr::starts_with("fix_")) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
}

Sigma_list <- list(
  c1 = read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low1.xlsx",
                  range = "B1:U20") |> as.matrix(),
  c2 = read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low2.xlsx",
                  range = "B1:U20") |> as.matrix(),
  c3 = read_excel("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low3.xlsx",
                  range = "B1:U20") |> as.matrix()
)







# 读取 → 强制对称 → 去名 → 成矩阵
safeSigma <- function(file, range){
  mat <- read_excel(file, range = range) |> as.matrix()
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]   # 下三角 = 上三角
  dimnames(mat) <- NULL                           # 去掉 row/col names
  as.matrix(nearPD(mat)$mat)                      # 确保正定
}

Sigma_list <- list(
  c1 = safeSigma("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low1.xlsx", "B1:U20"),
  c2 = safeSigma("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low2.xlsx", "B1:U20"),
  c3 = safeSigma("F:/文章/大论文/程序/模拟数据/相关系数矩阵/cov_matrix_low3.xlsx", "B1:U20")
)

# 自检
lapply(Sigma_list, function(s) c(dim = dim(s), sym = isSymmetric(s)))


raw_data <- sim_multi_class_optimized1(n = 200, n_time = 5,
                                 param_list = param_list,
                                 Sigma = Sigma_list)   # 同类同质

















