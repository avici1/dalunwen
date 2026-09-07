




##############function#########
sim_multi_class <- function(n = 200, n_time = 5, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 个体分配到 4 组轨迹
  class_assign <- rep(1:3, length.out = n)  # 均分到4组
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  
  df <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  for (par in param_list) {
    values <- numeric(n * n_time)
    
    # 为每个类别生成数据
    for (k in 1:3) {
      # 找到属于当前类别的所有个体ID
      class_ids <- which(class_assign == k)
      # 找到这些个体在长格式数据中的索引
      idx <- which(df$class == paste0("c", k))
      
      # 定义3组轨迹的截距和斜率（固定效应）
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
      
      # 个体数
      n_class <- length(class_ids)
      
      # 固定效应：同一类所有个体使用相同的截距和斜率
      intercept_long <- rep(mu_int, each = n_time * n_class)
      slope_long <- rep(mu_slope, each = n_time * n_class)
      
      # 个体噪声
      noise_i <- rnorm(n_class * n_time, mean = 0, sd = par$noise_i_sd)
      
      # 时间序列
      time_values <- rep(time[1:n_time], times = n_class)
      
      # 计算当前类别的值
      values[idx] <- intercept_long + slope_long * time_values + noise_i
    }
    
    df[[par$varname]] <- values
  }
  
  return(df)
}



# 运行示例
raw_multi_class <- sim_multi_class(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom
)%>%mutate(across(where(is.numeric), ~ round(.x, 2)))


ggplot(raw_multi_class, aes(x = time, y = glu, group = ID, color = class)) +
  geom_line(alpha = 0.5) +
  labs(x = "Time", y = "Glucose (glu)",
       title = "Trajectories of glu by class") +
  theme_minimal()


sim_multi_class_optimized <- function(n = 200, n_time = 5, param_list, Sigma = diag(20), seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 基本框架
  class_assign <- rep(1:3, length.out = n)
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  df <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  # 一次性生成随机效应
  rand_eff <- MASS::mvrnorm(n, mu = rep(0, 20), Sigma = Sigma)
  colnames(rand_eff) <- c(
    paste0("int_", sapply(param_list, `[[`, "varname")),
    paste0("slp_", sapply(param_list, `[[`, "varname"))
  )
  
  # 将随机效应展开到长格式
  rand_long <- as.data.frame(rand_eff)
  rand_long$ID <- 1:n
  df <- merge(df, rand_long, by = "ID")
  
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


################

sim_multi_class_optimized <- function(n = 200, n_time = 5, param_list, Sigma = diag(20), seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 基本框架
  class_assign <- rep(1:3, length.out = n)
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  df <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  # 一次性生成随机效应
  rand_eff <- MASS::mvrnorm(n, mu = rep(0, 20), Sigma = Sigma)
  colnames(rand_eff) <- c(
    paste0("int_", sapply(param_list, `[[`, "varname")),
    paste0("slp_", sapply(param_list, `[[`, "varname"))
  )
  
  # 将随机效应展开到长格式
  rand_long <- as.data.frame(rand_eff)
  rand_long$ID <- 1:n
  df <- merge(df, rand_long, by = "ID")
  
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



#######################
sim_multi_class <- function(n = 200, n_time = 5, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  
  # 个体分配到 4 组轨迹
  class_assign <- rep(1:4, length.out = n)  # 均分到4组
  ID <- rep(1:n, each = n_time)
  time <- rep(1:n_time, times = n)
  class <- rep(class_assign, each = n_time)
  
  df <- data.frame(ID = ID, time = time, class = paste0("c", class))
  
  for (par in param_list) {
    values <- numeric(n * n_time)
    
    # 为每个类别生成数据
    for (k in 1:4) {
      # 找到属于当前类别的所有个体ID
      class_ids <- which(class_assign == k)
      # 找到这些个体在长格式数据中的索引
      idx <- which(df$class == paste0("c", k))
      
      # 定义4组轨迹的截距和斜率
      if (k == 1) {
        mu_int <- par$mean_intercept + par$v_intercept
        mu_slope <- par$mean_slope + par$v_slope
      } else if (k == 2) {
        mu_int <- par$mean_intercept + par$v_intercept
        mu_slope <- par$mean_slope - par$v_slope
      } else if (k == 3) {
        mu_int <- par$mean_intercept - par$v_intercept
        mu_slope <- par$mean_slope + par$v_slope
      } else {
        mu_int <- par$mean_intercept - par$v_intercept
        mu_slope <- par$mean_slope - par$v_slope
      }
      
      # 只为当前类别的个体生成随机效应
      n_class <- length(class_ids)
      intercepts <- rnorm(n_class, mean = mu_int, sd = 1)
      slopes <- rnorm(n_class, mean = mu_slope, sd = 0.01)
      
      # 为每个时间点重复截距和斜率
      intercept_long <- rep(intercepts, each = n_time)
      slope_long <- rep(slopes, each = n_time)
      
      # 噪声 - 只为当前类别的观测生成
      noise_s <- rnorm(n_class * n_time, mean = 0, sd = par$noise_s_sd)
      noise_i <- rnorm(n_class * n_time, mean = 0, sd = par$noise_i_sd)
      
      # 计算当前类别的值
      time_values <- rep(time[1:n_time], times = n_class)  # 正确的时间序列
      values[idx] <- intercept_long + (slope_long + noise_s) * time_values + noise_i
    }
    
    df[[par$varname]] <- values
  }
  
  return(df)
}

# 运行示例
raw_multi_class <- sim_multi_class(
  n = 200, 
  n_time = 5, 
  param_list = param_list_custom
)%>%mutate(across(where(is.numeric), ~ round(.x, 2)))

head(raw_multi_class)


##############计算AUC BS C#############

beta1 <- fit_simdata_all$coefficients$beta
gamma1 <- fit_simdata_all$coefficients$gamma
lp_long <- model.matrix(~ t + V1_0.55 + V2_0.80 + V3_0.30 + V4_0.90 + V5_0.25 + V6_0.70, data = simdata3_clean) %*% beta
S_pred <- S0_t0 ^ exp(gamma * lp_long)
t0 <- 1
# 计算删失分布的Kaplan-Meier估计
censoring_model <- survfit(Surv(obs_time, 1-event) ~ 1, data = simdata3_clean)

# 计算逆概率权重
get_weights <- function(time, event, censoring_model, t0) {
  # 找到每个时间点对应的删失生存概率
  cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
  weights <- ifelse(time <= t0 & event == 1, 1/cens_probs, 
                    ifelse(time > t0, 1/summary(censoring_model, times = t0)$surv, 0))
  return(weights)
}

weights <- get_weights(simdata3_clean$obs_time, simdata3_clean$event, censoring_model, t0)

# 计算加权的Brier Score
S0_t0 <- summary(survfit(coxph(Surv(obs_time, event) ~ 1, data = simdata3_clean)), 
                 times = t0)$surv
S_pred <- S0_t0 ^ exp(0.1385 * lp_long)

Y_obs <- as.numeric(simdata3_clean$obs_time > t0 | 
                      (simdata3_clean$obs_time <= t0 & simdata3_clean$event == 0))

# 加权Brier Score
BS_t0_weighted <- mean(weights * (S_pred - Y_obs)^2, na.rm = TRUE)

