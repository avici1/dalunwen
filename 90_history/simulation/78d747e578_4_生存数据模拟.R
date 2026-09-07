library(dplyr)
library(flexsurv)
library(tidyr) 
library(ggplot2)
library(tidyverse)
library(survival)
library(purrr)
library(writexl)
library(readxl)
library(openxlsx)  # 保存为Excel
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

########展示各种生存分布#############

t  <- seq(0.01, 10, length = 200)
df <- data.frame(
  t = t,
  Exp   = dexp(t, 0.2),
  Weib  = dweibull(t, shape = 0.8, scale = 5),
  LN    = dlnorm(t, meanlog = 1.5, sdlog = 0.6),
  Gomp  = dgompertz(t, shape = 0.02, rate = 0.3),
  PG    = dgengamma(t, mu = 1.2, sigma = 0.7, Q = 0.5)
) %>% pivot_longer(-t, names_to = "dist", values_to = "h")
ggplot(df, aes(t, h, colour = dist)) + geom_line() +
  labs(y = "hazard") + theme_minimal()


#######引入数据######################
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据_低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据_中相关"
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据_高相关"


sim_data500_low <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_low, "/sim_data500_low", i, ".xlsx")
  read_xlsx(file_name)
})

sim_data500_mid <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_mid, "/sim_data500_mid", i, ".xlsx")
  read_xlsx(file_name)
})

sim_data500_high <- lapply(1:100, function(i) {
  file_name <- paste0(out_dir_high, "/sim_data500_high", i, ".xlsx")
  read_xlsx(file_name)
})

##########生存分布参数##################


# 设置Weibull分布参数和变量系数
weibull_params <- list(
  c1 = list(shape = 1, scale = 2),
  c2 = list(shape = 1.2, scale = 1.5),
  c3 = list(shape = 1.4, scale = 1)
)
# 设置变量系数 
#高影响变量 间 相关性高
beta_effects <- list(
  c1 = c(
    V1_0.55 = 0.6, V2_0.80 = -0.7, V3_0.30 = 0.8, 
    V4_0.90 = -0.9, V5_0.25 = 0.65, V6_0.70 = -0.75,
    V7_0.40 = 0.05, V8_0.95 = -0.03, V9_0.15 = 0.08, V10_0.60 = -0.04
  ),
  c2 = c(
    V1_0.55 = -0.8, V2_0.80 = 0.9, V3_0.30 = -0.7, 
    V4_0.90 = 0.6, V5_0.25 = -0.95, V6_0.70 = 0.85,
    V7_0.40 = 0.02, V8_0.95 = -0.07, V9_0.15 = 0.01, V10_0.60 = 0.05
  ),
  c3 = c(
    V1_0.55 = 0.7, V2_0.80 = -0.65, V3_0.30 = 0.9, 
    V4_0.90 = -0.8, V5_0.25 = 0.75, V6_0.70 = -0.85,
    V7_0.40 = -0.05, V8_0.95 = 0.03, V9_0.15 = -0.02, V10_0.60 = 0.06
  )
)

#变量与噪声相关性高
beta_effects_noise <- list(
  c1 = c(
    V1_0.55 = 0.6, V2_0.80 = -0.7, V3_0.30 = 0.8, 
    V4_0.90 = -0.9, V5_0.25 = 0.65, V6_0.70 = -0.07,
    V7_0.40 = 0.05, V8_0.95 = -0.03, V9_0.15 = 0.08, V10_0.60 = -0.04
  ),
  c2 = c(
    V1_0.55 = -0.8, V2_0.80 = 0.9, V3_0.30 = -0.7, 
    V4_0.90 = 0.6, V5_0.25 = -0.95, V6_0.70 = 0.05,
    V7_0.40 = 0.02, V8_0.95 = -0.07, V9_0.15 = 0.01, V10_0.60 = 0.05
  ),
  c3 = c(
    V1_0.55 = 0.7, V2_0.80 = -0.65, V3_0.30 = 0.9, 
    V4_0.90 = -0.8, V5_0.25 = 0.75, V6_0.70 = -0.05,
    V7_0.40 = -0.05, V8_0.95 = 0.03, V9_0.15 = -0.02, V10_0.60 = 0.06
  )
)


################程序################

add_surv <- function(data,
                     weibull_params,
                     beta_effects,
                     var_pattern = "^V\\d+_",
                     id_col = "ID",
                     class_col = "class",
                     seed = 123,
                     censor_dist = c("exponential", "uniform"),
                     censor_params = list(rate = 0.05, max = NULL),
                     target_censor = NULL,   # e.g. 0.5 or 0.9, NULL 不校准
                     tol = 1e-4,
                     max_iter = 50,
                     attach_longitudinal = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  censor_dist <- match.arg(censor_dist)
  
  # 1. 基线数据（每个 ID 的第一条）
  baseline <- data %>%
    group_by(.data[[id_col]]) %>%
    slice(1) %>%
    ungroup()
  
  # 2. 变量名自动识别（除非用户另行指定）
  var_names <- grep(var_pattern, names(baseline), value = TRUE)
  if (length(var_names) == 0) stop("无法根据 var_pattern 找到任何自变量列。")
  
  # 3. 检查类别与参数
  classes_in_data <- unique(as.character(baseline[[class_col]]))
  if (!all(classes_in_data %in% names(weibull_params))) {
    stop("weibull_params 必须包含数据中所有的 class 名称（例如 c1,c2,c3）。")
  }
  if (!all(classes_in_data %in% names(beta_effects))) {
    stop("beta_effects 必须包含数据中所有的 class 名称（例如 c1,c2,c3）。")
  }
  
  # 4. 计算 lp（对每个 class 向量化）
  baseline[[class_col]] <- as.character(baseline[[class_col]])
  n <- nrow(baseline)
  lp_vec <- numeric(n)
  
  for (cls in classes_in_data) {
    idx <- which(baseline[[class_col]] == cls)
    coefs <- beta_effects[[cls]]
    X <- as.matrix(baseline[idx, names(coefs), drop = FALSE]) # 保证顺序一致
    lp_vals <- as.numeric(X %*% coefs)
    lp_vec[idx] <- lp_vals
  }
  
  
  baseline$lp <- lp_vec
  
  # 5. 生成真实生存时间（使用常见的 scale * (...)^(1/shape) 参数化）
  u <- runif(n)
  # 构造 shape_vec / scale_vec
  shape_vec <- sapply(baseline[[class_col]],
                      function(cls) weibull_params[[cls]]$shape)
  scale_vec <- sapply(baseline[[class_col]],
                      function(cls) weibull_params[[cls]]$scale)
  
  surv_time <- scale_vec * (-log(u) / exp(lp_vec))^(1 / shape_vec)
  baseline$surv_time <- surv_time
  
  # 6. 删失时间生成（支持校准 target_censor）
  # 期望刪失比例（Exponential）: E[censor<surv] = mean(1 - exp(-rate * surv))
  calc_prop_exp <- function(rate, surv) mean(1 - exp(-rate * surv))
  
  # 期望刪失比例（Uniform(0, M)）: E[censor<surv] = mean(pmin(surv, M) / M)
  calc_prop_unif <- function(M, surv) mean(pmin(surv, M) / M)
  
  if (!is.null(target_censor)) {
    if (!(target_censor >= 0 && target_censor <= 1)) stop("target_censor 必须在 [0,1] 之间。")
    if (censor_dist == "exponential") {
      # 二分法查找 rate
      low <- 1e-12
      high <- 1
      # 扩大 high 直到 prop >= target 或达到上限
      iter_expand <- 0
      while (calc_prop_exp(high, surv_time) < target_censor && iter_expand < 60) {
        high <- high * 2
        iter_expand <- iter_expand + 1
      }
      if (iter_expand >= 60 && calc_prop_exp(high, surv_time) < target_censor) {
        warning("找不到合适的 rate 来达到目标删失率；使用当前 high 值。")
      }
      # 二分
      for (i in seq_len(max_iter)) {
        mid <- (low + high) / 2
        prop_mid <- calc_prop_exp(mid, surv_time)
        if (abs(prop_mid - target_censor) < tol) break
        if (prop_mid < target_censor) {
          low <- mid
        } else {
          high <- mid
        }
      }
      rate_final <- mid
      censor_time <- rexp(n, rate = rate_final)
      message(sprintf("校准 exponential rate = %.6g，使期望删失率 ≈ %.4f", rate_final, calc_prop_exp(rate_final, surv_time)))
    } else { # uniform
      # 二分查找 M
      low <- 1e-8
      high <- max(surv_time) * 10 + 1
      iter_expand <- 0
      while (calc_prop_unif(high, surv_time) > target_censor && iter_expand < 60) {
        # 若 high 太大，prop 会趋近于 mean(surv)/high -> 0，因此通常这里不会进入，但保留安全逻辑
        high <- high * 2
        iter_expand <- iter_expand + 1
      }
      # 使用二分
      for (i in seq_len(max_iter)) {
        mid <- (low + high) / 2
        prop_mid <- calc_prop_unif(mid, surv_time)
        if (abs(prop_mid - target_censor) < tol) break
        # 若 prop_mid > target -> M 太小（因为 pmin(surv,M)/M 较大），需要增大 M
        if (prop_mid > target_censor) {
          low <- mid
        } else {
          high <- mid
        }
      }
      M_final <- mid
      censor_time <- runif(n, min = 0, max = M_final)
      message(sprintf("校准 uniform max = %.6g，使期望删失率 ≈ %.4f", M_final, calc_prop_unif(M_final, surv_time)))
    }
  } else {
    # 若不校准，根据 censor_params 生成
    if (censor_dist == "exponential") {
      rate_use <- if (!is.null(censor_params$rate)) censor_params$rate else 0.05
      censor_time <- rexp(n, rate = rate_use)
    } else {
      # uniform
      M_use <- if (!is.null(censor_params$max)) censor_params$max else (quantile(surv_time, 0.8) * 2)
      censor_time <- runif(n, min = 0, max = M_use)
    }
  }
  
  baseline$censor_time <- censor_time
  baseline$obs_time <- pmin(baseline$surv_time, baseline$censor_time)
  baseline$event <- as.integer(baseline$surv_time <= baseline$censor_time)
  
  # 输出：基线（每个 ID 一行），或合并回原始纵向数据
  if (attach_longitudinal) {
    # 把生存信息合并回原始数据（按 ID）
    out <- data %>%
      left_join(baseline %>% select(.data[[id_col]], surv_time, censor_time, obs_time, event, lp),
                by = id_col)
    return(out)
  } else {
    return(baseline)
  }
}


merge_data <- function(long_data,
                       weibull_params,
                       beta_effects,
                       seed_start = 123,
                       censor_dist = "uniform",
                       target_censor = 0.3,
                       n_max = 100) {
  # 检查输入长度
  n_max <- min(n_max, length(long_data))
  
  # 新建list存储结果
  surv_list <- vector("list", n_max)
  
  for (i in 1:n_max) {
    # 取基线数据
    baseline_data <- long_data[[i]] %>%
      dplyr::group_by(ID) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    
    # 调用 add_surv
    df_surv <- add_surv(
      baseline_data,
      weibull_params = weibull_params,
      beta_effects   = beta_effects,
      seed           = seed_start + i,  # 每个数据集不同随机种子
      censor_dist    = censor_dist,
      target_censor  = target_censor
    )
    
    # 保存到list
    surv_list[[i]] <- df_surv
  }
  
  return(surv_list)
}


longer_data <- function(baseline_data, long_data) {
  # 检查两个 list 长度是否一致
  if (length(baseline_data) != length(long_data)) {
    stop("baseline_data 和 long_data 的长度不一致，请检查输入。")
  }
  
  # 新建list存储结果
  merged_list <- vector("list", length(long_data))
  
  for (i in seq_along(long_data)) {
    # 提取基线和纵向数据
    base_i <- baseline_data[[i]]
    long_i <- long_data[[i]]
    
    # 选出生存相关的变量
    surv_cols <- c("lp", "surv_time", "censor_time", "obs_time", "event")
    surv_info <- base_i %>% dplyr::select(ID, dplyr::all_of(surv_cols))
    
    # 合并：把基线的生存变量加到纵向数据中
    merged_i <- long_i %>%
      dplyr::left_join(surv_info, by = "ID")
    
    # 存入结果list
    merged_list[[i]] <- merged_i
  }
  
  return(merged_list)
}

write_list <- function(data, dir) {
  # 确认输入是 list
  if (!is.list(data)) stop("data 必须是一个 list，且每个元素应是 data.frame 或 tibble")
  
  # 获取变量名（即 list 的名字，例如 sim500_30_10V_lowINTER_3c）
  data_name <- deparse(substitute(data))
  
  # 如果目录不存在，创建目录
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # 遍历保存
  for (i in seq_along(data)) {
    file_path <- file.path(dir, paste0(data_name, "_", i, ".xls"))
    writexl::write_xlsx(data[[i]], path = file_path)
  }
  
  message("✅ 已保存 ", length(data), " 个文件到目录: ", dir)
}





###############生成生存数据####################

#######数据量500################
sim500_30_10V_lowINTER_3c <- merge_data(
  long_data      = sim_data500_low,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 100
)

sim500_70_10V_lowINTER_3c <- merge_data(
  long_data      = sim_data500_low,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 100
)

sim500_30_10V_midINTER_3c <- merge_data(
  long_data      = sim_data500_mid,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 100
)


sim500_70_10V_midINTER_3c <- merge_data(
  long_data      = sim_data500_mid,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 100
)



sim500_30_10V_highINTER_3c <- merge_data(
  long_data      = sim_data500_high,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 100
)

sim500_70_10V_highINTER_3c <- merge_data(
  long_data      = sim_data500_high,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 123,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 100
)

#########数据量1000#############







#########合并为纵向数据#######################
sim500_30_10V_lowINTER_3c<-longer_data(sim500_30_10V_lowINTER_3c, sim_data500_low)
sim500_30_10V_midINTER_3c<-longer_data(sim500_30_10V_midINTER_3c, sim_data500_mid)
sim500_30_10V_highINTER_3c<-longer_data(sim500_30_10V_highINTER_3c, sim_data500_high)
sim500_70_10V_lowINTER_3c<-longer_data(sim500_70_10V_lowINTER_3c, sim_data500_low)
sim500_70_10V_midINTER_3c<-longer_data(sim500_70_10V_midINTER_3c, sim_data500_mid)
sim500_70_10V_highINTER_3c<-longer_data(sim500_70_10V_highINTER_3c, sim_data500_high)




################保存数据##################

write_list(
  data = sim500_30_10V_lowINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_30删失_10V_LOW(变量间)_3类"
)
write_list(
  data = sim500_30_10V_midINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_30删失_10V_MID(变量间)_3类"
)
write_list(
  data = sim500_30_10V_highINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_30删失_10V_HIGH(变量间)_3类"
)



write_list(
  data = sim500_70_10V_lowINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_70删失_10V_LOW(变量间)_3类"
)
write_list(
  data = sim500_70_10V_midINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_70删失_10V_MID(变量间)_3类"
)
write_list(
  data = sim500_70_10V_highINTER_3c,
  dir  = "F:/文章/大论文/程序/模拟数据/500_70删失_10V_HIGH(变量间)_3类"
)




