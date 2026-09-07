# =============================================================================
# 4_补充_模拟数据_4V.R
# 生成配对测试集数据（仅修改 seed），整合 数据1+数据2+数据3
# 与训练集配对：训练集 seed vs 测试集 seed + SEED_OFFSET
# =============================================================================

library(tidyverse)
library(MASS)
library(writexl)
library(rlang)

options(scipen = 999)

# 测试集 seed 偏移量（与训练集完全独立）
SEED_OFFSET <- 100000L

# 输出目录（测试集）
BASE_OUT <- "F:/文章_大论文/0319大改/测试集/模拟数据_4V"
dir.create(file.path(BASE_OUT, "协变量"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(BASE_OUT, "生存"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(BASE_OUT, "生成Y"), showWarnings = FALSE, recursive = TRUE)

# 模拟开关：TRUE=执行该块，FALSE=跳过（可分别模拟 INTER 或 BTW）
RUN_INTER <- TRUE
RUN_BTW   <- TRUE


# ==================== 第一部分：数据1 - 纵向协变量生成（INTER 与 BTW 共用）====================

sim_single_class1 <- function(n = 200, n_time = 5, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  p         <- length(param_list)
  var_names <- sapply(param_list, `[[`, "varname")
  ID    <- rep(1:n, each = n_time)
  class <- rep("c1", n * n_time)
  time  <- rep(1:n_time, times = n)
  beta0 <- sapply(param_list, `[[`, "mean_intercept")
  beta1 <- sapply(param_list, `[[`, "mean_slope")
  noise <- sapply(param_list, `[[`, "noise_sd")
  t_mat <- matrix(NA, nrow = n, ncol = n_time)
  t_mat[, 1] <- 0
  if (n_time >= 2) t_mat[, 2] <- runif(n, 0, 0.25)
  if (n_time >= 3) t_mat[, 3] <- runif(n, 0.25, 0.5)
  if (n_time >= 4) t_mat[, 4] <- runif(n, 0.5, 0.75)
  if (n_time >= 5) t_mat[, 5] <- runif(n, 0.75, 1)
  t_mat <- t(apply(t_mat, 1, sort))
  t_vec <- as.vector(t(t_mat))
  Y_mat <- matrix(NA, nrow = n * n_time, ncol = p)
  colnames(Y_mat) <- var_names
  for (v in seq_len(p)) {
    mu_mat <- beta0[v] + beta1[v] * t_mat
    Y_mat[, v] <- as.vector(t(mu_mat)) + rnorm(n * n_time, 0, noise[v])
  }
  data.frame(Y_mat, ID = ID, class = class, time = time, t = t_vec)
}

put_in_list <- function(n_datasets = 200, n = 500, n_time = 5, param_list,
                        base_seed = 123, param_set = "L1", corr_level = "low") {
  result_list <- list()
  for (i in 1:n_datasets) {
    dataset <- sim_single_class1(n = n, n_time = n_time, param_list = param_list,
                                 seed = base_seed + i)
    list_name <- paste0("sim_data_", n, "_", corr_level, "_", param_set, "_", i)
    result_list[[list_name]] <- dataset
  }
  result_list
}

param_list_4v_1 <- list(
  list(varname = "V1", mean_intercept = 0.55, mean_slope =  0.30, noise_sd = 0.03),
  list(varname = "V2", mean_intercept = 0.70, mean_slope = -0.25, noise_sd = 0.10),
  list(varname = "V3", mean_intercept = 0.35, mean_slope =  0.45, noise_sd = 0.04),
  list(varname = "V4", mean_intercept = 0.80, mean_slope = -0.15, noise_sd = 0.11)
)
param_list_4v_2 <- list(
  list(varname = "V1", mean_intercept = 0.60, mean_slope =  0.45, noise_sd = 0.028),
  list(varname = "V2", mean_intercept = 0.65, mean_slope = -0.35, noise_sd = 0.12),
  list(varname = "V3", mean_intercept = 0.30, mean_slope =  0.60, noise_sd = 0.035),
  list(varname = "V4", mean_intercept = 0.85, mean_slope = -0.20, noise_sd = 0.10)
)
param_list_4v_3 <- list(
  list(varname = "V1", mean_intercept = 0.75, mean_slope =  0.20, noise_sd = 0.04),
  list(varname = "V2", mean_intercept = 0.80, mean_slope = -0.15, noise_sd = 0.09),
  list(varname = "V3", mean_intercept = 0.70, mean_slope =  0.25, noise_sd = 0.045),
  list(varname = "V4", mean_intercept = 0.85, mean_slope = -0.10, noise_sd = 0.085)
)
param_list_4v_4 <- list(
  list(varname = "V1", mean_intercept = 0.35, mean_slope =  0.55, noise_sd = 0.025),
  list(varname = "V2", mean_intercept = 0.40, mean_slope = -0.45, noise_sd = 0.11),
  list(varname = "V3", mean_intercept = 0.30, mean_slope =  0.65, noise_sd = 0.03),
  list(varname = "V4", mean_intercept = 0.45, mean_slope = -0.50, noise_sd = 0.12)
)
param_list_4v_5 <- list(
  list(varname = "V1", mean_intercept = 0.50, mean_slope =  0.35, noise_sd = 0.02),
  list(varname = "V2", mean_intercept = 0.75, mean_slope = -0.30, noise_sd = 0.14),
  list(varname = "V3", mean_intercept = 0.45, mean_slope =  0.40, noise_sd = 0.06),
  list(varname = "V4", mean_intercept = 0.65, mean_slope = -0.20, noise_sd = 0.10)
)

message("【步骤1】生成纵向协变量（测试集 seed = 原seed + ", SEED_OFFSET, "）...")
sim_data_500_low_c1_L1   <- put_in_list(200, 500, 5, param_list_4v_1, 201L + SEED_OFFSET, "L1", "low")
sim_data_500_low_c1_L2   <- put_in_list(200, 500, 5, param_list_4v_2, 202L + SEED_OFFSET, "L2", "low")
sim_data_500_low_c1_L3   <- put_in_list(200, 500, 5, param_list_4v_3, 203L + SEED_OFFSET, "L3", "low")
sim_data_500_low_c1_L4   <- put_in_list(200, 500, 5, param_list_4v_4, 204L + SEED_OFFSET, "L4", "low")
sim_data_500_low_c1_L5   <- put_in_list(200, 500, 5, param_list_4v_5, 205L + SEED_OFFSET, "L5", "low")
sim_data_500_medium_c1_L1 <- put_in_list(200, 500, 5, param_list_4v_1, 206L + SEED_OFFSET, "L1", "medium")
sim_data_500_medium_c1_L2 <- put_in_list(200, 500, 5, param_list_4v_2, 207L + SEED_OFFSET, "L2", "medium")
sim_data_500_medium_c1_L3 <- put_in_list(200, 500, 5, param_list_4v_3, 208L + SEED_OFFSET, "L3", "medium")
sim_data_500_medium_c1_L4 <- put_in_list(200, 500, 5, param_list_4v_4, 209L + SEED_OFFSET, "L4", "medium")
sim_data_500_medium_c1_L5 <- put_in_list(200, 500, 5, param_list_4v_5, 210L + SEED_OFFSET, "L5", "medium")
sim_data_500_high_c1_L1  <- put_in_list(200, 500, 5, param_list_4v_1, 211L + SEED_OFFSET, "L1", "high")
sim_data_500_high_c1_L2  <- put_in_list(200, 500, 5, param_list_4v_2, 212L + SEED_OFFSET, "L2", "high")
sim_data_500_high_c1_L3  <- put_in_list(200, 500, 5, param_list_4v_3, 213L + SEED_OFFSET, "L3", "high")
sim_data_500_high_c1_L4  <- put_in_list(200, 500, 5, param_list_4v_4, 214L + SEED_OFFSET, "L4", "high")
sim_data_500_high_c1_L5  <- put_in_list(200, 500, 5, param_list_4v_5, 215L + SEED_OFFSET, "L5", "high")
sim_data_1000_low_c1_L1  <- put_in_list(200, 1000, 5, param_list_4v_1, 301L + SEED_OFFSET, "L1", "low")
sim_data_1000_low_c1_L2  <- put_in_list(200, 1000, 5, param_list_4v_2, 302L + SEED_OFFSET, "L2", "low")
sim_data_1000_low_c1_L3  <- put_in_list(200, 1000, 5, param_list_4v_3, 303L + SEED_OFFSET, "L3", "low")
sim_data_1000_low_c1_L4  <- put_in_list(200, 1000, 5, param_list_4v_4, 304L + SEED_OFFSET, "L4", "low")
sim_data_1000_low_c1_L5  <- put_in_list(200, 1000, 5, param_list_4v_5, 305L + SEED_OFFSET, "L5", "low")
sim_data_1000_medium_c1_L1 <- put_in_list(200, 1000, 5, param_list_4v_1, 306L + SEED_OFFSET, "L1", "medium")
sim_data_1000_medium_c1_L2 <- put_in_list(200, 1000, 5, param_list_4v_2, 307L + SEED_OFFSET, "L2", "medium")
sim_data_1000_medium_c1_L3 <- put_in_list(200, 1000, 5, param_list_4v_3, 308L + SEED_OFFSET, "L3", "medium")
sim_data_1000_medium_c1_L4 <- put_in_list(200, 1000, 5, param_list_4v_4, 309L + SEED_OFFSET, "L4", "medium")
sim_data_1000_medium_c1_L5 <- put_in_list(200, 1000, 5, param_list_4v_5, 310L + SEED_OFFSET, "L5", "medium")
sim_data_1000_high_c1_L1  <- put_in_list(200, 1000, 5, param_list_4v_1, 311L + SEED_OFFSET, "L1", "high")
sim_data_1000_high_c1_L2  <- put_in_list(200, 1000, 5, param_list_4v_2, 312L + SEED_OFFSET, "L2", "high")
sim_data_1000_high_c1_L3  <- put_in_list(200, 1000, 5, param_list_4v_3, 313L + SEED_OFFSET, "L3", "high")
sim_data_1000_high_c1_L4  <- put_in_list(200, 1000, 5, param_list_4v_4, 314L + SEED_OFFSET, "L4", "high")
sim_data_1000_high_c1_L5  <- put_in_list(200, 1000, 5, param_list_4v_5, 315L + SEED_OFFSET, "L5", "high")


# ==================== 第二部分：数据2 - 生存数据 ====================

add_surv <- function(data, weibull_params, beta_effects, var_pattern = "^V\\d+$",
                     id_col = "ID", class_col = "class", seed = 123,
                     censor_dist = c("exponential", "uniform"),
                     target_censor = NULL, tol = 1e-4, max_iter = 50) {
  if (!is.null(seed)) set.seed(seed)
  censor_dist <- match.arg(censor_dist)
  baseline <- data %>% group_by(!!sym(id_col)) %>% slice(1) %>% ungroup()
  var_names <- grep(var_pattern, names(baseline), value = TRUE)
  if (length(var_names) == 0) stop("无法找到自变量列")
  coef_c1 <- beta_effects$c1
  keep_coef <- intersect(names(coef_c1), var_names)
  coef_c1 <- coef_c1[keep_coef]
  n <- nrow(baseline)
  X <- as.matrix(baseline[, names(coef_c1), drop = FALSE])
  lp_vec <- as.numeric(X %*% coef_c1)
  baseline$lp <- lp_vec
  shape <- weibull_params$c1$shape
  scale <- weibull_params$c1$scale
  u <- runif(n)
  surv_time <- scale * (-log(u) / exp(lp_vec))^(1 / shape)
  baseline$surv_time <- surv_time
  calc_prop_unif <- function(M, surv) mean(pmin(surv, M) / M)
  if (!is.null(target_censor)) {
    low <- 1e-8
    high <- max(surv_time) * 10 + 1
    for (i in seq_len(max_iter)) {
      mid <- (low + high) / 2
      if (abs(calc_prop_unif(mid, surv_time) - target_censor) < tol) break
      if (calc_prop_unif(mid, surv_time) > target_censor) low <- mid else high <- mid
    }
    censor_time <- runif(n, 0, mid)
  } else {
    M_use <- quantile(surv_time, 0.8) * 2
    censor_time <- runif(n, 0, M_use)
  }
  baseline$censor_time <- censor_time
  baseline$obs_time <- pmin(baseline$surv_time, baseline$censor_time)
  baseline$event <- as.integer(baseline$surv_time <= baseline$censor_time)
  baseline
}

merge_data <- function(long_data, weibull_params, beta_effects, seed_start = 123,
                       censor_dist = "uniform", target_censor = 0.3, n_max = 200) {
  n_max <- min(n_max, length(long_data))
  surv_list <- vector("list", n_max)
  for (i in 1:n_max) {
    baseline_data <- long_data[[i]] %>% group_by(ID) %>% slice(1) %>% ungroup()
    surv_list[[i]] <- add_surv(baseline_data, weibull_params, beta_effects,
                               seed = seed_start + i, censor_dist = censor_dist,
                               target_censor = target_censor)
  }
  surv_list
}

longer_data <- function(baseline_data, long_data) {
  merged_list <- vector("list", length(long_data))
  for (i in seq_along(long_data)) {
    surv_info <- baseline_data[[i]] %>% dplyr::select(ID, lp, surv_time, censor_time, obs_time, event)
    merged_list[[i]] <- long_data[[i]] %>%
      left_join(surv_info, by = "ID") %>%
      filter(t <= obs_time)  # 剔除 t > obs_time 的无效观测（患者已删失/死亡后不应有纵向记录）
  }
  merged_list
}

weibull_params <- list(c1 = list(shape = 1.2, scale = 2))
beta_effects <- list(c1 = c(V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.9))
# BTW：变量与噪声相关性高，V4 系数弱化（4V 无协方差矩阵，仅通过 beta 模拟高影响与噪声相关）
beta_effect_noise <- list(c1 = c(V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.09))


# ==================== 第三部分：数据3 - 生成 Y（函数与参数，块1/块2 共用）====================
add_Y <- function(sim_df, k, time_effects, id_col = "ID") {
  v_cols <- grep("^V\\d+", names(sim_df), value = TRUE)
  k_vec <- setNames(k[seq_along(v_cols)], v_cols)
  V_mat <- sim_df[, v_cols] |> as.matrix()
  X_effect <- as.numeric(V_mat %*% k_vec)
  beta0 <- time_effects[1]
  beta1 <- time_effects[2]
  sd_b0 <- time_effects[3]
  sd_b1 <- time_effects[4]
  id_vec <- unique(sim_df[[id_col]])
  re_df <- data.frame(b0i = rnorm(length(id_vec), 0, sd_b0), b1i = rnorm(length(id_vec), 0, sd_b1))
  rownames(re_df) <- id_vec
  sim_df %>% mutate(
    b0i = re_df[as.character(.data[[id_col]]), "b0i"],
    b1i = re_df[as.character(.data[[id_col]]), "b1i"],
    Y = X_effect + (beta0 + b0i) + (beta1 + b1i) * t + rnorm(n(), mean = 0, sd = 0.5)
  ) %>% dplyr::select(-b0i, -b1i)
}
add_Y_to_list <- function(data_list, k, time_effects, base_seed = 123) {
  lapply(seq_along(data_list), function(i) {
    set.seed(base_seed + i)
    v_cols <- grep("^V\\d+", names(data_list[[i]]), value = TRUE)
    k_adj <- if (length(k) != length(v_cols)) c(k, rep(0, max(0, length(v_cols) - length(k))))[seq_along(v_cols)] else k
    add_Y(data_list[[i]], k_adj, time_effects)
  })
}
k <- c(runif(2, 0.8, 1.2), rep(0.025, 2))
time_effects <- c(0.3, 1.0, 0.6, 0.4)
y_seed <- 123L + SEED_OFFSET


# ==================== 块1：INTER 数据模拟（纵向→生存→Y→保存）====================
if (RUN_INTER) {
message("【块1-INTER】添加生存数据（seed_start + ", SEED_OFFSET, "）...")
seed_off <- as.integer(SEED_OFFSET)
s30_1 <- merge_data(sim_data_500_low_c1_L1, weibull_params, beta_effects, 20100000L + seed_off, "uniform", 0.3)
s30_2 <- merge_data(sim_data_500_low_c1_L2, weibull_params, beta_effects, 20110000L + seed_off, "uniform", 0.3)
s30_3 <- merge_data(sim_data_500_low_c1_L3, weibull_params, beta_effects, 20120000L + seed_off, "uniform", 0.3)
s30_4 <- merge_data(sim_data_500_low_c1_L4, weibull_params, beta_effects, 20130000L + seed_off, "uniform", 0.3)
s30_5 <- merge_data(sim_data_500_low_c1_L5, weibull_params, beta_effects, 20140000L + seed_off, "uniform", 0.3)
s70_1 <- merge_data(sim_data_500_low_c1_L1, weibull_params, beta_effects, 20150000L + seed_off, "uniform", 0.7)
s70_2 <- merge_data(sim_data_500_low_c1_L2, weibull_params, beta_effects, 20160000L + seed_off, "uniform", 0.7)
s70_3 <- merge_data(sim_data_500_low_c1_L3, weibull_params, beta_effects, 20170000L + seed_off, "uniform", 0.7)
s70_4 <- merge_data(sim_data_500_low_c1_L4, weibull_params, beta_effects, 20180000L + seed_off, "uniform", 0.7)
s70_5 <- merge_data(sim_data_500_low_c1_L5, weibull_params, beta_effects, 20190000L + seed_off, "uniform", 0.7)
sim500_30_4V_lowINTER_1c_L1  <- longer_data(s30_1, sim_data_500_low_c1_L1)
sim500_30_4V_lowINTER_1c_L2  <- longer_data(s30_2, sim_data_500_low_c1_L2)
sim500_30_4V_lowINTER_1c_L3  <- longer_data(s30_3, sim_data_500_low_c1_L3)
sim500_30_4V_lowINTER_1c_L4  <- longer_data(s30_4, sim_data_500_low_c1_L4)
sim500_30_4V_lowINTER_1c_L5  <- longer_data(s30_5, sim_data_500_low_c1_L5)
sim500_70_4V_lowINTER_1c_L1  <- longer_data(s70_1, sim_data_500_low_c1_L1)
sim500_70_4V_lowINTER_1c_L2  <- longer_data(s70_2, sim_data_500_low_c1_L2)
sim500_70_4V_lowINTER_1c_L3  <- longer_data(s70_3, sim_data_500_low_c1_L3)
sim500_70_4V_lowINTER_1c_L4  <- longer_data(s70_4, sim_data_500_low_c1_L4)
sim500_70_4V_lowINTER_1c_L5  <- longer_data(s70_5, sim_data_500_low_c1_L5)
s30_1k_1 <- merge_data(sim_data_1000_low_c1_L1, weibull_params, beta_effects, 20300000L + seed_off, "uniform", 0.3)
s30_1k_2 <- merge_data(sim_data_1000_low_c1_L2, weibull_params, beta_effects, 20310000L + seed_off, "uniform", 0.3)
s30_1k_3 <- merge_data(sim_data_1000_low_c1_L3, weibull_params, beta_effects, 20320000L + seed_off, "uniform", 0.3)
s30_1k_4 <- merge_data(sim_data_1000_low_c1_L4, weibull_params, beta_effects, 20330000L + seed_off, "uniform", 0.3)
s30_1k_5 <- merge_data(sim_data_1000_low_c1_L5, weibull_params, beta_effects, 20340000L + seed_off, "uniform", 0.3)
s70_1k_1 <- merge_data(sim_data_1000_low_c1_L1, weibull_params, beta_effects, 20350000L + seed_off, "uniform", 0.7)
s70_1k_2 <- merge_data(sim_data_1000_low_c1_L2, weibull_params, beta_effects, 20360000L + seed_off, "uniform", 0.7)
s70_1k_3 <- merge_data(sim_data_1000_low_c1_L3, weibull_params, beta_effects, 20370000L + seed_off, "uniform", 0.7)
s70_1k_4 <- merge_data(sim_data_1000_low_c1_L4, weibull_params, beta_effects, 20380000L + seed_off, "uniform", 0.7)
s70_1k_5 <- merge_data(sim_data_1000_low_c1_L5, weibull_params, beta_effects, 20390000L + seed_off, "uniform", 0.7)
sim1000_30_4V_lowINTER_1c_L1 <- longer_data(s30_1k_1, sim_data_1000_low_c1_L1)
sim1000_30_4V_lowINTER_1c_L2 <- longer_data(s30_1k_2, sim_data_1000_low_c1_L2)
sim1000_30_4V_lowINTER_1c_L3 <- longer_data(s30_1k_3, sim_data_1000_low_c1_L3)
sim1000_30_4V_lowINTER_1c_L4 <- longer_data(s30_1k_4, sim_data_1000_low_c1_L4)
sim1000_30_4V_lowINTER_1c_L5 <- longer_data(s30_1k_5, sim_data_1000_low_c1_L5)
sim1000_70_4V_lowINTER_1c_L1 <- longer_data(s70_1k_1, sim_data_1000_low_c1_L1)
sim1000_70_4V_lowINTER_1c_L2 <- longer_data(s70_1k_2, sim_data_1000_low_c1_L2)
sim1000_70_4V_lowINTER_1c_L3 <- longer_data(s70_1k_3, sim_data_1000_low_c1_L3)
sim1000_70_4V_lowINTER_1c_L4 <- longer_data(s70_1k_4, sim_data_1000_low_c1_L4)
sim1000_70_4V_lowINTER_1c_L5 <- longer_data(s70_1k_5, sim_data_1000_low_c1_L5)
sm30_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effects, 21100000L + seed_off, "uniform", 0.3)
sm30_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effects, 21110000L + seed_off, "uniform", 0.3)
sm30_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effects, 21120000L + seed_off, "uniform", 0.3)
sm30_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effects, 21130000L + seed_off, "uniform", 0.3)
sm30_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effects, 21140000L + seed_off, "uniform", 0.3)
sm70_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effects, 21150000L + seed_off, "uniform", 0.7)
sm70_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effects, 21160000L + seed_off, "uniform", 0.7)
sm70_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effects, 21170000L + seed_off, "uniform", 0.7)
sm70_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effects, 21180000L + seed_off, "uniform", 0.7)
sm70_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effects, 21190000L + seed_off, "uniform", 0.7)
sim500_30_4V_midINTER_1c_L1  <- longer_data(sm30_1, sim_data_500_medium_c1_L1)
sim500_30_4V_midINTER_1c_L2  <- longer_data(sm30_2, sim_data_500_medium_c1_L2)
sim500_30_4V_midINTER_1c_L3  <- longer_data(sm30_3, sim_data_500_medium_c1_L3)
sim500_30_4V_midINTER_1c_L4  <- longer_data(sm30_4, sim_data_500_medium_c1_L4)
sim500_30_4V_midINTER_1c_L5  <- longer_data(sm30_5, sim_data_500_medium_c1_L5)
sim500_70_4V_midINTER_1c_L1  <- longer_data(sm70_1, sim_data_500_medium_c1_L1)
sim500_70_4V_midINTER_1c_L2  <- longer_data(sm70_2, sim_data_500_medium_c1_L2)
sim500_70_4V_midINTER_1c_L3  <- longer_data(sm70_3, sim_data_500_medium_c1_L3)
sim500_70_4V_midINTER_1c_L4  <- longer_data(sm70_4, sim_data_500_medium_c1_L4)
sim500_70_4V_midINTER_1c_L5  <- longer_data(sm70_5, sim_data_500_medium_c1_L5)
sm30_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effects, 21300000L + seed_off, "uniform", 0.3)
sm30_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effects, 21310000L + seed_off, "uniform", 0.3)
sm30_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effects, 21320000L + seed_off, "uniform", 0.3)
sm30_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effects, 21330000L + seed_off, "uniform", 0.3)
sm30_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effects, 21340000L + seed_off, "uniform", 0.3)
sm70_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effects, 21350000L + seed_off, "uniform", 0.7)
sm70_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effects, 21360000L + seed_off, "uniform", 0.7)
sm70_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effects, 21370000L + seed_off, "uniform", 0.7)
sm70_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effects, 21380000L + seed_off, "uniform", 0.7)
sm70_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effects, 21390000L + seed_off, "uniform", 0.7)
sim1000_30_4V_midINTER_1c_L1 <- longer_data(sm30_1k_1, sim_data_1000_medium_c1_L1)
sim1000_30_4V_midINTER_1c_L2 <- longer_data(sm30_1k_2, sim_data_1000_medium_c1_L2)
sim1000_30_4V_midINTER_1c_L3 <- longer_data(sm30_1k_3, sim_data_1000_medium_c1_L3)
sim1000_30_4V_midINTER_1c_L4 <- longer_data(sm30_1k_4, sim_data_1000_medium_c1_L4)
sim1000_30_4V_midINTER_1c_L5 <- longer_data(sm30_1k_5, sim_data_1000_medium_c1_L5)
sim1000_70_4V_midINTER_1c_L1 <- longer_data(sm70_1k_1, sim_data_1000_medium_c1_L1)
sim1000_70_4V_midINTER_1c_L2 <- longer_data(sm70_1k_2, sim_data_1000_medium_c1_L2)
sim1000_70_4V_midINTER_1c_L3 <- longer_data(sm70_1k_3, sim_data_1000_medium_c1_L3)
sim1000_70_4V_midINTER_1c_L4 <- longer_data(sm70_1k_4, sim_data_1000_medium_c1_L4)
sim1000_70_4V_midINTER_1c_L5 <- longer_data(sm70_1k_5, sim_data_1000_medium_c1_L5)
sh30_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effects, 22100000L + seed_off, "uniform", 0.3)
sh30_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effects, 22110000L + seed_off, "uniform", 0.3)
sh30_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effects, 22120000L + seed_off, "uniform", 0.3)
sh30_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effects, 22130000L + seed_off, "uniform", 0.3)
sh30_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effects, 22140000L + seed_off, "uniform", 0.3)
sh70_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effects, 22150000L + seed_off, "uniform", 0.7)
sh70_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effects, 22160000L + seed_off, "uniform", 0.7)
sh70_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effects, 22170000L + seed_off, "uniform", 0.7)
sh70_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effects, 22180000L + seed_off, "uniform", 0.7)
sh70_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effects, 22190000L + seed_off, "uniform", 0.7)
sim500_30_4V_highINTER_1c_L1  <- longer_data(sh30_1, sim_data_500_high_c1_L1)
sim500_30_4V_highINTER_1c_L2  <- longer_data(sh30_2, sim_data_500_high_c1_L2)
sim500_30_4V_highINTER_1c_L3  <- longer_data(sh30_3, sim_data_500_high_c1_L3)
sim500_30_4V_highINTER_1c_L4  <- longer_data(sh30_4, sim_data_500_high_c1_L4)
sim500_30_4V_highINTER_1c_L5  <- longer_data(sh30_5, sim_data_500_high_c1_L5)
sim500_70_4V_highINTER_1c_L1  <- longer_data(sh70_1, sim_data_500_high_c1_L1)
sim500_70_4V_highINTER_1c_L2  <- longer_data(sh70_2, sim_data_500_high_c1_L2)
sim500_70_4V_highINTER_1c_L3  <- longer_data(sh70_3, sim_data_500_high_c1_L3)
sim500_70_4V_highINTER_1c_L4  <- longer_data(sh70_4, sim_data_500_high_c1_L4)
sim500_70_4V_highINTER_1c_L5  <- longer_data(sh70_5, sim_data_500_high_c1_L5)
sh30_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effects, 22300000L + seed_off, "uniform", 0.3)
sh30_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effects, 22310000L + seed_off, "uniform", 0.3)
sh30_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effects, 22320000L + seed_off, "uniform", 0.3)
sh30_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effects, 22330000L + seed_off, "uniform", 0.3)
sh30_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effects, 22340000L + seed_off, "uniform", 0.3)
sh70_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effects, 22350000L + seed_off, "uniform", 0.7)
sh70_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effects, 22360000L + seed_off, "uniform", 0.7)
sh70_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effects, 22370000L + seed_off, "uniform", 0.7)
sh70_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effects, 22380000L + seed_off, "uniform", 0.7)
sh70_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effects, 22390000L + seed_off, "uniform", 0.7)
sim1000_30_4V_highINTER_1c_L1 <- longer_data(sh30_1k_1, sim_data_1000_high_c1_L1)
sim1000_30_4V_highINTER_1c_L2 <- longer_data(sh30_1k_2, sim_data_1000_high_c1_L2)
sim1000_30_4V_highINTER_1c_L3 <- longer_data(sh30_1k_3, sim_data_1000_high_c1_L3)
sim1000_30_4V_highINTER_1c_L4 <- longer_data(sh30_1k_4, sim_data_1000_high_c1_L4)
sim1000_30_4V_highINTER_1c_L5 <- longer_data(sh30_1k_5, sim_data_1000_high_c1_L5)
sim1000_70_4V_highINTER_1c_L1 <- longer_data(sh70_1k_1, sim_data_1000_high_c1_L1)
sim1000_70_4V_highINTER_1c_L2 <- longer_data(sh70_1k_2, sim_data_1000_high_c1_L2)
sim1000_70_4V_highINTER_1c_L3 <- longer_data(sh70_1k_3, sim_data_1000_high_c1_L3)
sim1000_70_4V_highINTER_1c_L4 <- longer_data(sh70_1k_4, sim_data_1000_high_c1_L4)
sim1000_70_4V_highINTER_1c_L5 <- longer_data(sh70_1k_5, sim_data_1000_high_c1_L5)

message("【块1-INTER】添加 Y（base_seed + ", SEED_OFFSET, "）...")
y_seed <- 123L + SEED_OFFSET
sim500_30_4V_lowINTER_1c_L1  <- add_Y_to_list(sim500_30_4V_lowINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_lowINTER_1c_L2  <- add_Y_to_list(sim500_30_4V_lowINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_lowINTER_1c_L3  <- add_Y_to_list(sim500_30_4V_lowINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_lowINTER_1c_L4  <- add_Y_to_list(sim500_30_4V_lowINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_lowINTER_1c_L5  <- add_Y_to_list(sim500_30_4V_lowINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_lowINTER_1c_L1  <- add_Y_to_list(sim500_70_4V_lowINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_lowINTER_1c_L2  <- add_Y_to_list(sim500_70_4V_lowINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_lowINTER_1c_L3  <- add_Y_to_list(sim500_70_4V_lowINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_lowINTER_1c_L4  <- add_Y_to_list(sim500_70_4V_lowINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_lowINTER_1c_L5  <- add_Y_to_list(sim500_70_4V_lowINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_30_4V_lowINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_30_4V_lowINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_30_4V_lowINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_30_4V_lowINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_30_4V_lowINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_70_4V_lowINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_70_4V_lowINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_70_4V_lowINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_70_4V_lowINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_70_4V_lowINTER_1c_L5, k, time_effects, y_seed)
sim500_30_4V_midINTER_1c_L1  <- add_Y_to_list(sim500_30_4V_midINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_midINTER_1c_L2  <- add_Y_to_list(sim500_30_4V_midINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_midINTER_1c_L3  <- add_Y_to_list(sim500_30_4V_midINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_midINTER_1c_L4  <- add_Y_to_list(sim500_30_4V_midINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_midINTER_1c_L5  <- add_Y_to_list(sim500_30_4V_midINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_midINTER_1c_L1  <- add_Y_to_list(sim500_70_4V_midINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_midINTER_1c_L2  <- add_Y_to_list(sim500_70_4V_midINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_midINTER_1c_L3  <- add_Y_to_list(sim500_70_4V_midINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_midINTER_1c_L4  <- add_Y_to_list(sim500_70_4V_midINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_midINTER_1c_L5  <- add_Y_to_list(sim500_70_4V_midINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_midINTER_1c_L1 <- add_Y_to_list(sim1000_30_4V_midINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_midINTER_1c_L2 <- add_Y_to_list(sim1000_30_4V_midINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_midINTER_1c_L3 <- add_Y_to_list(sim1000_30_4V_midINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_midINTER_1c_L4 <- add_Y_to_list(sim1000_30_4V_midINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_midINTER_1c_L5 <- add_Y_to_list(sim1000_30_4V_midINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_midINTER_1c_L1 <- add_Y_to_list(sim1000_70_4V_midINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_midINTER_1c_L2 <- add_Y_to_list(sim1000_70_4V_midINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_midINTER_1c_L3 <- add_Y_to_list(sim1000_70_4V_midINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_midINTER_1c_L4 <- add_Y_to_list(sim1000_70_4V_midINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_midINTER_1c_L5 <- add_Y_to_list(sim1000_70_4V_midINTER_1c_L5, k, time_effects, y_seed)
sim500_30_4V_highINTER_1c_L1  <- add_Y_to_list(sim500_30_4V_highINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_highINTER_1c_L2  <- add_Y_to_list(sim500_30_4V_highINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_highINTER_1c_L3  <- add_Y_to_list(sim500_30_4V_highINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_highINTER_1c_L4  <- add_Y_to_list(sim500_30_4V_highINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_highINTER_1c_L5  <- add_Y_to_list(sim500_30_4V_highINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_highINTER_1c_L1  <- add_Y_to_list(sim500_70_4V_highINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_highINTER_1c_L2  <- add_Y_to_list(sim500_70_4V_highINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_highINTER_1c_L3  <- add_Y_to_list(sim500_70_4V_highINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_highINTER_1c_L4  <- add_Y_to_list(sim500_70_4V_highINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_highINTER_1c_L5  <- add_Y_to_list(sim500_70_4V_highINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_highINTER_1c_L1 <- add_Y_to_list(sim1000_30_4V_highINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_highINTER_1c_L2 <- add_Y_to_list(sim1000_30_4V_highINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_highINTER_1c_L3 <- add_Y_to_list(sim1000_30_4V_highINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_highINTER_1c_L4 <- add_Y_to_list(sim1000_30_4V_highINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_highINTER_1c_L5 <- add_Y_to_list(sim1000_30_4V_highINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_highINTER_1c_L1 <- add_Y_to_list(sim1000_70_4V_highINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_highINTER_1c_L2 <- add_Y_to_list(sim1000_70_4V_highINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_highINTER_1c_L3 <- add_Y_to_list(sim1000_70_4V_highINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_highINTER_1c_L4 <- add_Y_to_list(sim1000_70_4V_highINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_highINTER_1c_L5 <- add_Y_to_list(sim1000_70_4V_highINTER_1c_L5, k, time_effects, y_seed)
out_y <- file.path(BASE_OUT, "生成Y")
nms_inter <- c("sim500_30_4V_lowINTER_1c_L1","sim500_30_4V_lowINTER_1c_L2","sim500_30_4V_lowINTER_1c_L3","sim500_30_4V_lowINTER_1c_L4","sim500_30_4V_lowINTER_1c_L5",
         "sim500_70_4V_lowINTER_1c_L1","sim500_70_4V_lowINTER_1c_L2","sim500_70_4V_lowINTER_1c_L3","sim500_70_4V_lowINTER_1c_L4","sim500_70_4V_lowINTER_1c_L5",
         "sim1000_30_4V_lowINTER_1c_L1","sim1000_30_4V_lowINTER_1c_L2","sim1000_30_4V_lowINTER_1c_L3","sim1000_30_4V_lowINTER_1c_L4","sim1000_30_4V_lowINTER_1c_L5",
         "sim1000_70_4V_lowINTER_1c_L1","sim1000_70_4V_lowINTER_1c_L2","sim1000_70_4V_lowINTER_1c_L3","sim1000_70_4V_lowINTER_1c_L4","sim1000_70_4V_lowINTER_1c_L5",
         "sim500_30_4V_midINTER_1c_L1","sim500_30_4V_midINTER_1c_L2","sim500_30_4V_midINTER_1c_L3","sim500_30_4V_midINTER_1c_L4","sim500_30_4V_midINTER_1c_L5",
         "sim500_70_4V_midINTER_1c_L1","sim500_70_4V_midINTER_1c_L2","sim500_70_4V_midINTER_1c_L3","sim500_70_4V_midINTER_1c_L4","sim500_70_4V_midINTER_1c_L5",
         "sim1000_30_4V_midINTER_1c_L1","sim1000_30_4V_midINTER_1c_L2","sim1000_30_4V_midINTER_1c_L3","sim1000_30_4V_midINTER_1c_L4","sim1000_30_4V_midINTER_1c_L5",
         "sim1000_70_4V_midINTER_1c_L1","sim1000_70_4V_midINTER_1c_L2","sim1000_70_4V_midINTER_1c_L3","sim1000_70_4V_midINTER_1c_L4","sim1000_70_4V_midINTER_1c_L5",
         "sim500_30_4V_highINTER_1c_L1","sim500_30_4V_highINTER_1c_L2","sim500_30_4V_highINTER_1c_L3","sim500_30_4V_highINTER_1c_L4","sim500_30_4V_highINTER_1c_L5",
         "sim500_70_4V_highINTER_1c_L1","sim500_70_4V_highINTER_1c_L2","sim500_70_4V_highINTER_1c_L3","sim500_70_4V_highINTER_1c_L4","sim500_70_4V_highINTER_1c_L5",
         "sim1000_30_4V_highINTER_1c_L1","sim1000_30_4V_highINTER_1c_L2","sim1000_30_4V_highINTER_1c_L3","sim1000_30_4V_highINTER_1c_L4","sim1000_30_4V_highINTER_1c_L5",
         "sim1000_70_4V_highINTER_1c_L1","sim1000_70_4V_highINTER_1c_L2","sim1000_70_4V_highINTER_1c_L3","sim1000_70_4V_highINTER_1c_L4","sim1000_70_4V_highINTER_1c_L5")
for (nm in nms_inter) write_xlsx(get(nm), path = file.path(out_y, paste0(nm, ".xlsx")))
message("✅ 块1-INTER 完成：已保存 ", length(nms_inter), " 个INTER数据集")
}
# ==================== 块1 结束 ====================


# ==================== 块2：BTW 数据模拟（纵向→生存→Y→保存）====================
if (RUN_BTW) {
message("【块2-BTW】添加 BTW 生存数据（beta_effect_noise）...")
sb30_1 <- merge_data(sim_data_500_low_c1_L1, weibull_params, beta_effect_noise, 22400000L + seed_off, "uniform", 0.3)
sb30_2 <- merge_data(sim_data_500_low_c1_L2, weibull_params, beta_effect_noise, 22410000L + seed_off, "uniform", 0.3)
sb30_3 <- merge_data(sim_data_500_low_c1_L3, weibull_params, beta_effect_noise, 22420000L + seed_off, "uniform", 0.3)
sb30_4 <- merge_data(sim_data_500_low_c1_L4, weibull_params, beta_effect_noise, 22430000L + seed_off, "uniform", 0.3)
sb30_5 <- merge_data(sim_data_500_low_c1_L5, weibull_params, beta_effect_noise, 22440000L + seed_off, "uniform", 0.3)
sb70_1 <- merge_data(sim_data_500_low_c1_L1, weibull_params, beta_effect_noise, 22450000L + seed_off, "uniform", 0.7)
sb70_2 <- merge_data(sim_data_500_low_c1_L2, weibull_params, beta_effect_noise, 22460000L + seed_off, "uniform", 0.7)
sb70_3 <- merge_data(sim_data_500_low_c1_L3, weibull_params, beta_effect_noise, 22470000L + seed_off, "uniform", 0.7)
sb70_4 <- merge_data(sim_data_500_low_c1_L4, weibull_params, beta_effect_noise, 22480000L + seed_off, "uniform", 0.7)
sb70_5 <- merge_data(sim_data_500_low_c1_L5, weibull_params, beta_effect_noise, 22490000L + seed_off, "uniform", 0.7)
sim500_30_4V_lowBTW_1c_L1  <- longer_data(sb30_1, sim_data_500_low_c1_L1)
sim500_30_4V_lowBTW_1c_L2  <- longer_data(sb30_2, sim_data_500_low_c1_L2)
sim500_30_4V_lowBTW_1c_L3  <- longer_data(sb30_3, sim_data_500_low_c1_L3)
sim500_30_4V_lowBTW_1c_L4  <- longer_data(sb30_4, sim_data_500_low_c1_L4)
sim500_30_4V_lowBTW_1c_L5  <- longer_data(sb30_5, sim_data_500_low_c1_L5)
sim500_70_4V_lowBTW_1c_L1  <- longer_data(sb70_1, sim_data_500_low_c1_L1)
sim500_70_4V_lowBTW_1c_L2  <- longer_data(sb70_2, sim_data_500_low_c1_L2)
sim500_70_4V_lowBTW_1c_L3  <- longer_data(sb70_3, sim_data_500_low_c1_L3)
sim500_70_4V_lowBTW_1c_L4  <- longer_data(sb70_4, sim_data_500_low_c1_L4)
sim500_70_4V_lowBTW_1c_L5  <- longer_data(sb70_5, sim_data_500_low_c1_L5)
sb30_1k_1 <- merge_data(sim_data_1000_low_c1_L1, weibull_params, beta_effect_noise, 22600000L + seed_off, "uniform", 0.3)
sb30_1k_2 <- merge_data(sim_data_1000_low_c1_L2, weibull_params, beta_effect_noise, 22610000L + seed_off, "uniform", 0.3)
sb30_1k_3 <- merge_data(sim_data_1000_low_c1_L3, weibull_params, beta_effect_noise, 22620000L + seed_off, "uniform", 0.3)
sb30_1k_4 <- merge_data(sim_data_1000_low_c1_L4, weibull_params, beta_effect_noise, 22630000L + seed_off, "uniform", 0.3)
sb30_1k_5 <- merge_data(sim_data_1000_low_c1_L5, weibull_params, beta_effect_noise, 22640000L + seed_off, "uniform", 0.3)
sb70_1k_1 <- merge_data(sim_data_1000_low_c1_L1, weibull_params, beta_effect_noise, 22650000L + seed_off, "uniform", 0.7)
sb70_1k_2 <- merge_data(sim_data_1000_low_c1_L2, weibull_params, beta_effect_noise, 22660000L + seed_off, "uniform", 0.7)
sb70_1k_3 <- merge_data(sim_data_1000_low_c1_L3, weibull_params, beta_effect_noise, 22670000L + seed_off, "uniform", 0.7)
sb70_1k_4 <- merge_data(sim_data_1000_low_c1_L4, weibull_params, beta_effect_noise, 22680000L + seed_off, "uniform", 0.7)
sb70_1k_5 <- merge_data(sim_data_1000_low_c1_L5, weibull_params, beta_effect_noise, 22690000L + seed_off, "uniform", 0.7)
sim1000_30_4V_lowBTW_1c_L1 <- longer_data(sb30_1k_1, sim_data_1000_low_c1_L1)
sim1000_30_4V_lowBTW_1c_L2 <- longer_data(sb30_1k_2, sim_data_1000_low_c1_L2)
sim1000_30_4V_lowBTW_1c_L3 <- longer_data(sb30_1k_3, sim_data_1000_low_c1_L3)
sim1000_30_4V_lowBTW_1c_L4 <- longer_data(sb30_1k_4, sim_data_1000_low_c1_L4)
sim1000_30_4V_lowBTW_1c_L5 <- longer_data(sb30_1k_5, sim_data_1000_low_c1_L5)
sim1000_70_4V_lowBTW_1c_L1 <- longer_data(sb70_1k_1, sim_data_1000_low_c1_L1)
sim1000_70_4V_lowBTW_1c_L2 <- longer_data(sb70_1k_2, sim_data_1000_low_c1_L2)
sim1000_70_4V_lowBTW_1c_L3 <- longer_data(sb70_1k_3, sim_data_1000_low_c1_L3)
sim1000_70_4V_lowBTW_1c_L4 <- longer_data(sb70_1k_4, sim_data_1000_low_c1_L4)
sim1000_70_4V_lowBTW_1c_L5 <- longer_data(sb70_1k_5, sim_data_1000_low_c1_L5)
sbm30_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effect_noise, 22800000L + seed_off, "uniform", 0.3)
sbm30_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effect_noise, 22810000L + seed_off, "uniform", 0.3)
sbm30_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effect_noise, 22820000L + seed_off, "uniform", 0.3)
sbm30_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effect_noise, 22830000L + seed_off, "uniform", 0.3)
sbm30_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effect_noise, 22840000L + seed_off, "uniform", 0.3)
sbm70_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effect_noise, 22850000L + seed_off, "uniform", 0.7)
sbm70_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effect_noise, 22860000L + seed_off, "uniform", 0.7)
sbm70_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effect_noise, 22870000L + seed_off, "uniform", 0.7)
sbm70_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effect_noise, 22880000L + seed_off, "uniform", 0.7)
sbm70_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effect_noise, 22890000L + seed_off, "uniform", 0.7)
sim500_30_4V_midBTW_1c_L1  <- longer_data(sbm30_1, sim_data_500_medium_c1_L1)
sim500_30_4V_midBTW_1c_L2  <- longer_data(sbm30_2, sim_data_500_medium_c1_L2)
sim500_30_4V_midBTW_1c_L3  <- longer_data(sbm30_3, sim_data_500_medium_c1_L3)
sim500_30_4V_midBTW_1c_L4  <- longer_data(sbm30_4, sim_data_500_medium_c1_L4)
sim500_30_4V_midBTW_1c_L5  <- longer_data(sbm30_5, sim_data_500_medium_c1_L5)
sim500_70_4V_midBTW_1c_L1  <- longer_data(sbm70_1, sim_data_500_medium_c1_L1)
sim500_70_4V_midBTW_1c_L2  <- longer_data(sbm70_2, sim_data_500_medium_c1_L2)
sim500_70_4V_midBTW_1c_L3  <- longer_data(sbm70_3, sim_data_500_medium_c1_L3)
sim500_70_4V_midBTW_1c_L4  <- longer_data(sbm70_4, sim_data_500_medium_c1_L4)
sim500_70_4V_midBTW_1c_L5  <- longer_data(sbm70_5, sim_data_500_medium_c1_L5)
sbm30_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effect_noise, 23000000L + seed_off, "uniform", 0.3)
sbm30_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effect_noise, 23010000L + seed_off, "uniform", 0.3)
sbm30_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effect_noise, 23020000L + seed_off, "uniform", 0.3)
sbm30_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effect_noise, 23030000L + seed_off, "uniform", 0.3)
sbm30_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effect_noise, 23040000L + seed_off, "uniform", 0.3)
sbm70_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effect_noise, 23050000L + seed_off, "uniform", 0.7)
sbm70_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effect_noise, 23060000L + seed_off, "uniform", 0.7)
sbm70_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effect_noise, 23070000L + seed_off, "uniform", 0.7)
sbm70_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effect_noise, 23080000L + seed_off, "uniform", 0.7)
sbm70_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effect_noise, 23090000L + seed_off, "uniform", 0.7)
sim1000_30_4V_midBTW_1c_L1 <- longer_data(sbm30_1k_1, sim_data_1000_medium_c1_L1)
sim1000_30_4V_midBTW_1c_L2 <- longer_data(sbm30_1k_2, sim_data_1000_medium_c1_L2)
sim1000_30_4V_midBTW_1c_L3 <- longer_data(sbm30_1k_3, sim_data_1000_medium_c1_L3)
sim1000_30_4V_midBTW_1c_L4 <- longer_data(sbm30_1k_4, sim_data_1000_medium_c1_L4)
sim1000_30_4V_midBTW_1c_L5 <- longer_data(sbm30_1k_5, sim_data_1000_medium_c1_L5)
sim1000_70_4V_midBTW_1c_L1 <- longer_data(sbm70_1k_1, sim_data_1000_medium_c1_L1)
sim1000_70_4V_midBTW_1c_L2 <- longer_data(sbm70_1k_2, sim_data_1000_medium_c1_L2)
sim1000_70_4V_midBTW_1c_L3 <- longer_data(sbm70_1k_3, sim_data_1000_medium_c1_L3)
sim1000_70_4V_midBTW_1c_L4 <- longer_data(sbm70_1k_4, sim_data_1000_medium_c1_L4)
sim1000_70_4V_midBTW_1c_L5 <- longer_data(sbm70_1k_5, sim_data_1000_medium_c1_L5)
sbh30_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effect_noise, 23200000L + seed_off, "uniform", 0.3)
sbh30_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effect_noise, 23210000L + seed_off, "uniform", 0.3)
sbh30_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effect_noise, 23220000L + seed_off, "uniform", 0.3)
sbh30_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effect_noise, 23230000L + seed_off, "uniform", 0.3)
sbh30_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effect_noise, 23240000L + seed_off, "uniform", 0.3)
sbh70_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effect_noise, 23250000L + seed_off, "uniform", 0.7)
sbh70_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effect_noise, 23260000L + seed_off, "uniform", 0.7)
sbh70_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effect_noise, 23270000L + seed_off, "uniform", 0.7)
sbh70_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effect_noise, 23280000L + seed_off, "uniform", 0.7)
sbh70_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effect_noise, 23290000L + seed_off, "uniform", 0.7)
sim500_30_4V_highBTW_1c_L1  <- longer_data(sbh30_1, sim_data_500_high_c1_L1)
sim500_30_4V_highBTW_1c_L2  <- longer_data(sbh30_2, sim_data_500_high_c1_L2)
sim500_30_4V_highBTW_1c_L3  <- longer_data(sbh30_3, sim_data_500_high_c1_L3)
sim500_30_4V_highBTW_1c_L4  <- longer_data(sbh30_4, sim_data_500_high_c1_L4)
sim500_30_4V_highBTW_1c_L5  <- longer_data(sbh30_5, sim_data_500_high_c1_L5)
sim500_70_4V_highBTW_1c_L1  <- longer_data(sbh70_1, sim_data_500_high_c1_L1)
sim500_70_4V_highBTW_1c_L2  <- longer_data(sbh70_2, sim_data_500_high_c1_L2)
sim500_70_4V_highBTW_1c_L3  <- longer_data(sbh70_3, sim_data_500_high_c1_L3)
sim500_70_4V_highBTW_1c_L4  <- longer_data(sbh70_4, sim_data_500_high_c1_L4)
sim500_70_4V_highBTW_1c_L5  <- longer_data(sbh70_5, sim_data_500_high_c1_L5)
sbh30_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effect_noise, 23400000L + seed_off, "uniform", 0.3)
sbh30_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effect_noise, 23410000L + seed_off, "uniform", 0.3)
sbh30_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effect_noise, 23420000L + seed_off, "uniform", 0.3)
sbh30_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effect_noise, 23430000L + seed_off, "uniform", 0.3)
sbh30_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effect_noise, 23440000L + seed_off, "uniform", 0.3)
sbh70_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effect_noise, 23450000L + seed_off, "uniform", 0.7)
sbh70_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effect_noise, 23460000L + seed_off, "uniform", 0.7)
sbh70_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effect_noise, 23470000L + seed_off, "uniform", 0.7)
sbh70_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effect_noise, 23480000L + seed_off, "uniform", 0.7)
sbh70_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effect_noise, 23490000L + seed_off, "uniform", 0.7)
sim1000_30_4V_highBTW_1c_L1 <- longer_data(sbh30_1k_1, sim_data_1000_high_c1_L1)
sim1000_30_4V_highBTW_1c_L2 <- longer_data(sbh30_1k_2, sim_data_1000_high_c1_L2)
sim1000_30_4V_highBTW_1c_L3 <- longer_data(sbh30_1k_3, sim_data_1000_high_c1_L3)
sim1000_30_4V_highBTW_1c_L4 <- longer_data(sbh30_1k_4, sim_data_1000_high_c1_L4)
sim1000_30_4V_highBTW_1c_L5 <- longer_data(sbh30_1k_5, sim_data_1000_high_c1_L5)
sim1000_70_4V_highBTW_1c_L1 <- longer_data(sbh70_1k_1, sim_data_1000_high_c1_L1)
sim1000_70_4V_highBTW_1c_L2 <- longer_data(sbh70_1k_2, sim_data_1000_high_c1_L2)
sim1000_70_4V_highBTW_1c_L3 <- longer_data(sbh70_1k_3, sim_data_1000_high_c1_L3)
sim1000_70_4V_highBTW_1c_L4 <- longer_data(sbh70_1k_4, sim_data_1000_high_c1_L4)
sim1000_70_4V_highBTW_1c_L5 <- longer_data(sbh70_1k_5, sim_data_1000_high_c1_L5)

message("【块2-BTW】添加 Y...")
sim500_30_4V_lowBTW_1c_L1  <- add_Y_to_list(sim500_30_4V_lowBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_lowBTW_1c_L2  <- add_Y_to_list(sim500_30_4V_lowBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_lowBTW_1c_L3  <- add_Y_to_list(sim500_30_4V_lowBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_lowBTW_1c_L4  <- add_Y_to_list(sim500_30_4V_lowBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_lowBTW_1c_L5  <- add_Y_to_list(sim500_30_4V_lowBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_lowBTW_1c_L1  <- add_Y_to_list(sim500_70_4V_lowBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_lowBTW_1c_L2  <- add_Y_to_list(sim500_70_4V_lowBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_lowBTW_1c_L3  <- add_Y_to_list(sim500_70_4V_lowBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_lowBTW_1c_L4  <- add_Y_to_list(sim500_70_4V_lowBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_lowBTW_1c_L5  <- add_Y_to_list(sim500_70_4V_lowBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_30_4V_lowBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_30_4V_lowBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_30_4V_lowBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_30_4V_lowBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_30_4V_lowBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_70_4V_lowBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_70_4V_lowBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_70_4V_lowBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_70_4V_lowBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_70_4V_lowBTW_1c_L5, k, time_effects, y_seed)
sim500_30_4V_midBTW_1c_L1  <- add_Y_to_list(sim500_30_4V_midBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_midBTW_1c_L2  <- add_Y_to_list(sim500_30_4V_midBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_midBTW_1c_L3  <- add_Y_to_list(sim500_30_4V_midBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_midBTW_1c_L4  <- add_Y_to_list(sim500_30_4V_midBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_midBTW_1c_L5  <- add_Y_to_list(sim500_30_4V_midBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_midBTW_1c_L1  <- add_Y_to_list(sim500_70_4V_midBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_midBTW_1c_L2  <- add_Y_to_list(sim500_70_4V_midBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_midBTW_1c_L3  <- add_Y_to_list(sim500_70_4V_midBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_midBTW_1c_L4  <- add_Y_to_list(sim500_70_4V_midBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_midBTW_1c_L5  <- add_Y_to_list(sim500_70_4V_midBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_midBTW_1c_L1 <- add_Y_to_list(sim1000_30_4V_midBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_midBTW_1c_L2 <- add_Y_to_list(sim1000_30_4V_midBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_midBTW_1c_L3 <- add_Y_to_list(sim1000_30_4V_midBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_midBTW_1c_L4 <- add_Y_to_list(sim1000_30_4V_midBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_midBTW_1c_L5 <- add_Y_to_list(sim1000_30_4V_midBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_midBTW_1c_L1 <- add_Y_to_list(sim1000_70_4V_midBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_midBTW_1c_L2 <- add_Y_to_list(sim1000_70_4V_midBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_midBTW_1c_L3 <- add_Y_to_list(sim1000_70_4V_midBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_midBTW_1c_L4 <- add_Y_to_list(sim1000_70_4V_midBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_midBTW_1c_L5 <- add_Y_to_list(sim1000_70_4V_midBTW_1c_L5, k, time_effects, y_seed)
sim500_30_4V_highBTW_1c_L1  <- add_Y_to_list(sim500_30_4V_highBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_4V_highBTW_1c_L2  <- add_Y_to_list(sim500_30_4V_highBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_4V_highBTW_1c_L3  <- add_Y_to_list(sim500_30_4V_highBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_4V_highBTW_1c_L4  <- add_Y_to_list(sim500_30_4V_highBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_4V_highBTW_1c_L5  <- add_Y_to_list(sim500_30_4V_highBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_4V_highBTW_1c_L1  <- add_Y_to_list(sim500_70_4V_highBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_4V_highBTW_1c_L2  <- add_Y_to_list(sim500_70_4V_highBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_4V_highBTW_1c_L3  <- add_Y_to_list(sim500_70_4V_highBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_4V_highBTW_1c_L4  <- add_Y_to_list(sim500_70_4V_highBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_4V_highBTW_1c_L5  <- add_Y_to_list(sim500_70_4V_highBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_4V_highBTW_1c_L1 <- add_Y_to_list(sim1000_30_4V_highBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_4V_highBTW_1c_L2 <- add_Y_to_list(sim1000_30_4V_highBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_4V_highBTW_1c_L3 <- add_Y_to_list(sim1000_30_4V_highBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_4V_highBTW_1c_L4 <- add_Y_to_list(sim1000_30_4V_highBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_4V_highBTW_1c_L5 <- add_Y_to_list(sim1000_30_4V_highBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_4V_highBTW_1c_L1 <- add_Y_to_list(sim1000_70_4V_highBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_4V_highBTW_1c_L2 <- add_Y_to_list(sim1000_70_4V_highBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_4V_highBTW_1c_L3 <- add_Y_to_list(sim1000_70_4V_highBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_4V_highBTW_1c_L4 <- add_Y_to_list(sim1000_70_4V_highBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_4V_highBTW_1c_L5 <- add_Y_to_list(sim1000_70_4V_highBTW_1c_L5, k, time_effects, y_seed)
out_y <- file.path(BASE_OUT, "生成Y")
nms_btw <- c("sim500_30_4V_lowBTW_1c_L1","sim500_30_4V_lowBTW_1c_L2","sim500_30_4V_lowBTW_1c_L3","sim500_30_4V_lowBTW_1c_L4","sim500_30_4V_lowBTW_1c_L5",
         "sim500_70_4V_lowBTW_1c_L1","sim500_70_4V_lowBTW_1c_L2","sim500_70_4V_lowBTW_1c_L3","sim500_70_4V_lowBTW_1c_L4","sim500_70_4V_lowBTW_1c_L5",
         "sim1000_30_4V_lowBTW_1c_L1","sim1000_30_4V_lowBTW_1c_L2","sim1000_30_4V_lowBTW_1c_L3","sim1000_30_4V_lowBTW_1c_L4","sim1000_30_4V_lowBTW_1c_L5",
         "sim1000_70_4V_lowBTW_1c_L1","sim1000_70_4V_lowBTW_1c_L2","sim1000_70_4V_lowBTW_1c_L3","sim1000_70_4V_lowBTW_1c_L4","sim1000_70_4V_lowBTW_1c_L5",
         "sim500_30_4V_midBTW_1c_L1","sim500_30_4V_midBTW_1c_L2","sim500_30_4V_midBTW_1c_L3","sim500_30_4V_midBTW_1c_L4","sim500_30_4V_midBTW_1c_L5",
         "sim500_70_4V_midBTW_1c_L1","sim500_70_4V_midBTW_1c_L2","sim500_70_4V_midBTW_1c_L3","sim500_70_4V_midBTW_1c_L4","sim500_70_4V_midBTW_1c_L5",
         "sim1000_30_4V_midBTW_1c_L1","sim1000_30_4V_midBTW_1c_L2","sim1000_30_4V_midBTW_1c_L3","sim1000_30_4V_midBTW_1c_L4","sim1000_30_4V_midBTW_1c_L5",
         "sim1000_70_4V_midBTW_1c_L1","sim1000_70_4V_midBTW_1c_L2","sim1000_70_4V_midBTW_1c_L3","sim1000_70_4V_midBTW_1c_L4","sim1000_70_4V_midBTW_1c_L5",
         "sim500_30_4V_highBTW_1c_L1","sim500_30_4V_highBTW_1c_L2","sim500_30_4V_highBTW_1c_L3","sim500_30_4V_highBTW_1c_L4","sim500_30_4V_highBTW_1c_L5",
         "sim500_70_4V_highBTW_1c_L1","sim500_70_4V_highBTW_1c_L2","sim500_70_4V_highBTW_1c_L3","sim500_70_4V_highBTW_1c_L4","sim500_70_4V_highBTW_1c_L5",
         "sim1000_30_4V_highBTW_1c_L1","sim1000_30_4V_highBTW_1c_L2","sim1000_30_4V_highBTW_1c_L3","sim1000_30_4V_highBTW_1c_L4","sim1000_30_4V_highBTW_1c_L5",
         "sim1000_70_4V_highBTW_1c_L1","sim1000_70_4V_highBTW_1c_L2","sim1000_70_4V_highBTW_1c_L3","sim1000_70_4V_highBTW_1c_L4","sim1000_70_4V_highBTW_1c_L5")
for (nm in nms_btw) write_xlsx(get(nm), path = file.path(out_y, paste0(nm, ".xlsx")))
message("✅ 块2-BTW 完成：已保存 ", length(nms_btw), " 个BTW数据集")
}
# ==================== 块2 结束 ====================
message("✅ 4V 测试集生成完成！输出目录: ", BASE_OUT)
