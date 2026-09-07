# =============================================================================
# 4_TEST_模拟数据.r
# 基于 4_补充_模拟数据_4V.R 的 50 样本量小数据集
# 整合：纵向协变量 + 生存数据 + 生成Y
# =============================================================================

library(tidyverse)
library(MASS)
library(writexl)
library(rlang)

options(scipen = 999)

set.seed(42)
N_SAMPLE <- 50L
N_TIME   <- 5L

# 输出目录
BASE_OUT <- "F:/文章_大论文/0319大改/代码"
dir.create(BASE_OUT, showWarnings = FALSE, recursive = TRUE)


# ==================== 第一部分：纵向协变量生成 ====================

## 混合效应方程：Y_ij = (beta0 + b0i) + (beta1 + b1i) * t_ij + epsilon_ij
## beta0, beta1 为固定效应；b0i, b1i 为个体随机效应；epsilon 为残差
sim_single_class1 <- function(n = 50, n_time = 5, param_list, seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  p         <- length(param_list)
  var_names <- sapply(param_list, `[[`, "varname")
  ID    <- rep(1:n, each = n_time)
  class <- rep("c1", n * n_time)
  time  <- rep(1:n_time, times = n)
  ## 固定效应
  beta0 <- sapply(param_list, `[[`, "mean_intercept")
  beta1 <- sapply(param_list, `[[`, "mean_slope")
  noise <- sapply(param_list, `[[`, "noise_sd")
  ## 随机效应 SD（若未指定则默认为 0，即无随机效应）
  sd_b0 <- sapply(param_list, function(x) if (is.null(x$sd_intercept)) 0 else x$sd_intercept)
  sd_b1 <- sapply(param_list, function(x) if (is.null(x$sd_slope)) 0 else x$sd_slope)
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
    ## 个体随机效应：b0i ~ N(0, sd_b0^2), b1i ~ N(0, sd_b1^2)
    b0i <- rnorm(n, 0, sd_b0[v])
    b1i <- rnorm(n, 0, sd_b1[v])
    ## 混合效应：mu_ij = (beta0 + b0i) + (beta1 + b1i) * t_ij
    mu_mat <- (beta0[v] + b0i) + (beta1[v] + b1i) * t_mat
    Y_mat[, v] <- as.vector(t(mu_mat)) + rnorm(n * n_time, 0, noise[v])
  }
  data.frame(Y_mat, ID = ID, class = class, time = time, t = t_vec)
}

param_list_4v <- list(
  list(varname = "V1", mean_intercept = 0.55, mean_slope =  0.30, noise_sd = 0.03,
       sd_intercept = 0.05, sd_slope = 0.04),
  list(varname = "V2", mean_intercept = 0.70, mean_slope = -0.25, noise_sd = 0.10,
       sd_intercept = 0.08, sd_slope = 0.06),
  list(varname = "V3", mean_intercept = 0.35, mean_slope =  0.45, noise_sd = 0.04,
       sd_intercept = 0.04, sd_slope = 0.05),
  list(varname = "V4", mean_intercept = 0.80, mean_slope = -0.15, noise_sd = 0.11,
       sd_intercept = 0.07, sd_slope = 0.05)
)

message("【步骤1】生成纵向协变量 (n=", N_SAMPLE, ")...")
long_data <- sim_single_class1(n = N_SAMPLE, n_time = N_TIME, param_list = param_list_4v, seed = 42)


# ==================== 第二部分：生存数据 ====================

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

weibull_params <- list(c1 = list(shape = 1.2, scale = 2))
beta_effects   <- list(c1 = c(V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.9))

message("【步骤2】添加生存数据...")
baseline_surv <- add_surv(long_data, weibull_params, beta_effects, seed = 42, target_censor = 0.3)
surv_info <- baseline_surv %>% dplyr::select(ID, lp, surv_time, censor_time, obs_time, event)
merged_data <- long_data %>% left_join(surv_info, by = "ID")


# ==================== 第三部分：生成 Y ====================

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

k <- c(runif(2, 0.8, 1.2), rep(0.025, 2))
time_effects <- c(0.3, 1.0, 0.6, 0.4)

message("【步骤3】添加 Y...")
set.seed(42)
sim_test_50 <- add_Y(merged_data, k, time_effects)


# ==================== 第四部分：保存 ====================

out_file_xlsx <- file.path(BASE_OUT, "4_TEST_模拟数据_50样本.xlsx")
out_file_rds  <- file.path(BASE_OUT, "4_TEST_模拟数据_50样本.rds")

message("【步骤4】保存数据...")
write_xlsx(sim_test_50, path = out_file_xlsx)
saveRDS(sim_test_50, out_file_rds)

message("✅ 50样本小数据集生成完成！")
message("  - Excel: ", out_file_xlsx)
message("  - RDS:   ", out_file_rds)
message("  - 样本量: ", N_SAMPLE, " 人，", nrow(sim_test_50), " 行（纵向）")


# ==================== 第五部分：200 样本数据生成 ====================

N_SAMPLE_200 <- 200L
message("\n【步骤5】生成 200 样本数据...")
set.seed(43)
long_data_200 <- sim_single_class1(n = N_SAMPLE_200, n_time = N_TIME, param_list = param_list_4v, seed = 43)
baseline_surv_200 <- add_surv(long_data_200, weibull_params, beta_effects, seed = 43, target_censor = 0.3)
surv_info_200 <- baseline_surv_200 %>% dplyr::select(ID, lp, surv_time, censor_time, obs_time, event)
merged_data_200 <- long_data_200 %>% left_join(surv_info_200, by = "ID")
set.seed(43)
sim_test_200 <- add_Y(merged_data_200, k, time_effects)

TEST_DIR <- "F:/文章_大论文/0319大改/代码/TEST"
dir.create(TEST_DIR, showWarnings = FALSE, recursive = TRUE)
out_file_200_xlsx <- file.path(TEST_DIR, "4_TEST_模拟数据_200样本.xlsx")
out_file_200_rds  <- file.path(TEST_DIR, "4_TEST_模拟数据_200样本.rds")
write_xlsx(sim_test_200, path = out_file_200_xlsx)
saveRDS(sim_test_200, out_file_200_rds)

message("✅ 200样本数据集生成完成！")
message("  - Excel: ", out_file_200_xlsx)
message("  - RDS:   ", out_file_200_rds)
message("  - 样本量: ", N_SAMPLE_200, " 人，", nrow(sim_test_200), " 行（纵向）")
