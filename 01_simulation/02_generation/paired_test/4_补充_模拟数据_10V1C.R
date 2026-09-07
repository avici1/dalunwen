# =============================================================================
# 4_补充_模拟数据_10V1C.R
# 生成配对测试集数据（仅修改 seed），整合 数据1+数据2+数据3
# 10变量 1类别，使用 sim_single_class
# 与训练集配对：训练集 seed vs 测试集 seed + SEED_OFFSET
# =============================================================================

library(readxl)
library(tidyverse)
library(MASS)
library(writexl)
library(rlang)

options(scipen = 999)

# 测试集 seed 偏移量（与训练集完全独立）
SEED_OFFSET <- 100000L

# 输出目录（测试集）
BASE_OUT <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V1C"
dir.create(file.path(BASE_OUT, "协变量"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(BASE_OUT, "生存"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(BASE_OUT, "生成Y"), showWarnings = FALSE, recursive = TRUE)


# ==================== 协方差矩阵加载（tryCatch + diag(20) 备用）====================
COV_PATH <- "F:/文章/大论文/程序/模拟数据/相关系数矩阵/"
load_cov <- function(fname) {
  tryCatch({
    raw <- read_excel(paste0(COV_PATH, fname))
    m <- as.matrix(raw)
    if (ncol(m) == 21) m <- m[, -1]
    if (nrow(m) != 20 || ncol(m) != 20) stop("wrong dim")
    storage.mode(m) <- "numeric"
    m
  }, error = function(e) {
    message("使用 diag(20) 备用: ", fname)
    diag(20)
  })
}
cov_matrix_low    <- load_cov("cov_matrix_low1.xlsx")
cov_matrix_medium <- load_cov("cov_matrix_medium1.xlsx")
cov_matrix_high   <- load_cov("cov_matrix_high1.xlsx")
# BTW：高影响变量与噪声变量相关性高的协方差矩阵（直接套用设计的协方差矩阵）
cov_matrix_low_btw    <- load_cov("cov_matrix_low_btw1.xlsx")
cov_matrix_medium_btw <- load_cov("cov_matrix_medium_btw1.xlsx")
cov_matrix_high_btw   <- load_cov("cov_matrix_high_btw1.xlsx")


# ==================== 第一部分：数据1 - 纵向协变量生成 ====================

sim_single_class <- function(n = 200, n_time = 5, param_list, Sigma = diag(20), seed = 123) {
  if (!is.null(seed)) set.seed(seed)
  class_assign <- rep("c1", n)
  ID           <- rep(1:n, each = n_time)
  time         <- rep(1:n_time, times = n)
  class        <- rep(class_assign, each = n_time)
  re <- MASS::mvrnorm(n, mu = rep(0, 20), Sigma = Sigma)
  var_names <- sapply(param_list, `[[`, "varname")
  colnames(re) <- c(rbind(paste0("int_", var_names), paste0("slp_", var_names)))
  rand_long <- as.data.frame(re)
  rand_long$ID <- 1:n
  rand_long$class <- class_assign
  num_cols <- setdiff(names(rand_long), c("ID", "class"))
  rand_long[num_cols] <- round(rand_long[num_cols], 2)
  int_mu <- sapply(param_list, `[[`, "mean_intercept")
  slp_mu <- sapply(param_list, `[[`, "mean_slope")
  fixed_mu <- as.vector(rbind(int_mu, slp_mu))
  fixed_eff <- matrix(rep(fixed_mu, each = n), nrow = n, byrow = TRUE)
  colnames(fixed_eff) <- colnames(re)
  fixed_long <- as.data.frame(fixed_eff)
  fixed_long$ID <- 1:n
  fixed_long$class <- class_assign
  noise_sd <- sapply(param_list, `[[`, "noise_sd")
  n_id <- nrow(rand_long)
  vars <- var_names
  t_mat <- matrix(NA, nrow = n_id, ncol = n_time)
  t_mat[, 1] <- 0
  t_mat[, 2] <- runif(n_id, 0, 0.25)
  t_mat[, 3] <- runif(n_id, 0.25, 0.5)
  t_mat[, 4] <- runif(n_id, 0.5, 0.75)
  t_mat[, 5] <- runif(n_id, 0.75, 1)
  t_mat <- t(apply(t_mat, 1, sort))
  ID_vec <- rep(rand_long$ID, each = n_time)
  class_vec <- rep(rand_long$class, each = n_time)
  time_vec_seq <- rep(1:n_time, times = n_id)
  t_vec_long <- c(t(t_mat))
  Y_mat <- matrix(NA, nrow = n_id * n_time, ncol = length(vars))
  colnames(Y_mat) <- vars
  for (i in seq_along(vars)) {
    vn <- vars[i]
    rand_int <- rand_long[[paste0("int_", vn)]]
    rand_slp <- rand_long[[paste0("slp_", vn)]]
    fix_int <- fixed_long[[paste0("int_", vn)]]
    fix_slp <- fixed_long[[paste0("slp_", vn)]]
    sys <- fix_int + rand_int + (fix_slp + rand_slp) * t_mat
    sys_long <- as.vector(t(sys))
    Y_mat[, i] <- sys_long + rnorm(n_id * n_time, 0, noise_sd[i])
  }
  data.frame(Y_mat, ID = ID_vec, class = class_vec, time = time_vec_seq, t = t_vec_long)
}

put_in_list <- function(n_datasets = 200, n = 500, n_time = 5, param_list, Sigma, base_seed = 123,
                        param_set = "L1", corr_level = "low") {
  result_list <- list()
  for (i in 1:n_datasets) {
    dataset <- sim_single_class(n = n, n_time = n_time, param_list = param_list,
                                Sigma = Sigma, seed = base_seed + i)
    list_name <- paste0("sim_data_", n, "_", corr_level, "_", param_set, "_", i)
    result_list[[list_name]] <- dataset
  }
  result_list
}

# param_list (10变量，来自 4_模拟数据1_数据生成_V10C1.R)
param_list_1 <- list(
  list(varname = "V1", mean_intercept = 0.55, mean_slope = 0.35, noise_sd = 0.03),
  list(varname = "V2", mean_intercept = 0.70, mean_slope = -0.25, noise_sd = 0.12),
  list(varname = "V3", mean_intercept = 0.30, mean_slope = 0.60, noise_sd = 0.04),
  list(varname = "V4", mean_intercept = 0.85, mean_slope = -0.15, noise_sd = 0.115),
  list(varname = "V5", mean_intercept = 0.45, mean_slope = -0.45, noise_sd = 0.025),
  list(varname = "V6", mean_intercept = 0.65, mean_slope = 0.20, noise_sd = 0.11),
  list(varname = "V7", mean_intercept = 0.75, mean_slope = -0.30, noise_sd = 0.035),
  list(varname = "V8", mean_intercept = 0.90, mean_slope = 0.40, noise_sd = 0.045),
  list(varname = "V9", mean_intercept = 0.25, mean_slope = -0.20, noise_sd = 0.022),
  list(varname = "V10", mean_intercept = 0.50, mean_slope = 0.25, noise_sd = 0.128)
)
param_list_2 <- list(
  list(varname = "V1", mean_intercept = 0.60, mean_slope = 0.30, noise_sd = 0.035),
  list(varname = "V2", mean_intercept = 0.65, mean_slope = -0.20, noise_sd = 0.11),
  list(varname = "V3", mean_intercept = 0.35, mean_slope = 0.55, noise_sd = 0.045),
  list(varname = "V4", mean_intercept = 0.80, mean_slope = -0.12, noise_sd = 0.105),
  list(varname = "V5", mean_intercept = 0.50, mean_slope = -0.40, noise_sd = 0.03),
  list(varname = "V6", mean_intercept = 0.70, mean_slope = 0.15, noise_sd = 0.095),
  list(varname = "V7", mean_intercept = 0.80, mean_slope = -0.25, noise_sd = 0.04),
  list(varname = "V8", mean_intercept = 0.85, mean_slope = 0.35, noise_sd = 0.05),
  list(varname = "V9", mean_intercept = 0.30, mean_slope = -0.18, noise_sd = 0.025),
  list(varname = "V10", mean_intercept = 0.55, mean_slope = 0.20, noise_sd = 0.115)
)
param_list_3 <- list(
  list(varname = "V1", mean_intercept = 0.50, mean_slope = 0.40, noise_sd = 0.028),
  list(varname = "V2", mean_intercept = 0.75, mean_slope = -0.30, noise_sd = 0.125),
  list(varname = "V3", mean_intercept = 0.25, mean_slope = 0.65, noise_sd = 0.038),
  list(varname = "V4", mean_intercept = 0.90, mean_slope = -0.18, noise_sd = 0.098),
  list(varname = "V5", mean_intercept = 0.40, mean_slope = -0.50, noise_sd = 0.022),
  list(varname = "V6", mean_intercept = 0.60, mean_slope = 0.25, noise_sd = 0.108),
  list(varname = "V7", mean_intercept = 0.70, mean_slope = -0.35, noise_sd = 0.042),
  list(varname = "V8", mean_intercept = 0.95, mean_slope = 0.30, noise_sd = 0.055),
  list(varname = "V9", mean_intercept = 0.20, mean_slope = -0.22, noise_sd = 0.018),
  list(varname = "V10", mean_intercept = 0.45, mean_slope = 0.30, noise_sd = 0.122)
)
param_list_4 <- list(
  list(varname = "V1", mean_intercept = 0.65, mean_slope = 0.25, noise_sd = 0.032),
  list(varname = "V2", mean_intercept = 0.60, mean_slope = -0.35, noise_sd = 0.118),
  list(varname = "V3", mean_intercept = 0.40, mean_slope = 0.50, noise_sd = 0.042),
  list(varname = "V4", mean_intercept = 0.75, mean_slope = -0.20, noise_sd = 0.112),
  list(varname = "V5", mean_intercept = 0.35, mean_slope = -0.45, noise_sd = 0.028),
  list(varname = "V6", mean_intercept = 0.75, mean_slope = 0.18, noise_sd = 0.102),
  list(varname = "V7", mean_intercept = 0.85, mean_slope = -0.28, noise_sd = 0.038),
  list(varname = "V8", mean_intercept = 0.80, mean_slope = 0.45, noise_sd = 0.048),
  list(varname = "V9", mean_intercept = 0.35, mean_slope = -0.15, noise_sd = 0.03),
  list(varname = "V10", mean_intercept = 0.60, mean_slope = 0.22, noise_sd = 0.135)
)
param_list_5 <- list(
  list(varname = "V1", mean_intercept = 0.45, mean_slope = 0.45, noise_sd = 0.026),
  list(varname = "V2", mean_intercept = 0.80, mean_slope = -0.22, noise_sd = 0.108),
  list(varname = "V3", mean_intercept = 0.32, mean_slope = 0.58, noise_sd = 0.036),
  list(varname = "V4", mean_intercept = 0.88, mean_slope = -0.14, noise_sd = 0.092),
  list(varname = "V5", mean_intercept = 0.42, mean_slope = -0.48, noise_sd = 0.02),
  list(varname = "V6", mean_intercept = 0.68, mean_slope = 0.22, noise_sd = 0.098),
  list(varname = "V7", mean_intercept = 0.78, mean_slope = -0.32, noise_sd = 0.032),
  list(varname = "V8", mean_intercept = 0.92, mean_slope = 0.38, noise_sd = 0.052),
  list(varname = "V9", mean_intercept = 0.28, mean_slope = -0.25, noise_sd = 0.015),
  list(varname = "V10", mean_intercept = 0.52, mean_slope = 0.28, noise_sd = 0.118)
)

message("【步骤1】生成纵向协变量（测试集 seed = 原seed + ", SEED_OFFSET, "）...")
SO <- as.integer(SEED_OFFSET)
sim_data_500_low_c1_L1   <- put_in_list(200, 500, 5, param_list_1, cov_matrix_low, 123L + SO, "L1", "low")
sim_data_500_low_c1_L2   <- put_in_list(200, 500, 5, param_list_2, cov_matrix_low, 124L + SO, "L2", "low")
sim_data_500_low_c1_L3   <- put_in_list(200, 500, 5, param_list_3, cov_matrix_low, 125L + SO, "L3", "low")
sim_data_500_low_c1_L4   <- put_in_list(200, 500, 5, param_list_4, cov_matrix_low, 126L + SO, "L4", "low")
sim_data_500_low_c1_L5   <- put_in_list(200, 500, 5, param_list_5, cov_matrix_low, 127L + SO, "L5", "low")
sim_data_500_medium_c1_L1 <- put_in_list(200, 500, 5, param_list_1, cov_matrix_medium, 128L + SO, "L1", "medium")
sim_data_500_medium_c1_L2 <- put_in_list(200, 500, 5, param_list_2, cov_matrix_medium, 129L + SO, "L2", "medium")
sim_data_500_medium_c1_L3 <- put_in_list(200, 500, 5, param_list_3, cov_matrix_medium, 130L + SO, "L3", "medium")
sim_data_500_medium_c1_L4 <- put_in_list(200, 500, 5, param_list_4, cov_matrix_medium, 131L + SO, "L4", "medium")
sim_data_500_medium_c1_L5 <- put_in_list(200, 500, 5, param_list_5, cov_matrix_medium, 132L + SO, "L5", "medium")
sim_data_500_high_c1_L1  <- put_in_list(200, 500, 5, param_list_1, cov_matrix_high, 133L + SO, "L1", "high")
sim_data_500_high_c1_L2  <- put_in_list(200, 500, 5, param_list_2, cov_matrix_high, 134L + SO, "L2", "high")
sim_data_500_high_c1_L3  <- put_in_list(200, 500, 5, param_list_3, cov_matrix_high, 135L + SO, "L3", "high")
sim_data_500_high_c1_L4  <- put_in_list(200, 500, 5, param_list_4, cov_matrix_high, 136L + SO, "L4", "high")
sim_data_500_high_c1_L5  <- put_in_list(200, 500, 5, param_list_5, cov_matrix_high, 137L + SO, "L5", "high")
sim_data_1000_low_c1_L1  <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_low, 138L + SO, "L1", "low")
sim_data_1000_low_c1_L2  <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_low, 139L + SO, "L2", "low")
sim_data_1000_low_c1_L3  <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_low, 140L + SO, "L3", "low")
sim_data_1000_low_c1_L4  <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_low, 141L + SO, "L4", "low")
sim_data_1000_low_c1_L5  <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_low, 142L + SO, "L5", "low")
sim_data_1000_medium_c1_L1 <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_medium, 143L + SO, "L1", "medium")
sim_data_1000_medium_c1_L2 <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_medium, 144L + SO, "L2", "medium")
sim_data_1000_medium_c1_L3 <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_medium, 145L + SO, "L3", "medium")
sim_data_1000_medium_c1_L4 <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_medium, 146L + SO, "L4", "medium")
sim_data_1000_medium_c1_L5 <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_medium, 147L + SO, "L5", "medium")
sim_data_1000_high_c1_L1  <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_high, 148L + SO, "L1", "high")
sim_data_1000_high_c1_L2  <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_high, 149L + SO, "L2", "high")
sim_data_1000_high_c1_L3  <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_high, 150L + SO, "L3", "high")
sim_data_1000_high_c1_L4  <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_high, 151L + SO, "L4", "high")
sim_data_1000_high_c1_L5  <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_high, 152L + SO, "L5", "high")

## BTW 纵向数据（协方差矩阵：高影响变量与噪声变量相关性高）
message("【步骤1-BTW】生成 BTW 纵向协变量（协方差矩阵：高影响与噪声变量相关高）...")
sim_data_500_low_btw_c1_L1   <- put_in_list(200, 500, 5, param_list_1, cov_matrix_low_btw, 153L + SO, "L1", "low")
sim_data_500_low_btw_c1_L2   <- put_in_list(200, 500, 5, param_list_2, cov_matrix_low_btw, 154L + SO, "L2", "low")
sim_data_500_low_btw_c1_L3   <- put_in_list(200, 500, 5, param_list_3, cov_matrix_low_btw, 155L + SO, "L3", "low")
sim_data_500_low_btw_c1_L4   <- put_in_list(200, 500, 5, param_list_4, cov_matrix_low_btw, 156L + SO, "L4", "low")
sim_data_500_low_btw_c1_L5   <- put_in_list(200, 500, 5, param_list_5, cov_matrix_low_btw, 157L + SO, "L5", "low")
sim_data_500_medium_btw_c1_L1 <- put_in_list(200, 500, 5, param_list_1, cov_matrix_medium_btw, 158L + SO, "L1", "medium")
sim_data_500_medium_btw_c1_L2 <- put_in_list(200, 500, 5, param_list_2, cov_matrix_medium_btw, 159L + SO, "L2", "medium")
sim_data_500_medium_btw_c1_L3 <- put_in_list(200, 500, 5, param_list_3, cov_matrix_medium_btw, 160L + SO, "L3", "medium")
sim_data_500_medium_btw_c1_L4 <- put_in_list(200, 500, 5, param_list_4, cov_matrix_medium_btw, 161L + SO, "L4", "medium")
sim_data_500_medium_btw_c1_L5 <- put_in_list(200, 500, 5, param_list_5, cov_matrix_medium_btw, 162L + SO, "L5", "medium")
sim_data_500_high_btw_c1_L1  <- put_in_list(200, 500, 5, param_list_1, cov_matrix_high_btw, 163L + SO, "L1", "high")
sim_data_500_high_btw_c1_L2  <- put_in_list(200, 500, 5, param_list_2, cov_matrix_high_btw, 164L + SO, "L2", "high")
sim_data_500_high_btw_c1_L3  <- put_in_list(200, 500, 5, param_list_3, cov_matrix_high_btw, 165L + SO, "L3", "high")
sim_data_500_high_btw_c1_L4  <- put_in_list(200, 500, 5, param_list_4, cov_matrix_high_btw, 166L + SO, "L4", "high")
sim_data_500_high_btw_c1_L5  <- put_in_list(200, 500, 5, param_list_5, cov_matrix_high_btw, 167L + SO, "L5", "high")
sim_data_1000_low_btw_c1_L1  <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_low_btw, 168L + SO, "L1", "low")
sim_data_1000_low_btw_c1_L2  <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_low_btw, 169L + SO, "L2", "low")
sim_data_1000_low_btw_c1_L3  <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_low_btw, 170L + SO, "L3", "low")
sim_data_1000_low_btw_c1_L4  <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_low_btw, 171L + SO, "L4", "low")
sim_data_1000_low_btw_c1_L5  <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_low_btw, 172L + SO, "L5", "low")
sim_data_1000_medium_btw_c1_L1 <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_medium_btw, 173L + SO, "L1", "medium")
sim_data_1000_medium_btw_c1_L2 <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_medium_btw, 174L + SO, "L2", "medium")
sim_data_1000_medium_btw_c1_L3 <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_medium_btw, 175L + SO, "L3", "medium")
sim_data_1000_medium_btw_c1_L4 <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_medium_btw, 176L + SO, "L4", "medium")
sim_data_1000_medium_btw_c1_L5 <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_medium_btw, 177L + SO, "L5", "medium")
sim_data_1000_high_btw_c1_L1  <- put_in_list(200, 1000, 5, param_list_1, cov_matrix_high_btw, 178L + SO, "L1", "high")
sim_data_1000_high_btw_c1_L2  <- put_in_list(200, 1000, 5, param_list_2, cov_matrix_high_btw, 179L + SO, "L2", "high")
sim_data_1000_high_btw_c1_L3  <- put_in_list(200, 1000, 5, param_list_3, cov_matrix_high_btw, 180L + SO, "L3", "high")
sim_data_1000_high_btw_c1_L4  <- put_in_list(200, 1000, 5, param_list_4, cov_matrix_high_btw, 181L + SO, "L4", "high")
sim_data_1000_high_btw_c1_L5  <- put_in_list(200, 1000, 5, param_list_5, cov_matrix_high_btw, 182L + SO, "L5", "high")


# ==================== 第二部分：数据2 - 生存数据 ====================

add_surv <- function(data, weibull_params, beta_effects, var_pattern = "^V\\d+$",
                    id_col = "ID", class_col = "class", seed = 123,
                    censor_dist = c("exponential", "uniform"), target_censor = NULL,
                    tol = 1e-4, max_iter = 50) {
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

weibull_params <- list(c1 = list(shape = 1, scale = 2))
beta_effects <- list(c1 = c(V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.9, V5 = 0.65, V6 = -0.75,
                            V7 = 0.05, V8 = -0.03, V9 = 0.08, V10 = -0.04))
# BTW：变量与噪声相关性高，V6 系数弱化（参考 4_转换单一时间点数据.R）
beta_effect_noise <- list(c1 = c(V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.9, V5 = 0.65, V6 = -0.07,
                                 V7 = 0.05, V8 = -0.03, V9 = 0.08, V10 = -0.04))

message("【步骤2】添加生存数据（seed_start + ", SEED_OFFSET, "）...")
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
sim500_30_10V_lowINTER_1c_L1  <- longer_data(s30_1, sim_data_500_low_c1_L1)
sim500_30_10V_lowINTER_1c_L2  <- longer_data(s30_2, sim_data_500_low_c1_L2)
sim500_30_10V_lowINTER_1c_L3  <- longer_data(s30_3, sim_data_500_low_c1_L3)
sim500_30_10V_lowINTER_1c_L4  <- longer_data(s30_4, sim_data_500_low_c1_L4)
sim500_30_10V_lowINTER_1c_L5  <- longer_data(s30_5, sim_data_500_low_c1_L5)
sim500_70_10V_lowINTER_1c_L1  <- longer_data(s70_1, sim_data_500_low_c1_L1)
sim500_70_10V_lowINTER_1c_L2  <- longer_data(s70_2, sim_data_500_low_c1_L2)
sim500_70_10V_lowINTER_1c_L3  <- longer_data(s70_3, sim_data_500_low_c1_L3)
sim500_70_10V_lowINTER_1c_L4  <- longer_data(s70_4, sim_data_500_low_c1_L4)
sim500_70_10V_lowINTER_1c_L5  <- longer_data(s70_5, sim_data_500_low_c1_L5)
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
sim1000_30_10V_lowINTER_1c_L1 <- longer_data(s30_1k_1, sim_data_1000_low_c1_L1)
sim1000_30_10V_lowINTER_1c_L2 <- longer_data(s30_1k_2, sim_data_1000_low_c1_L2)
sim1000_30_10V_lowINTER_1c_L3 <- longer_data(s30_1k_3, sim_data_1000_low_c1_L3)
sim1000_30_10V_lowINTER_1c_L4 <- longer_data(s30_1k_4, sim_data_1000_low_c1_L4)
sim1000_30_10V_lowINTER_1c_L5 <- longer_data(s30_1k_5, sim_data_1000_low_c1_L5)
sim1000_70_10V_lowINTER_1c_L1 <- longer_data(s70_1k_1, sim_data_1000_low_c1_L1)
sim1000_70_10V_lowINTER_1c_L2 <- longer_data(s70_1k_2, sim_data_1000_low_c1_L2)
sim1000_70_10V_lowINTER_1c_L3 <- longer_data(s70_1k_3, sim_data_1000_low_c1_L3)
sim1000_70_10V_lowINTER_1c_L4 <- longer_data(s70_1k_4, sim_data_1000_low_c1_L4)
sim1000_70_10V_lowINTER_1c_L5 <- longer_data(s70_1k_5, sim_data_1000_low_c1_L5)
sm30_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effects, 20500000L + seed_off, "uniform", 0.3)
sm30_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effects, 20510000L + seed_off, "uniform", 0.3)
sm30_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effects, 20520000L + seed_off, "uniform", 0.3)
sm30_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effects, 20530000L + seed_off, "uniform", 0.3)
sm30_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effects, 20540000L + seed_off, "uniform", 0.3)
sm70_1 <- merge_data(sim_data_500_medium_c1_L1, weibull_params, beta_effects, 20550000L + seed_off, "uniform", 0.7)
sm70_2 <- merge_data(sim_data_500_medium_c1_L2, weibull_params, beta_effects, 20560000L + seed_off, "uniform", 0.7)
sm70_3 <- merge_data(sim_data_500_medium_c1_L3, weibull_params, beta_effects, 20570000L + seed_off, "uniform", 0.7)
sm70_4 <- merge_data(sim_data_500_medium_c1_L4, weibull_params, beta_effects, 20580000L + seed_off, "uniform", 0.7)
sm70_5 <- merge_data(sim_data_500_medium_c1_L5, weibull_params, beta_effects, 20590000L + seed_off, "uniform", 0.7)
sim500_30_10V_midINTER_1c_L1  <- longer_data(sm30_1, sim_data_500_medium_c1_L1)
sim500_30_10V_midINTER_1c_L2  <- longer_data(sm30_2, sim_data_500_medium_c1_L2)
sim500_30_10V_midINTER_1c_L3  <- longer_data(sm30_3, sim_data_500_medium_c1_L3)
sim500_30_10V_midINTER_1c_L4  <- longer_data(sm30_4, sim_data_500_medium_c1_L4)
sim500_30_10V_midINTER_1c_L5  <- longer_data(sm30_5, sim_data_500_medium_c1_L5)
sim500_70_10V_midINTER_1c_L1  <- longer_data(sm70_1, sim_data_500_medium_c1_L1)
sim500_70_10V_midINTER_1c_L2  <- longer_data(sm70_2, sim_data_500_medium_c1_L2)
sim500_70_10V_midINTER_1c_L3  <- longer_data(sm70_3, sim_data_500_medium_c1_L3)
sim500_70_10V_midINTER_1c_L4  <- longer_data(sm70_4, sim_data_500_medium_c1_L4)
sim500_70_10V_midINTER_1c_L5  <- longer_data(sm70_5, sim_data_500_medium_c1_L5)
sm30_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effects, 20700000L + seed_off, "uniform", 0.3)
sm30_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effects, 20710000L + seed_off, "uniform", 0.3)
sm30_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effects, 20720000L + seed_off, "uniform", 0.3)
sm30_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effects, 20730000L + seed_off, "uniform", 0.3)
sm30_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effects, 20740000L + seed_off, "uniform", 0.3)
sm70_1k_1 <- merge_data(sim_data_1000_medium_c1_L1, weibull_params, beta_effects, 20750000L + seed_off, "uniform", 0.7)
sm70_1k_2 <- merge_data(sim_data_1000_medium_c1_L2, weibull_params, beta_effects, 20760000L + seed_off, "uniform", 0.7)
sm70_1k_3 <- merge_data(sim_data_1000_medium_c1_L3, weibull_params, beta_effects, 20770000L + seed_off, "uniform", 0.7)
sm70_1k_4 <- merge_data(sim_data_1000_medium_c1_L4, weibull_params, beta_effects, 20780000L + seed_off, "uniform", 0.7)
sm70_1k_5 <- merge_data(sim_data_1000_medium_c1_L5, weibull_params, beta_effects, 20790000L + seed_off, "uniform", 0.7)
sim1000_30_10V_midINTER_1c_L1 <- longer_data(sm30_1k_1, sim_data_1000_medium_c1_L1)
sim1000_30_10V_midINTER_1c_L2 <- longer_data(sm30_1k_2, sim_data_1000_medium_c1_L2)
sim1000_30_10V_midINTER_1c_L3 <- longer_data(sm30_1k_3, sim_data_1000_medium_c1_L3)
sim1000_30_10V_midINTER_1c_L4 <- longer_data(sm30_1k_4, sim_data_1000_medium_c1_L4)
sim1000_30_10V_midINTER_1c_L5 <- longer_data(sm30_1k_5, sim_data_1000_medium_c1_L5)
sim1000_70_10V_midINTER_1c_L1 <- longer_data(sm70_1k_1, sim_data_1000_medium_c1_L1)
sim1000_70_10V_midINTER_1c_L2 <- longer_data(sm70_1k_2, sim_data_1000_medium_c1_L2)
sim1000_70_10V_midINTER_1c_L3 <- longer_data(sm70_1k_3, sim_data_1000_medium_c1_L3)
sim1000_70_10V_midINTER_1c_L4 <- longer_data(sm70_1k_4, sim_data_1000_medium_c1_L4)
sim1000_70_10V_midINTER_1c_L5 <- longer_data(sm70_1k_5, sim_data_1000_medium_c1_L5)
sh30_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effects, 20900000L + seed_off, "uniform", 0.3)
sh30_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effects, 20910000L + seed_off, "uniform", 0.3)
sh30_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effects, 20920000L + seed_off, "uniform", 0.3)
sh30_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effects, 20930000L + seed_off, "uniform", 0.3)
sh30_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effects, 20940000L + seed_off, "uniform", 0.3)
sh70_1 <- merge_data(sim_data_500_high_c1_L1, weibull_params, beta_effects, 20950000L + seed_off, "uniform", 0.7)
sh70_2 <- merge_data(sim_data_500_high_c1_L2, weibull_params, beta_effects, 20960000L + seed_off, "uniform", 0.7)
sh70_3 <- merge_data(sim_data_500_high_c1_L3, weibull_params, beta_effects, 20970000L + seed_off, "uniform", 0.7)
sh70_4 <- merge_data(sim_data_500_high_c1_L4, weibull_params, beta_effects, 20980000L + seed_off, "uniform", 0.7)
sh70_5 <- merge_data(sim_data_500_high_c1_L5, weibull_params, beta_effects, 20990000L + seed_off, "uniform", 0.7)
sim500_30_10V_highINTER_1c_L1  <- longer_data(sh30_1, sim_data_500_high_c1_L1)
sim500_30_10V_highINTER_1c_L2  <- longer_data(sh30_2, sim_data_500_high_c1_L2)
sim500_30_10V_highINTER_1c_L3  <- longer_data(sh30_3, sim_data_500_high_c1_L3)
sim500_30_10V_highINTER_1c_L4  <- longer_data(sh30_4, sim_data_500_high_c1_L4)
sim500_30_10V_highINTER_1c_L5  <- longer_data(sh30_5, sim_data_500_high_c1_L5)
sim500_70_10V_highINTER_1c_L1  <- longer_data(sh70_1, sim_data_500_high_c1_L1)
sim500_70_10V_highINTER_1c_L2  <- longer_data(sh70_2, sim_data_500_high_c1_L2)
sim500_70_10V_highINTER_1c_L3  <- longer_data(sh70_3, sim_data_500_high_c1_L3)
sim500_70_10V_highINTER_1c_L4  <- longer_data(sh70_4, sim_data_500_high_c1_L4)
sim500_70_10V_highINTER_1c_L5  <- longer_data(sh70_5, sim_data_500_high_c1_L5)
sh30_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effects, 21100000L + seed_off, "uniform", 0.3)
sh30_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effects, 21110000L + seed_off, "uniform", 0.3)
sh30_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effects, 21120000L + seed_off, "uniform", 0.3)
sh30_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effects, 21130000L + seed_off, "uniform", 0.3)
sh30_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effects, 21140000L + seed_off, "uniform", 0.3)
sh70_1k_1 <- merge_data(sim_data_1000_high_c1_L1, weibull_params, beta_effects, 21150000L + seed_off, "uniform", 0.7)
sh70_1k_2 <- merge_data(sim_data_1000_high_c1_L2, weibull_params, beta_effects, 21160000L + seed_off, "uniform", 0.7)
sh70_1k_3 <- merge_data(sim_data_1000_high_c1_L3, weibull_params, beta_effects, 21170000L + seed_off, "uniform", 0.7)
sh70_1k_4 <- merge_data(sim_data_1000_high_c1_L4, weibull_params, beta_effects, 21180000L + seed_off, "uniform", 0.7)
sh70_1k_5 <- merge_data(sim_data_1000_high_c1_L5, weibull_params, beta_effects, 21190000L + seed_off, "uniform", 0.7)
sim1000_30_10V_highINTER_1c_L1 <- longer_data(sh30_1k_1, sim_data_1000_high_c1_L1)
sim1000_30_10V_highINTER_1c_L2 <- longer_data(sh30_1k_2, sim_data_1000_high_c1_L2)
sim1000_30_10V_highINTER_1c_L3 <- longer_data(sh30_1k_3, sim_data_1000_high_c1_L3)
sim1000_30_10V_highINTER_1c_L4 <- longer_data(sh30_1k_4, sim_data_1000_high_c1_L4)
sim1000_30_10V_highINTER_1c_L5 <- longer_data(sh30_1k_5, sim_data_1000_high_c1_L5)
sim1000_70_10V_highINTER_1c_L1 <- longer_data(sh70_1k_1, sim_data_1000_high_c1_L1)
sim1000_70_10V_highINTER_1c_L2 <- longer_data(sh70_1k_2, sim_data_1000_high_c1_L2)
sim1000_70_10V_highINTER_1c_L3 <- longer_data(sh70_1k_3, sim_data_1000_high_c1_L3)
sim1000_70_10V_highINTER_1c_L4 <- longer_data(sh70_1k_4, sim_data_1000_high_c1_L4)
sim1000_70_10V_highINTER_1c_L5 <- longer_data(sh70_1k_5, sim_data_1000_high_c1_L5)

## BTW 生存数据（beta_effect_noise，高影响变量与噪声变量相关高）
message("【步骤2-BTW】添加 BTW 生存数据（beta_effect_noise）...")
sb30_1 <- merge_data(sim_data_500_low_btw_c1_L1, weibull_params, beta_effect_noise, 21200000L + seed_off, "uniform", 0.3)
sb30_2 <- merge_data(sim_data_500_low_btw_c1_L2, weibull_params, beta_effect_noise, 21210000L + seed_off, "uniform", 0.3)
sb30_3 <- merge_data(sim_data_500_low_btw_c1_L3, weibull_params, beta_effect_noise, 21220000L + seed_off, "uniform", 0.3)
sb30_4 <- merge_data(sim_data_500_low_btw_c1_L4, weibull_params, beta_effect_noise, 21230000L + seed_off, "uniform", 0.3)
sb30_5 <- merge_data(sim_data_500_low_btw_c1_L5, weibull_params, beta_effect_noise, 21240000L + seed_off, "uniform", 0.3)
sb70_1 <- merge_data(sim_data_500_low_btw_c1_L1, weibull_params, beta_effect_noise, 21250000L + seed_off, "uniform", 0.7)
sb70_2 <- merge_data(sim_data_500_low_btw_c1_L2, weibull_params, beta_effect_noise, 21260000L + seed_off, "uniform", 0.7)
sb70_3 <- merge_data(sim_data_500_low_btw_c1_L3, weibull_params, beta_effect_noise, 21270000L + seed_off, "uniform", 0.7)
sb70_4 <- merge_data(sim_data_500_low_btw_c1_L4, weibull_params, beta_effect_noise, 21280000L + seed_off, "uniform", 0.7)
sb70_5 <- merge_data(sim_data_500_low_btw_c1_L5, weibull_params, beta_effect_noise, 21290000L + seed_off, "uniform", 0.7)
sim500_30_10V_lowBTW_1c_L1  <- longer_data(sb30_1, sim_data_500_low_btw_c1_L1)
sim500_30_10V_lowBTW_1c_L2  <- longer_data(sb30_2, sim_data_500_low_btw_c1_L2)
sim500_30_10V_lowBTW_1c_L3  <- longer_data(sb30_3, sim_data_500_low_btw_c1_L3)
sim500_30_10V_lowBTW_1c_L4  <- longer_data(sb30_4, sim_data_500_low_btw_c1_L4)
sim500_30_10V_lowBTW_1c_L5  <- longer_data(sb30_5, sim_data_500_low_btw_c1_L5)
sim500_70_10V_lowBTW_1c_L1  <- longer_data(sb70_1, sim_data_500_low_btw_c1_L1)
sim500_70_10V_lowBTW_1c_L2  <- longer_data(sb70_2, sim_data_500_low_btw_c1_L2)
sim500_70_10V_lowBTW_1c_L3  <- longer_data(sb70_3, sim_data_500_low_btw_c1_L3)
sim500_70_10V_lowBTW_1c_L4  <- longer_data(sb70_4, sim_data_500_low_btw_c1_L4)
sim500_70_10V_lowBTW_1c_L5  <- longer_data(sb70_5, sim_data_500_low_btw_c1_L5)
sb30_1k_1 <- merge_data(sim_data_1000_low_btw_c1_L1, weibull_params, beta_effect_noise, 21400000L + seed_off, "uniform", 0.3)
sb30_1k_2 <- merge_data(sim_data_1000_low_btw_c1_L2, weibull_params, beta_effect_noise, 21410000L + seed_off, "uniform", 0.3)
sb30_1k_3 <- merge_data(sim_data_1000_low_btw_c1_L3, weibull_params, beta_effect_noise, 21420000L + seed_off, "uniform", 0.3)
sb30_1k_4 <- merge_data(sim_data_1000_low_btw_c1_L4, weibull_params, beta_effect_noise, 21430000L + seed_off, "uniform", 0.3)
sb30_1k_5 <- merge_data(sim_data_1000_low_btw_c1_L5, weibull_params, beta_effect_noise, 21440000L + seed_off, "uniform", 0.3)
sb70_1k_1 <- merge_data(sim_data_1000_low_btw_c1_L1, weibull_params, beta_effect_noise, 21450000L + seed_off, "uniform", 0.7)
sb70_1k_2 <- merge_data(sim_data_1000_low_btw_c1_L2, weibull_params, beta_effect_noise, 21460000L + seed_off, "uniform", 0.7)
sb70_1k_3 <- merge_data(sim_data_1000_low_btw_c1_L3, weibull_params, beta_effect_noise, 21470000L + seed_off, "uniform", 0.7)
sb70_1k_4 <- merge_data(sim_data_1000_low_btw_c1_L4, weibull_params, beta_effect_noise, 21480000L + seed_off, "uniform", 0.7)
sb70_1k_5 <- merge_data(sim_data_1000_low_btw_c1_L5, weibull_params, beta_effect_noise, 21490000L + seed_off, "uniform", 0.7)
sim1000_30_10V_lowBTW_1c_L1 <- longer_data(sb30_1k_1, sim_data_1000_low_btw_c1_L1)
sim1000_30_10V_lowBTW_1c_L2 <- longer_data(sb30_1k_2, sim_data_1000_low_btw_c1_L2)
sim1000_30_10V_lowBTW_1c_L3 <- longer_data(sb30_1k_3, sim_data_1000_low_btw_c1_L3)
sim1000_30_10V_lowBTW_1c_L4 <- longer_data(sb30_1k_4, sim_data_1000_low_btw_c1_L4)
sim1000_30_10V_lowBTW_1c_L5 <- longer_data(sb30_1k_5, sim_data_1000_low_btw_c1_L5)
sim1000_70_10V_lowBTW_1c_L1 <- longer_data(sb70_1k_1, sim_data_1000_low_btw_c1_L1)
sim1000_70_10V_lowBTW_1c_L2 <- longer_data(sb70_1k_2, sim_data_1000_low_btw_c1_L2)
sim1000_70_10V_lowBTW_1c_L3 <- longer_data(sb70_1k_3, sim_data_1000_low_btw_c1_L3)
sim1000_70_10V_lowBTW_1c_L4 <- longer_data(sb70_1k_4, sim_data_1000_low_btw_c1_L4)
sim1000_70_10V_lowBTW_1c_L5 <- longer_data(sb70_1k_5, sim_data_1000_low_btw_c1_L5)
sbm30_1 <- merge_data(sim_data_500_medium_btw_c1_L1, weibull_params, beta_effect_noise, 21600000L + seed_off, "uniform", 0.3)
sbm30_2 <- merge_data(sim_data_500_medium_btw_c1_L2, weibull_params, beta_effect_noise, 21610000L + seed_off, "uniform", 0.3)
sbm30_3 <- merge_data(sim_data_500_medium_btw_c1_L3, weibull_params, beta_effect_noise, 21620000L + seed_off, "uniform", 0.3)
sbm30_4 <- merge_data(sim_data_500_medium_btw_c1_L4, weibull_params, beta_effect_noise, 21630000L + seed_off, "uniform", 0.3)
sbm30_5 <- merge_data(sim_data_500_medium_btw_c1_L5, weibull_params, beta_effect_noise, 21640000L + seed_off, "uniform", 0.3)
sbm70_1 <- merge_data(sim_data_500_medium_btw_c1_L1, weibull_params, beta_effect_noise, 21650000L + seed_off, "uniform", 0.7)
sbm70_2 <- merge_data(sim_data_500_medium_btw_c1_L2, weibull_params, beta_effect_noise, 21660000L + seed_off, "uniform", 0.7)
sbm70_3 <- merge_data(sim_data_500_medium_btw_c1_L3, weibull_params, beta_effect_noise, 21670000L + seed_off, "uniform", 0.7)
sbm70_4 <- merge_data(sim_data_500_medium_btw_c1_L4, weibull_params, beta_effect_noise, 21680000L + seed_off, "uniform", 0.7)
sbm70_5 <- merge_data(sim_data_500_medium_btw_c1_L5, weibull_params, beta_effect_noise, 21690000L + seed_off, "uniform", 0.7)
sim500_30_10V_midBTW_1c_L1  <- longer_data(sbm30_1, sim_data_500_medium_btw_c1_L1)
sim500_30_10V_midBTW_1c_L2  <- longer_data(sbm30_2, sim_data_500_medium_btw_c1_L2)
sim500_30_10V_midBTW_1c_L3  <- longer_data(sbm30_3, sim_data_500_medium_btw_c1_L3)
sim500_30_10V_midBTW_1c_L4  <- longer_data(sbm30_4, sim_data_500_medium_btw_c1_L4)
sim500_30_10V_midBTW_1c_L5  <- longer_data(sbm30_5, sim_data_500_medium_btw_c1_L5)
sim500_70_10V_midBTW_1c_L1  <- longer_data(sbm70_1, sim_data_500_medium_btw_c1_L1)
sim500_70_10V_midBTW_1c_L2  <- longer_data(sbm70_2, sim_data_500_medium_btw_c1_L2)
sim500_70_10V_midBTW_1c_L3  <- longer_data(sbm70_3, sim_data_500_medium_btw_c1_L3)
sim500_70_10V_midBTW_1c_L4  <- longer_data(sbm70_4, sim_data_500_medium_btw_c1_L4)
sim500_70_10V_midBTW_1c_L5  <- longer_data(sbm70_5, sim_data_500_medium_btw_c1_L5)
sbm30_1k_1 <- merge_data(sim_data_1000_medium_btw_c1_L1, weibull_params, beta_effect_noise, 21800000L + seed_off, "uniform", 0.3)
sbm30_1k_2 <- merge_data(sim_data_1000_medium_btw_c1_L2, weibull_params, beta_effect_noise, 21810000L + seed_off, "uniform", 0.3)
sbm30_1k_3 <- merge_data(sim_data_1000_medium_btw_c1_L3, weibull_params, beta_effect_noise, 21820000L + seed_off, "uniform", 0.3)
sbm30_1k_4 <- merge_data(sim_data_1000_medium_btw_c1_L4, weibull_params, beta_effect_noise, 21830000L + seed_off, "uniform", 0.3)
sbm30_1k_5 <- merge_data(sim_data_1000_medium_btw_c1_L5, weibull_params, beta_effect_noise, 21840000L + seed_off, "uniform", 0.3)
sbm70_1k_1 <- merge_data(sim_data_1000_medium_btw_c1_L1, weibull_params, beta_effect_noise, 21850000L + seed_off, "uniform", 0.7)
sbm70_1k_2 <- merge_data(sim_data_1000_medium_btw_c1_L2, weibull_params, beta_effect_noise, 21860000L + seed_off, "uniform", 0.7)
sbm70_1k_3 <- merge_data(sim_data_1000_medium_btw_c1_L3, weibull_params, beta_effect_noise, 21870000L + seed_off, "uniform", 0.7)
sbm70_1k_4 <- merge_data(sim_data_1000_medium_btw_c1_L4, weibull_params, beta_effect_noise, 21880000L + seed_off, "uniform", 0.7)
sbm70_1k_5 <- merge_data(sim_data_1000_medium_btw_c1_L5, weibull_params, beta_effect_noise, 21890000L + seed_off, "uniform", 0.7)
sim1000_30_10V_midBTW_1c_L1 <- longer_data(sbm30_1k_1, sim_data_1000_medium_btw_c1_L1)
sim1000_30_10V_midBTW_1c_L2 <- longer_data(sbm30_1k_2, sim_data_1000_medium_btw_c1_L2)
sim1000_30_10V_midBTW_1c_L3 <- longer_data(sbm30_1k_3, sim_data_1000_medium_btw_c1_L3)
sim1000_30_10V_midBTW_1c_L4 <- longer_data(sbm30_1k_4, sim_data_1000_medium_btw_c1_L4)
sim1000_30_10V_midBTW_1c_L5 <- longer_data(sbm30_1k_5, sim_data_1000_medium_btw_c1_L5)
sim1000_70_10V_midBTW_1c_L1 <- longer_data(sbm70_1k_1, sim_data_1000_medium_btw_c1_L1)
sim1000_70_10V_midBTW_1c_L2 <- longer_data(sbm70_1k_2, sim_data_1000_medium_btw_c1_L2)
sim1000_70_10V_midBTW_1c_L3 <- longer_data(sbm70_1k_3, sim_data_1000_medium_btw_c1_L3)
sim1000_70_10V_midBTW_1c_L4 <- longer_data(sbm70_1k_4, sim_data_1000_medium_btw_c1_L4)
sim1000_70_10V_midBTW_1c_L5 <- longer_data(sbm70_1k_5, sim_data_1000_medium_btw_c1_L5)
sbh30_1 <- merge_data(sim_data_500_high_btw_c1_L1, weibull_params, beta_effect_noise, 22000000L + seed_off, "uniform", 0.3)
sbh30_2 <- merge_data(sim_data_500_high_btw_c1_L2, weibull_params, beta_effect_noise, 22010000L + seed_off, "uniform", 0.3)
sbh30_3 <- merge_data(sim_data_500_high_btw_c1_L3, weibull_params, beta_effect_noise, 22020000L + seed_off, "uniform", 0.3)
sbh30_4 <- merge_data(sim_data_500_high_btw_c1_L4, weibull_params, beta_effect_noise, 22030000L + seed_off, "uniform", 0.3)
sbh30_5 <- merge_data(sim_data_500_high_btw_c1_L5, weibull_params, beta_effect_noise, 22040000L + seed_off, "uniform", 0.3)
sbh70_1 <- merge_data(sim_data_500_high_btw_c1_L1, weibull_params, beta_effect_noise, 22050000L + seed_off, "uniform", 0.7)
sbh70_2 <- merge_data(sim_data_500_high_btw_c1_L2, weibull_params, beta_effect_noise, 22060000L + seed_off, "uniform", 0.7)
sbh70_3 <- merge_data(sim_data_500_high_btw_c1_L3, weibull_params, beta_effect_noise, 22070000L + seed_off, "uniform", 0.7)
sbh70_4 <- merge_data(sim_data_500_high_btw_c1_L4, weibull_params, beta_effect_noise, 22080000L + seed_off, "uniform", 0.7)
sbh70_5 <- merge_data(sim_data_500_high_btw_c1_L5, weibull_params, beta_effect_noise, 22090000L + seed_off, "uniform", 0.7)
sim500_30_10V_highBTW_1c_L1  <- longer_data(sbh30_1, sim_data_500_high_btw_c1_L1)
sim500_30_10V_highBTW_1c_L2  <- longer_data(sbh30_2, sim_data_500_high_btw_c1_L2)
sim500_30_10V_highBTW_1c_L3  <- longer_data(sbh30_3, sim_data_500_high_btw_c1_L3)
sim500_30_10V_highBTW_1c_L4  <- longer_data(sbh30_4, sim_data_500_high_btw_c1_L4)
sim500_30_10V_highBTW_1c_L5  <- longer_data(sbh30_5, sim_data_500_high_btw_c1_L5)
sim500_70_10V_highBTW_1c_L1  <- longer_data(sbh70_1, sim_data_500_high_btw_c1_L1)
sim500_70_10V_highBTW_1c_L2  <- longer_data(sbh70_2, sim_data_500_high_btw_c1_L2)
sim500_70_10V_highBTW_1c_L3  <- longer_data(sbh70_3, sim_data_500_high_btw_c1_L3)
sim500_70_10V_highBTW_1c_L4  <- longer_data(sbh70_4, sim_data_500_high_btw_c1_L4)
sim500_70_10V_highBTW_1c_L5  <- longer_data(sbh70_5, sim_data_500_high_btw_c1_L5)
sbh30_1k_1 <- merge_data(sim_data_1000_high_btw_c1_L1, weibull_params, beta_effect_noise, 22200000L + seed_off, "uniform", 0.3)
sbh30_1k_2 <- merge_data(sim_data_1000_high_btw_c1_L2, weibull_params, beta_effect_noise, 22210000L + seed_off, "uniform", 0.3)
sbh30_1k_3 <- merge_data(sim_data_1000_high_btw_c1_L3, weibull_params, beta_effect_noise, 22220000L + seed_off, "uniform", 0.3)
sbh30_1k_4 <- merge_data(sim_data_1000_high_btw_c1_L4, weibull_params, beta_effect_noise, 22230000L + seed_off, "uniform", 0.3)
sbh30_1k_5 <- merge_data(sim_data_1000_high_btw_c1_L5, weibull_params, beta_effect_noise, 22240000L + seed_off, "uniform", 0.3)
sbh70_1k_1 <- merge_data(sim_data_1000_high_btw_c1_L1, weibull_params, beta_effect_noise, 22250000L + seed_off, "uniform", 0.7)
sbh70_1k_2 <- merge_data(sim_data_1000_high_btw_c1_L2, weibull_params, beta_effect_noise, 22260000L + seed_off, "uniform", 0.7)
sbh70_1k_3 <- merge_data(sim_data_1000_high_btw_c1_L3, weibull_params, beta_effect_noise, 22270000L + seed_off, "uniform", 0.7)
sbh70_1k_4 <- merge_data(sim_data_1000_high_btw_c1_L4, weibull_params, beta_effect_noise, 22280000L + seed_off, "uniform", 0.7)
sbh70_1k_5 <- merge_data(sim_data_1000_high_btw_c1_L5, weibull_params, beta_effect_noise, 22290000L + seed_off, "uniform", 0.7)
sim1000_30_10V_highBTW_1c_L1 <- longer_data(sbh30_1k_1, sim_data_1000_high_btw_c1_L1)
sim1000_30_10V_highBTW_1c_L2 <- longer_data(sbh30_1k_2, sim_data_1000_high_btw_c1_L2)
sim1000_30_10V_highBTW_1c_L3 <- longer_data(sbh30_1k_3, sim_data_1000_high_btw_c1_L3)
sim1000_30_10V_highBTW_1c_L4 <- longer_data(sbh30_1k_4, sim_data_1000_high_btw_c1_L4)
sim1000_30_10V_highBTW_1c_L5 <- longer_data(sbh30_1k_5, sim_data_1000_high_btw_c1_L5)
sim1000_70_10V_highBTW_1c_L1 <- longer_data(sbh70_1k_1, sim_data_1000_high_btw_c1_L1)
sim1000_70_10V_highBTW_1c_L2 <- longer_data(sbh70_1k_2, sim_data_1000_high_btw_c1_L2)
sim1000_70_10V_highBTW_1c_L3 <- longer_data(sbh70_1k_3, sim_data_1000_high_btw_c1_L3)
sim1000_70_10V_highBTW_1c_L4 <- longer_data(sbh70_1k_4, sim_data_1000_high_btw_c1_L4)
sim1000_70_10V_highBTW_1c_L5 <- longer_data(sbh70_1k_5, sim_data_1000_high_btw_c1_L5)


# ==================== 第三部分：数据3 - 生成 Y ====================

add_Y <- function(sim_df, k, time_effects, id_col = "ID") {
  v_cols <- grep("^V\\d+", names(sim_df), value = TRUE)
  k_vec <- setNames(k[1:length(v_cols)], v_cols)
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
    k_adj <- if (length(k) != length(v_cols)) c(k, rep(0.025, max(0, length(v_cols) - length(k))))[1:length(v_cols)] else k
    add_Y(data_list[[i]], k_adj, time_effects)
  })
}

set.seed(123)
k <- c(runif(6, 0.8, 1.2), rep(0.025, 4))
time_effects <- c(0.3, 1.0, 0.6, 0.4)

message("【步骤3】添加 Y（base_seed + ", SEED_OFFSET, "）...")
y_seed <- 123L + SEED_OFFSET
sim500_30_10V_lowINTER_1c_L1  <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_lowINTER_1c_L2  <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_lowINTER_1c_L3  <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_lowINTER_1c_L4  <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_lowINTER_1c_L5  <- add_Y_to_list(sim500_30_10V_lowINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_lowINTER_1c_L1  <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_lowINTER_1c_L2  <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_lowINTER_1c_L3  <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_lowINTER_1c_L4  <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_lowINTER_1c_L5  <- add_Y_to_list(sim500_70_10V_lowINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_lowINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_lowINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_lowINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_lowINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_lowINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_lowINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_lowINTER_1c_L5, k, time_effects, y_seed)
sim500_30_10V_midINTER_1c_L1  <- add_Y_to_list(sim500_30_10V_midINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_midINTER_1c_L2  <- add_Y_to_list(sim500_30_10V_midINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_midINTER_1c_L3  <- add_Y_to_list(sim500_30_10V_midINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_midINTER_1c_L4  <- add_Y_to_list(sim500_30_10V_midINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_midINTER_1c_L5  <- add_Y_to_list(sim500_30_10V_midINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_midINTER_1c_L1  <- add_Y_to_list(sim500_70_10V_midINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_midINTER_1c_L2  <- add_Y_to_list(sim500_70_10V_midINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_midINTER_1c_L3  <- add_Y_to_list(sim500_70_10V_midINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_midINTER_1c_L4  <- add_Y_to_list(sim500_70_10V_midINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_midINTER_1c_L5  <- add_Y_to_list(sim500_70_10V_midINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_midINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_midINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_midINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_midINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_midINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_midINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_midINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_midINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_midINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_midINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_midINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_midINTER_1c_L5, k, time_effects, y_seed)
sim500_30_10V_highINTER_1c_L1  <- add_Y_to_list(sim500_30_10V_highINTER_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_highINTER_1c_L2  <- add_Y_to_list(sim500_30_10V_highINTER_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_highINTER_1c_L3  <- add_Y_to_list(sim500_30_10V_highINTER_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_highINTER_1c_L4  <- add_Y_to_list(sim500_30_10V_highINTER_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_highINTER_1c_L5  <- add_Y_to_list(sim500_30_10V_highINTER_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_highINTER_1c_L1  <- add_Y_to_list(sim500_70_10V_highINTER_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_highINTER_1c_L2  <- add_Y_to_list(sim500_70_10V_highINTER_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_highINTER_1c_L3  <- add_Y_to_list(sim500_70_10V_highINTER_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_highINTER_1c_L4  <- add_Y_to_list(sim500_70_10V_highINTER_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_highINTER_1c_L5  <- add_Y_to_list(sim500_70_10V_highINTER_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_highINTER_1c_L1 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_highINTER_1c_L2 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_highINTER_1c_L3 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_highINTER_1c_L4 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_highINTER_1c_L5 <- add_Y_to_list(sim1000_30_10V_highINTER_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_highINTER_1c_L1 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_highINTER_1c_L2 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_highINTER_1c_L3 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_highINTER_1c_L4 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_highINTER_1c_L5 <- add_Y_to_list(sim1000_70_10V_highINTER_1c_L5, k, time_effects, y_seed)
sim500_30_10V_lowBTW_1c_L1  <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_lowBTW_1c_L2  <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_lowBTW_1c_L3  <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_lowBTW_1c_L4  <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_lowBTW_1c_L5  <- add_Y_to_list(sim500_30_10V_lowBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_lowBTW_1c_L1  <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_lowBTW_1c_L2  <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_lowBTW_1c_L3  <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_lowBTW_1c_L4  <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_lowBTW_1c_L5  <- add_Y_to_list(sim500_70_10V_lowBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_lowBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_lowBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_lowBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_lowBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_lowBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_lowBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_lowBTW_1c_L5, k, time_effects, y_seed)
sim500_30_10V_midBTW_1c_L1  <- add_Y_to_list(sim500_30_10V_midBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_midBTW_1c_L2  <- add_Y_to_list(sim500_30_10V_midBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_midBTW_1c_L3  <- add_Y_to_list(sim500_30_10V_midBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_midBTW_1c_L4  <- add_Y_to_list(sim500_30_10V_midBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_midBTW_1c_L5  <- add_Y_to_list(sim500_30_10V_midBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_midBTW_1c_L1  <- add_Y_to_list(sim500_70_10V_midBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_midBTW_1c_L2  <- add_Y_to_list(sim500_70_10V_midBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_midBTW_1c_L3  <- add_Y_to_list(sim500_70_10V_midBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_midBTW_1c_L4  <- add_Y_to_list(sim500_70_10V_midBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_midBTW_1c_L5  <- add_Y_to_list(sim500_70_10V_midBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_midBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_midBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_midBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_midBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_midBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_midBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_midBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_midBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_midBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_midBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_midBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_midBTW_1c_L5, k, time_effects, y_seed)
sim500_30_10V_highBTW_1c_L1  <- add_Y_to_list(sim500_30_10V_highBTW_1c_L1,  k, time_effects, y_seed)
sim500_30_10V_highBTW_1c_L2  <- add_Y_to_list(sim500_30_10V_highBTW_1c_L2,  k, time_effects, y_seed)
sim500_30_10V_highBTW_1c_L3  <- add_Y_to_list(sim500_30_10V_highBTW_1c_L3,  k, time_effects, y_seed)
sim500_30_10V_highBTW_1c_L4  <- add_Y_to_list(sim500_30_10V_highBTW_1c_L4,  k, time_effects, y_seed)
sim500_30_10V_highBTW_1c_L5  <- add_Y_to_list(sim500_30_10V_highBTW_1c_L5,  k, time_effects, y_seed)
sim500_70_10V_highBTW_1c_L1  <- add_Y_to_list(sim500_70_10V_highBTW_1c_L1,  k, time_effects, y_seed)
sim500_70_10V_highBTW_1c_L2  <- add_Y_to_list(sim500_70_10V_highBTW_1c_L2,  k, time_effects, y_seed)
sim500_70_10V_highBTW_1c_L3  <- add_Y_to_list(sim500_70_10V_highBTW_1c_L3,  k, time_effects, y_seed)
sim500_70_10V_highBTW_1c_L4  <- add_Y_to_list(sim500_70_10V_highBTW_1c_L4,  k, time_effects, y_seed)
sim500_70_10V_highBTW_1c_L5  <- add_Y_to_list(sim500_70_10V_highBTW_1c_L5,  k, time_effects, y_seed)
sim1000_30_10V_highBTW_1c_L1 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L1, k, time_effects, y_seed)
sim1000_30_10V_highBTW_1c_L2 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L2, k, time_effects, y_seed)
sim1000_30_10V_highBTW_1c_L3 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L3, k, time_effects, y_seed)
sim1000_30_10V_highBTW_1c_L4 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L4, k, time_effects, y_seed)
sim1000_30_10V_highBTW_1c_L5 <- add_Y_to_list(sim1000_30_10V_highBTW_1c_L5, k, time_effects, y_seed)
sim1000_70_10V_highBTW_1c_L1 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L1, k, time_effects, y_seed)
sim1000_70_10V_highBTW_1c_L2 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L2, k, time_effects, y_seed)
sim1000_70_10V_highBTW_1c_L3 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L3, k, time_effects, y_seed)
sim1000_70_10V_highBTW_1c_L4 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L4, k, time_effects, y_seed)
sim1000_70_10V_highBTW_1c_L5 <- add_Y_to_list(sim1000_70_10V_highBTW_1c_L5, k, time_effects, y_seed)


# ==================== 第四部分：保存 ====================

out_y <- file.path(BASE_OUT, "生成Y")
message("【步骤4】保存到 ", out_y, " ...")
nms <- c("sim500_30_10V_lowINTER_1c_L1","sim500_30_10V_lowINTER_1c_L2","sim500_30_10V_lowINTER_1c_L3","sim500_30_10V_lowINTER_1c_L4","sim500_30_10V_lowINTER_1c_L5",
         "sim500_70_10V_lowINTER_1c_L1","sim500_70_10V_lowINTER_1c_L2","sim500_70_10V_lowINTER_1c_L3","sim500_70_10V_lowINTER_1c_L4","sim500_70_10V_lowINTER_1c_L5",
         "sim1000_30_10V_lowINTER_1c_L1","sim1000_30_10V_lowINTER_1c_L2","sim1000_30_10V_lowINTER_1c_L3","sim1000_30_10V_lowINTER_1c_L4","sim1000_30_10V_lowINTER_1c_L5",
         "sim1000_70_10V_lowINTER_1c_L1","sim1000_70_10V_lowINTER_1c_L2","sim1000_70_10V_lowINTER_1c_L3","sim1000_70_10V_lowINTER_1c_L4","sim1000_70_10V_lowINTER_1c_L5",
         "sim500_30_10V_midINTER_1c_L1","sim500_30_10V_midINTER_1c_L2","sim500_30_10V_midINTER_1c_L3","sim500_30_10V_midINTER_1c_L4","sim500_30_10V_midINTER_1c_L5",
         "sim500_70_10V_midINTER_1c_L1","sim500_70_10V_midINTER_1c_L2","sim500_70_10V_midINTER_1c_L3","sim500_70_10V_midINTER_1c_L4","sim500_70_10V_midINTER_1c_L5",
         "sim1000_30_10V_midINTER_1c_L1","sim1000_30_10V_midINTER_1c_L2","sim1000_30_10V_midINTER_1c_L3","sim1000_30_10V_midINTER_1c_L4","sim1000_30_10V_midINTER_1c_L5",
         "sim1000_70_10V_midINTER_1c_L1","sim1000_70_10V_midINTER_1c_L2","sim1000_70_10V_midINTER_1c_L3","sim1000_70_10V_midINTER_1c_L4","sim1000_70_10V_midINTER_1c_L5",
         "sim500_30_10V_highINTER_1c_L1","sim500_30_10V_highINTER_1c_L2","sim500_30_10V_highINTER_1c_L3","sim500_30_10V_highINTER_1c_L4","sim500_30_10V_highINTER_1c_L5",
         "sim500_70_10V_highINTER_1c_L1","sim500_70_10V_highINTER_1c_L2","sim500_70_10V_highINTER_1c_L3","sim500_70_10V_highINTER_1c_L4","sim500_70_10V_highINTER_1c_L5",
         "sim1000_30_10V_highINTER_1c_L1","sim1000_30_10V_highINTER_1c_L2","sim1000_30_10V_highINTER_1c_L3","sim1000_30_10V_highINTER_1c_L4","sim1000_30_10V_highINTER_1c_L5",
         "sim1000_70_10V_highINTER_1c_L1","sim1000_70_10V_highINTER_1c_L2","sim1000_70_10V_highINTER_1c_L3","sim1000_70_10V_highINTER_1c_L4","sim1000_70_10V_highINTER_1c_L5",
         "sim500_30_10V_lowBTW_1c_L1","sim500_30_10V_lowBTW_1c_L2","sim500_30_10V_lowBTW_1c_L3","sim500_30_10V_lowBTW_1c_L4","sim500_30_10V_lowBTW_1c_L5",
         "sim500_70_10V_lowBTW_1c_L1","sim500_70_10V_lowBTW_1c_L2","sim500_70_10V_lowBTW_1c_L3","sim500_70_10V_lowBTW_1c_L4","sim500_70_10V_lowBTW_1c_L5",
         "sim1000_30_10V_lowBTW_1c_L1","sim1000_30_10V_lowBTW_1c_L2","sim1000_30_10V_lowBTW_1c_L3","sim1000_30_10V_lowBTW_1c_L4","sim1000_30_10V_lowBTW_1c_L5",
         "sim1000_70_10V_lowBTW_1c_L1","sim1000_70_10V_lowBTW_1c_L2","sim1000_70_10V_lowBTW_1c_L3","sim1000_70_10V_lowBTW_1c_L4","sim1000_70_10V_lowBTW_1c_L5",
         "sim500_30_10V_midBTW_1c_L1","sim500_30_10V_midBTW_1c_L2","sim500_30_10V_midBTW_1c_L3","sim500_30_10V_midBTW_1c_L4","sim500_30_10V_midBTW_1c_L5",
         "sim500_70_10V_midBTW_1c_L1","sim500_70_10V_midBTW_1c_L2","sim500_70_10V_midBTW_1c_L3","sim500_70_10V_midBTW_1c_L4","sim500_70_10V_midBTW_1c_L5",
         "sim1000_30_10V_midBTW_1c_L1","sim1000_30_10V_midBTW_1c_L2","sim1000_30_10V_midBTW_1c_L3","sim1000_30_10V_midBTW_1c_L4","sim1000_30_10V_midBTW_1c_L5",
         "sim1000_70_10V_midBTW_1c_L1","sim1000_70_10V_midBTW_1c_L2","sim1000_70_10V_midBTW_1c_L3","sim1000_70_10V_midBTW_1c_L4","sim1000_70_10V_midBTW_1c_L5",
         "sim500_30_10V_highBTW_1c_L1","sim500_30_10V_highBTW_1c_L2","sim500_30_10V_highBTW_1c_L3","sim500_30_10V_highBTW_1c_L4","sim500_30_10V_highBTW_1c_L5",
         "sim500_70_10V_highBTW_1c_L1","sim500_70_10V_highBTW_1c_L2","sim500_70_10V_highBTW_1c_L3","sim500_70_10V_highBTW_1c_L4","sim500_70_10V_highBTW_1c_L5",
         "sim1000_30_10V_highBTW_1c_L1","sim1000_30_10V_highBTW_1c_L2","sim1000_30_10V_highBTW_1c_L3","sim1000_30_10V_highBTW_1c_L4","sim1000_30_10V_highBTW_1c_L5",
         "sim1000_70_10V_highBTW_1c_L1","sim1000_70_10V_highBTW_1c_L2","sim1000_70_10V_highBTW_1c_L3","sim1000_70_10V_highBTW_1c_L4","sim1000_70_10V_highBTW_1c_L5")
for (nm in nms) {
  write_xlsx(get(nm), path = file.path(out_y, paste0(nm, ".xlsx")))
}
message("✅ 10V1C 测试集生成完成（INTER + BTW）！输出目录: ", BASE_OUT)
