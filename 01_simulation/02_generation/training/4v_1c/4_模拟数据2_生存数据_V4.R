library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
library(purrr) 
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


out_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/协变量"


############数据导入##############


out_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/协变量"

## 低相关
sim_data_500_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L1.xlsx"), sheet = .x))

sim_data_500_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L2.xlsx"), sheet = .x))

sim_data_500_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L3.xlsx"), sheet = .x))

sim_data_500_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L4.xlsx"), sheet = .x))

sim_data_500_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx")),
                                       excel_sheets(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"))),
                              ~ read_xlsx(file.path(out_dir, "sim_data_500_low_c1_L5.xlsx"), sheet = .x))

sim_data_1000_low_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L1.xlsx"), sheet = .x))

sim_data_1000_low_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L2.xlsx"), sheet = .x))

sim_data_1000_low_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L3.xlsx"), sheet = .x))

sim_data_1000_low_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L4.xlsx"), sheet = .x))

sim_data_1000_low_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_1000_low_c1_L5.xlsx"), sheet = .x))

## 中相关
sim_data_500_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L1.xlsx"), sheet = .x))

sim_data_500_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L2.xlsx"), sheet = .x))

sim_data_500_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L3.xlsx"), sheet = .x))

sim_data_500_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L4.xlsx"), sheet = .x))

sim_data_500_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx")),
                                          excel_sheets(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"))),
                                 ~ read_xlsx(file.path(out_dir, "sim_data_500_medium_c1_L5.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L1.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L2.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L3.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L4.xlsx"), sheet = .x))

sim_data_1000_medium_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx")),
                                           excel_sheets(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir, "sim_data_1000_medium_c1_L5.xlsx"), sheet = .x))

## 高相关
sim_data_500_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L1.xlsx"), sheet = .x))

sim_data_500_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L2.xlsx"), sheet = .x))

sim_data_500_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L3.xlsx"), sheet = .x))

sim_data_500_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L4.xlsx"), sheet = .x))

sim_data_500_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx")),
                                        excel_sheets(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"))),
                               ~ read_xlsx(file.path(out_dir, "sim_data_500_high_c1_L5.xlsx"), sheet = .x))

sim_data_1000_high_c1_L1 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L1.xlsx"), sheet = .x))

sim_data_1000_high_c1_L2 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L2.xlsx"), sheet = .x))

sim_data_1000_high_c1_L3 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L3.xlsx"), sheet = .x))

sim_data_1000_high_c1_L4 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L4.xlsx"), sheet = .x))

sim_data_1000_high_c1_L5 <- map(setNames(excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx")),
                                         excel_sheets(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"))),
                                ~ read_xlsx(file.path(out_dir, "sim_data_1000_high_c1_L5.xlsx"), sheet = .x))





################程序################

add_surv <- function(data,
                     weibull_params,
                     beta_effects,
                     var_pattern = "^V\\d+$",
                     id_col = "ID",
                     class_col = "class",
                     seed = 123,
                     censor_dist = c("exponential", "uniform"),
                     censor_params = list(rate = 0.05, max = NULL),
                     target_censor = NULL,
                     tol = 1e-4,
                     max_iter = 50,
                     attach_longitudinal = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  censor_dist <- match.arg(censor_dist)
  
  # 1. 基线数据（每个 ID 的第一条）
  if (!(id_col %in% names(data))) {
    stop(paste("列", id_col, "在数据中不存在。可用的列名:", paste(names(data), collapse = ", ")))
  }
  
  baseline <- data %>%
    group_by(!!sym(id_col)) %>%
    slice(1) %>%
    ungroup()
  
  # 2. 变量名自动识别
  var_names <- grep(var_pattern, names(baseline), value = TRUE)
  if (length(var_names) == 0) stop("无法根据 var_pattern 找到任何自变量列。")
  
  # 3. 单潜类：只保留 c1，并剔除数据里不存在的变量
  if (!"c1" %in% names(beta_effects)) stop("beta_effects 必须包含名为 'c1' 的列表元素。")
  coef_c1 <- beta_effects$c1
  keep_coef <- intersect(names(coef_c1), var_names)   # 只留数据里有的
  if (length(keep_coef) == 0) stop("beta_effects$c1 与数据中的变量无交集。")
  coef_c1 <- coef_c1[keep_coef]
  
  # 4. 计算线性预测项 lp
  n <- nrow(baseline)
  X <- as.matrix(baseline[, names(coef_c1), drop = FALSE])
  lp_vec <- as.numeric(X %*% coef_c1)
  baseline$lp <- lp_vec
  
  # 5. 生成真实生存时间（Weibull）
  u <- runif(n)
  shape <- weibull_params$c1$shape
  scale <- weibull_params$c1$scale
  surv_time <- scale * (-log(u) / exp(lp_vec))^(1 / shape)
  baseline$surv_time <- surv_time
  
  # 6. 删失时间生成（校准或默认）
  calc_prop_exp <- function(rate, surv) mean(1 - exp(-rate * surv))
  calc_prop_unif <- function(M, surv) mean(pmin(surv, M) / M)
  
  if (!is.null(target_censor)) {
    if (!(target_censor >= 0 && target_censor <= 1)) stop("target_censor 必须在 [0,1] 之间。")
    if (censor_dist == "exponential") {
      low <- 1e-12; high <- 1
      iter_expand <- 0
      while (calc_prop_exp(high, surv_time) < target_censor && iter_expand < 60) {
        high <- high * 2; iter_expand <- iter_expand + 1
      }
      for (i in seq_len(max_iter)) {
        mid <- (low + high) / 2
        prop_mid <- calc_prop_exp(mid, surv_time)
        if (abs(prop_mid - target_censor) < tol) break
        if (prop_mid < target_censor) low <- mid else high <- mid
      }
      rate_final <- mid
      censor_time <- rexp(n, rate = rate_final)
      message(sprintf("校准 exponential rate = %.6g，期望删失率 ≈ %.4f",
                      rate_final, calc_prop_exp(rate_final, surv_time)))
    } else { # uniform
      low <- 1e-8; high <- max(surv_time) * 10 + 1
      iter_expand <- 0
      while (calc_prop_unif(high, surv_time) > target_censor && iter_expand < 60) {
        high <- high * 2; iter_expand <- iter_expand + 1
      }
      for (i in seq_len(max_iter)) {
        mid <- (low + high) / 2
        prop_mid <- calc_prop_unif(mid, surv_time)
        if (abs(prop_mid - target_censor) < tol) break
        if (prop_mid > target_censor) low <- mid else high <- mid
      }
      M_final <- mid
      censor_time <- runif(n, 0, M_final)
      message(sprintf("校准 uniform max = %.6g，期望删失率 ≈ %.4f",
                      M_final, calc_prop_unif(M_final, surv_time)))
    }
  } else {
    if (censor_dist == "exponential") {
      rate_use <- if (!is.null(censor_params$rate)) censor_params$rate else 0.05
      censor_time <- rexp(n, rate = rate_use)
    } else {
      M_use <- if (!is.null(censor_params$max)) censor_params$max else (quantile(surv_time, 0.8) * 2)
      censor_time <- runif(n, 0, M_use)
    }
  }
  
  baseline$censor_time <- censor_time
  baseline$obs_time <- pmin(baseline$surv_time, baseline$censor_time)
  baseline$event <- as.integer(baseline$surv_time <= baseline$censor_time)
  
  # 7. 返回
  if (attach_longitudinal) {
    out <- data %>%
      left_join(baseline %>% dplyr::select(.data[[id_col]], surv_time, censor_time, obs_time, event, lp),
                by = id_col)
    if ("t" %in% names(out) && "obs_time" %in% names(out)) {
      out <- out %>% filter(t <= obs_time)  # 剔除 t > obs_time 的无效观测
    }
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
    
    # 选生存相关的变量
    surv_cols <- c("lp", "surv_time", "censor_time", "obs_time", "event")
    surv_info <- base_i %>% dplyr::select(ID, dplyr::all_of(surv_cols))
    
    # 合并：把基线的生存变量加到纵向数据中
    merged_i <- long_i %>%
      dplyr::left_join(surv_info, by = "ID") %>%
      dplyr::filter(t <= obs_time)  # 剔除 t > obs_time 的无效观测（患者已删失/死亡后不应有纵向记录）
    
    # 存入结果list
    merged_list[[i]] <- merged_i
  }
  
  return(merged_list)
}

write_list <- function(data, dir) {
  # 确认输入是 list
  if (!is.list(data)) stop("data 必须是一个 list，且每个元素应是 data.frame 或 tibble")
  
  # 获取变量名（即 list 的名字，例如 sim500_30_4V_lowINTER_3c）
  data_name <- deparse(substitute(data))
  
  # 如果目录不存在，创建目录
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # 遍历保存
  for (i in seq_along(data)) {
    file_path <- file.path(dir, paste0(data_name, "_", i, ".xlsx"))
    writexl::write_xlsx(data[[i]], path = file_path)
  }
  
  message("✅ 已保存 ", length(data), " 个文件到目录: ", dir)
}


##########生存分布参数##################

weibull_params <- list(
  c1 = list(shape = 1.2, scale = 2)
)

beta_effects <- list(
  c1 = c(
    V1 = 0.6, V2 = -0.7, V3 = 0.8, V4 = -0.9
  )
)

#############生成生存数据#####


## 1. 500 样本 -------------------------------------------------------------
## 1.1 低相关-变量间-30%
sim500_30_4V_lowINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_low_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20100000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_lowINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_low_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20110000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_lowINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_low_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20120000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_lowINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_low_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20130000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_lowINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_low_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20140000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 1.2 低相关-变量间-70%
sim500_70_4V_lowINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_low_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20150000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_lowINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_low_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20160000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_lowINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_low_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20170000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_lowINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_low_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20180000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_lowINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_low_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20190000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)


## 2. 1000 样本 -------------------------------------------------------------
## 2.1 低相关-变量间-30%
sim1000_30_4V_lowINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_low_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20300000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_lowINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_low_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20310000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_lowINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_low_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20320000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_lowINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_low_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20330000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_lowINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_low_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20340000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 2.2 低相关-变量间-70%
sim1000_70_4V_lowINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_low_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20350000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_lowINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_low_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20360000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_lowINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_low_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20370000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_lowINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_low_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20380000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_lowINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_low_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 20390000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)



## 1. 500 样本 —— 中相关 4V ###############################################
## 1.1 30% 删失
sim500_30_4V_midINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_medium_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21100000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_midINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_medium_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21110000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_midINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_medium_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21120000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_midINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_medium_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21130000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_midINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_medium_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21140000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 1.2 70% 删失
sim500_70_4V_midINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_medium_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21150000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_midINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_medium_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21160000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_midINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_medium_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21170000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_midINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_medium_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21180000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_midINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_medium_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21190000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)

## 2. 1000 样本 —— 中相关 4V #############################################
## 2.1 30% 删失
sim1000_30_4V_midINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21300000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_midINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21310000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_midINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21320000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_midINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21330000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_midINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21340000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 2.2 70% 删失
sim1000_70_4V_midINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21350000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_midINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21360000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_midINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21370000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_midINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21380000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_midINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_medium_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 21390000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)

## 3. 500 样本 —— 高相关 4V ###############################################
## 3.1 30% 删失
sim500_30_4V_highINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_high_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22100000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_highINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_high_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22110000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_highINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_high_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22120000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_highINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_high_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22130000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim500_30_4V_highINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_high_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22140000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 3.2 70% 删失
sim500_70_4V_highINTER_1c_L1 <- merge_data(
  long_data      = sim_data_500_high_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22150000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_highINTER_1c_L2 <- merge_data(
  long_data      = sim_data_500_high_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22160000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_highINTER_1c_L3 <- merge_data(
  long_data      = sim_data_500_high_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22170000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_highINTER_1c_L4 <- merge_data(
  long_data      = sim_data_500_high_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22180000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim500_70_4V_highINTER_1c_L5 <- merge_data(
  long_data      = sim_data_500_high_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22190000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)

## 4. 1000 样本 —— 高相关 4V #############################################
## 4.1 30% 删失
sim1000_30_4V_highINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_high_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22300000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_highINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_high_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22310000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_highINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_high_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22320000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_highINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_high_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22330000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)
sim1000_30_4V_highINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_high_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22340000,
  censor_dist    = "uniform",
  target_censor  = 0.3,
  n_max          = 200
)

## 4.2 70% 删失
sim1000_70_4V_highINTER_1c_L1 <- merge_data(
  long_data      = sim_data_1000_high_c1_L1,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22350000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_highINTER_1c_L2 <- merge_data(
  long_data      = sim_data_1000_high_c1_L2,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22360000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_highINTER_1c_L3 <- merge_data(
  long_data      = sim_data_1000_high_c1_L3,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22370000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_highINTER_1c_L4 <- merge_data(
  long_data      = sim_data_1000_high_c1_L4,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22380000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)
sim1000_70_4V_highINTER_1c_L5 <- merge_data(
  long_data      = sim_data_1000_high_c1_L5,
  weibull_params = weibull_params,
  beta_effects   = beta_effects,
  seed_start     = 22390000,
  censor_dist    = "uniform",
  target_censor  = 0.7,
  n_max          = 200
)




#########合并为纵向数据#######################




##################低相关#####
sim500_30_4V_lowINTER_1c_L1 <-longer_data(sim500_30_4V_lowINTER_1c_L1, sim_data_500_low_c1_L1)
sim500_30_4V_lowINTER_1c_L2 <-longer_data(sim500_30_4V_lowINTER_1c_L2, sim_data_500_low_c1_L2)
sim500_30_4V_lowINTER_1c_L3 <-longer_data(sim500_30_4V_lowINTER_1c_L3, sim_data_500_low_c1_L3)
sim500_30_4V_lowINTER_1c_L4 <-longer_data(sim500_30_4V_lowINTER_1c_L4, sim_data_500_low_c1_L4)
sim500_30_4V_lowINTER_1c_L5 <-longer_data(sim500_30_4V_lowINTER_1c_L5, sim_data_500_low_c1_L5)

sim500_70_4V_lowINTER_1c_L1 <-longer_data(sim500_70_4V_lowINTER_1c_L1, sim_data_500_low_c1_L1)
sim500_70_4V_lowINTER_1c_L2 <-longer_data(sim500_70_4V_lowINTER_1c_L2, sim_data_500_low_c1_L2)
sim500_70_4V_lowINTER_1c_L3 <-longer_data(sim500_70_4V_lowINTER_1c_L3, sim_data_500_low_c1_L3)
sim500_70_4V_lowINTER_1c_L4 <-longer_data(sim500_70_4V_lowINTER_1c_L4, sim_data_500_low_c1_L4)
sim500_70_4V_lowINTER_1c_L5 <-longer_data(sim500_70_4V_lowINTER_1c_L5, sim_data_500_low_c1_L5)

sim1000_30_4V_lowINTER_1c_L1 <-longer_data(sim1000_30_4V_lowINTER_1c_L1, sim_data_1000_low_c1_L1)
sim1000_30_4V_lowINTER_1c_L2 <-longer_data(sim1000_30_4V_lowINTER_1c_L2, sim_data_1000_low_c1_L2)
sim1000_30_4V_lowINTER_1c_L3 <-longer_data(sim1000_30_4V_lowINTER_1c_L3, sim_data_1000_low_c1_L3)
sim1000_30_4V_lowINTER_1c_L4 <-longer_data(sim1000_30_4V_lowINTER_1c_L4, sim_data_1000_low_c1_L4)
sim1000_30_4V_lowINTER_1c_L5 <-longer_data(sim1000_30_4V_lowINTER_1c_L5, sim_data_1000_low_c1_L5)

sim1000_70_4V_lowINTER_1c_L1 <-longer_data(sim1000_70_4V_lowINTER_1c_L1, sim_data_1000_low_c1_L1)
sim1000_70_4V_lowINTER_1c_L2 <-longer_data(sim1000_70_4V_lowINTER_1c_L2, sim_data_1000_low_c1_L2)
sim1000_70_4V_lowINTER_1c_L3 <-longer_data(sim1000_70_4V_lowINTER_1c_L3, sim_data_1000_low_c1_L3)
sim1000_70_4V_lowINTER_1c_L4 <-longer_data(sim1000_70_4V_lowINTER_1c_L4, sim_data_1000_low_c1_L4)
sim1000_70_4V_lowINTER_1c_L5 <-longer_data(sim1000_70_4V_lowINTER_1c_L5, sim_data_1000_low_c1_L5)


#####################中相关性##########
sim500_30_4V_midINTER_1c_L1 <-longer_data(sim500_30_4V_midINTER_1c_L1, sim_data_500_medium_c1_L1)
sim500_30_4V_midINTER_1c_L2 <-longer_data(sim500_30_4V_midINTER_1c_L2, sim_data_500_medium_c1_L2)
sim500_30_4V_midINTER_1c_L3 <-longer_data(sim500_30_4V_midINTER_1c_L3, sim_data_500_medium_c1_L3)
sim500_30_4V_midINTER_1c_L4 <-longer_data(sim500_30_4V_midINTER_1c_L4, sim_data_500_medium_c1_L4)
sim500_30_4V_midINTER_1c_L5 <-longer_data(sim500_30_4V_midINTER_1c_L5, sim_data_500_medium_c1_L5)

sim500_70_4V_midINTER_1c_L1 <-longer_data(sim500_70_4V_midINTER_1c_L1, sim_data_500_medium_c1_L1)
sim500_70_4V_midINTER_1c_L2 <-longer_data(sim500_70_4V_midINTER_1c_L2, sim_data_500_medium_c1_L2)
sim500_70_4V_midINTER_1c_L3 <-longer_data(sim500_70_4V_midINTER_1c_L3, sim_data_500_medium_c1_L3)
sim500_70_4V_midINTER_1c_L4 <-longer_data(sim500_70_4V_midINTER_1c_L4, sim_data_500_medium_c1_L4)
sim500_70_4V_midINTER_1c_L5 <-longer_data(sim500_70_4V_midINTER_1c_L5, sim_data_500_medium_c1_L5)

sim1000_30_4V_midINTER_1c_L1 <-longer_data(sim1000_30_4V_midINTER_1c_L1, sim_data_1000_medium_c1_L1)
sim1000_30_4V_midINTER_1c_L2 <-longer_data(sim1000_30_4V_midINTER_1c_L2, sim_data_1000_medium_c1_L2)
sim1000_30_4V_midINTER_1c_L3 <-longer_data(sim1000_30_4V_midINTER_1c_L3, sim_data_1000_medium_c1_L3)
sim1000_30_4V_midINTER_1c_L4 <-longer_data(sim1000_30_4V_midINTER_1c_L4, sim_data_1000_medium_c1_L4)
sim1000_30_4V_midINTER_1c_L5 <-longer_data(sim1000_30_4V_midINTER_1c_L5, sim_data_1000_medium_c1_L5)

sim1000_70_4V_midINTER_1c_L1 <-longer_data(sim1000_70_4V_midINTER_1c_L1, sim_data_1000_medium_c1_L1)
sim1000_70_4V_midINTER_1c_L2 <-longer_data(sim1000_70_4V_midINTER_1c_L2, sim_data_1000_medium_c1_L2)
sim1000_70_4V_midINTER_1c_L3 <-longer_data(sim1000_70_4V_midINTER_1c_L3, sim_data_1000_medium_c1_L3)
sim1000_70_4V_midINTER_1c_L4 <-longer_data(sim1000_70_4V_midINTER_1c_L4, sim_data_1000_medium_c1_L4)
sim1000_70_4V_midINTER_1c_L5 <-longer_data(sim1000_70_4V_midINTER_1c_L5, sim_data_1000_medium_c1_L5)



#####################高相关性#########
sim500_30_4V_highINTER_1c_L1 <-longer_data(sim500_30_4V_highINTER_1c_L1, sim_data_500_high_c1_L1)
sim500_30_4V_highINTER_1c_L2 <-longer_data(sim500_30_4V_highINTER_1c_L2, sim_data_500_high_c1_L2)
sim500_30_4V_highINTER_1c_L3 <-longer_data(sim500_30_4V_highINTER_1c_L3, sim_data_500_high_c1_L3)
sim500_30_4V_highINTER_1c_L4 <-longer_data(sim500_30_4V_highINTER_1c_L4, sim_data_500_high_c1_L4)
sim500_30_4V_highINTER_1c_L5 <-longer_data(sim500_30_4V_highINTER_1c_L5, sim_data_500_high_c1_L5)

sim500_70_4V_highINTER_1c_L1 <-longer_data(sim500_70_4V_highINTER_1c_L1, sim_data_500_high_c1_L1)
sim500_70_4V_highINTER_1c_L2 <-longer_data(sim500_70_4V_highINTER_1c_L2, sim_data_500_high_c1_L2)
sim500_70_4V_highINTER_1c_L3 <-longer_data(sim500_70_4V_highINTER_1c_L3, sim_data_500_high_c1_L3)
sim500_70_4V_highINTER_1c_L4 <-longer_data(sim500_70_4V_highINTER_1c_L4, sim_data_500_high_c1_L4)
sim500_70_4V_highINTER_1c_L5 <-longer_data(sim500_70_4V_highINTER_1c_L5, sim_data_500_high_c1_L5)

sim1000_30_4V_highINTER_1c_L1 <-longer_data(sim1000_30_4V_highINTER_1c_L1, sim_data_1000_high_c1_L1)
sim1000_30_4V_highINTER_1c_L2 <-longer_data(sim1000_30_4V_highINTER_1c_L2, sim_data_1000_high_c1_L2)
sim1000_30_4V_highINTER_1c_L3 <-longer_data(sim1000_30_4V_highINTER_1c_L3, sim_data_1000_high_c1_L3)
sim1000_30_4V_highINTER_1c_L4 <-longer_data(sim1000_30_4V_highINTER_1c_L4, sim_data_1000_high_c1_L4)
sim1000_30_4V_highINTER_1c_L5 <-longer_data(sim1000_30_4V_highINTER_1c_L5, sim_data_1000_high_c1_L5)

sim1000_70_4V_highINTER_1c_L1 <-longer_data(sim1000_70_4V_highINTER_1c_L1, sim_data_1000_high_c1_L1)
sim1000_70_4V_highINTER_1c_L2 <-longer_data(sim1000_70_4V_highINTER_1c_L2, sim_data_1000_high_c1_L2)
sim1000_70_4V_highINTER_1c_L3 <-longer_data(sim1000_70_4V_highINTER_1c_L3, sim_data_1000_high_c1_L3)
sim1000_70_4V_highINTER_1c_L4 <-longer_data(sim1000_70_4V_highINTER_1c_L4, sim_data_1000_high_c1_L4)
sim1000_70_4V_highINTER_1c_L5 <-longer_data(sim1000_70_4V_highINTER_1c_L5, sim_data_1000_high_c1_L5)







###################保存数据#############
out_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/生存"



write_xlsx(sim500_30_4V_lowINTER_1c_L1, path = file.path(out_dir, "sim500_30_4V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_4V_lowINTER_1c_L2, path = file.path(out_dir, "sim500_30_4V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_4V_lowINTER_1c_L3, path = file.path(out_dir, "sim500_30_4V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_4V_lowINTER_1c_L4, path = file.path(out_dir, "sim500_30_4V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_4V_lowINTER_1c_L5, path = file.path(out_dir, "sim500_30_4V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim500_70_4V_lowINTER_1c_L1, path = file.path(out_dir, "sim500_70_4V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_4V_lowINTER_1c_L2, path = file.path(out_dir, "sim500_70_4V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_4V_lowINTER_1c_L3, path = file.path(out_dir, "sim500_70_4V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_4V_lowINTER_1c_L4, path = file.path(out_dir, "sim500_70_4V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_4V_lowINTER_1c_L5, path = file.path(out_dir, "sim500_70_4V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim1000_30_4V_lowINTER_1c_L1, path = file.path(out_dir, "sim1000_30_4V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_4V_lowINTER_1c_L2, path = file.path(out_dir, "sim1000_30_4V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_4V_lowINTER_1c_L3, path = file.path(out_dir, "sim1000_30_4V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_4V_lowINTER_1c_L4, path = file.path(out_dir, "sim1000_30_4V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_4V_lowINTER_1c_L5, path = file.path(out_dir, "sim1000_30_4V_lowINTER_1c_L5.xlsx"))

write_xlsx(sim1000_70_4V_lowINTER_1c_L1, path = file.path(out_dir, "sim1000_70_4V_lowINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_4V_lowINTER_1c_L2, path = file.path(out_dir, "sim1000_70_4V_lowINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_4V_lowINTER_1c_L3, path = file.path(out_dir, "sim1000_70_4V_lowINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_4V_lowINTER_1c_L4, path = file.path(out_dir, "sim1000_70_4V_lowINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_4V_lowINTER_1c_L5, path = file.path(out_dir, "sim1000_70_4V_lowINTER_1c_L5.xlsx"))

# 保存中相关性数据
write_xlsx(sim500_30_4V_midINTER_1c_L1, path = file.path(out_dir, "sim500_30_4V_midINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_4V_midINTER_1c_L2, path = file.path(out_dir, "sim500_30_4V_midINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_4V_midINTER_1c_L3, path = file.path(out_dir, "sim500_30_4V_midINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_4V_midINTER_1c_L4, path = file.path(out_dir, "sim500_30_4V_midINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_4V_midINTER_1c_L5, path = file.path(out_dir, "sim500_30_4V_midINTER_1c_L5.xlsx"))

write_xlsx(sim500_70_4V_midINTER_1c_L1, path = file.path(out_dir, "sim500_70_4V_midINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_4V_midINTER_1c_L2, path = file.path(out_dir, "sim500_70_4V_midINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_4V_midINTER_1c_L3, path = file.path(out_dir, "sim500_70_4V_midINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_4V_midINTER_1c_L4, path = file.path(out_dir, "sim500_70_4V_midINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_4V_midINTER_1c_L5, path = file.path(out_dir, "sim500_70_4V_midINTER_1c_L5.xlsx"))

write_xlsx(sim1000_30_4V_midINTER_1c_L1, path = file.path(out_dir, "sim1000_30_4V_midINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_4V_midINTER_1c_L2, path = file.path(out_dir, "sim1000_30_4V_midINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_4V_midINTER_1c_L3, path = file.path(out_dir, "sim1000_30_4V_midINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_4V_midINTER_1c_L4, path = file.path(out_dir, "sim1000_30_4V_midINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_4V_midINTER_1c_L5, path = file.path(out_dir, "sim1000_30_4V_midINTER_1c_L5.xlsx"))

write_xlsx(sim1000_70_4V_midINTER_1c_L1, path = file.path(out_dir, "sim1000_70_4V_midINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_4V_midINTER_1c_L2, path = file.path(out_dir, "sim1000_70_4V_midINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_4V_midINTER_1c_L3, path = file.path(out_dir, "sim1000_70_4V_midINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_4V_midINTER_1c_L4, path = file.path(out_dir, "sim1000_70_4V_midINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_4V_midINTER_1c_L5, path = file.path(out_dir, "sim1000_70_4V_midINTER_1c_L5.xlsx"))

# 保存高相关性数据
write_xlsx(sim500_30_4V_highINTER_1c_L1, path = file.path(out_dir, "sim500_30_4V_highINTER_1c_L1.xlsx"))
write_xlsx(sim500_30_4V_highINTER_1c_L2, path = file.path(out_dir, "sim500_30_4V_highINTER_1c_L2.xlsx"))
write_xlsx(sim500_30_4V_highINTER_1c_L3, path = file.path(out_dir, "sim500_30_4V_highINTER_1c_L3.xlsx"))
write_xlsx(sim500_30_4V_highINTER_1c_L4, path = file.path(out_dir, "sim500_30_4V_highINTER_1c_L4.xlsx"))
write_xlsx(sim500_30_4V_highINTER_1c_L5, path = file.path(out_dir, "sim500_30_4V_highINTER_1c_L5.xlsx"))

write_xlsx(sim500_70_4V_highINTER_1c_L1, path = file.path(out_dir, "sim500_70_4V_highINTER_1c_L1.xlsx"))
write_xlsx(sim500_70_4V_highINTER_1c_L2, path = file.path(out_dir, "sim500_70_4V_highINTER_1c_L2.xlsx"))
write_xlsx(sim500_70_4V_highINTER_1c_L3, path = file.path(out_dir, "sim500_70_4V_highINTER_1c_L3.xlsx"))
write_xlsx(sim500_70_4V_highINTER_1c_L4, path = file.path(out_dir, "sim500_70_4V_highINTER_1c_L4.xlsx"))
write_xlsx(sim500_70_4V_highINTER_1c_L5, path = file.path(out_dir, "sim500_70_4V_highINTER_1c_L5.xlsx"))

write_xlsx(sim1000_30_4V_highINTER_1c_L1, path = file.path(out_dir, "sim1000_30_4V_highINTER_1c_L1.xlsx"))
write_xlsx(sim1000_30_4V_highINTER_1c_L2, path = file.path(out_dir, "sim1000_30_4V_highINTER_1c_L2.xlsx"))
write_xlsx(sim1000_30_4V_highINTER_1c_L3, path = file.path(out_dir, "sim1000_30_4V_highINTER_1c_L3.xlsx"))
write_xlsx(sim1000_30_4V_highINTER_1c_L4, path = file.path(out_dir, "sim1000_30_4V_highINTER_1c_L4.xlsx"))
write_xlsx(sim1000_30_4V_highINTER_1c_L5, path = file.path(out_dir, "sim1000_30_4V_highINTER_1c_L5.xlsx"))

write_xlsx(sim1000_70_4V_highINTER_1c_L1, path = file.path(out_dir, "sim1000_70_4V_highINTER_1c_L1.xlsx"))
write_xlsx(sim1000_70_4V_highINTER_1c_L2, path = file.path(out_dir, "sim1000_70_4V_highINTER_1c_L2.xlsx"))
write_xlsx(sim1000_70_4V_highINTER_1c_L3, path = file.path(out_dir, "sim1000_70_4V_highINTER_1c_L3.xlsx"))
write_xlsx(sim1000_70_4V_highINTER_1c_L4, path = file.path(out_dir, "sim1000_70_4V_highINTER_1c_L4.xlsx"))
write_xlsx(sim1000_70_4V_highINTER_1c_L5, path = file.path(out_dir, "sim1000_70_4V_highINTER_1c_L5.xlsx"))




#######################

