# =============================================================================
# 5_JMBayes_TEST.r
# 使用 JMbayes2 包拟合 JM 模型，对 4_TEST_模拟数据_200样本.xlsx 建模
# 使用 tvROC、tvAUC、tvBrier 计算 AUC、BS、C-index 等指标
# 结构与 JoineRML 的 JM 模型保持一致
# =============================================================================

library(survival)
library(nlme)
library(JMbayes2)
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)

set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)


# ==================== 路径设置 ====================

DATA_FILE <- "F:/文章_大论文/0319大改/代码/TEST/4_TEST_模拟数据_200样本.xlsx"
OUT_DIR   <- "F:/文章_大论文/0319大改/代码/TEST"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ==================== 读取数据 ====================

message("读取数据: ", DATA_FILE)
sim_test_200 <- read_xlsx(DATA_FILE)


# ==================== 多时间点 JM 建模（JMbayes2 + tvROC/tvAUC/tvBrier） ====================

cal_jmbayes_multi <- function(data, t0_vec = c(0.5, 1.3, 2)) {

  tryCatch({

    # 1. 数据清理：仅保留 t <= obs_time 的观测（与 JoineRML 一致）
    data_clean <- data[data$t <= data$obs_time, ]

    # 2. 准备 Cox 模型数据（每人一行）
    data_surv <- data_clean %>%
      group_by(ID) %>%
      slice(1) %>%
      ungroup() %>%
      as.data.frame()

    # 3. 准备 LME 数据（长格式）
    data_long <- as.data.frame(data_clean)

    # 4. 拟合混合效应模型（与 JoineRML 结构一致：Y ~ t + V1 + V2 + V3 + V4, random = ~ t | ID）
    fm1 <- lme(
      fixed  = Y ~ t + V1 + V2 + V3 + V4,
      random = ~ t | ID,
      data   = data_long,
      control = lmeControl(opt = "optim")
    )

    # 5. 拟合 Cox 模型（与 JoineRML 一致：无协变量）
    CoxFit <- coxph(Surv(obs_time, event) ~ 1, data = data_surv)

    # 6. 拟合联合模型（JMbayes2）
    jointFit <- jm(
      CoxFit,
      fm1,
      time_var = "t",
      id_var   = "ID",
      n_chains = 1L,
      n_iter   = 8000L,
      n_burnin = 2000L,
      n_thin   = 2L,
      control  = list(cores = 1L)
    )

    # 7. 对每个时间点计算 AUC、BS、C-index
    res_list <- lapply(t0_vec, function(t0) {

      # Tstart: 使用基线纵向信息；Dt: 预测区间长度
      # 预测在 (0, t0] 内发生事件的概率
      Tstart <- 0
      Dt     <- t0

      # tvROC + tvAUC：使用 tvROC 计算 ROC，tvAUC 提取 AUC
      auc_obj <- tryCatch({
        roc_obj <- tvROC(jointFit, newdata = data_long, Tstart = Tstart, Dt = Dt,
                         type_weights = "model-based", cores = 1L)
        tvAUC(roc_obj)
      }, error = function(e) {
        message("  tvROC/tvAUC 失败 t0=", t0, ": ", e$message)
        list(auc = NA)
      })
      AUC <- round(auc_obj$auc, 4)

      # tvBrier：Brier Score
      brier_obj <- tryCatch({
        tvBrier(jointFit, newdata = data_long, Tstart = Tstart, Dt = Dt,
                integrated = FALSE, type_weights = "model-based", cores = 1L)
      }, error = function(e) {
        message("  tvBrier 失败 t0=", t0, ": ", e$message)
        list(Brier = NA)
      })
      BS <- round(brier_obj$Brier, 6)

      # C-index：从联合模型提取 fixef、ranef、coef 计算风险得分，与 JoineRML 逻辑一致
      Cindex <- tryCatch({
        beta <- fixef(jointFit, outcome = 1)
        re_mat <- ranef(jointFit)
        coef_ev <- coef(jointFit)
        alpha <- if (is.list(coef_ev) && "association" %in% names(coef_ev)) {
          coef_ev$association
        } else {
          coef_ev
        }
        if (length(alpha) > 1) alpha <- alpha[1]
        clip <- function(x, lim = 20) pmax(pmin(x, lim), -lim)

        surv_data <- data_surv
        surv_data$t <- t0
        X_mat <- model.matrix(~ t + V1 + V2 + V3 + V4, data = surv_data)
        fixed_part <- clip(as.numeric(X_mat %*% beta))
        id_match <- match(as.character(surv_data$ID), rownames(re_mat))
        re_b <- re_mat[id_match, , drop = FALSE]
        re_b[is.na(re_b)] <- 0
        n_re <- ncol(re_b)
        random_part <- if (n_re >= 2) {
          clip(as.numeric(re_b[, 1] + re_b[, 2] * t0))
        } else {
          clip(as.numeric(re_b[, 1]))
        }
        comprehensive_risk <- as.numeric(alpha) * (fixed_part + random_part)

        surv_time <- surv_data$obs_time
        surv_status <- surv_data$event
        n <- length(surv_time)
        if (n < 2) return(NA)
        idx <- combn(n, 2)
        i <- idx[1, ]; j <- idx[2, ]
        comparable_ij <- (surv_status[i] == 1 & surv_time[i] < surv_time[j])
        comparable_ji <- (surv_status[j] == 1 & surv_time[j] < surv_time[i])
        comparable <- comparable_ij | comparable_ji
        concordant <- (comparable_ij & (comprehensive_risk[i] > comprehensive_risk[j])) |
          (comparable_ji & (comprehensive_risk[j] > comprehensive_risk[i]))
        tied <- comparable & (abs(comprehensive_risk[i] - comprehensive_risk[j]) < 1e-10)
        n_pairs <- sum(comparable)
        cidx <- ifelse(n_pairs > 0, round((sum(concordant) + 0.5 * sum(tied)) / n_pairs, 4), AUC)
        cidx
      }, error = function(e) {
        message("  C-index 失败 t0=", t0, ": ", e$message)
        AUC
      })

      data.frame(t0 = t0, AUC = AUC, BS = BS, Cindex = Cindex)
    })

    do.call(rbind, res_list)

  }, error = function(e) {
    message("⚠️ cal_jmbayes_multi failed: ", e$message)
    data.frame(t0 = t0_vec, AUC = NA, BS = NA, Cindex = NA)
  })
}


# ==================== 建模并保存 ====================

t0_vec <- c(0.5, 1.3, 2)

message("【JMbayes2 Joint Model】对 200 样本数据建模，计算 t0 = ",
        paste(t0_vec, collapse = ", "), " 的 AUC、BS、Cindex...")
result_jmbayes <- cal_jmbayes_multi(sim_test_200, t0_vec = t0_vec)

# 保存结果
out_xlsx <- file.path(OUT_DIR, "6_TEST_模拟JMBayes.xlsx")
write_xlsx(result_jmbayes, path = out_xlsx)

message("✅ 建模完成！结果已保存: ", out_xlsx)
print(result_jmbayes)
