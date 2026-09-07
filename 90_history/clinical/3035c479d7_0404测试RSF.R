
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
  library(writexl)
})

set.seed(123)

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
out_xlsx <- file.path(work_dir, "0404_RSF_folds1to4_default.xlsx")
fold_xlsx <- file.path(work_dir, "0404_DATA_5fold.xlsx")
if (!file.exists(fold_xlsx)) stop("未找到 0404_DATA_5fold.xlsx")

data_train <- readxl::read_xlsx(fold_xlsx, sheet = 1L) %>%
  dplyr::filter(fold %in% 1L:4L) %>%
  as.data.frame(stringsAsFactors = FALSE)

dat <- dplyr::select(data_train, -dplyr::any_of("fold")) %>%
  as.data.frame(stringsAsFactors = FALSE)

# 时间与事件列若为 Excel 读入的字符，先转为数值（避免后续被转成 factor 影响 Surv）
for (nm in c("los_hosp_days", "death")) {
  if (nm %in% names(dat) && is.character(dat[[nm]])) {
    dat[[nm]] <- suppressWarnings(as.numeric(dat[[nm]]))
  }
}
# rfsrc 要求：不能有 character 型列，其余字符型预测变量须为 factor
dat[] <- lapply(dat, function(x) if (is.character(x)) factor(x) else x)

p_pred <- ncol(dat) - 2L
# 偏快：树少、叶结点样本多；mtry 用常见 sqrt(p) 且不超过 p
ntree_rsf <- 200L
mtry_rsf <- max(1L, min(p_pred, as.integer(ceiling(sqrt(p_pred)))))
nodesize_rsf <- 20L


fit <- randomForestSRC::rfsrc(
  as.formula("Surv(los_hosp_days, death) ~ . - los_hosp_days - death"),
  data = dat,
  importance = FALSE,
  proximity = FALSE,
  na.action = "na.impute",
  seed = 123L
)



fit1 <- randomForestSRC::rfsrc(
  as.formula("Surv(los_hosp_days, death) ~ . - los_hosp_days - death"),
  data = dat,
  ntree = 200,
  mtry = mtry_rsf,
  nodesize = nodesize_rsf,
  importance = FALSE,
  proximity = FALSE,
  na.action = "na.impute",
  seed = 123L
)

# 第 5 折作为验证集（与 dat 相同预处理，便于 predict）
data_v <- readxl::read_xlsx(fold_xlsx, sheet = 1L) %>%
  dplyr::filter(fold == 5L) %>%
  dplyr::select(-dplyr::any_of("fold")) %>%
  as.data.frame(stringsAsFactors = FALSE)
for (nm in c("los_hosp_days", "death")) {
  if (nm %in% names(data_v) && is.character(data_v[[nm]])) {
    data_v[[nm]] <- suppressWarnings(as.numeric(data_v[[nm]]))
  }
}
data_v[] <- lapply(data_v, function(x) if (is.character(x)) factor(x) else x)
for (nm in names(data_v)) {
  if (is.factor(data_v[[nm]]) && nm %in% names(dat) && is.factor(dat[[nm]])) {
    data_v[[nm]] <- factor(as.character(data_v[[nm]]), levels = levels(dat[[nm]]))
  }
}





extract_risk_at_t0 <- function(fit_or_pred, t0, n) {
  risk <- NULL
  surv_probs <- NULL
  s <- fit_or_pred$survival
  ti <- fit_or_pred$time.interest
  if (!is.null(s) && !is.null(ti) && length(ti) > 0) {
    t_idx <- which.min(abs(ti - t0))
    surv_probs <- if (is.matrix(s)) as.numeric(s[, t_idx]) else as.numeric(s)
    if (length(surv_probs) == n) risk <- 1 - surv_probs
  }
  if (is.null(risk) && !is.null(fit_or_pred$predicted) && length(fit_or_pred$predicted) == n) {
    risk <- as.numeric(fit_or_pred$predicted)
    surv_probs <- pmax(0, pmin(1, 1 - risk))
  }
  list(risk = risk, surv_probs = surv_probs)
}

calc_metrics_v <- function(surv_time, surv_status, risk_marker, survival_probs, t0, df_for_cens) {
  roc1 <- tryCatch(
    timeROC::timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0),
    error = function(e) NULL
  )
  roc2 <- tryCatch(
    timeROC::timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0),
    error = function(e) NULL
  )
  auc1 <- if (!is.null(roc1) && !is.null(roc1$AUC)) {
    aucv <- as.numeric(roc1$AUC)
    if (length(aucv) >= 2) aucv[2] else aucv[1]
  } else NA_real_
  auc2 <- if (!is.null(roc2) && !is.null(roc2$AUC)) {
    aucv <- as.numeric(roc2$AUC)
    if (length(aucv) >= 2) aucv[2] else aucv[1]
  } else NA_real_
  AUC <- max(c(auc1, auc2), na.rm = TRUE)
  if (!is.finite(AUC)) AUC <- NA_real_

  CINDEX <- tryCatch(
    as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance),
    error = function(e) NA_real_
  )
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    CINDEX <- tryCatch(
      as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ I(-risk_marker))$concordance),
      error = function(e) NA_real_
    )
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- tryCatch(
    survival::survfit(survival::Surv(obs_time, 1 - event) ~ 1, data = df_for_cens),
    error = function(e) NULL
  )
  if (!is.null(censoring_model)) {
    get_weights <- function(time, event, censoring_model, t0) {
      cens_probs <- tryCatch(summary(censoring_model, times = pmin(time, t0))$surv, error = function(e) NULL)
      if (is.null(cens_probs) || length(cens_probs) != length(time)) {
        cens_probs <- tryCatch(
          rep(summary(censoring_model, times = t0)$surv, length(time)),
          error = function(e) rep(1, length(time))
        )
      }
      if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
      ifelse(
        time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
        ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0)
      )
    }
    weights <- get_weights(surv_time, surv_status, censoring_model, t0)
    BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)
  } else {
    BS <- mean((survival_probs - Y_obs)^2, na.rm = TRUE)
  }
  if (!is.finite(BS) || is.na(BS)) BS <- NA_real_
  list(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
}

cal_3 <- function(fit, t0) {
  if (!exists("data_v", inherits = TRUE)) stop("需要已存在的 data_v（验证集数据框）")
  nd <- data_v
  if (!all(c("los_hosp_days", "death") %in% names(nd))) {
    stop("data_v 须含列 los_hosp_days、death")
  }
  n_v <- nrow(nd)
  death_v <- as.integer(nd[["death"]])
  pred_v <- predict(fit, newdata = nd, na.action = "na.impute")
  ex_v <- extract_risk_at_t0(pred_v, t0, n_v)
  if (is.null(ex_v$risk) || length(ex_v$risk) != n_v) {
    stop("predict 未能得到与 data_v 行数一致的风险/生存概率")
  }
  if (anyNA(ex_v$surv_probs)) stop("该 t0 下预测生存概率含 NA，无法计算 BS/AUC")
  df_cens <- data.frame(obs_time = nd[["los_hosp_days"]], event = death_v)
  mt_v <- calc_metrics_v(
    nd[["los_hosp_days"]],
    death_v,
    ex_v$risk,
    ex_v$surv_probs,
    t0,
    df_cens
  )
  data.frame(
    cindex = mt_v$CINDEX,
    auc = mt_v$AUC,
    bs = mt_v$BS,
    t0 = round(t0, 4),
    row.names = NULL
  )
}

# t0：验证集全体患者 los_hosp_days 的中位数；亦可 cal_3(fit, 任意 t0)
t0_v <- stats::median(data_v[["los_hosp_days"]], na.rm = TRUE)
result1 <- cal_3(fit1, t0_v)
result<- cal_3(fit,t0_v)


############################























