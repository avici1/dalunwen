# =============================================================================
# RSFLC (DynForest) 建模：从 cal_3_RSF_LC 提取，并纳入 V1–V10 供 SHAP
# =============================================================================
# 原版 cal_3_RSF_LC 的 fixedData 只有 lp/class，不含 V1–V10。
# 本脚本将 V1–V10（每 ID 取首次随访）作为固定协变量加入 DynForest，
# 纵向标记仍为 Y，结局为 Surv(obs_time, event)。
#
# 注意：DynForest >=1.2 的 rf 树结构已变化，原「叶子风险」提取不可用；
#       改为 predict() 在指定时点的 CIF 作为风险标量。
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(DynForest)
  library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

prepare_RSFLC_data <- function(data, feature_cols = paste0("V", 1:10)) {
  stopifnot(all(c("ID", "t", "Y", "obs_time", "event") %in% names(data)))
  miss <- setdiff(feature_cols, names(data))
  if (length(miss) > 0) stop("缺少特征列: ", paste(miss, collapse = ", "))

  longitudinal_data <- data %>%
    transmute(id = as.numeric(ID), time = as.numeric(t), Y = as.numeric(Y))

  survival_data <- data %>%
    group_by(ID) %>%
    summarise(
      time  = unique(obs_time)[1],
      event = unique(event)[1],
      .groups = "drop"
    ) %>%
    transmute(id = as.numeric(ID), time = as.numeric(time), event = as.numeric(event))

  # V1–V10：最早随访作为基线固定协变量（SHAP 解释对象）
  baseline_V <- data %>%
    group_by(ID) %>%
    slice_min(order_by = t, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(id = as.numeric(ID), across(all_of(feature_cols), as.numeric))

  fixed_data <- survival_data %>%
    select(id) %>%
    left_join(baseline_V, by = "id") %>%
    arrange(id)

  list(
    longitudinal_data = as.data.frame(longitudinal_data),
    survival_data     = as.data.frame(survival_data),
    fixed_data        = as.data.frame(fixed_data),
    feature_cols      = feature_cols
  )
}

# 从 predict.dynforest 提取标量风险（指定时点 CIF，越高风险越大）
extract_risk_from_pred <- function(pred_obj, t_horizon) {
  pi <- pred_obj$pred_indiv
  times <- as.numeric(pred_obj$times)
  if (is.null(pi)) stop("predict 结果缺少 pred_indiv")
  pi <- as.matrix(pi)
  if (length(times) != ncol(pi)) {
    return(as.numeric(pi[, ncol(pi)]))
  }
  j <- which.min(abs(times - t_horizon))
  as.numeric(pi[, j])
}

predict_risk_scores <- function(model, longitudinal_data, fixed_data,
                                t0 = 1, t_horizon = 2) {
  pred <- predict(
    model,
    timeData  = as.data.frame(longitudinal_data),
    fixedData = as.data.frame(fixed_data),
    idVar     = "id",
    timeVar   = "time",
    t0        = t0
  )
  extract_risk_from_pred(pred, t_horizon = t_horizon)
}

fit_RSFLC <- function(data,
                      feature_cols = paste0("V", 1:10),
                      ntree = 100,
                      mtry = NULL,
                      nodesize = 10,
                      minsplit = 2,
                      ncores = 1,
                      seed = 123,
                      t0 = 1,
                      t_horizon = 2) {
  prep <- prepare_RSFLC_data(data, feature_cols = feature_cols)
  p <- length(feature_cols)
  if (is.null(mtry)) mtry <- max(1L, min(3L, p))

  set.seed(seed)
  dyn_model <- DynForest::dynforest(
    timeData     = prep$longitudinal_data,
    fixedData    = prep$fixed_data,
    idVar        = "id",
    timeVar      = "time",
    timeVarModel = list(
      Y = list(
        model  = "linear",
        fixed  = ~ 1,
        random = ~ 1 + time | id
      )
    ),
    Y = list(
      type = "surv",
      Y = data.frame(
        id    = prep$survival_data$id,
        time  = prep$survival_data$time,
        event = as.numeric(prep$survival_data$event)
      )
    ),
    ntree         = ntree,
    mtry          = mtry,
    nodesize      = nodesize,
    minsplit      = minsplit,
    nsplit_option = "quantile",
    ncores        = ncores,
    seed          = seed,
    verbose       = TRUE
  )

  risk_score <- predict_risk_scores(
    dyn_model, prep$longitudinal_data, prep$fixed_data,
    t0 = t0, t_horizon = t_horizon
  )

  # 不反转模型输出：SHAP 必须解释 predict() 的原始标量
  if (requireNamespace("survival", quietly = TRUE) && all(is.finite(risk_score))) {
    c0 <- survival::concordance(
      survival::Surv(prep$survival_data$time, prep$survival_data$event) ~ risk_score
    )$concordance
    c_dir <- if (!is.na(c0) && c0 < 0.5) 1 - c0 else c0
    message(sprintf(
      "C-index raw=%.4f | 方向校正后=%.4f  (SHAP 使用原始 risk，不反转)",
      c0, c_dir
    ))
  }

  list(
    model             = dyn_model,
    longitudinal_data = prep$longitudinal_data,
    survival_data     = prep$survival_data,
    fixed_data        = prep$fixed_data,
    feature_cols      = feature_cols,
    risk_score        = risk_score,
    params = list(
      ntree = ntree, mtry = mtry, nodesize = nodesize, minsplit = minsplit,
      seed = seed, t0 = t0, t_horizon = t_horizon
    )
  )
}

save_RSFLC_artifacts <- function(fit, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  model_path <- file.path(out_dir, "rsflc_model.rds")
  saveRDS(fit$model, model_path)

  feat <- fit$fixed_data[, c("id", fit$feature_cols), drop = FALSE]
  feat$risk_score <- fit$risk_score
  write.csv(feat, file.path(out_dir, "subject_features.csv"), row.names = FALSE)
  write.csv(fit$longitudinal_data, file.path(out_dir, "longitudinal.csv"), row.names = FALSE)
  write.csv(fit$survival_data, file.path(out_dir, "survival.csv"), row.names = FALSE)

  meta <- list(
    feature_cols = fit$feature_cols,
    n_subjects   = nrow(feat),
    params       = fit$params,
    model_path   = normalizePath(model_path, winslash = "/", mustWork = FALSE),
    note = paste(
      "RSFLC=DynForest; longitudinal marker=Y;",
      "fixed covariates=V1-V10 (first visit);",
      "risk = predicted CIF at t_horizon;",
      "SHAP explains subject-level V1-V10 -> risk"
    )
  )
  writeLines(toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
            file.path(out_dir, "meta.json"))
  message("已保存: ", normalizePath(out_dir, winslash = "/"))
  invisible(list(model_path = model_path, out_dir = out_dir, meta = meta))
}

# ---------- CLI ----------
# Rscript RSFLC_fit_model.R <xlsx> <out_dir> [ntree] [t_horizon]
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  data_path  <- args[1] %||% "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_30_10V_highBTW_1c_L5.xlsx"
  out_dir    <- args[2] %||% file.path(getwd(), "rsflc_shap_artifacts")
  ntree      <- as.integer(args[3] %||% 100)
  t_horizon  <- as.numeric(args[4] %||% 2)

  message("读取: ", data_path)
  raw <- as.data.frame(read_xlsx(data_path))
  message(sprintf("行=%d 列=%d ntree=%d t_horizon=%.2f", nrow(raw), ncol(raw), ntree, t_horizon))

  fit <- fit_RSFLC(raw, ntree = ntree, ncores = 1, t0 = 1, t_horizon = t_horizon)
  save_RSFLC_artifacts(fit, out_dir)
  message("拟合完成。")
}
