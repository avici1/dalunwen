cal_3_test <- function(model, t0, timeData, fixedData, baseline_data) {
  stopifnot(inherits(model, "dynforest"))
  stopifnot(is.data.frame(timeData), is.data.frame(fixedData), is.data.frame(baseline_data))
  if (model$type != "factor") {
    stop("cal_3_test 仅支持 dynforest 分类结局 (type = 'factor')")
  }
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("请先安装 pROC: install.packages('pROC')")
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("请先安装 survival")
  }

  idVar <- "hadm_id"
  timeVar <- model$timeVar
  model_name <- attr(model, "model_name", exact = TRUE)
  if (is.null(model_name)) model_name <- "dynforest"

  dead_label <- "dead"
  if (!dead_label %in% model$levels) {
    dead_label <- model$levels[length(model$levels)]
  }

  eval_ids <- unique(as.integer(fixedData[[idVar]]))
  y_df <- baseline_data %>%
    dplyr::filter(hadm_id %in% eval_ids) %>%
    dplyr::transmute(
      hadm_id = as.integer(hadm_id),
      y_dead  = as.integer(death_28d == 1)
    ) %>%
    dplyr::distinct(hadm_id, .keep_all = TRUE) %>%
    as.data.frame()

  if (!all(c(idVar, "intime", "deathtime", "death_28d") %in% names(baseline_data))) {
    stop("baseline_data 需含 hadm_id, intime, deathtime, death_28d")
  }

  cat(
    "[cal_3_test] 模型:", model_name,
    "| t0 =", t0,
    "| n =", length(eval_ids), "\n"
  )

  cat("[cal_3_test] 正在 predict（样本量大时耗时较长）...\n")
  pred_dyn <- predict(
    object    = model,
    timeData  = timeData,
    fixedData = fixedData,
    idVar     = idVar,
    timeVar   = timeVar,
    t0        = t0
  )

  pred_hadm_id <- as.integer(names(pred_dyn$pred_indiv))
  pred_class   <- unname(pred_dyn$pred_indiv)
  prob_dead    <- ifelse(
    pred_class == dead_label,
    unname(pred_dyn$pred_indiv_proba),
    1 - unname(pred_dyn$pred_indiv_proba)
  )

  pred_df <- data.frame(
    hadm_id   = pred_hadm_id,
    prob_dead = prob_dead,
    stringsAsFactors = FALSE
  )
  pred_df <- merge(pred_df, y_df, by = "hadm_id", sort = FALSE)
  cat("[cal_3_test] 完成预测与 pred_df 整理 | nrow =", nrow(pred_df), "\n")

  # 剔除非有限 prob_dead（NA / Inf / -Inf），与调试代码一致
  n_na   <- sum(is.na(pred_df$prob_dead))
  n_inf  <- sum(pred_df$prob_dead == Inf,  na.rm = TRUE)
  n_ninf <- sum(pred_df$prob_dead == -Inf, na.rm = TRUE)
  pred_df_eval <- pred_df[is.finite(pred_df$prob_dead), , drop = FALSE]

  cat(
    "[cal_3_test] prob_dead NA 数:", n_na,
    "| Inf 数:", n_inf,
    "| -Inf 数:", n_ninf,
    "| 用于指标计算样本数:", nrow(pred_df_eval),
    "（剔除非有限值后）\n"
  )

  if (nrow(pred_df_eval) == 0) {
    stop("cal_3_test：剔除 NA/Inf 后无有效 prob_dead，无法计算指标")
  }
  if (length(unique(pred_df_eval$y_dead)) < 2) {
    stop("cal_3_test：有效样本中 y_dead 仅单一类别，无法计算 AUC")
  }

  roc_obj <- pROC::roc(
    response  = pred_df_eval$y_dead,
    predictor = pred_df_eval$prob_dead,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )
  auc_val <- as.numeric(pROC::auc(roc_obj))
  cat("[cal_3_test] 完成计算 AUC =", round(auc_val, 4), "\n")

  eval_cindex <- pred_df
  base_sub <- baseline_data[, c(idVar, "intime", "deathtime", "death_28d")]
  base_sub <- base_sub[!duplicated(base_sub[[idVar]]), , drop = FALSE]
  eval_cindex <- merge(eval_cindex, base_sub, by = idVar, all.x = TRUE, sort = FALSE)

  intime_parsed <- as.POSIXct(eval_cindex$intime, format = "%d/%m/%Y %H:%M:%S")
  deathtime_chr <- ifelse(
    eval_cindex$deathtime == "" | is.na(eval_cindex$deathtime),
    NA_character_,
    eval_cindex$deathtime
  )
  deathtime_parsed <- as.POSIXct(deathtime_chr, format = "%d/%m/%Y %H:%M:%S")
  time_to_death <- as.numeric(difftime(deathtime_parsed, intime_parsed, units = "days"))
  time_to_death[is.na(time_to_death)] <- 28
  eval_cindex$time28   <- pmin(time_to_death, 28)
  eval_cindex$status28 <- as.integer(eval_cindex$death_28d == 1)

  eval_cindex_ci <- eval_cindex[is.finite(eval_cindex$prob_dead), , drop = FALSE]
  cindex_obj <- survival::concordance(
    survival::Surv(eval_cindex_ci$time28, eval_cindex_ci$status28) ~ eval_cindex_ci$prob_dead,
    reverse = TRUE
  )
  cindex_val <- cindex_obj$concordance
  cat("[cal_3_test] 完成计算 C-index =", round(cindex_val, 4), "\n")

  bs_val <- mean((pred_df_eval$y_dead - pred_df_eval$prob_dead)^2)
  cat("[cal_3_test] 完成计算 Brier Score =", round(bs_val, 4), "\n")

  out_df <- data.frame(
    model   = model_name,
    auc     = round(auc_val, 4),
    cindex  = round(cindex_val, 4),
    bs      = round(bs_val, 4),
    n       = nrow(pred_df_eval),
    n_total = nrow(pred_df),
    stringsAsFactors = FALSE
  )
  cat("[cal_3_test] 全部指标计算完成\n")
  out_df
}