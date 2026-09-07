# =============================================================================
# 卒中 RSFLC 预测桥（factor 结局 → P(dead)）
# =============================================================================
# CLI:
#   Rscript RSFLC_stroke_predict.R <artifact_dir> <features.csv> <out.csv> [t0] [default_long_id]
# features.csv: top10 特征列 + 可选 long_id
# =============================================================================

suppressPackageStartupMessages({
  library(DynForest)
  library(jsonlite)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

load_artifacts <- function(artifact_dir) {
  meta <- fromJSON(file.path(artifact_dir, "meta.json"))
  model <- readRDS(file.path(artifact_dir, "rsflc_model.rds"))
  long_data <- read.csv(file.path(artifact_dir, "longitudinal.csv"), stringsAsFactors = FALSE)
  list(model = model, meta = meta, long_data = long_data, feature_cols = meta$feature_cols)
}

build_inputs <- function(features, long_data, feature_cols, default_long_id = NULL) {
  n <- nrow(features)
  if (is.null(default_long_id)) default_long_id <- long_data$id[1]
  if (!"long_id" %in% names(features)) features$long_id <- default_long_id
  features$pred_id <- seq_len(n)

  time_list <- vector("list", n)
  fixed_list <- vector("list", n)
  for (i in seq_len(n)) {
    lid <- features$long_id[i]
    pid <- features$pred_id[i]
    long_i <- long_data[long_data$id == lid, , drop = FALSE]
    if (nrow(long_i) == 0) stop(sprintf("long_id=%s 不存在", as.character(lid)))
    long_i$id <- pid
    time_list[[i]] <- long_i
    rowf <- features[i, feature_cols, drop = FALSE]
    for (v in feature_cols) rowf[[v]] <- as.numeric(rowf[[v]])
    fixed_list[[i]] <- data.frame(id = pid, rowf, check.names = FALSE)
  }
  list(
    timeData = do.call(rbind, time_list),
    fixedData = do.call(rbind, fixed_list),
    pred_id = features$pred_id
  )
}

extract_prob_dead <- function(pred, model) {
  dead_label <- if (!is.null(model$levels) && "dead" %in% model$levels) {
    "dead"
  } else if (!is.null(model$levels)) {
    model$levels[length(model$levels)]
  } else {
    "dead"
  }
  pred_class <- unname(pred$pred_indiv)
  proba <- unname(pred$pred_indiv_proba)
  ifelse(pred_class == dead_label, as.numeric(proba), 1 - as.numeric(proba))
}

predict_risk <- function(artifact_dir, features, t0 = 5, default_long_id = NULL) {
  art <- load_artifacts(artifact_dir)
  feature_cols <- art$feature_cols
  miss <- setdiff(feature_cols, names(features))
  if (length(miss) > 0) stop("特征缺失: ", paste(miss, collapse = ", "))
  if (!is.null(art$meta$params$t0)) t0 <- as.numeric(art$meta$params$t0)

  inputs <- build_inputs(features, art$long_data, feature_cols, default_long_id)
  pred <- predict(
    art$model,
    timeData  = inputs$timeData,
    fixedData = inputs$fixedData,
    idVar     = "id",
    timeVar   = "time",
    t0        = t0
  )
  risk <- extract_prob_dead(pred, art$model)
  data.frame(pred_id = inputs$pred_id, risk = risk)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    stop("用法: Rscript RSFLC_stroke_predict.R <artifact_dir> <features.csv> <out.csv> [t0] [default_long_id]")
  }
  artifact_dir <- args[1]
  feat_path <- args[2]
  out_path <- args[3]
  t0 <- as.numeric(args[4] %||% 5)
  default_long_id <- if (length(args) >= 5 && nzchar(args[5])) as.numeric(args[5]) else NULL

  features <- read.csv(feat_path, stringsAsFactors = FALSE)
  res <- predict_risk(artifact_dir, features, t0 = t0, default_long_id = default_long_id)
  write.csv(res, out_path, row.names = FALSE)
  message("预测完成 n=", nrow(res), " -> ", out_path)
}
