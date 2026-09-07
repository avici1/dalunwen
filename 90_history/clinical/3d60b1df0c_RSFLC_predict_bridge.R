# =============================================================================
# RSFLC 预测桥：供 Python Kernel SHAP 调用
# =============================================================================
# 输入 features.csv: V1..V10，可选 long_id（克隆哪位受试者的纵向 Y 轨迹）
# 输出: pred_id, risk  （t_horizon 处 CIF）
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

extract_risk_from_pred <- function(pred_obj, t_horizon) {
  pi <- as.matrix(pred_obj$pred_indiv)
  times <- as.numeric(pred_obj$times)
  if (length(times) == ncol(pi)) {
    j <- which.min(abs(times - t_horizon))
    as.numeric(pi[, j])
  } else {
    as.numeric(pi[, ncol(pi)])
  }
}

build_predict_inputs <- function(features, long_data, feature_cols, default_long_id = NULL) {
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
    fixed_list[[i]] <- data.frame(
      id = pid,
      features[i, feature_cols, drop = FALSE],
      check.names = FALSE
    )
  }
  list(
    timeData = do.call(rbind, time_list),
    fixedData = do.call(rbind, fixed_list),
    pred_id = features$pred_id
  )
}

predict_RSFLC_risk <- function(artifact_dir, features, t0 = 1, t_horizon = 2,
                               default_long_id = NULL) {
  art <- load_artifacts(artifact_dir)
  feature_cols <- art$feature_cols
  miss <- setdiff(feature_cols, names(features))
  if (length(miss) > 0) stop("特征缺失: ", paste(miss, collapse = ", "))

  # 若 meta 里有训练时的 t_horizon，优先使用
  if (!is.null(art$meta$params$t_horizon)) {
    t_horizon <- as.numeric(art$meta$params$t_horizon)
  }
  if (!is.null(art$meta$params$t0)) {
    t0 <- as.numeric(art$meta$params$t0)
  }

  inputs <- build_predict_inputs(features, art$long_data, feature_cols, default_long_id)
  pred <- predict(
    art$model,
    timeData  = inputs$timeData,
    fixedData = inputs$fixedData,
    idVar     = "id",
    timeVar   = "time",
    t0        = t0
  )
  risk <- extract_risk_from_pred(pred, t_horizon = t_horizon)
  data.frame(pred_id = inputs$pred_id, risk = risk)
}

# CLI:
# Rscript RSFLC_predict_bridge.R <artifact_dir> <features.csv> <out.csv> [t0] [default_long_id] [t_horizon]
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    stop("用法: Rscript RSFLC_predict_bridge.R <artifact_dir> <features.csv> <out.csv> [t0] [default_long_id] [t_horizon]")
  }
  artifact_dir <- args[1]
  feat_path <- args[2]
  out_path <- args[3]
  t0 <- as.numeric(args[4] %||% 1)
  default_long_id <- if (length(args) >= 5 && nzchar(args[5])) as.numeric(args[5]) else NULL
  t_horizon <- as.numeric(args[6] %||% 2)

  features <- read.csv(feat_path, stringsAsFactors = FALSE)
  res <- predict_RSFLC_risk(
    artifact_dir = artifact_dir,
    features = features,
    t0 = t0,
    t_horizon = t_horizon,
    default_long_id = default_long_id
  )
  write.csv(res, out_path, row.names = FALSE)
  message("预测完成 n=", nrow(res), " -> ", out_path)
}
