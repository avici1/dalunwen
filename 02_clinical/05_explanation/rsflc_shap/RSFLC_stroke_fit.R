# =============================================================================
# 卒中数据 RSFLC：变量筛选 Top10 + DynForest 拟合 + 导出 SHAP 接口产物
# =============================================================================
# 基线: stroke_baselinedata_filter1_0531.csv (sep=;)
# 纵向: stroke_longitudinal_inputed_0603.csv
# 纵向标记: gcs (前 5 天, 每人>=2条) —— 对齐既有建模RSFLC.R
# 结局: death_28d (factor: alive/dead)
# 固定协变量: 从候选池中用随机森林筛出重要性最高的 10 个
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(DynForest)
  library(jsonlite)
  library(randomForest)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------- 路径 ----------
BASE_PATH <- "F:/文章_大论文/0521/处理后文件/stroke_baselinedata_filter1_0531.csv"
LONG_PATH <- "F:/文章_大论文/0521/处理后文件/stroke_longitudinal_inputed_0603.csv"
OUT_DIR   <- "F:/文章_大论文/0722/pythonProject1/stroke_RSFLC_SHAP"

# CLI: Rscript RSFLC_stroke_fit.R [out_dir] [ntree] [max_n] [t0]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) OUT_DIR <- args[1]
NTREE  <- as.integer(args[2] %||% 40)
MAX_N  <- as.integer(args[3] %||% 1200)   # DynForest 子样；全队列太慢
T0     <- as.numeric(args[4] %||% 5)
SEED   <- 1234L
TOP_K  <- 10L

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
message(sprintf("OUT_DIR=%s | ntree=%d | max_n=%d | t0=%g", OUT_DIR, NTREE, MAX_N, T0))

# ---------- 读数 ----------
baseline <- read.csv2(BASE_PATH, stringsAsFactors = FALSE)
longit   <- read.csv(LONG_PATH, stringsAsFactors = FALSE)

# ---------- 纵向 GCS（前5天）----------
timeData_all <- longit %>%
  filter(times <= 5) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    time    = as.integer(times),
    gcs     = as.numeric(gcs)
  ) %>%
  group_by(hadm_id) %>%
  filter(n() >= 2, sum(is.finite(gcs)) == n()) %>%
  ungroup() %>%
  as.data.frame()

valid_ids <- unique(timeData_all$hadm_id)
message("合格患者(前5天GCS>=2条): ", length(valid_ids))

# ---------- 候选固定协变量池 ----------
# 基线评分/人口学 + 第1天纵向快照（不含 gcs，gcs 已作轨迹标记）
day1 <- longit %>%
  filter(times == 1) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    sofa_d1 = as.numeric(sofa_24hours),
    lactate_d1 = as.numeric(lactate),
    creat_d1 = as.numeric(creat),
    aki_d1 = as.numeric(aki_stage),
    hr_d1 = as.numeric(item_220045),
    rr_d1 = as.numeric(item_220210),
    spo2_d1 = as.numeric(item_220277),
    plate_d1 = as.numeric(item_51265),
    bun_d1 = as.numeric(item_51006),
    urine_d1 = as.numeric(total_urine_output),
    ph_d1 = as.numeric(ph),
    hb_d1 = as.numeric(hemoglobin),
    map_d1 = as.numeric(item_220181)
  ) %>%
  distinct(hadm_id, .keep_all = TRUE)

fixed_pool <- baseline %>%
  filter(hadm_id %in% valid_ids) %>%
  transmute(
    hadm_id = as.integer(hadm_id),
    age = as.numeric(age),
    charlson = as.numeric(charlson_comorbidity_index),
    apsiii = as.numeric(apsiii),
    sapsii = as.numeric(sapsii),
    oasis = as.numeric(oasis),
    preiculos = as.numeric(preiculos),
    mechvent = as.numeric(mechvent),
    electivesurgery = as.numeric(electivesurgery),
    death_28d = as.integer(death_28d)
  ) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  left_join(day1, by = "hadm_id")

cand_cols <- setdiff(names(fixed_pool), c("hadm_id", "death_28d"))
# 去掉全 NA / 零方差
keep <- vapply(cand_cols, function(v) {
  x <- fixed_pool[[v]]
  is.numeric(x) && sum(is.finite(x)) > 50 && sd(x, na.rm = TRUE) > 0
}, logical(1))
cand_cols <- cand_cols[keep]
message("候选变量数: ", length(cand_cols), " -> ", paste(cand_cols, collapse = ", "))

# 简单中位数填补（missForest 太慢；候选已基本无缺失）
for (v in cand_cols) {
  x <- fixed_pool[[v]]
  if (anyNA(x)) {
    med <- median(x, na.rm = TRUE)
    fixed_pool[[v]][is.na(x)] <- med
  }
}

# ---------- 随机森林筛选 Top10 ----------
set.seed(SEED)
rf_x <- as.data.frame(fixed_pool[, cand_cols, drop = FALSE])
rf_y <- factor(fixed_pool$death_28d, levels = c(0, 1), labels = c("alive", "dead"))
# 子样加速筛选（最多 3000）
rf_n <- min(nrow(rf_x), 3000L)
rf_idx <- sample.int(nrow(rf_x), rf_n)
message("RF 筛选样本量: ", rf_n)
rf_fit <- randomForest(
  x = rf_x[rf_idx, , drop = FALSE],
  y = rf_y[rf_idx],
  ntree = 300,
  importance = TRUE,
  na.action = na.omit
)
imp <- importance(rf_fit, type = 1)  # MeanDecreaseAccuracy
imp_vec <- sort(imp[, 1], decreasing = TRUE)
top10 <- names(imp_vec)[seq_len(min(TOP_K, length(imp_vec)))]
message("Top10: ", paste(top10, collapse = ", "))

imp_df <- data.frame(
  variable = names(imp_vec),
  MeanDecreaseAccuracy = as.numeric(imp_vec),
  selected = names(imp_vec) %in% top10,
  stringsAsFactors = FALSE
)
write.csv(imp_df, file.path(OUT_DIR, "variable_importance_RF.csv"), row.names = FALSE)
writeLines(top10, file.path(OUT_DIR, "top10_variables.txt"))

# ---------- 分层子样供 DynForest ----------
set.seed(SEED)
pool2 <- fixed_pool %>% select(hadm_id, death_28d, all_of(top10))
ids0 <- pool2$hadm_id[pool2$death_28d == 0]
ids1 <- pool2$hadm_id[pool2$death_28d == 1]
n1 <- min(length(ids1), max(50L, as.integer(round(MAX_N * mean(pool2$death_28d)))))
n0 <- min(length(ids0), MAX_N - n1)
samp_ids <- c(sample(ids0, n0), sample(ids1, n1))
message(sprintf("DynForest 子样 n=%d (dead=%d, alive=%d)", length(samp_ids), n1, n0))

timeData <- timeData_all %>% filter(hadm_id %in% samp_ids) %>% arrange(hadm_id, time)
fixedData <- pool2 %>%
  filter(hadm_id %in% samp_ids) %>%
  select(hadm_id, all_of(top10)) %>%
  arrange(hadm_id) %>%
  as.data.frame()
# 保证数值型（DynForest 对 factor 也可，这里统一 numeric）
for (v in top10) fixedData[[v]] <- as.numeric(fixedData[[v]])

Y <- list(
  type = "factor",
  Y = pool2 %>%
    filter(hadm_id %in% samp_ids) %>%
    transmute(
      hadm_id = as.integer(hadm_id),
      event = factor(death_28d, levels = c(0, 1), labels = c("alive", "dead"))
    ) %>%
    arrange(hadm_id) %>%
    as.data.frame()
)

stopifnot(
  identical(sort(unique(timeData$hadm_id)), sort(fixedData$hadm_id)),
  identical(sort(fixedData$hadm_id), sort(Y$Y$hadm_id))
)

timeVarModel <- list(
  gcs = list(fixed = gcs ~ time, random = ~ time)
)

mtry <- max(1L, min(3L, length(top10)))
message("开始 DynForest: ntree=", NTREE, " mtry=", mtry)
set.seed(SEED)
dyn_model <- DynForest::dynforest(
  timeData     = timeData,
  fixedData    = fixedData,
  idVar        = "hadm_id",
  timeVar      = "time",
  timeVarModel = timeVarModel,
  Y            = Y,
  ntree        = NTREE,
  mtry         = mtry,
  nodesize     = 5,
  minsplit     = 2,
  ncores       = 1,
  seed         = SEED,
  verbose      = TRUE
)

# ---------- 训练预测：P(dead) ----------
message("训练样本 predict ...")
pred <- predict(
  dyn_model,
  timeData = timeData,
  fixedData = fixedData,
  idVar = "hadm_id",
  timeVar = "time",
  t0 = T0
)
dead_label <- if ("dead" %in% dyn_model$levels) "dead" else dyn_model$levels[length(dyn_model$levels)]
pred_class <- unname(pred$pred_indiv)
prob_dead <- ifelse(
  pred_class == dead_label,
  unname(pred$pred_indiv_proba),
  1 - unname(pred$pred_indiv_proba)
)
risk_df <- data.frame(
  hadm_id = as.integer(names(pred$pred_indiv)),
  risk = as.numeric(prob_dead),
  pred_class = as.character(pred_class),
  stringsAsFactors = FALSE
)
y_df <- Y$Y %>% transmute(hadm_id, y_dead = as.integer(event == "dead"))
risk_df <- merge(risk_df, y_df, by = "hadm_id", sort = FALSE)

if (requireNamespace("pROC", quietly = TRUE)) {
  ok <- is.finite(risk_df$risk) & !is.na(risk_df$y_dead)
  if (sum(ok) > 10 && length(unique(risk_df$y_dead[ok])) > 1) {
    auc <- as.numeric(pROC::auc(pROC::roc(
      response = risk_df$y_dead[ok],
      predictor = risk_df$risk[ok],
      levels = c(0, 1), direction = "<", quiet = TRUE
    )))
    message(sprintf("训练表观 AUC = %.4f", auc))
  }
}

# ---------- 保存产物 ----------
saveRDS(dyn_model, file.path(OUT_DIR, "rsflc_model.rds"))

# longitudinal 统一列名 id/time/gcs，便于预测桥
long_out <- timeData %>% transmute(id = hadm_id, time = time, gcs = gcs)
write.csv(long_out, file.path(OUT_DIR, "longitudinal.csv"), row.names = FALSE)

feat_out <- fixedData %>%
  rename(id = hadm_id) %>%
  left_join(risk_df %>% transmute(id = hadm_id, risk, y_dead), by = "id")
write.csv(feat_out, file.path(OUT_DIR, "subject_features.csv"), row.names = FALSE)

meta <- list(
  data = list(baseline = BASE_PATH, longitudinal = LONG_PATH),
  feature_cols = top10,
  idVar = "id",
  timeVar = "time",
  marker = "gcs",
  outcome = "death_28d",
  model_type = "factor",
  dead_label = dead_label,
  params = list(
    ntree = NTREE, mtry = mtry, t0 = T0, max_n = MAX_N, seed = SEED,
    times_max = 5
  ),
  n_subjects = nrow(fixedData),
  note = "RSFLC=DynForest; longitudinal=gcs(<=5d); fixed=RF-selected top10; risk=P(dead)"
)
writeLines(toJSON(meta, auto_unbox = TRUE, pretty = TRUE), file.path(OUT_DIR, "meta.json"))

# DynForest 自带 VIMP（若可用）作对照
vimp_ok <- tryCatch({
  vv <- DynForest::compute_vimp(dyn_model, pcs = FALSE)
  # 结构因版本而异，尽量写出
  if (is.list(vv) && !is.null(vv$importance)) {
    write.csv(as.data.frame(vv$importance), file.path(OUT_DIR, "dynforest_vimp.csv"), row.names = TRUE)
  } else {
    capture.output(print(vv), file = file.path(OUT_DIR, "dynforest_vimp.txt"))
  }
  TRUE
}, error = function(e) {
  message("compute_vimp 跳过: ", e$message)
  FALSE
})

message("完成。产物目录: ", normalizePath(OUT_DIR, winslash = "/"))
