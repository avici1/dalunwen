suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(DynForest)
})

args <- commandArgs(trailingOnly = TRUE)
args <- args[!grepl("\\.R$", args, ignore.case = TRUE)]
if (!length(args)) stop("用法: Rscript 02_dynamic_fixed_5fold_worker.R <fold>")
k <- as.integer(args[1])
if (!k %in% 1:5) stop("fold必须为1到5")

supp_root <- Sys.getenv("CH5_SUPPLEMENT_ROOT", unset = "supplement_output")
script_root <- Sys.getenv("CH5_SOURCE_CODE", unset = "source_code")
model_root <- Sys.getenv("CH5_SOURCE_MODELS", unset = "source_models")
table_root <- Sys.getenv("CH5_SOURCE_TABLES", unset = "source_tables")
data_root <- Sys.getenv("CH5_SOURCE_DATA", unset = "source_data")
Sys.setenv(CH5_OUTPUT_DIR = Sys.getenv("CH5_SOURCE_OUTPUT", unset = "source_output"))
source(file.path(script_root, "00_config.R"), encoding = "UTF-8")
tbl_out <- file.path(supp_root, "tables", "RSFLC_5fold")
model_out <- file.path(supp_root, "model_cache", "RSFLC_5fold")
dir.create(tbl_out, recursive = TRUE, showWarnings = FALSE)
dir.create(model_out, recursive = TRUE, showWarnings = FALSE)
result_path <- file.path(tbl_out, sprintf("fold_%d.csv", k))
if (file.exists(result_path)) {
  message("fold=", k, " 已存在，跳过")
  quit(save = "no", status = 0L)
}

base <- readRDS(file.path(model_root, "dynamic_base.rds"))
long <- readRDS(file.path(model_root, "dynamic_long.rds"))
fold_key <- data.table::fread(file.path(data_root, "baseline.csv"),
                              select = c("hadm_id", "fold"), encoding = "UTF-8") %>%
  transmute(hadm_id = as.integer(hadm_id), fold = as.integer(fold)) %>%
  distinct(hadm_id, .keep_all = TRUE)
base <- base %>% filter(group == 1L) %>% left_join(fold_key, by = "hadm_id") %>% filter(fold %in% 1:5)
bad_path <- file.path(table_root, "JM_undefined_prediction_ids.csv")
if (file.exists(bad_path)) {
  bad <- suppressWarnings(read.csv(bad_path)$hadm_id)
  base <- base %>% filter(!hadm_id %in% bad)
}
long <- long %>% filter(hadm_id %in% base$hadm_id)

train_b <- base %>% filter(fold != k)
valid_b <- base %>% filter(fold == k)
train_l <- long %>% filter(hadm_id %in% train_b$hadm_id)
valid_l <- long %>% filter(hadm_id %in% valid_b$hadm_id)
for (nm in intersect(factor_vars, names(train_b))) {
  train_b[[nm]] <- factor(train_b[[nm]])
  valid_b[[nm]] <- factor(valid_b[[nm]], levels = levels(train_b[[nm]]))
}
fixed_train <- train_b %>% select(hadm_id, all_of(dynamic_fixed25)) %>% as.data.frame()
fixed_valid <- valid_b %>% select(hadm_id, all_of(dynamic_fixed25)) %>% as.data.frame()
time_train <- train_l %>% select(hadm_id, time, all_of(traj3)) %>% as.data.frame()
time_valid <- valid_l %>% select(hadm_id, time, all_of(traj3)) %>% as.data.frame()
Y_train <- train_b %>% select(hadm_id, time = lm_time, event = lm_status) %>% as.data.frame()
time_models <- stats::setNames(lapply(traj3, function(nm) {
  list(fixed = stats::as.formula(paste(nm, "~ time")), random = ~ time)
}), traj3)

ncores <- as.integer(Sys.getenv("RSFLC_WORKER_CORES", unset = "2"))
set.seed(seed_value + k)
fit <- DynForest::dynforest(
  timeData = time_train, fixedData = fixed_train,
  idVar = "hadm_id", timeVar = "time", timeVarModel = time_models,
  Y = list(type = "surv", Y = Y_train),
  ntree = rsflc_par$ntree, mtry = rsflc_par$mtry,
  nodesize = rsflc_par$nodesize, minsplit = rsflc_par$minsplit,
  cause = 1, nsplit_option = "quantile", ncores = ncores,
  seed = 2026L, verbose = TRUE
)

p <- predict(fit, timeData = time_valid, fixedData = fixed_valid,
             idVar = "hadm_id", timeVar = "time", t0 = landmark)
ids <- suppressWarnings(as.integer(rownames(p$pred_indiv)))
if (length(ids) != nrow(p$pred_indiv) || anyNA(ids)) ids <- as.integer(fixed_valid$hadm_id[seq_len(nrow(p$pred_indiv))])
risk <- t(vapply(seq_len(nrow(p$pred_indiv)), function(i) {
  idx <- findInterval(dynamic_times, p$times)
  out <- numeric(length(dynamic_times)); keep <- idx > 0L
  out[keep] <- p$pred_indiv[i, pmin(idx[keep], ncol(p$pred_indiv))]
  out
}, numeric(length(dynamic_times))))
eval <- valid_b[match(ids, valid_b$hadm_id), ]
G_train <- km_censor_function(train_b$lm_time, train_b$lm_status)
met <- metric_bundle(eval$lm_time, eval$lm_status, risk, dynamic_times, G_train)
one <- data.frame(
  fold = k, n_train = nrow(train_b), n_valid = nrow(eval),
  events_valid = sum(eval$lm_status == 1L),
  C_index = met$cindex, AUC_28 = tail(met$auc, 1),
  Brier_28 = tail(met$brier, 1), IBS_5_28 = met$ibs,
  ntree = rsflc_par$ntree, mtry = rsflc_par$mtry,
  nodesize = rsflc_par$nodesize, minsplit = rsflc_par$minsplit
)
utils::write.csv(one, result_path, row.names = FALSE, fileEncoding = "UTF-8")
if (tolower(Sys.getenv("SAVE_RSFLC_CV_MODELS", unset = "false")) == "true") {
  saveRDS(fit, file.path(model_out, sprintf("rsflc_fold_%d.rds", k)))
}
cat("RSFLC_FIXED_5FOLD_WORKER_OK fold=", k, "\n", sep = "")
