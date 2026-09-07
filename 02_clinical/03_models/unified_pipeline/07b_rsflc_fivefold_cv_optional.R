# =============================================================================
# 表5-3B：动态任务 RSFLC 五折超参数网格搜索（文章最终口径）
#
# 变量：3 条轨迹 traj3 = {gcs, sofa_24hours, cns_24hours}
#       + 28 个固定变量 dynamic_fixed28（11 基线 + 第1天 17 个非轨迹截面）
# 搜索空间（00_config.R / rsflc_grid）：
#   ntree    = {50, 100, 200}
#   mtry     = {3, 6, 9, 12}
#   nodesize = {1, 3, 5}
# minsplit 固定为 rsflc_par$minsplit（文章取值 2）；nsplit_option=quantile。
# landmark = 第 5 天，预测至第 28 天。仅 group=1；group=2 不参与选参。
# 选优：最大化五折验证 C-index 均值，并列时取更小的 IPCW-Brier。
#
# 运行前需先完成 01_stage_inputs.R、02_prepare_data.R。
# 默认不执行。运行：
#   Sys.setenv(RUN_OPTIONAL_CV = "true")
#   source("07b_rsflc_fivefold_cv_optional.R", encoding = "UTF-8")
#
# 更早的 20 条轨迹五折原稿在：
#   ../individual_0826/rsflc/RSFLC_20V_tune_fold5.R
# 用外验证集选参的 0826 网格在：
#   ../individual_0826/rsflc/0826_RSFLC超参数筛选.R
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(DynForest)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
if (!tolower(Sys.getenv("RUN_OPTIONAL_CV", unset = "false")) %in% c("true", "1", "yes")) {
  stop("Optional RSFLC CV is disabled. Set RUN_OPTIONAL_CV=true to execute the full grid.")
}

base_path <- file.path(cache_dir, "dynamic_base.rds")
long_path <- file.path(cache_dir, "dynamic_long.rds")
if (!file.exists(base_path) || !file.exists(long_path)) {
  stop("Missing dynamic cache. Run 01_stage_inputs.R and 02_prepare_data.R first.")
}

base <- readRDS(base_path) %>% filter(group == 1L, fold %in% 1:5)
long <- readRDS(long_path) %>% filter(hadm_id %in% base$hadm_id)
if (dplyr::n_distinct(base$fold) != 5L) stop("Training data does not contain folds 1-5.")
if (any(base$group != 1L)) stop("group=2 must not enter RSFLC hyperparameter search.")

time_models <- stats::setNames(lapply(traj3, function(nm) {
  list(fixed = stats::as.formula(paste(nm, "~ time")), random = ~ time)
}), traj3)
ncores_use <- min(8L, max(1L, parallel::detectCores() - 2L))

path_folds <- file.path(artifact_dir, "RSFLC_\u4e94\u6298CV_\u5206\u6298\u7ed3\u679c_\u53ef\u9009.csv")
path_summary <- file.path(artifact_dir, "RSFLC_\u4e94\u6298CV_\u7f51\u683c\u6c47\u603b_\u53ef\u9009.csv")
path_best <- file.path(artifact_dir, "RSFLC_\u4e94\u6298CV_\u6700\u4f18\u8d85\u53c2\u6570.csv")
path_table <- file.path(artifact_dir, "\u88685-3B_\u52a8\u6001\u4efb\u52a1RSFLC\u8d85\u53c2\u6570\u7ec4\u5408_\u4e94\u6298\u641c\u7d22.csv")

existing <- if (file.exists(path_folds)) {
  utils::read.csv(path_folds, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
} else {
  data.frame()
}
rows <- if (nrow(existing)) split(existing, seq_len(nrow(existing))) else list()
k_done <- nrow(existing)

grid <- rsflc_grid
grid$grid_id <- seq_len(nrow(grid))
log_progress("RSFLC_CV_START", sprintf("combos=%d folds=5 traj=%d fixed=%d",
                                       nrow(grid), length(traj3), length(dynamic_fixed28)))

for (g in seq_len(nrow(grid))) {
  par_g <- grid[g, ]
  for (fold_i in 1:5) {
    already <- nrow(existing) > 0L && any(
      existing$grid_id == par_g$grid_id & existing$fold == fold_i &
        is.finite(existing$C_index)
    )
    if (already) {
      log_progress("RSFLC_CV", sprintf("skip grid=%d fold=%d", g, fold_i))
      next
    }
    tr_b <- base %>% filter(fold != fold_i)
    va_b <- base %>% filter(fold == fold_i)
    tr_l <- long %>% filter(hadm_id %in% tr_b$hadm_id)
    va_l <- long %>% filter(hadm_id %in% va_b$hadm_id)
    for (nm in intersect(factor_vars, dynamic_fixed28)) {
      lev <- levels(factor(tr_b[[nm]]))
      tr_b[[nm]] <- factor(tr_b[[nm]], levels = lev)
      va_b[[nm]] <- factor(va_b[[nm]], levels = lev)
    }
    fixed_tr <- tr_b %>% select(hadm_id, all_of(dynamic_fixed28)) %>% as.data.frame()
    fixed_va <- va_b %>% select(hadm_id, all_of(dynamic_fixed28)) %>% as.data.frame()
    time_tr <- tr_l %>% select(hadm_id, time = time_u, all_of(traj3)) %>% as.data.frame()
    time_va <- va_l %>% select(hadm_id, time = time_u, all_of(traj3)) %>% as.data.frame()
    Y_tr <- tr_b %>% select(hadm_id, time = lm_time_u, event = lm_status) %>% as.data.frame()
    set.seed(seed_value + g * 10L + fold_i)
    fit <- tryCatch(
      dynforest(
        timeData = time_tr, fixedData = fixed_tr,
        idVar = "hadm_id", timeVar = "time", timeVarModel = time_models,
        Y = list(type = "surv", Y = Y_tr),
        ntree = par_g$ntree, mtry = par_g$mtry, nodesize = par_g$nodesize,
        minsplit = rsflc_par$minsplit, cause = 1, nsplit_option = "quantile",
        ncores = ncores_use, seed = 2026L, verbose = FALSE
      ),
      error = function(e) NULL
    )
    c_val <- auc_val <- brier_val <- NA_real_
    if (!is.null(fit)) {
      pr <- tryCatch(
        predict(fit, timeData = time_va, fixedData = fixed_va,
                idVar = "hadm_id", timeVar = "time", t0 = landmark_u),
        error = function(e) NULL
      )
      if (!is.null(pr) && !is.null(pr$pred_indiv)) {
        j <- which.min(abs(pr$times - 1))
        risk <- pr$pred_indiv[, j]
        ids <- suppressWarnings(as.integer(rownames(pr$pred_indiv)))
        if (length(ids) != length(risk) || anyNA(ids)) ids <- fixed_va$hadm_id[seq_along(risk)]
        out <- va_b[match(ids, va_b$hadm_id), ]
        keep <- is.finite(risk) & !is.na(out$lm_status)
        if (any(keep)) {
          G <- km_censor_function(tr_b$lm_time_u, tr_b$lm_status)
          c_val <- survival::concordance(
            Surv(out$lm_time_u[keep], out$lm_status[keep]) ~ risk[keep], reverse = TRUE
          )$concordance
          auc_val <- ipcw_auc(out$lm_time_u[keep], out$lm_status[keep], risk[keep], 1, G)
          brier_val <- ipcw_brier(out$lm_time_u[keep], out$lm_status[keep], risk[keep], 1, G)
        }
      }
    }
    k_done <- k_done + 1L
    rows[[k_done]] <- data.frame(
      grid_id = par_g$grid_id, fold = fold_i,
      ntree = par_g$ntree, mtry = par_g$mtry, nodesize = par_g$nodesize,
      minsplit = rsflc_par$minsplit,
      n_train = nrow(tr_b), n_valid = nrow(va_b),
      C_index = as.numeric(c_val), AUC_u1 = as.numeric(auc_val), Brier_u1 = as.numeric(brier_val)
    )
    write_utf8(bind_rows(rows), path_folds)
    log_progress("RSFLC_CV", sprintf("grid=%d/%d fold=%d/5 C=%.4f", g, nrow(grid), fold_i, c_val))
  }
}

raw <- bind_rows(rows)
summary <- raw %>%
  group_by(grid_id, ntree, mtry, nodesize) %>%
  summarise(
    n_folds = sum(is.finite(C_index)),
    C_index_mean = mean(C_index, na.rm = TRUE), C_index_sd = sd(C_index, na.rm = TRUE),
    AUC_mean = mean(AUC_u1, na.rm = TRUE), AUC_sd = sd(AUC_u1, na.rm = TRUE),
    Brier_mean = mean(Brier_u1, na.rm = TRUE), Brier_sd = sd(Brier_u1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_folds), desc(C_index_mean), Brier_mean)
write_utf8(summary, path_summary)

eligible <- summary %>% filter(n_folds == 5L, is.finite(C_index_mean))
if (nrow(eligible) == 0L) stop("No RSFLC hyperparameter combination completed all 5 folds.")
best <- eligible[1, ]
write_utf8(
  data.frame(
    parameter = c("grid_id", "ntree", "mtry", "nodesize", "minsplit",
                  "nsplit_option", "landmark_u", "selection", "group_used"),
    value = c(best$grid_id, best$ntree, best$mtry, best$nodesize, rsflc_par$minsplit,
              "quantile", landmark_u, "max mean 5-fold C-index, then min IPCW-Brier", 1)
  ),
  path_best
)

fmt <- function(m, s) sprintf("%.4f +/- %.4f", m, s)
table_5_3b <- data.frame(
  model = "RSFLC",
  search_space = "ntree={50,100,200}; mtry={3,6,9,12}; nodesize={1,3,5}; fixed uL=5/28; nsplit_option=quantile",
  selected_parameters = sprintf("ntree=%d; mtry=%d; nodesize=%d; minsplit=%d; uL=5/28",
                                best$ntree, best$mtry, best$nodesize, rsflc_par$minsplit),
  fivefold_C_index_mean_sd = fmt(best$C_index_mean, best$C_index_sd),
  fivefold_AUC_mean_sd = fmt(best$AUC_mean, best$AUC_sd),
  fivefold_IPCW_Brier_mean_sd = fmt(best$Brier_mean, best$Brier_sd),
  note = "Selected on group=1 five-fold CV only; group=2 held out"
)
write_utf8(table_5_3b, path_table)
log_progress("RSFLC_CV_OK", table_5_3b$selected_parameters)
cat("RSFLC_OPTIONAL_CV_OK\n")
print(table_5_3b, row.names = FALSE)
