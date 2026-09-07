# =============================================================================
# 表5-3A：静态任务 RSF 五折超参数网格搜索（文章最终 31 变量口径）
#
# 搜索空间（00_config.R / rsf_grid）：
#   ntree    = {300, 500, 1000}
#   mtry     = {3, 6, 9}
#   nodesize = {10, 20, 30, 40}
#   nsplit   = {10, 25, 50}
# 共 108 组 × 5 折。仅 group=1；group=2 不参与选参。
# 选优：最大化五折验证 C-index 均值，并列时取更小的 IPCW-Brier。
#
# 运行前需先完成 01_stage_inputs.R、02_prepare_data.R。
# 默认不执行。运行：
#   Sys.setenv(RUN_OPTIONAL_CV = "true")
#   source("07a_rsf_fivefold_cv_optional.R", encoding = "UTF-8")
#
# 更早的完整原稿（8 基线 + 第1天 20 截面、折内插补）在：
#   ../individual_0826/rsf/0830_RSF_表5-3A_五折交叉验证调参.R
# 用外验证集选参的 0826 网格在：
#   ../individual_0826/rsf/0826_RSF_超参数筛选.R
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
if (!tolower(Sys.getenv("RUN_OPTIONAL_CV", unset = "false")) %in% c("true", "1", "yes")) {
  stop("Optional RSF CV is disabled. Set RUN_OPTIONAL_CV=true to execute the full grid.")
}

cache_path <- file.path(cache_dir, "static_data.rds")
if (!file.exists(cache_path)) {
  stop("Missing cache/static_data.rds. Run 01_stage_inputs.R and 02_prepare_data.R first.")
}

options(rf.cores = max(1L, min(12L, parallel::detectCores(logical = TRUE) - 2L)))
d <- readRDS(cache_path) %>% filter(group == 1L, fold %in% 1:5)
if (dplyr::n_distinct(d$fold) != 5L) stop("Training data does not contain folds 1-5.")
if (any(d$group != 1L)) stop("group=2 must not enter RSF hyperparameter search.")

path_folds <- file.path(artifact_dir, "RSF_\u4e94\u6298CV_\u5206\u6298\u7ed3\u679c_\u53ef\u9009.csv")
path_summary <- file.path(artifact_dir, "RSF_\u4e94\u6298CV_\u7f51\u683c\u6c47\u603b_\u53ef\u9009.csv")
path_best <- file.path(artifact_dir, "RSF_\u4e94\u6298CV_\u6700\u4f18\u8d85\u53c2\u6570.csv")
path_table <- file.path(artifact_dir, "\u88685-3A_\u9759\u6001\u4efb\u52a1RSF\u8d85\u53c2\u6570\u7ec4\u5408_\u4e94\u6298\u641c\u7d22.csv")

existing <- if (file.exists(path_folds)) {
  utils::read.csv(path_folds, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
} else {
  data.frame()
}
rows <- if (nrow(existing)) split(existing, seq_len(nrow(existing))) else list()
k_done <- nrow(existing)

grid <- rsf_grid
grid$grid_id <- seq_len(nrow(grid))
log_progress("RSF_CV_START", sprintf("combos=%d folds=5 predictors=%d", nrow(grid), length(static31)))

for (g in seq_len(nrow(grid))) {
  par_g <- grid[g, ]
  for (fold_i in 1:5) {
    already <- nrow(existing) > 0L && any(
      existing$grid_id == par_g$grid_id & existing$fold == fold_i &
        is.finite(existing$C_index)
    )
    if (already) {
      log_progress("RSF_CV", sprintf("skip grid=%d fold=%d", g, fold_i))
      next
    }
    tr0 <- d %>% filter(fold != fold_i)
    va0 <- d %>% filter(fold == fold_i)
    tr <- tr0[, c("time_u", "status28", static31), drop = FALSE] %>% as.data.frame()
    va <- va0[, c("time_u", "status28", static31), drop = FALSE] %>% as.data.frame()
    for (nm in intersect(factor_vars, static31)) {
      tr[[nm]] <- factor(tr[[nm]])
      va[[nm]] <- factor(va[[nm]], levels = levels(tr[[nm]]))
    }
    set.seed(seed_value + g * 10L + fold_i)
    f <- as.formula(paste("Surv(time_u,status28) ~", paste(static31, collapse = " + ")))
    fit <- tryCatch(
      rfsrc(
        f, data = tr, ntree = par_g$ntree, mtry = par_g$mtry,
        nodesize = par_g$nodesize, nsplit = par_g$nsplit,
        splitrule = "logrank", na.action = "na.impute", importance = FALSE,
        seed = seed_value + g * 10L + fold_i
      ),
      error = function(e) NULL
    )
    c_val <- auc_val <- brier_val <- NA_real_
    if (!is.null(fit)) {
      pr <- predict(fit, newdata = va, na.action = "na.impute")
      j <- which.min(abs(pr$time.interest - 1))
      risk <- 1 - pr$survival[, j]
      G <- km_censor_function(tr$time_u, tr$status28)
      c_val <- survival::concordance(Surv(va$time_u, va$status28) ~ risk, reverse = TRUE)$concordance
      auc_val <- ipcw_auc(va$time_u, va$status28, risk, 1, G)
      brier_val <- ipcw_brier(va$time_u, va$status28, risk, 1, G)
    }
    k_done <- k_done + 1L
    rows[[k_done]] <- data.frame(
      grid_id = par_g$grid_id, fold = fold_i,
      ntree = par_g$ntree, mtry = par_g$mtry,
      nodesize = par_g$nodesize, nsplit = par_g$nsplit,
      n_train = nrow(tr), n_valid = nrow(va),
      C_index = as.numeric(c_val), AUC_u1 = as.numeric(auc_val), Brier_u1 = as.numeric(brier_val)
    )
    write_utf8(bind_rows(rows), path_folds)
    log_progress("RSF_CV", sprintf("grid=%d/%d fold=%d/5 C=%.4f", g, nrow(grid), fold_i, c_val))
  }
}

raw <- bind_rows(rows)
summary <- raw %>%
  group_by(grid_id, ntree, mtry, nodesize, nsplit) %>%
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
if (nrow(eligible) == 0L) stop("No RSF hyperparameter combination completed all 5 folds.")
best <- eligible[1, ]
write_utf8(
  data.frame(
    parameter = c("grid_id", "ntree", "mtry", "nodesize", "nsplit",
                  "selection", "predictors", "group_used"),
    value = c(best$grid_id, best$ntree, best$mtry, best$nodesize, best$nsplit,
              "max mean 5-fold C-index, then min IPCW-Brier", length(static31), 1)
  ),
  path_best
)

fmt <- function(m, s) sprintf("%.4f +/- %.4f", m, s)
table_5_3a <- data.frame(
  model = "RSF",
  search_space = "ntree={300,500,1000}; mtry={3,6,9}; nodesize={10,20,30,40}; nsplit={10,25,50}",
  selected_parameters = sprintf("ntree=%d; mtry=%d; nodesize=%d; nsplit=%d",
                                best$ntree, best$mtry, best$nodesize, best$nsplit),
  fivefold_C_index_mean_sd = fmt(best$C_index_mean, best$C_index_sd),
  fivefold_AUC_mean_sd = fmt(best$AUC_mean, best$AUC_sd),
  fivefold_IPCW_Brier_mean_sd = fmt(best$Brier_mean, best$Brier_sd),
  note = "Selected on group=1 five-fold CV only; group=2 held out"
)
write_utf8(table_5_3a, path_table)
log_progress("RSF_CV_OK", table_5_3a$selected_parameters)
cat("RSF_OPTIONAL_CV_OK\n")
print(table_5_3a, row.names = FALSE)
