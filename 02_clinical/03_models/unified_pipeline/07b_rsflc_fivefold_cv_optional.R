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

base <- readRDS(file.path(cache_dir, "dynamic_base.rds")) %>% filter(group == 1L)
long <- readRDS(file.path(cache_dir, "dynamic_long.rds")) %>% filter(hadm_id %in% base$hadm_id)
time_models <- stats::setNames(lapply(traj3, function(nm) {
  list(fixed = stats::as.formula(paste(nm, "~ time")), random = ~ time)
}), traj3)

rows <- list(); k <- 0L
for (g in seq_len(nrow(rsflc_grid))) {
  par_g <- rsflc_grid[g, ]
  for (fold_i in 1:5) {
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
    fit <- dynforest(
      timeData = time_tr, fixedData = fixed_tr,
      idVar = "hadm_id", timeVar = "time", timeVarModel = time_models,
      Y = list(type = "surv", Y = Y_tr),
      ntree = par_g$ntree, mtry = par_g$mtry, nodesize = par_g$nodesize,
      minsplit = rsflc_par$minsplit, cause = 1, nsplit_option = "quantile",
      ncores = min(8L, max(1L, parallel::detectCores() - 2L)),
      seed = 2026L, verbose = FALSE
    )
    pr <- predict(fit, timeData = time_va, fixedData = fixed_va,
                  idVar = "hadm_id", timeVar = "time", t0 = landmark_u)
    j <- which.min(abs(pr$times - 1))
    risk <- pr$pred_indiv[, j]
    ids <- suppressWarnings(as.integer(rownames(pr$pred_indiv)))
    if (length(ids) != length(risk) || anyNA(ids)) ids <- fixed_va$hadm_id[seq_along(risk)]
    out <- va_b[match(ids, va_b$hadm_id), ]
    G <- km_censor_function(tr_b$lm_time_u, tr_b$lm_status)
    k <- k + 1L
    rows[[k]] <- data.frame(
      grid_id = g, fold = fold_i, par_g,
      C_index = concordance(Surv(out$lm_time_u, out$lm_status) ~ risk, reverse = TRUE)$concordance,
      AUC_u1 = ipcw_auc(out$lm_time_u, out$lm_status, risk, 1, G),
      Brier_u1 = ipcw_brier(out$lm_time_u, out$lm_status, risk, 1, G)
    )
    write_utf8(bind_rows(rows), file.path(artifact_dir, "RSFLC_\u4e94\u6298CV_\u5206\u6298\u7ed3\u679c_\u53ef\u9009.csv"))
    log_progress("RSFLC_CV", sprintf("grid=%d/%d fold=%d/5", g, nrow(rsflc_grid), fold_i))
  }
}
raw <- bind_rows(rows)
summary <- raw %>% group_by(grid_id, ntree, mtry, nodesize) %>% summarise(
  C_index_mean = mean(C_index), C_index_sd = sd(C_index),
  AUC_mean = mean(AUC_u1), AUC_sd = sd(AUC_u1),
  Brier_mean = mean(Brier_u1), Brier_sd = sd(Brier_u1), .groups = "drop"
) %>% arrange(desc(C_index_mean), Brier_mean)
write_utf8(summary, file.path(artifact_dir, "RSFLC_\u4e94\u6298CV_\u7f51\u683c\u6c47\u603b_\u53ef\u9009.csv"))
cat("RSFLC_OPTIONAL_CV_OK\n")
