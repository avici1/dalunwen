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

d <- readRDS(file.path(cache_dir, "static_data.rds")) %>% filter(group == 1L)
rows <- list(); k <- 0L
for (g in seq_len(nrow(rsf_grid))) {
  par_g <- rsf_grid[g, ]
  for (fold_i in 1:5) {
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
    fit <- rfsrc(f, data = tr, ntree = par_g$ntree, mtry = par_g$mtry,
                 nodesize = par_g$nodesize, nsplit = par_g$nsplit,
                 splitrule = "logrank", na.action = "na.impute", importance = FALSE)
    pr <- predict(fit, newdata = va, na.action = "na.impute")
    j <- which.min(abs(pr$time.interest - 1))
    risk <- 1 - pr$survival[, j]
    G <- km_censor_function(tr$time_u, tr$status28)
    k <- k + 1L
    rows[[k]] <- data.frame(
      grid_id = g, fold = fold_i, par_g,
      C_index = concordance(Surv(va$time_u, va$status28) ~ risk, reverse = TRUE)$concordance,
      AUC_u1 = ipcw_auc(va$time_u, va$status28, risk, 1, G),
      Brier_u1 = ipcw_brier(va$time_u, va$status28, risk, 1, G)
    )
    write_utf8(bind_rows(rows), file.path(artifact_dir, "RSF_\u4e94\u6298CV_\u5206\u6298\u7ed3\u679c_\u53ef\u9009.csv"))
    log_progress("RSF_CV", sprintf("grid=%d/%d fold=%d/5", g, nrow(rsf_grid), fold_i))
  }
}
raw <- bind_rows(rows)
summary <- raw %>% group_by(grid_id, ntree, mtry, nodesize, nsplit) %>% summarise(
  C_index_mean = mean(C_index), C_index_sd = sd(C_index),
  AUC_mean = mean(AUC_u1), AUC_sd = sd(AUC_u1),
  Brier_mean = mean(Brier_u1), Brier_sd = sd(Brier_u1), .groups = "drop"
) %>% arrange(desc(C_index_mean), Brier_mean)
write_utf8(summary, file.path(artifact_dir, "RSF_\u4e94\u6298CV_\u7f51\u683c\u6c47\u603b_\u53ef\u9009.csv"))
cat("RSF_OPTIONAL_CV_OK\n")
