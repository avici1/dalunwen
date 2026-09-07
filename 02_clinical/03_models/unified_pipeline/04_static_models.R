suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
  library(riskRegression)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
options(rf.cores = max(1L, min(12L, parallel::detectCores(logical = TRUE) - 2L)))
log_progress("STATIC_CORE_START", "COX and RSF with locked 31 predictors")

d <- readRDS(file.path(cache_dir, "static_data.rds"))
train0 <- d %>% filter(group == 1L)
valid0 <- d %>% filter(group == 2L)
model_cols <- c("time_u", "status28", static31)
train <- train0[, model_cols, drop = FALSE] %>% as.data.frame()
valid <- valid0[, model_cols, drop = FALSE] %>% as.data.frame()

for (nm in intersect(factor_vars, static31)) {
  train[[nm]] <- factor(train[[nm]])
  valid[[nm]] <- factor(valid[[nm]], levels = levels(train[[nm]]))
}

cox_path <- file.path(cache_dir, "cox_static_31.rds")
rsf_path <- file.path(cache_dir, "rsf_static_31.rds")

if (file.exists(cox_path)) {
  cox_fit <- readRDS(cox_path)
} else {
  cox_formula <- as.formula(paste("Surv(time_u, status28) ~", paste(static31, collapse = " + ")))
  cox_fit <- coxph(cox_formula, data = train, ties = "efron", x = TRUE, y = TRUE, model = TRUE)
  saveRDS(cox_fit, cox_path)
}

if (file.exists(rsf_path)) {
  rsf_fit <- readRDS(rsf_path)
} else {
  set.seed(seed_value)
  rsf_formula <- as.formula(paste("Surv(time_u, status28) ~", paste(static31, collapse = " + ")))
  rsf_fit <- rfsrc(
    rsf_formula, data = train,
    ntree = rsf_par$ntree, mtry = rsf_par$mtry,
    nodesize = rsf_par$nodesize, nsplit = rsf_par$nsplit,
    splitrule = "logrank", importance = FALSE,
    block.size = 1L, na.action = "na.impute", seed = seed_value
  )
  saveRDS(rsf_fit, rsf_path)
}

cox_risk_matrix <- function(newdata, times) {
  out <- as.matrix(riskRegression::predictRisk(cox_fit, newdata = newdata, times = times))
  if (ncol(out) != length(times)) out <- matrix(out, nrow = nrow(newdata), ncol = length(times))
  out
}
rsf_risk_matrix <- function(newdata, times) {
  pr <- predict(rsf_fit, newdata = newdata, na.action = "na.impute")
  out <- t(vapply(seq_len(nrow(newdata)), function(i) {
    1 - step_at(pr$time.interest, pr$survival[i, ], times, initial = 1)
  }, numeric(length(times))))
  colnames(out) <- times
  out
}

cox_train_risk <- cox_risk_matrix(train, static_eval_u)
cox_valid_risk <- cox_risk_matrix(valid, static_eval_u)
rsf_train_risk <- rsf_risk_matrix(train, static_eval_u)
rsf_valid_risk <- rsf_risk_matrix(valid, static_eval_u)
G_train <- km_censor_function(train$time_u, train$status28)

cox_train_metrics <- metric_bundle(train$time_u, train$status28, cox_train_risk, static_eval_u, G_train)
cox_valid_metrics <- metric_bundle(valid$time_u, valid$status28, cox_valid_risk, static_eval_u, G_train)
rsf_train_metrics <- metric_bundle(train$time_u, train$status28, rsf_train_risk, static_eval_u, G_train)
rsf_valid_metrics <- metric_bundle(valid$time_u, valid$status28, rsf_valid_risk, static_eval_u, G_train)
rsf_oob_c <- 1 - tail(as.numeric(rsf_fit$err.rate), 1L)

table_5_4a <- data.frame(
  model = c("COX", "RSF"),
  setting = c(
    "31 predictors; Efron ties",
    sprintf("ntree=%d; mtry=%d; nodesize=%d; nsplit=%d", rsf_par$ntree, rsf_par$mtry, rsf_par$nodesize, rsf_par$nsplit)
  ),
  train_C_index = c(cox_train_metrics$cindex, rsf_oob_c),
  validation_C_index = c(cox_valid_metrics$cindex, rsf_valid_metrics$cindex),
  AUC_u1 = c(tail(cox_valid_metrics$auc, 1L), tail(rsf_valid_metrics$auc, 1L)),
  Brier_u1 = c(tail(cox_valid_metrics$brier, 1L), tail(rsf_valid_metrics$brier, 1L)),
  IBS_0_1 = c(cox_valid_metrics$ibs, rsf_valid_metrics$ibs)
)
write_utf8(table_5_4a, file.path(artifact_dir, "\u88685-4A_\u9759\u6001\u4efb\u52a1\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6027\u80fd.csv"))

curve_tbl <- bind_rows(
  data.frame(model = "COX", u = static_eval_u, day = static_eval_u * 28,
             AUC = cox_valid_metrics$auc, Brier = cox_valid_metrics$brier),
  data.frame(model = "RSF", u = static_eval_u, day = static_eval_u * 28,
             AUC = rsf_valid_metrics$auc, Brier = rsf_valid_metrics$brier)
)
write_utf8(curve_tbl, file.path(artifact_dir, "\u9759\u6001\u4efb\u52a1_\u65f6\u95f4\u4f9d\u8d56\u6027\u80fd.csv"))

ci_static <- bind_rows(
  transform(bootstrap_metrics(valid$time_u, valid$status28, cox_valid_risk, static_eval_u,
                              G_train, cluster_id = valid0$subject_id), model = "COX"),
  transform(bootstrap_metrics(valid$time_u, valid$status28, rsf_valid_risk, static_eval_u,
                              G_train, cluster_id = valid0$subject_id), model = "RSF")
)
write_utf8(ci_static, file.path(artifact_dir, "\u88685-4A_95CI_\u60a3\u8005\u7c07bootstrap.csv"))

sm <- summary(cox_fit)
coef_terms <- rownames(sm$coefficients)
scale_unit <- ifelse(coef_terms == "ph", 0.01, 1)
cox_coef <- data.frame(
  term = coef_terms,
  beta = sm$coefficients[, "coef"],
  display_increment = scale_unit,
  HR = exp(sm$coefficients[, "coef"] * scale_unit),
  lower95 = exp((sm$coefficients[, "coef"] - 1.96 * sm$coefficients[, "se(coef)"]) * scale_unit),
  upper95 = exp((sm$coefficients[, "coef"] + 1.96 * sm$coefficients[, "se(coef)"]) * scale_unit),
  p_value = sm$coefficients[, "Pr(>|z|)"], row.names = NULL
)
write_utf8(cox_coef, file.path(artifact_dir, "\u88685-5_COX\u6a21\u578b\u534f\u53d8\u91cf\u98ce\u9669\u6bd4.csv"))

ph <- cox.zph(cox_fit)
ph_tbl <- data.frame(term = rownames(ph$table), ph$table, row.names = NULL)
write_utf8(ph_tbl, file.path(artifact_dir, "COX_\u6bd4\u4f8b\u98ce\u9669\u5047\u8bbe\u68c0\u9a8c.csv"))

set.seed(seed_value)
vimp_obj <- vimp(rsf_fit, importance = "permute", seed = seed_value)
vimp_tbl <- data.frame(variable = names(vimp_obj$importance), OOB_VIMP = as.numeric(vimp_obj$importance)) %>%
  arrange(desc(OOB_VIMP))
write_utf8(vimp_tbl, file.path(artifact_dir, "RSF_OOB_VIMP.csv"))

md_obj <- max.subtree(rsf_fit)
md_order <- md_obj[["order"]]
md_tbl <- data.frame(variable = rownames(md_order), minimal_depth = md_order[, 1], row.names = NULL) %>%
  arrange(minimal_depth)
write_utf8(md_tbl, file.path(artifact_dir, "RSF_\u6700\u5c0f\u6df1\u5ea6.csv"))

# 表5-3A 写入的是文章已锁定的选参结果。重新网格搜索见 07a_rsf_fivefold_cv_optional.R。
table_5_3a <- data.frame(
  model = "RSF",
  search_space = "ntree={300,500,1000}; mtry={3,6,9}; nodesize={10,20,30,40}; nsplit={10,25,50}",
  selected_parameters = "ntree=500; mtry=3; nodesize=10; nsplit=10",
  fivefold_C_index_mean_sd = "0.8274 +/- 0.0090",
  fivefold_AUC_mean_sd = "0.8850 +/- 0.0097",
  fivefold_IPCW_Brier_mean_sd = "0.0872 +/- 0.0074",
  note = "Current-article selection result; optional CV script retained but not rerun"
)
write_utf8(table_5_3a, file.path(artifact_dir, "\u88685-3A_\u9759\u6001\u4efb\u52a1RSF\u8d85\u53c2\u6570\u7ec4\u5408.csv"))

saveRDS(list(
  train = train0, validation = valid0,
  cox_train_risk = cox_train_risk, cox_validation_risk = cox_valid_risk,
  rsf_train_risk = rsf_train_risk, rsf_validation_risk = rsf_valid_risk,
  eval_u = static_eval_u, G_train = G_train
), file.path(cache_dir, "static_predictions.rds"))

log_progress("STATIC_CORE_OK", sprintf("COX_C=%.4f; RSF_C=%.4f", cox_valid_metrics$cindex, rsf_valid_metrics$cindex))
cat("STATIC_CORE_OK\n")
