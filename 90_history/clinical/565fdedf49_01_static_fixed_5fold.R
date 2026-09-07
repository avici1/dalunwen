suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
})

supp_root <- Sys.getenv("CH5_SUPPLEMENT_ROOT", unset = "supplement_output")
script_root <- Sys.getenv("CH5_SOURCE_CODE", unset = "source_code")
model_root <- Sys.getenv("CH5_SOURCE_MODELS", unset = "source_models")
data_root <- Sys.getenv("CH5_SOURCE_DATA", unset = "source_data")
Sys.setenv(CH5_OUTPUT_DIR = Sys.getenv("CH5_SOURCE_OUTPUT", unset = "source_output"))
source(file.path(script_root, "00_config.R"), encoding = "UTF-8")
tbl_out <- file.path(supp_root, "tables")
log_out <- file.path(supp_root, "logs")
dir.create(tbl_out, recursive = TRUE, showWarnings = FALSE)
dir.create(log_out, recursive = TRUE, showWarnings = FALSE)

write_utf8_local <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

static_data <- readRDS(file.path(model_root, "static_data.rds"))
fold_key <- data.table::fread(file.path(data_root, "baseline.csv"),
                              select = c("hadm_id", "fold"), encoding = "UTF-8") %>%
  transmute(hadm_id = as.integer(hadm_id), fold = as.integer(fold)) %>%
  distinct(hadm_id, .keep_all = TRUE)
d <- static_data %>%
  filter(group == 1L) %>%
  left_join(fold_key, by = "hadm_id") %>%
  filter(fold %in% 1:5)
if (n_distinct(d$fold) != 5L) stop("静态训练队列未获得完整5折标识")

fold_path <- file.path(tbl_out, "Table_5-3A_RSF_fixed_5fold_by_fold.csv")
existing <- if (file.exists(fold_path)) read.csv(fold_path, check.names = FALSE) else data.frame()
rows <- list()
if (nrow(existing)) rows[[1L]] <- existing
options(rf.cores = max(1L, parallel::detectCores(logical = TRUE) - 2L))

for (k in 1:5) {
  if (nrow(existing) && k %in% existing$fold) {
    message("跳过已完成静态fold=", k)
    next
  }
  train0 <- d %>% filter(fold != k) %>% select(-hadm_id, -group, -fold) %>% as.data.frame()
  valid0 <- d %>% filter(fold == k) %>% select(-hadm_id, -group, -fold) %>% as.data.frame()
  for (nm in intersect(factor_vars, names(train0))) {
    train0[[nm]] <- factor(train0[[nm]])
    valid0[[nm]] <- factor(valid0[[nm]], levels = levels(train0[[nm]]))
  }
  set.seed(seed_value + k)
  fit <- randomForestSRC::rfsrc(
    Surv(time28, status28) ~ ., data = train0,
    ntree = rsf_par$ntree, mtry = rsf_par$mtry,
    nodesize = rsf_par$nodesize, nsplit = rsf_par$nsplit,
    splitrule = "logrank", importance = FALSE,
    block.size = 1L, na.action = "na.impute", seed = seed_value + k
  )
  pr <- predict(fit, newdata = valid0, na.action = "na.impute")
  risk <- t(vapply(seq_len(nrow(valid0)), function(i) {
    1 - step_at(pr$time.interest, pr$survival[i, ], static_times, initial = 1)
  }, numeric(length(static_times))))
  G_train <- km_censor_function(train0$time28, train0$status28)
  met <- metric_bundle(valid0$time28, valid0$status28, risk, static_times, G_train)
  one <- data.frame(
    fold = k, n_train = nrow(train0), n_valid = nrow(valid0),
    events_valid = sum(valid0$status28 == 1L),
    C_index = met$cindex, AUC_28 = tail(met$auc, 1),
    Brier_28 = tail(met$brier, 1), IBS_0_28 = met$ibs
  )
  existing <- bind_rows(existing, one) %>% arrange(fold)
  write_utf8_local(existing, fold_path)
  rm(fit, pr, risk); gc()
}

metrics <- c("C_index", "AUC_28", "Brier_28", "IBS_0_28")
summary_tbl <- data.frame(
  metric = metrics,
  mean = vapply(existing[metrics], mean, numeric(1), na.rm = TRUE),
  sd = vapply(existing[metrics], sd, numeric(1), na.rm = TRUE)
)
write_utf8_local(summary_tbl, file.path(tbl_out, "Table_5-3A_RSF_fixed_5fold_summary.csv"))

fmt <- function(nm) sprintf("%.4f ± %.4f", summary_tbl$mean[summary_tbl$metric == nm], summary_tbl$sd[summary_tbl$metric == nm])
table53a <- data.frame(
  model = "RSF",
  search_space = "ntree={300,500,1000}; mtry={3,6,9}; nodesize={10,20,30,40}; nsplit={10,25,50}",
  fixed_parameters = sprintf("ntree=%d; mtry=%d; nodesize=%d; nsplit=%d", rsf_par$ntree, rsf_par$mtry, rsf_par$nodesize, rsf_par$nsplit),
  cv_cindex = fmt("C_index"),
  cv_auc = fmt("AUC_28"),
  cv_brier = fmt("Brier_28"),
  cv_ibs = fmt("IBS_0_28"),
  note = "当前31变量口径下对锁定参数组合进行5折评价；并非重新遍历完整参数网格",
  check.names = FALSE
)
names(table53a) <- c("模型", "搜索空间", "固定评价参数", "五折验证 C-index", "五折验证 AUC", "五折验证 Brier", "五折验证 IBS", "说明")
write_utf8_local(table53a, file.path(tbl_out, "Table_5-3A_RSF_fixed_5fold_ready_to_fill.csv"))
cat("STATIC_FIXED_5FOLD_OK\n")
