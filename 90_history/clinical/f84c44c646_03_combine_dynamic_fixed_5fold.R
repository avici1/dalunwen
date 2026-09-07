suppressPackageStartupMessages(library(dplyr))

supp_root <- Sys.getenv("CH5_SUPPLEMENT_ROOT", unset = "supplement_output")
tbl_out <- file.path(supp_root, "tables")
fold_dir <- file.path(tbl_out, "RSFLC_5fold")
paths <- file.path(fold_dir, sprintf("fold_%d.csv", 1:5))
missing <- paths[!file.exists(paths)]
if (length(missing)) stop("尚缺少以下折结果: ", paste(basename(missing), collapse = ", "))
folds <- bind_rows(lapply(paths, read.csv, check.names = FALSE)) %>% arrange(fold)
utils::write.csv(folds, file.path(tbl_out, "Table_5-3B_RSFLC_fixed_5fold_by_fold.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
metrics <- c("C_index", "AUC_28", "Brier_28", "IBS_5_28")
summary_tbl <- data.frame(
  metric = metrics,
  mean = vapply(folds[metrics], mean, numeric(1), na.rm = TRUE),
  sd = vapply(folds[metrics], sd, numeric(1), na.rm = TRUE)
)
utils::write.csv(summary_tbl, file.path(tbl_out, "Table_5-3B_RSFLC_fixed_5fold_summary.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
fmt <- function(nm) sprintf("%.4f ± %.4f", summary_tbl$mean[summary_tbl$metric == nm], summary_tbl$sd[summary_tbl$metric == nm])
table53b <- data.frame(
  model = "RSFLC",
  search_space = "ntree={50,100,200}; mtry={3,6,9,12}; nodesize={1,3,5}; minsplit=2; t0=5",
  fixed_parameters = "ntree=200; mtry=3; nodesize=1; minsplit=2; t0=5",
  cv_cindex = fmt("C_index"),
  cv_auc = fmt("AUC_28"),
  cv_brier = fmt("Brier_28"),
  cv_ibs = fmt("IBS_5_28"),
  note = "当前3轨迹+25固定变量口径下对锁定参数组合进行5折评价；并非重新遍历完整参数网格",
  check.names = FALSE
)
names(table53b) <- c("模型", "搜索空间", "固定评价参数", "五折验证 C-index", "五折验证 AUC", "五折验证 Brier", "五折验证 IBS", "说明")
utils::write.csv(table53b, file.path(tbl_out, "Table_5-3B_RSFLC_fixed_5fold_ready_to_fill.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
cat("DYNAMIC_FIXED_5FOLD_COMBINE_OK\n")
