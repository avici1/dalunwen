suppressPackageStartupMessages({
  library(dplyr)
})

supp_root <- Sys.getenv(
  "CH5_SUPPLEMENT_ROOT",
  unset = "supplement_output"
)
fig_src <- Sys.getenv("CH5_SOURCE_FIGURES", unset = "source_figures")
tbl_src <- Sys.getenv("CH5_SOURCE_TABLES", unset = "source_tables")
model_src <- Sys.getenv("CH5_SOURCE_MODELS", unset = "source_models")
data_src <- Sys.getenv("CH5_SOURCE_DATA", unset = "source_data")
fig_out <- file.path(supp_root, "figures")
tbl_out <- file.path(supp_root, "tables")
log_out <- file.path(supp_root, "logs")
dir.create(fig_out, recursive = TRUE, showWarnings = FALSE)
dir.create(tbl_out, recursive = TRUE, showWarnings = FALSE)
dir.create(log_out, recursive = TRUE, showWarnings = FALSE)

write_utf8 <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

# 表5-1b中原文留空的28天和90天死亡结局。
base <- data.table::fread(file.path(data_src, "baseline.csv"),
                         select = c("death_28d", "death_90d"), encoding = "UTF-8")
mortality <- bind_rows(
  data.frame(
    outcome = "28天死亡",
    n = sum(!is.na(base$death_28d)),
    deaths = sum(base$death_28d == 1L, na.rm = TRUE)
  ),
  data.frame(
    outcome = "90天死亡",
    n = sum(!is.na(base$death_90d)),
    deaths = sum(base$death_90d == 1L, na.rm = TRUE)
  )
) %>%
  mutate(
    percent = 100 * deaths / n,
    display = sprintf("%s (%.1f)", format(deaths, big.mark = ","), percent)
  )
names(mortality) <- c("结局", "样本量", "死亡例数", "百分比", "例数(%)")
write_utf8(mortality, file.path(tbl_out, "Table_5-1b_mortality_supplement.csv"))

# 七张已生成但在DOCX中误插到目录区域的图，以及六张生成后未嵌入的图。
figure_map <- data.frame(
  status = c(rep("已生成但DOCX位置错误", 7), rep("已生成但DOCX未嵌入", 6)),
  suggested_number = c(
    "图5-7", "图5-8", "图5-9", "图5-10", "图5-11", "图5-12", "图5-13",
    "补图A", "补图B", "补图C", "补图D", "补图E", "补图F"
  ),
  title = c(
    "Landmark生存RSFLC原生OOB变量重要性",
    "RSFLC验证集置换变量重要性",
    "RSFLC固定协变量SHAP分布",
    "RSFLC典型患者局部SHAP解释",
    "RSFLC首要固定协变量SHAP依赖关系",
    "JM关联参数MCMC轨迹图",
    "JM关联参数后验密度图",
    "RSF OOB收敛",
    "RSF验证集置换变量重要性",
    "RSF最小深度",
    "静态任务时间依赖指标",
    "RSFLC时间依赖指标",
    "四模型验证性能汇总"
  ),
  source_file = c(
    "RSFLC_01_OOB_VIMP.png",
    "RSFLC_02_validation_permutation_VIMP.png",
    "RSFLC_03_SHAP_beeswarm.png",
    "RSFLC_04_typical_patient_SHAP.png",
    "RSFLC_05_SHAP_dependence.png",
    "JM_03_MCMC_trace.png",
    "JM_04_MCMC_density.png",
    "RSF_01_OOB_convergence.png",
    "RSF_03_validation_permutation_VIMP.png",
    "RSF_04_minimal_depth.png",
    "STATIC_11_time_metrics.png",
    "RSFLC_08_time_metrics.png",
    "ALL_02_validation_performance.png"
  ),
  stringsAsFactors = FALSE
)
figure_map$output_file <- sprintf("Supplement_%02d_%s", seq_len(nrow(figure_map)), figure_map$source_file)
for (i in seq_len(nrow(figure_map))) {
  src <- file.path(fig_src, figure_map$source_file[i])
  dst <- file.path(fig_out, figure_map$output_file[i])
  if (!file.exists(src)) stop("缺少源图: ", src)
  ok <- file.copy(src, dst, overwrite = TRUE, copy.date = FALSE)
  if (!ok) stop("复制失败: ", src)
}
names(figure_map) <- c("状态", "建议编号", "题名", "源文件", "输出文件")
write_utf8(figure_map, file.path(tbl_out, "missing_and_misplaced_figure_manifest.csv"))

# 为未嵌入图补齐可复核的绘图数据表。
support_tables <- c(
  "rsf_validation_permutation_VIMP.csv",
  "rsf_minimal_depth.csv",
  "static_time_metrics.csv",
  "rsflc_time_metrics.csv",
  "all_model_validation_performance.csv",
  "table_5_7_rsflc_OOB_VIMP.csv",
  "rsflc_validation_permutation_VIMP.csv",
  "rsflc_SHAP_importance_fixed_covariates.csv",
  "rsflc_typical_patients.csv",
  "table_5_6_JM_association.csv"
)
for (nm in support_tables) {
  src <- file.path(tbl_src, nm)
  if (file.exists(src)) file.copy(src, file.path(tbl_out, nm), overwrite = TRUE, copy.date = FALSE)
}

# OOB收敛图的逐树数据此前未单独输出，从已保存RSF模型对象中恢复。
rsf_fit <- readRDS(file.path(model_src, "rsf_static.rds"))
oob_tbl <- data.frame(
  tree = seq_along(rsf_fit$err.rate),
  OOB_C_index = 1 - as.numeric(rsf_fit$err.rate)
)
write_utf8(oob_tbl, file.path(tbl_out, "RSF_01_OOB_convergence.csv"))

cat("EXPORT_EXISTING_MISSING_OK\n")
