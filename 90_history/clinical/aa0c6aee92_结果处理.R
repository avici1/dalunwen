library(dplyr)
library(tidyr)
library(rstatix)



metrics_rsf_cv <- read.csv("F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_rsf_cv.csv")
metrics_jm_cv <- read.csv("F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_jm_cv.csv")
metrics_gcs_cv <- read.csv("F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_gcs_cv.csv")
metrics_cox_cv <- read.csv("F:/文章_大论文/0521/处理后文件/结果/RSFLC交叉验证结果/metrics_cox_cv.csv")










#################################################
# 按 fold 对齐（JM 用 train_fold）
metrics_rsf_cv <- metrics_rsf_cv[order(metrics_rsf_cv$fold), ]
metrics_jm_cv  <- metrics_jm_cv[order(metrics_jm_cv$train_fold), ]
metrics_gcs_cv <- metrics_gcs_cv[order(metrics_gcs_cv$fold), ]
metrics_cox_cv <- metrics_cox_cv[order(metrics_cox_cv$fold), ]
# 构建 Friedman 检验矩阵：行 = fold（区组），列 = 模型（处理组）
build_friedman_mat <- function(metric) {
  cbind(
    RSF = metrics_rsf_cv[[metric]],
    JM  = metrics_jm_cv[[metric]],
    GCS = metrics_gcs_cv[[metric]],
    COX = metrics_cox_cv[[metric]]
  )
}
# 对 auc / cindex / bs 分别做 Friedman 检验
metric_names <- c("auc", "cindex", "bs")
friedman_results <- lapply(metric_names, function(m) {
  mat <- build_friedman_mat(m)
  ft  <- friedman.test(mat)
  list(
    metric = m,
    matrix = mat,
    statistic = ft$statistic,
    p_value   = ft$p.value,
    method    = ft$method
  )
})
# 汇总输出
friedman_summary <- data.frame(
  metric    = sapply(friedman_results, `[[`, "metric"),
  statistic = sapply(friedman_results, function(x) unname(x$statistic)),
  p_value   = sapply(friedman_results, function(x) x$p_value)
)
print(friedman_summary)





auc_mat <- build_friedman_mat("auc")
pairs <- combn(colnames(auc_mat), 2, simplify = FALSE)
pairwise_wilcox <- lapply(pairs, function(p) {
  wt <- wilcox.test(auc_mat[, p[1]], auc_mat[, p[2]], paired = TRUE)
  data.frame(
    metric  = "auc",
    group1  = p[1],
    group2  = p[2],
    p_value = wt$p.value
  )
})
do.call(rbind, pairwise_wilcox)

#######################################################


# 1. 合并四个模型结果
metrics_all <- bind_rows(
  metrics_rsf_cv  %>% mutate(model = "RSF"),
  metrics_jm_cv   %>% mutate(model = "JM"),
  metrics_gcs_cv  %>% mutate(model = "RSFLC"),
  metrics_cox_cv  %>% mutate(model = "COX")
)

# 统一 fold 编号（JM 用的是 train_fold/test_fold）
metrics_all <- metrics_all %>%
  mutate(
    fold = case_when(
      model == "JM" ~ row_number(),
      TRUE ~ fold
    )
  ) %>%
  select(model, fold, auc, cindex, bs)

metrics_all


metrics_all <- bind_rows(
  
  metrics_rsf_cv %>%
    mutate(
      model = "RSF",
      fold = 1:5
    ) %>%
    select(model, fold, auc, cindex, bs),
  
  metrics_jm_cv %>%
    mutate(
      model = "JM",
      fold = 1:5
    ) %>%
    select(model, fold, auc, cindex, bs),
  
  metrics_gcs_cv %>%
    mutate(
      model = "RSFLC",
      fold = 1:5
    ) %>%
    select(model, fold, auc, cindex, bs),
  
  metrics_cox_cv %>%
    mutate(
      model = "COX",
      fold = 1:5
    ) %>%
    select(model, fold, auc, cindex, bs)
)

metrics_all

friedman_auc <- friedman.test(
  auc ~ model | fold,
  data = metrics_all
)

friedman_cindex <- friedman.test(
  cindex ~ model | fold,
  data = metrics_all
)

friedman_bs <- friedman.test(
  bs ~ model | fold,
  data = metrics_all
)

friedman_auc
friedman_cindex
friedman_bs










