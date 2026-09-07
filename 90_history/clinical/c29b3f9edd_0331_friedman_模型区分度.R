## ==========================================
## 五折数据 Friedman + 配对 Wilcoxon（Holm）
## 指标：测试集 C-index、AUC、BS
##
## 前提（本分析按此执行）：
## - 四个模型的 fold 1–5 对应**同一套数据划分**（您已确认）。
## - dat.xlsx 仅为汇总展示；检验使用各模型 xlsx 的 cv_detail。
##
## 数据路径：优先本脚本所在目录下的 *5fold_results.xlsx，其次 0323实例比较。
## 输出：控制台 + friedman_omnibus.csv + friedman_pairwise.csv
##
## 依赖：readxl、dplyr、tidyr、tibble、writexl
## ==========================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(writexl)
})

options(scipen = 999)

this_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0331作图保存"
cmp_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"

dat_path <- file.path(this_dir, "dat.xlsx")

cv_names <- tibble(
  model_label = c("RSF", "RSF_LC", "COX", "JM"),
  basename = c(
    "0323_RSF_5fold_results.xlsx",
    "0323_RSFLC_5fold_results.xlsx",
    "0323_COX_5fold_results.xlsx",
    "0323_JM_5fold_results.xlsx"
  )
)

resolve_cv_path <- function(bn) {
  p1 <- file.path(this_dir, bn)
  p2 <- file.path(cmp_dir, bn)
  if (file.exists(p1)) return(p1)
  if (file.exists(p2)) return(p2)
  stop("未找到文件：", bn, "（已查：", this_dir, " 与 ", cmp_dir, "）")
}

cv_files <- cv_names %>%
  mutate(path = vapply(basename, resolve_cv_path, character(1)))

# ---------- dat.xlsx（可选核对）----------
if (file.exists(dat_path)) {
  dat_sum <- readxl::read_xlsx(dat_path) %>% as.data.frame(stringsAsFactors = FALSE)
  names(dat_sum)[1] <- "model"
  dat_sum <- dat_sum %>% tidyr::fill(model, .direction = "down")
  dat_sum$model <- trimws(as.character(dat_sum$model))
  cat("=== dat.xlsx 汇总（前几行）===\n")
  print(head(dat_sum, 12))
} else {
  cat("（未找到 dat.xlsx，跳过汇总展示）\n")
}

# ---------- 生成 bootstrap_summary2.xlsx（列名 p_bootstrap；兼容旧表 boostrap_summary.xlsx）----------
bs_out <- file.path(this_dir, "bootstrap_summary2.xlsx")
bs_legacy <- file.path(this_dir, "boostrap_summary.xlsx")
bs_src <- if (file.exists(bs_legacy)) bs_legacy else if (file.exists(bs_out)) bs_out else NA_character_
if (!is.na(bs_src)) {
  bs <- readxl::read_xlsx(bs_src, sheet = "summary")
  if ("p_bootstrap_heuristic" %in% names(bs)) {
    bs <- bs %>% rename(p_bootstrap = p_bootstrap_heuristic)
  }
  writexl::write_xlsx(list(summary = bs), path = bs_out)
  cat("\n已写入：", bs_out, "（来源：", basename(bs_src), "）\n", sep = "")
} else {
  cat("\n（未找到 boostrap_summary.xlsx / bootstrap_summary2.xlsx，跳过汇总表导出）\n")
}

# ---------- 读 cv_detail：fold 1–5，test_CINDEX / test_AUC / test_BS ----------
read_cv_metric <- function(path, model_name) {
  d <- readxl::read_xlsx(path, sheet = "cv_detail") %>%
    as.data.frame(stringsAsFactors = FALSE)
  if (!"fold" %in% names(d)) stop(path, " 缺少 fold 列")
  d$fold <- suppressWarnings(as.integer(d$fold))
  d <- d[is.finite(d$fold) & d$fold >= 1L & d$fold <= 5L, , drop = FALSE]
  need <- c("test_CINDEX", "test_AUC", "test_BS")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop(path, " 缺少列：", paste(miss, collapse = ", "))
  d <- d[order(d$fold), c("fold", need)]
  names(d)[names(d) == "test_CINDEX"] <- "CINDEX"
  names(d)[names(d) == "test_AUC"] <- "AUC"
  names(d)[names(d) == "test_BS"] <- "BS"
  d$model <- model_name
  d
}

cv_list <- vector("list", nrow(cv_files))
for (i in seq_len(nrow(cv_files))) {
  cv_list[[i]] <- read_cv_metric(cv_files$path[i], cv_files$model_label[i])
}
cv_long <- dplyr::bind_rows(cv_list)

model_order <- cv_files$model_label

build_wide_mat <- function(value_col) {
  w <- cv_long %>%
    select(fold, model, value = all_of(value_col)) %>%
    pivot_wider(names_from = model, values_from = value) %>%
    arrange(fold)
  mat <- as.matrix(w[, model_order, drop = FALSE])
  rownames(mat) <- w$fold
  mat
}

mat_cindex <- build_wide_mat("CINDEX")
mat_auc <- build_wide_mat("AUC")
mat_bs <- build_wide_mat("BS")

cat("\n=== 5×4 测试集 C-index ===\n")
print(round(mat_cindex, 4))
cat("\n=== 5×4 测试集 AUC ===\n")
print(round(mat_auc, 4))
cat("\n=== 5×4 测试集 BS ===\n")
print(round(mat_bs, 4))

fried_run <- function(mat, label) {
  k <- ncol(mat)
  n <- nrow(mat)
  if (any(is.na(mat))) stop(label, " 含 NA，请检查 cv_detail")
  ft <- stats::friedman.test(mat)
  q_stat <- unname(as.numeric(ft$statistic))
  w <- if (n > 1L && k > 1L) q_stat / (n * (k - 1)) else NA_real_
  tibble::tibble(
    metric = label,
    chi_squared = q_stat,
    df = unname(as.numeric(ft$parameter)),
    p_value = unname(ft$p.value),
    kendall_W = w,
    n_folds = n
  )
}

pairwise_paired_wilcox <- function(mat) {
  models <- colnames(mat)
  pairs <- utils::combn(models, 2, simplify = FALSE)
  out <- lapply(pairs, function(nm) {
    d <- stats::wilcox.test(mat[, nm[1]], mat[, nm[2]], paired = TRUE, exact = FALSE)
    tibble::tibble(
      contrast = paste(nm[1], "-", nm[2]),
      V = unname(d$statistic),
      p.value = d$p.value
    )
  })
  dplyr::bind_rows(out) %>%
    mutate(p.holm = p.adjust(p.value, method = "holm"))
}

omni <- dplyr::bind_rows(
  fried_run(mat_cindex, "test_CINDEX"),
  fried_run(mat_auc, "test_AUC"),
  fried_run(mat_bs, "test_BS")
)

pair_c <- pairwise_paired_wilcox(mat_cindex) %>% mutate(metric = "test_CINDEX", .before = 1)
pair_a <- pairwise_paired_wilcox(mat_auc) %>% mutate(metric = "test_AUC", .before = 1)
pair_b <- pairwise_paired_wilcox(mat_bs) %>% mutate(metric = "test_BS", .before = 1)
pair_all <- dplyr::bind_rows(pair_c, pair_a, pair_b)

out_omni <- file.path(this_dir, "friedman_omnibus.csv")
out_pair <- file.path(this_dir, "friedman_pairwise.csv")
utils::write.csv(omni, out_omni, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(pair_all, out_pair, row.names = FALSE, fileEncoding = "UTF-8")

cat("\n========== Friedman 整体检验（折 = 区组，模型 = 处理）==========\n")
print(as.data.frame(omni))

cat("\n========== 事后：折内配对 Wilcoxon，Holm 校正 ==========\n")
print(as.data.frame(pair_all))

cat("\n已写入：\n ", out_omni, "\n ", out_pair, "\n", sep = "")
cat("\n========== 完成 ==========\n")

