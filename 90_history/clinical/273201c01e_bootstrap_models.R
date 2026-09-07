## ==========================================
## Bootstrap：5 折测试集上四模型两两配对（RSF, RSF_LC, COX, JM）
##
## 每个指标：d_k = model_i_k - model_j_k，对折向量 d 做有放回 bootstrap
## 得到均值差的百分位 / BCa 95% CI + 配对 Wilcoxon + bootstrap p（列名 p_bootstrap）。
##
## 输出（本脚本目录）：
##   boostrap_by_fold.xlsx  — cv 宽表 + 逐折逐对逐指标差值
##   bootstrap_summary2.xlsx  — 全部配对的 bootstrap 汇总（列名含 p_bootstrap）
##
## 依赖：readxl、dplyr、tibble、writexl；可选 boot（BCa）
## ==========================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(writexl)
})

options(scipen = 999)

this_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0331作图保存"
cmp_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"

B_BOOT <- 10000L
set.seed(20260331)

cv_names <- tibble(
  model = c("RSF", "RSF_LC", "COX", "JM"),
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

read_cv_metric <- function(path) {
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
  d
}

mean_boot <- function(d, indices) mean(d[indices])

pairwise_bootstrap <- function(
  vec_a,
  vec_b,
  name_a,
  name_b,
  metric_name,
  R = B_BOOT
) {
  d <- vec_a - vec_b
  n <- length(d)
  if (any(is.na(d))) {
    stop(metric_name, " ", name_a, " vs ", name_b, " 含 NA")
  }
  obs_mean <- mean(d)

  boot_means <- replicate(
    R,
    mean(d[sample.int(n, n, replace = TRUE)])
  )

  ci_perc <- as.numeric(stats::quantile(boot_means, c(0.025, 0.975), names = FALSE))

  ci_bca <- c(NA_real_, NA_real_)
  if (requireNamespace("boot", quietly = TRUE)) {
    b <- boot::boot(data = d, statistic = mean_boot, R = R, stype = "i")
    bc <- try(
      boot::boot.ci(b, type = "bca", conf = 0.95),
      silent = TRUE
    )
    if (!inherits(bc, "try-error")) {
      bb <- bc$bca
      if (!is.null(bb) && length(bb) >= 5 && all(is.finite(bb[4:5]))) {
        ci_bca <- bb[4:5]
      }
    }
  }

  w <- stats::wilcox.test(vec_a, vec_b, paired = TRUE, exact = FALSE)

  low <- (1 + sum(boot_means <= 0)) / (R + 1)
  high <- (1 + sum(boot_means >= 0)) / (R + 1)
  p_boot <- min(2 * min(low, high), 1)

  contrast <- paste(name_a, "-", name_b)

  tibble(
    metric = metric_name,
    contrast = contrast,
    model_left = name_a,
    model_right = name_b,
    n_folds = n,
    mean_left = mean(vec_a),
    mean_right = mean(vec_b),
    mean_diff_left_minus_right = obs_mean,
    sd_diff = stats::sd(d),
    CI_percentile_2.5 = ci_perc[1],
    CI_percentile_97.5 = ci_perc[2],
    CI_BCa_2.5 = ci_bca[1],
    CI_BCa_97.5 = ci_bca[2],
    p_wilcoxon_paired = unname(w$p.value),
    p_bootstrap = p_boot
  )
}

# ---------- 读入四模型 ----------
cv_names <- cv_names %>%
  mutate(path = vapply(basename, resolve_cv_path, character(1)))

mat_list <- vector("list", nrow(cv_names))
for (i in seq_len(nrow(cv_names))) {
  mat_list[[i]] <- read_cv_metric(cv_names$path[i])
}

fold_ref <- mat_list[[1]]$fold
for (i in seq_along(mat_list)) {
  if (!identical(mat_list[[i]]$fold, fold_ref)) {
    stop("模型 ", cv_names$model[i], " 的 fold 与其它文件不一致")
  }
}

model_order <- cv_names$model
names(mat_list) <- model_order

# 宽表：每折一行，各模型三指标
cv_wide <- mat_list[[1]][, "fold", drop = FALSE]
for (m in model_order) {
  mm <- mat_list[[m]]
  cv_wide[[paste0("CINDEX_", m)]] <- mm$CINDEX
  cv_wide[[paste0("AUC_", m)]] <- mm$AUC
  cv_wide[[paste0("BS_", m)]] <- mm$BS
}

# 全配对（6 对）
pairs_idx <- utils::combn(seq_along(model_order), 2, simplify = FALSE)

metric_cols <- c(CINDEX = "test_CINDEX", AUC = "test_AUC", BS = "test_BS")

pair_diff_long <- list()
summary_rows <- list()

for (pr in pairs_idx) {
  ia <- pr[1]
  ib <- pr[2]
  na <- model_order[ia]
  nb <- model_order[ib]
  ma <- mat_list[[na]]
  mb <- mat_list[[nb]]

  for (mc in names(metric_cols)) {
    col <- mc
    va <- ma[[col]]
    vb <- mb[[col]]
    metric_label <- metric_cols[[mc]]

    summary_rows[[length(summary_rows) + 1L]] <- pairwise_bootstrap(
      va, vb, na, nb, metric_label, R = B_BOOT
    )

    pair_diff_long[[length(pair_diff_long) + 1L]] <- tibble(
      fold = ma$fold,
      metric = metric_label,
      contrast = paste(na, "-", nb),
      diff_left_minus_right = va - vb
    )
  }
}

summary_tbl <- bind_rows(summary_rows)
pair_diff_tbl <- bind_rows(pair_diff_long)

out_fold <- file.path(this_dir, "boostrap_by_fold.xlsx")
out_sum <- file.path(this_dir, "bootstrap_summary2.xlsx")

writexl::write_xlsx(
  list(
    cv_wide = cv_wide,
    pairwise_diff_by_fold = pair_diff_tbl
  ),
  out_fold
)

writexl::write_xlsx(
  list(summary = summary_tbl),
  out_sum
)

cat("已读取模型 cv_detail：\n")
print(cv_names %>% select(model, path))

cat("\n========== Bootstrap 汇总（全部配对 × 三指标），R =", B_BOOT, "==========\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\n已写入：\n ", out_fold, "\n ", out_sum, "\n", sep = "")
cat("\n========== 完成 ==========\n")
