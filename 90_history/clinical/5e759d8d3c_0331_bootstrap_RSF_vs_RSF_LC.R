## ==========================================
## Bootstrap：5 折测试集上 RSF vs RSF_LC（配对折）
##
## 做法：每个指标在 fold 1–5 上取 d_k = RSF_k - RSF_LC_k，
##       对向量 d 做有放回重抽样（n=折数），bootstrap 均值的
##       百分位 / BCa 95% CI；并给出配对 Wilcoxon（与 Friedman 脚本一致）便于对照。
##
## 说明：
## - C-index、AUC：越大越好；d > 0 表示 RSF 更高。
## - Brier：越小越好；d > 0 表示 RSF 更高（更差）；解读时注意符号。
## - 折数只有 5 时，bootstrap 分布较粗，结论以 CI 是否含 0 为主，p 值作辅助。
##
## 依赖：readxl、dplyr；可选 boot（用于 BCa，失败则仅用百分位法）
## ==========================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

options(scipen = 999)

this_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0331作图保存"
cmp_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"

B_BOOT <- 10000L
set.seed(20260331)

resolve_cv_path <- function(bn) {
  p1 <- file.path(this_dir, bn)
  p2 <- file.path(cmp_dir, bn)
  if (file.exists(p1)) return(p1)
  if (file.exists(p2)) return(p2)
  stop("未找到文件：", bn)
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

path_rsf <- resolve_cv_path("0323_RSF_5fold_results.xlsx")
path_rsflc <- resolve_cv_path("0323_RSFLC_5fold_results.xlsx")

dr <- read_cv_metric(path_rsf)
dl <- read_cv_metric(path_rsflc)

if (!identical(dr$fold, dl$fold)) {
  stop("两文件 fold 顺序或编号不一致，请检查 cv_detail")
}

mean_boot <- function(d, indices) mean(d[indices])

one_metric_boot <- function(rsf_vec, rsflc_vec, metric_name, R = B_BOOT) {
  d <- rsf_vec - rsflc_vec
  n <- length(d)
  if (any(is.na(d))) stop(metric_name, " 含 NA")
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

  w <- stats::wilcox.test(rsf_vec, rsflc_vec, paired = TRUE, exact = FALSE)

  ## 启发式 bootstrap p（双侧，检验均值差是否为 0）：用 (1+count)/(R+1) 避免 p=0
  low <- (1 + sum(boot_means <= 0)) / (R + 1)
  high <- (1 + sum(boot_means >= 0)) / (R + 1)
  p_boot <- 2 * min(low, high)
  p_boot <- min(p_boot, 1)

  tibble::tibble(
    metric = metric_name,
    n_folds = n,
    mean_RSF = mean(rsf_vec),
    mean_RSF_LC = mean(rsflc_vec),
    mean_diff_RSF_minus_RSF_LC = obs_mean,
    sd_diff = stats::sd(d),
    CI_percentile_2.5 = ci_perc[1],
    CI_percentile_97.5 = ci_perc[2],
    CI_BCa_2.5 = ci_bca[1],
    CI_BCa_97.5 = ci_bca[2],
    p_wilcoxon_paired = unname(w$p.value),
    p_bootstrap = p_boot
  )
}

res <- dplyr::bind_rows(
  one_metric_boot(dr$CINDEX, dl$CINDEX, "test_CINDEX"),
  one_metric_boot(dr$AUC, dl$AUC, "test_AUC"),
  one_metric_boot(dr$BS, dl$BS, "test_BS")
)

detail <- tibble::tibble(
  fold = dr$fold,
  RSF_CINDEX = dr$CINDEX,
  RSF_LC_CINDEX = dl$CINDEX,
  diff_CINDEX = dr$CINDEX - dl$CINDEX,
  RSF_AUC = dr$AUC,
  RSF_LC_AUC = dl$AUC,
  diff_AUC = dr$AUC - dl$AUC,
  RSF_BS = dr$BS,
  RSF_LC_BS = dl$BS,
  diff_BS = dr$BS - dl$BS
)

out_csv <- file.path(this_dir, "bootstrap_RSF_vs_RSF_LC_summary.csv")
out_detail <- file.path(this_dir, "bootstrap_RSF_vs_RSF_LC_by_fold.csv")
utils::write.csv(res, out_csv, row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(detail, out_detail, row.names = FALSE, fileEncoding = "UTF-8")

cat("数据文件：\n ", path_rsf, "\n ", path_rsflc, "\n\n", sep = "")
cat("========== 折间配对差值（RSF - RSF_LC）==========\n")
print(as.data.frame(detail), row.names = FALSE)

cat("\n========== Bootstrap 均值差（RSF - RSF_LC），R =", B_BOOT, "==========\n")
print(as.data.frame(res), row.names = FALSE)

cat(
  "\n已写入：\n ", out_csv, "\n ", out_detail,
  "\n",
  sep = ""
)
cat("\n========== 完成 ==========\n")
