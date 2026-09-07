## 生存时间描述统计：均数±标准差，最大最小值
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/0314_widedata_merge.xlsx"
if (!file.exists(path)) stop("文件不存在: ", path)

dat <- readxl::read_xlsx(path)

## 生存时间列
if (!("itemid_los_hosp_days" %in% names(dat))) stop("未找到 itemid_los_hosp_days")

## 与建模一致：仅 Obstimes=0/1，每患者一条（优先 Obstimes=1）
id_col <- if ("subject_id" %in% names(dat)) "subject_id" else if ("subjectid" %in% names(dat)) "subjectid" else stop("未找到 subject_id/subjectid")
if (!("Obstimes" %in% names(dat))) stop("未找到 Obstimes")

to_numeric_safe <- function(x) suppressWarnings(as.numeric(x))
dat$Obstimes <- to_numeric_safe(dat$Obstimes)
dat$itemid_los_hosp_days <- to_numeric_safe(dat$itemid_los_hosp_days)

surv_df <- dat %>%
  dplyr::filter(Obstimes %in% c(0, 1)) %>%
  dplyr::mutate(.id = as.character(.data[[id_col]])) %>%
  dplyr::arrange(.id, dplyr::desc(Obstimes)) %>%
  dplyr::distinct(.id, .keep_all = TRUE) %>%
  dplyr::mutate(obs_time = itemid_los_hosp_days) %>%
  dplyr::filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0)

t <- surv_df$obs_time
n <- length(t)
m <- mean(t)
s <- sd(t)
min_t <- min(t)
max_t <- max(t)

cat("\n========== 生存时间描述统计 ==========\n")
cat("样本量: ", n, "\n", sep = "")
cat("均数 ± 标准差: ", round(m, 4), " ± ", round(s, 4), " 天\n", sep = "")
cat("最小值: ", round(min_t, 4), " 天\n", sep = "")
cat("最大值: ", round(max_t, 4), " 天\n", sep = "")
cat("中位数: ", round(median(t), 4), " 天\n", sep = "")
cat("======================================\n\n")
