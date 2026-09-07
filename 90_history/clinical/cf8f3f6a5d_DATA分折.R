## ==========================================
## 重复 5 折交叉验证：用 3 个不同种子各做一次 5 折划分
## 输入：0323_DATA.xlsx
## 输出：0323_DATA_5fold.xlsx（3 个 sheet，分别为三次重复的带 fold 列数据）
## 说明：与单次 5 折逻辑一致；若存在 event 列则按事件分层分配各折样本量
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

options(scipen = 999)

# =========================
# 路径与参数
# =========================
work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
data_path <- file.path(work_dir, "0323_DATA.xlsx")
out_path <- file.path(work_dir, "0323_DATA_5fold.xlsx")

K <- 5L
## 三次重复对应的随机种子（可改）
seeds <- c(123L, 456L, 789L)

# =========================
# 检测分层列（与 5折代码.R 一致）
# =========================
detect_event_col <- function(dat) {
  if ("event" %in% names(dat)) return("event")
  if ("death" %in% names(dat)) return("death")
  if ("hospitalmortality" %in% names(dat)) return("hospitalmortality")
  if ("hospital_mortality" %in% names(dat)) return("hospital_mortality")
  NULL
}

#' 对 n 行数据生成 1..K 折编号；可选按 event 分层
make_fold_vec <- function(n, event_col_name, dat, K, seed) {
  set.seed(seed)
  if (is.null(event_col_name)) {
    perm <- sample.int(n, n)
    fold_vec <- integer(n)
    fold_vec[perm] <- as.integer(cut(seq_len(n), breaks = K, labels = FALSE))
    return(fold_vec)
  }
  event_vec <- as.numeric(dat[[event_col_name]])
  event_vec[is.na(event_vec) | event_vec > 0] <- 1L
  event_vec[event_vec != 1L] <- 0L
  strata <- as.factor(event_vec)
  fold_vec <- integer(n)
  for (s in levels(strata)) {
    idx_s <- which(strata == s)
    n_s <- length(idx_s)
    ord <- sample.int(n_s, n_s)
    folds_s <- as.integer(cut(seq_len(n_s), breaks = K, labels = FALSE))
    fold_vec[idx_s[ord]] <- folds_s
  }
  fold_vec
}

# =========================
# 读取数据
# =========================
if (!file.exists(data_path)) stop("未找到 0323_DATA.xlsx：", data_path)
dat <- readxl::read_xlsx(data_path)
n <- nrow(dat)

event_col <- detect_event_col(dat)

# =========================
# 三次重复划分 → 三个数据框
# =========================
sheet_list <- vector("list", length(seeds))
names(sheet_list) <- paste0("repeat_", seq_along(seeds), "_seed", seeds)

for (r in seq_along(seeds)) {
  seed_r <- seeds[r]
  fold_vec <- make_fold_vec(n, event_col, dat, K, seed_r)
  sheet_list[[r]] <- dat %>%
    dplyr::mutate(fold = as.integer(fold_vec))
}

# =========================
# 写出多 sheet xlsx
# =========================
writexl::write_xlsx(sheet_list, out_path)
