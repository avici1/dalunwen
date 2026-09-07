## ==========================================
## 5折交叉验证 - 数据分折
## 数据来源：0323_DATA.xlsx
## 输出：fold_indices（每行所属折号）、分折结果保存至 0402SHAP作图 文件夹
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

set.seed(123)
options(scipen = 999)

# =========================
# 路径设置
# =========================
work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0323实例比较"
out_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0402SHAP作图"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 优先使用 0402SHAP作图 下的 0323_DATA.xlsx，否则回退到 0323实例比较
data_path <- file.path(out_dir, "0323_DATA.xlsx")
if (!file.exists(data_path)) {
  data_path <- file.path(work_dir, "0323_DATA.xlsx")
}

# =========================
# 1) 读取数据
# =========================
message("读取数据: ", data_path)
if (!file.exists(data_path)) stop("未找到 0323_DATA.xlsx")
dat <- readxl::read_xlsx(data_path)

message("数据维度: ", nrow(dat), " 行 × ", ncol(dat), " 列")

# =========================
# 2) 确定分层变量（可选）
# =========================
# 若存在结局/事件列，可进行分层抽样，保证每折中事件比例相近
event_col <- NULL
id_col <- NULL
if ("event" %in% names(dat)) {
  event_col <- "event"
} else if ("hospitalmortality" %in% names(dat)) {
  event_col <- "hospitalmortality"
} else if ("hospital_mortality" %in% names(dat)) {
  event_col <- "hospital_mortality"
}
if ("subject_id" %in% names(dat)) {
  id_col <- "subject_id"
} else if ("subjectid" %in% names(dat)) {
  id_col <- "subjectid"
} else if ("hadm_id" %in% names(dat)) {
  id_col <- "hadm_id"
}

# =========================
# 3) 5折划分
# =========================
n <- nrow(dat)
K <- 5L

if (!is.null(event_col)) {
  # 分层抽样：按事件状态分层，保证每折事件比例相近
  event_vec <- as.numeric(dat[[event_col]])
  event_vec[is.na(event_vec) | event_vec > 0] <- 1
  event_vec[event_vec != 1] <- 0
  strata <- as.factor(event_vec)
  fold_vec <- integer(n)
  for (s in levels(strata)) {
    idx_s <- which(strata == s)
    n_s <- length(idx_s)
    ord <- sample(n_s)                    # 随机打乱顺序
    folds_s <- cut(seq_len(n_s), breaks = K, labels = FALSE)
    fold_vec[idx_s[ord]] <- folds_s       # 按随机顺序分配到各折
  }
  message("已按事件状态分层，划分为 5 折")
} else {
  # 随机划分
  perm <- sample(n)
  fold_vec <- integer(n)
  fold_vec[perm] <- cut(seq_len(n), breaks = K, labels = FALSE)
  message("随机划分为 5 折")
}

# =========================
# 4) 将折号加入数据并保存
# =========================
dat_folds <- dat %>%
  dplyr::mutate(fold = as.integer(fold_vec))

# 保存带折号标记的完整数据（便于后续建模直接使用）
folds_data_path <- file.path(out_dir, "0323_DATA_with_folds.xlsx")
writexl::write_xlsx(dat_folds, folds_data_path)
message("已保存带折号的数据: ", folds_data_path)

# =========================
# 5) 输出各折样本量
# =========================
fold_summary <- dat_folds %>%
  dplyr::group_by(fold) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop")
if (!is.null(event_col)) {
  fold_summary_ev <- dat_folds %>%
    dplyr::mutate(event_bin = as.numeric(.data[[event_col]] > 0)) %>%
    dplyr::group_by(fold) %>%
    dplyr::summarise(n = dplyr::n(), n_event = sum(event_bin, na.rm = TRUE), .groups = "drop")
  fold_summary <- fold_summary_ev
}
print(fold_summary)
message("\n各折划分完成。fold 列表示该行属于第几折（1-5）。")

# =========================
# 6) 保存折索引列表（供后续模型直接调用）
# =========================
# 生成 train/test 索引：第 i 折时，fold==i 为 test，其余为 train
fold_list <- vector("list", K)
for (i in 1L:K) {
  test_idx <- which(fold_vec == i)
  train_idx <- which(fold_vec != i)
  fold_list[[i]] <- list(train = train_idx, test = test_idx)
}
save(fold_list, fold_vec, dat_folds, file = file.path(out_dir, "0323_5fold_indices.RData"))
message("已保存折索引: ", file.path(out_dir, "0323_5fold_indices.RData"))
message("  - fold_list: 长度为5的列表，fold_list[[i]]$train / $test 为第 i 折的 train/test 行索引")
message("  - fold_vec: 每行所属折号 (1-5)")
message("  - dat_folds: 带折号的数据框")
