## ============================================================
## 基于 fold1inner：保留外层 fold1，按 5 种「外层测试折」分别生成内层三折
## fold2：外层测试 fold1=5 时，内层训练集（fold1∈{1–4}）的 A/B/C，seed=666
## fold3：外层测试 fold1=4 时，内层训练集（fold1≠4）的 A/B/C，seed=667
## fold4：外层测试 fold1=3，seed=668
## fold5：外层测试 fold1=2，seed=669
## fold6：外层测试 fold1=1，seed=670
## 外层测试折上的样本对应列 fold2–fold6 为 NA（不参与该场景下的内层分折）
## 输出：DATA/0407_DATA_fold1.xlsx
## ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
})

options(scipen = 999)

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
data_dir <- file.path(work_dir, "DATA")
in_xlsx <- file.path(data_dir, "0323_DATA_fold1inner.xlsx")
out_xlsx <- file.path(data_dir, "0407_DATA_fold1.xlsx")

id_col <- "subject_id"

#' 与 0404_DATA_嵌套分折实现.R 中 assign_inner_abc 一致：对 id 随机打乱后均分三组 A/B/C
assign_inner_abc <- function(ids_vec, seed) {
  set.seed(as.integer(seed))
  n <- length(ids_vec)
  if (n == 0L) {
    return(data.frame(id = ids_vec, lab = character(0), stringsAsFactors = FALSE))
  }
  if (n < 3L) stop("内层训练集中唯一患者数仅 ", n, "，不足以做三折（seed=", seed, "）。")
  perm <- sample.int(n, n)
  ids_shuf <- ids_vec[perm]
  K <- 3L
  base <- n %/% K
  rem <- n %% K
  sizes <- rep(as.integer(base), K)
  if (rem > 0L) sizes[seq_len(rem)] <- sizes[seq_len(rem)] + 1L
  lab <- rep(c("A", "B", "C"), times = sizes)
  data.frame(id = ids_shuf, lab = lab, stringsAsFactors = FALSE)
}

#' 为指定「外层测试折」写一列内层标签：仅内层训练池中的行有 A/B/C，测试折为 NA
add_inner_fold_col <- function(dat, outer_test, out_col, seed, fold_col) {
  fv <- suppressWarnings(as.integer(round(as.numeric(dat[[fold_col]]))))
  in_pool <- !is.na(fv) & fv %in% 1L:5L & fv != as.integer(outer_test)
  dat[[out_col]] <- NA_character_
  ids_inner <- unique(dat[[id_col]][in_pool])
  ids_inner <- ids_inner[!is.na(ids_inner)]
  map_df <- assign_inner_abc(ids_inner, seed)
  names(map_df)[1] <- id_col
  names(map_df)[2] <- out_col
  j <- match(dat[[id_col]], map_df[[id_col]])
  ok <- in_pool & !is.na(j)
  dat[[out_col]][ok] <- map_df[[out_col]][j[ok]]
  dat
}

if (!file.exists(in_xlsx)) {
  stop("未找到输入：", in_xlsx)
}

dat <- as.data.frame(readxl::read_xlsx(in_xlsx), stringsAsFactors = FALSE)
if (!id_col %in% names(dat)) stop("数据中缺少列 ", id_col)

fold_col <- if ("fold1" %in% names(dat)) {
  "fold1"
} else if ("fold_1" %in% names(dat)) {
  "fold_1"
} else {
  stop("数据中缺少 fold1 或 fold_1 列")
}

## 外层测试折 5,4,3,2,1 → 内层列 fold2…fold6，种子 666…670
outer_tests <- c(5L, 4L, 3L, 2L, 1L)
inner_cols <- c("fold2", "fold3", "fold4", "fold5", "fold6")
seeds <- 666L:670L

for (i in seq_along(outer_tests)) {
  dat <- add_inner_fold_col(dat, outer_tests[[i]], inner_cols[[i]], seeds[[i]], fold_col)
}

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
writexl::write_xlsx(dat, out_xlsx)

## 简要校验信息
fv <- suppressWarnings(as.integer(round(as.numeric(dat[[fold_col]]))))
for (i in seq_along(outer_tests)) {
  ot <- outer_tests[[i]]
  col <- inner_cols[[i]]
  n_te <- length(unique(dat[[id_col]][!is.na(fv) & fv == ot]))
  pool <- !is.na(fv) & fv %in% 1L:5L & fv != ot
  n_pool_id <- length(unique(dat[[id_col]][pool]))
  n_abc <- vapply(c("A", "B", "C"), function(lab) {
    length(unique(dat[[id_col]][pool & dat[[col]] == lab]))
  }, integer(1))
  message(
    "外层测试 fold1=", ot, " → 列 ", col, " | seed=", seeds[[i]],
    " | 测试折患者数=", n_te, " | 内层池患者数=", n_pool_id,
    " | A/B/C 患者数=", paste(n_abc, collapse = "/")
  )
}

message("已写出：", out_xlsx)
