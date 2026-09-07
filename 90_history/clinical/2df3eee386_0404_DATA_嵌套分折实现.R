## ==========================================
## 外层 5 折：按 subject_id 划分（同一患者所有行 fold_1 相同，避免泄露）
## 输入：0323_DATA1.xlsx
## 输出：0323_DATA_fold1.xlsx / fold2 / fold3（各为一次独立随机划分）
## 785 个 id → 每折 157 人（157×5=785）
## ==========================================
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
})

options(scipen = 999)

work_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0404新设计"
data_xlsx <- file.path(work_dir, "0323_DATA1.xlsx")
out_files <- file.path(
  work_dir,
  c("0323_DATA_fold1.xlsx", "0323_DATA_fold2.xlsx", "0323_DATA_fold3.xlsx")
)

seeds <- c(123L, 456L, 789L)
K <- 5L
id_col <- "subject_id"

########### fold1 折（及 fold2、fold3）：按患者 5 折 ###########

if (!file.exists(data_xlsx)) {
  message("未找到：", data_xlsx, "，跳过外层 5 折写出（若仅需内层，请保证 work_dir 或 DATA 下已有 fold1–3 xlsx）。")
} else {
dat <- as.data.frame(readxl::read_xlsx(data_xlsx), stringsAsFactors = FALSE)
if (!id_col %in% names(dat)) stop("数据中缺少列 ", id_col)
dat$.row_order <- seq_len(nrow(dat))

ids <- unique(dat[[id_col]])
ids <- ids[!is.na(ids)]
n_id <- length(ids)
if (n_id != 785L) {
  message("提示：唯一 ", id_col, " 个数为 ", n_id, "（非 785），仍按均分 5 折处理。")
}

#' n 个 id 随机打乱后，尽量均分为 K 组，返回 data.frame(id, fold_1)
assign_fold_by_id <- function(ids_vec, K, seed) {
  set.seed(seed)
  n <- length(ids_vec)
  if (n < K) stop("患者数 ", n, " 小于折数 ", K)
  perm <- sample.int(n, n)
  ids_shuf <- ids_vec[perm]
  base <- n %/% K
  rem <- n %% K
  sizes <- rep(as.integer(base), K)
  if (rem > 0L) sizes[seq_len(rem)] <- sizes[seq_len(rem)] + 1L
  fold_1 <- rep(1L:K, times = sizes)
  data.frame(
    id = ids_shuf,
    fold_1 = as.integer(fold_1),
    stringsAsFactors = FALSE
  )
}

for (r in seq_along(seeds)) {
  map_df <- assign_fold_by_id(ids, K, seeds[r])
  names(map_df)[1] <- id_col
  out <- merge(dat, map_df, by = id_col, all.x = TRUE, sort = FALSE)
  out <- out[order(out$.row_order), , drop = FALSE]
  out$.row_order <- NULL
  if (anyNA(out$fold_1)) stop("合并后存在 fold_1 为 NA，请检查 ", id_col, " 是否一致。")
  writexl::write_xlsx(out, out_files[r])
  n_per_fold <- vapply(split(out[[id_col]], out$fold_1), function(x) length(unique(x)), integer(1))
  message(
    "已写出：", out_files[r], " （seed=", seeds[r],
    ", 每折患者数：", paste(names(n_per_fold), n_per_fold, sep = ":", collapse = " "), "）"
  )
}
}


############ fold2：fold_1∈{1–4} 内层三折 A/B/C；fold_1=5 为验证集（fold2 为 NA）############

data_dir <- file.path(work_dir, "DATA")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

fold_xlsx_names <- c("0323_DATA_fold1.xlsx", "0323_DATA_fold2.xlsx", "0323_DATA_fold3.xlsx")
inner_seeds <- c(9123L, 9456L, 9789L)

#' 仅对给定 id 均分为 3 组，标记为 A、B、C（同一顺序与 assign_fold_by_id 的余数分配一致）
assign_inner_abc <- function(ids_vec, seed) {
  set.seed(seed)
  n <- length(ids_vec)
  if (n == 0L) {
    return(data.frame(id = ids_vec, fold2 = character(0), stringsAsFactors = FALSE))
  }
  if (n < 3L) stop("fold_1 为 1–4 的患者数仅 ", n, "，不足以做三折。")
  perm <- sample.int(n, n)
  ids_shuf <- ids_vec[perm]
  K <- 3L
  base <- n %/% K
  rem <- n %% K
  sizes <- rep(as.integer(base), K)
  if (rem > 0L) sizes[seq_len(rem)] <- sizes[seq_len(rem)] + 1L
  fold2 <- rep(c("A", "B", "C"), times = sizes)
  data.frame(id = ids_shuf, fold2 = fold2, stringsAsFactors = FALSE)
}

add_fold2_column <- function(dat, seed_inner, idc) {
  if (!"fold_1" %in% names(dat)) stop("数据中缺少列 fold_1")
  dat$fold_1 <- as.integer(dat$fold_1)
  in_pool <- dat$fold_1 %in% 1L:4L
  ids_inner <- unique(dat[[idc]][in_pool])
  ids_inner <- ids_inner[!is.na(ids_inner)]
  map2 <- assign_inner_abc(ids_inner, seed_inner)
  names(map2)[1] <- idc
  dat$fold2 <- NA_character_
  j <- match(dat[[idc]], map2[[idc]])
  ok <- in_pool & !is.na(j)
  dat$fold2[ok] <- map2$fold2[j[ok]]
  dat
}

for (ii in seq_along(fold_xlsx_names)) {
  fn <- fold_xlsx_names[ii]
  path_in <- file.path(work_dir, fn)
  if (!file.exists(path_in)) path_in <- file.path(data_dir, fn)
  if (!file.exists(path_in)) stop("未找到：", fn, "（已试 work_dir 与 DATA）")
  raw <- as.data.frame(readxl::read_xlsx(path_in), stringsAsFactors = FALSE)
  raw$.row_order <- seq_len(nrow(raw))
  out2 <- add_fold2_column(raw, inner_seeds[ii], id_col)
  out2 <- out2[order(out2$.row_order), , drop = FALSE]
  out2$.row_order <- NULL
  out_name <- sub("\\.xlsx$", "inner.xlsx", fn, ignore.case = TRUE)
  path_out <- file.path(data_dir, out_name)
  writexl::write_xlsx(out2, path_out)
  n_abc <- vapply(c("A", "B", "C"), function(lab) {
    length(unique(out2[[id_col]][out2$fold_1 %in% 1L:4L & out2$fold2 == lab]))
  }, integer(1))
  n_v <- length(unique(out2[[id_col]][out2$fold_1 == 5L]))
  message(
    "内层划分已写出：", path_out,
    " （验证集 fold_1=5 患者数 ", n_v,
    "；内层 A/B/C 患者数 ", paste(n_abc, collapse = "/"), "）"
  )
}

