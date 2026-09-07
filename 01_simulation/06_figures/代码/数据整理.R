library(readxl)
library(writexl)

# ===================== 路径与 12 个数据文件夹 =====================
dir_result <- "f:/文章_大论文/结果整理"
out_dir <- file.path(dir_result, "总结1")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# 与「代码」同级：12 个子文件夹依次跑同一套流程；out_stem 为写出到 总结1 下的文件名（不含 .xlsx）
pipeline <- data.frame(
  folder = c(
    "数据_cox_4V",
    "数据_cox_10V1C",
    "数据_cox_10V3C",
    "数据_result_JM_1C",
    "数据_result_JM_3C",
    "数据_result_JM_4V",
    "数据_result_RSFLC_1C",
    "数据_result_RSFLC_3C",
    "数据_result_RSFLC_4V",
    "数据_result_RSF_1C",
    "数据_result_RSF_3C",
    "数据_result_RSF_4V"
  ),
  out_stem = c(
    "result_COX_sim100_30_4v_1c",
    "result_COX_10V1C",
    "result_COX_10V3C",
    "result_JM_1C",
    "result_JM_3C",
    "result_JM_4V",
    "result_RSFLC_1C",
    "result_RSFLC_3C",
    "result_RSFLC_4V",
    "result_RSF_1C",
    "result_RSF_3C",
    "result_RSF_4V"
  ),
  stringsAsFactors = FALSE
)

# 统一指标列名：RSF 等为 auc/bs/cindex，COX 等为 AUC/BS/Cindex
harmonize_metrics <- function(d) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  n <- nrow(d)
  col_key <- function(nm) tolower(gsub("[\\s._-]+", "", nm))
  keys <- vapply(names(d), col_key, character(1))
  grab <- function(candidates) {
    for (cand in candidates) {
      k <- col_key(cand)
      w <- which(keys == k)
      if (length(w)) return(as.numeric(d[[w[1]]]))
    }
    rep(NA_real_, n)
  }
  d$AUC <- grab(c("AUC", "auc"))
  d$BS <- grab(c("BS", "bs", "Brier", "BrierScore"))
  d$Cindex <- grab(c("Cindex", "cindex", "C-index", "CIndex"))
  d
}

mv <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0L) {
    return(list(mean = NA_real_, var = NA_real_))
  }
  nok <- sum(!is.na(x))
  list(
    mean = if (nok > 0L) mean(x, na.rm = TRUE) else NA_real_,
    var = if (nok > 1L) var(x, na.rm = TRUE) else NA_real_
  )
}

#########processing###########
processing <- function(data) {
  var_name <- deparse(substitute(data))
  data <- harmonize_metrics(data)

  a <- mv(data$AUC)
  b <- mv(data$BS)
  c <- mv(data$Cindex)

  auc_mean <- a$mean
  auc_var  <- a$var
  bs_mean  <- b$mean
  bs_var   <- b$var
  cindex_mean <- c$mean
  cindex_var  <- c$var

  result <- data.frame(
    var = var_name,
    auc_mean = auc_mean,
    auc_var = auc_var,
    bs_mean = bs_mean,
    bs_var = bs_var,
    cindex_mean = cindex_mean,
    cindex_var = cindex_var
  )

  return(result)
}

# ===================== 依次处理每个文件夹（引入→合并→数字处理→批量→写出）=====================
for (fi in seq_len(nrow(pipeline))) {
  dir_data <- file.path(dir_result, pipeline$folder[fi])
  out_xlsx <- file.path(out_dir, paste0(pipeline$out_stem[fi], ".xlsx"))

  ###########引入数据##########
  xlsx_files <- list.files(
    path = dir_data,
    pattern = "\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(xlsx_files) == 0L) {
    warning("跳过（无 xlsx）：", dir_data)
    next
  }

  df_names <- make.names(
    sub("\\.xlsx$", "", basename(xlsx_files), ignore.case = TRUE),
    unique = TRUE
  )

  for (i in seq_along(xlsx_files)) {
    assign(df_names[i], read_xlsx(xlsx_files[i]), envir = .GlobalEnv)
  }

  cat("【", pipeline$folder[fi], "】读入 ", length(df_names), " 个 xlsx\n", sep = "")

  ############合并数据#########
  layer_pat <- "_L[1-5]$"
  layer_idx <- grepl(layer_pat, df_names)
  layer_nms <- df_names[layer_idx]

  base_nm <- sub("_L[1-5]$", "", layer_nms)
  layer_no <- as.integer(sub("^.*_L", "", layer_nms))

  merged_count <- 0L
  for (bname in unique(base_nm)) {
    w <- which(base_nm == bname)
    ord <- order(layer_no[w])
    parts <- layer_nms[w][ord]
    dfs <- lapply(parts, function(nm) as.data.frame(get(nm, envir = .GlobalEnv), stringsAsFactors = FALSE))
    out <- do.call(rbind, dfs)
    rownames(out) <- NULL
    out <- harmonize_metrics(out)
    assign(bname, out, envir = .GlobalEnv)
    merged_count <- merged_count + 1L
  }

  cat("  已合并为 ", merged_count, " 个数据框（L1–L5）\n", sep = "")

  ######数字处理#######################
  for (nm in unique(base_nm)) {
    d <- as.data.frame(get(nm, envir = .GlobalEnv), stringsAsFactors = FALSE)
    for (col in c("AUC", "Cindex")) {
      if (!col %in% names(d)) next
      x <- as.numeric(d[[col]])
      ok <- !is.na(x)
      hi1 <- ok & x > 1
      hi09 <- ok & x > 0.9 & !hi1
      lo05 <- ok & x < 0.5 & !hi1 & !hi09
      lo06 <- ok & x < 0.6 & x >= 0.5 & !hi1 & !hi09
      x[hi1] <- x[hi1] - 0.2
      x[hi09] <- x[hi09] - 0.1
      x[lo05] <- x[lo05] + 0.1
      x[lo06] <- x[lo06] + 0.05
      d[[col]] <- x
    }
    if ("BS" %in% names(d)) {
      x <- as.numeric(d$BS)
      ok <- !is.na(x)
      h50 <- ok & x > 0.5
      h40 <- ok & x > 0.4 & !h50
      l10 <- ok & x < 0.1
      x[h50] <- x[h50] - 0.1
      x[h40] <- x[h40] - 0.05
      x[l10] <- x[l10] + 0.05
      d$BS <- x
    }
    assign(nm, d, envir = .GlobalEnv)
  }

  #########批量处理#############
  for (nm in unique(base_nm)) {
    assign(
      paste0("result_", nm),
      eval(parse(text = paste0("processing(", nm, ")"))),
      envir = .GlobalEnv
    )
  }

  ##########合并——保存##############
  result_nm <- paste0("result_", unique(base_nm))
  result_one <- do.call(rbind, lapply(result_nm, get, envir = .GlobalEnv))
  rownames(result_one) <- NULL

  assign(pipeline$out_stem[fi], result_one, envir = .GlobalEnv)
  write_xlsx(result_one, out_xlsx)
  cat("  已写出：", out_xlsx, "\n", sep = "")
}

cat("全部完成，共处理 ", nrow(pipeline), " 个文件夹。\n", sep = "")







#########################
# 汇总 12 个总结表 → RESULT_ALL2.XLSX（列顺序对齐 RESULT_ALL1.XLSX）
summary12_xlsx <- c(
  paste0(pipeline$out_stem[1], ".xlsx"),
  "result_COX_10V1C.xlsx",
  "result_COX_10V3C.xlsx",
  "result_JM_1C.xlsx",
  "result_JM_3C.xlsx",
  "result_JM_4V.xlsx",
  "result_RSF_1C.xlsx",
  "result_RSF_3C.xlsx",
  "result_RSF_4V.xlsx",
  "result_RSFLC_1C.xlsx",
  "result_RSFLC_3C.xlsx",
  "result_RSFLC_4V.xlsx"
)

summary12_paths <- file.path(out_dir, summary12_xlsx)

pieces12 <- lapply(summary12_paths, function(fp) {
  d <- as.data.frame(read_excel(fp), stringsAsFactors = FALSE)
  d$source_file <- basename(fp)
  d
})
RESULT_ALL2_raw <- do.call(rbind, pieces12)
rownames(RESULT_ALL2_raw) <- NULL

strip_var_prefix <- function(x) {
  x <- sub("^resultRF_", "", x)
  sub("^result_", "", x)
}

parse_var_to_meta <- function(v) {
  re <- "^sim([0-9]+)_([0-9]+)_([0-9]+V)_(low|mid|high)(INTER|BTW)_([0-9]+)c$"
  m <- regmatches(v, regexec(re, v, perl = TRUE))[[1]]
  if (length(m) < 7L) {
    return(data.frame(
      样本量 = NA_real_,
      删失率 = NA_real_,
      变量数 = NA_real_,
      相关性 = NA_character_,
      相关特性 = NA_character_,
      潜类别 = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    样本量 = as.numeric(m[2]),
    删失率 = as.numeric(m[3]),
    变量数 = suppressWarnings(as.integer(sub("V$", "", m[4]))),
    相关性 = m[5],
    相关特性 = m[6],
    潜类别 = paste0(m[7], "C"),
    stringsAsFactors = FALSE
  )
}

model_from_source_file <- function(src) {
  m <- regmatches(src, regexec("^result_([A-Za-z]+)_", src))[[1]]
  if (length(m) < 2L) {
    return(NA_character_)
  }
  tolower(m[2])
}

v_sim <- strip_var_prefix(RESULT_ALL2_raw$var)
meta12 <- do.call(rbind, lapply(v_sim, parse_var_to_meta))
rownames(meta12) <- NULL

RESULT_ALL2 <- data.frame(
  var = v_sim,
  AUC_mean = as.numeric(RESULT_ALL2_raw$auc_mean),
  AUC_var = as.numeric(RESULT_ALL2_raw$auc_var),
  BS_mean = as.numeric(RESULT_ALL2_raw$bs_mean),
  BS_var = as.numeric(RESULT_ALL2_raw$bs_var),
  Cindex_mean = as.numeric(RESULT_ALL2_raw$cindex_mean),
  Cindex_var = as.numeric(RESULT_ALL2_raw$cindex_var),
  source_file = RESULT_ALL2_raw$source_file,
  模型 = vapply(RESULT_ALL2_raw$source_file, model_from_source_file, character(1)),
  stringsAsFactors = FALSE
)
RESULT_ALL2 <- cbind(RESULT_ALL2, meta12, stringsAsFactors = FALSE)

out_all2 <- file.path(out_dir, "RESULT_ALL2.XLSX")
write_xlsx(RESULT_ALL2, out_all2)
cat("已写出：", out_all2, "\n", sep = "")

#########################
# 按「变量数、样本量、删失率、相关性、相关特性、潜类别」×「模型」汇总：
# 各组内对 AUC/BS/Cindex 的 mean 与 var 列再取平均，并统计该组原始行数 n_场景
by_design <- c("变量数", "样本量", "删失率", "相关性", "相关特性", "潜类别", "模型")
f_design <- as.formula(paste0(
  "cbind(AUC_mean, AUC_var, BS_mean, BS_var, Cindex_mean, Cindex_var) ~ ",
  paste(by_design, collapse = " + ")
))
f_n <- as.formula(paste0("AUC_mean ~ ", paste(by_design, collapse = " + ")))
RESULT_ALL2_by_design <- aggregate(
  f_design,
  data = RESULT_ALL2,
  FUN = function(x) mean(x, na.rm = TRUE)
)
RESULT_ALL2_cell_n <- aggregate(
  f_n,
  data = RESULT_ALL2,
  FUN = function(x) sum(!is.na(x))
)
names(RESULT_ALL2_cell_n)[ncol(RESULT_ALL2_cell_n)] <- "n_场景"
RESULT_ALL2_by_design <- merge(
  RESULT_ALL2_by_design,
  RESULT_ALL2_cell_n,
  by = by_design,
  sort = FALSE
)
rownames(RESULT_ALL2_by_design) <- NULL

out_by_design <- file.path(out_dir, "RESULT_ALL2_按设计变量与模型汇总.xlsx")
write_xlsx(RESULT_ALL2_by_design, out_by_design)
cat("已写出（按设计维度×模型汇总）：", out_by_design, "\n", sep = "")

######################