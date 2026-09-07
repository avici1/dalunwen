## 0314 宽表数据汇总
## 对 0314_final_widedata.xlsx 进行描述性统计：
## 1) 患者数量（唯一 subject_id 数）
## 2) 每个变量（每列/itemid）的缺失值比例
## 结果保存至 0314_summary.xlsx

## 如未安装以下包，请先运行：
## install.packages(c("readxl", "writexl", "dplyr"))

library(readxl)
library(writexl)
library(dplyr)

## 工作目录：脚本所在目录（0314）
script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
data_dir   <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理"
setwd(script_dir)

## 1. 读取宽表数据
wide_path <- file.path(data_dir, "0314_final_widedata.xlsx")
dat       <- read_xlsx(wide_path)

## 2. 患者数量：唯一 subject_id
n_patients <- length(unique(dat$subject_id))
cat("患者数量（唯一 subject_id）:", n_patients, "\n")

## 3. 各列缺失值统计
## 缺失定义：NA 或 空字符串 ""
is_missing <- function(x) {
  is.na(x) | (is.character(x) & trimws(as.character(x)) == "")
}

n_rows <- nrow(dat)
missing_stats <- data.frame(
  column     = names(dat),
  n_missing  = vapply(dat, function(col) sum(is_missing(col)), integer(1)),
  n_total    = n_rows,
  stringsAsFactors = FALSE
)
missing_stats$missing_pct <- round(100 * missing_stats$n_missing / missing_stats$n_total, 2)
missing_stats$non_missing <- missing_stats$n_total - missing_stats$n_missing

## 4. 汇总表：先写“总体汇总”，再写“各列缺失”
summary_overall <- data.frame(
  item       = c("subject_id_数量_患者数", "总行数", "总列数"),
  value      = c(n_patients, n_rows, ncol(dat)),
  stringsAsFactors = FALSE
)

col_detail <- missing_stats %>%
  select(column, n_missing, non_missing, n_total, missing_pct) %>%
  rename(
    itemid_列名 = column,
    缺失数 = n_missing,
    非缺失数 = non_missing,
    总行数 = n_total,
    缺失比例_percent = missing_pct
  )

## 5. 写出到 0314_summary.xlsx（两个 sheet）
out_path <- file.path(script_dir, "0314_summary.xlsx")
write_xlsx(
  list(
    "总体汇总" = summary_overall,
    "各列缺失比例" = col_detail
  ),
  out_path
)

cat("结果已保存至:", out_path, "\n")
cat("Sheet [总体汇总]: 患者数、总行数、总列数\n")
cat("Sheet [各列缺失比例]: 每列缺失数、非缺失数、缺失比例(%)\n")


#################################
## 6. 变量重命名 + 删除 100% 缺失列，保存 0314_widedata_1.xlsx
## 6.1 删除缺失率 100% 的变量（如 gcs_total, hr_min_24 等）
cols_100_missing <- missing_stats$column[missing_stats$missing_pct >= 100]
dat1 <- dat %>% select(-all_of(cols_100_missing))
cat("已删除缺失率 100% 的变量数:", length(cols_100_missing), "\n")

## 6.2 保留的 ID 列（不加重命名）
id_cols <- c("subject_id", "hadm_id", "stay_id", "specimen_id", "charttime")
id_cols <- intersect(id_cols, names(dat1))

## 6.3 其余列统一加前缀 itemid_（item_50802 -> itemid_50802，50802 -> itemid_50802）
rename_itemid <- function(nm) {
  if (nm %in% id_cols) return(nm)
  if (grepl("^itemid_", nm)) return(nm)
  if (grepl("^item_", nm)) return(sub("^item_", "itemid_", nm))
  return(paste0("itemid_", nm))
}
names(dat1) <- vapply(names(dat1), rename_itemid, character(1))

## 6.4 保存到 0314_widedata_1.xlsx
out_wide <- file.path(script_dir, "0314_widedata_1.xlsx")
write_xlsx(dat1, out_wide)
cat("修改后宽表已保存至:", out_wide, "\n")
cat("列数: 原", ncol(dat), "-> 现", ncol(dat1), "\n")

#################################
## 7. 按 subject_id 过滤：若某患者 80% 的 itemid 变量为缺失，则删除该患者
## 结果保存至 0314_widedata_2.xlsx
itemid_cols <- names(dat1)[grepl("^itemid_", names(dat1))]
n_itemid    <- length(itemid_cols)

## 按患者计算：该患者所有行、所有 itemid 格子中缺失比例
subject_miss <- dat1 %>%
  mutate(.n_miss = rowSums(across(all_of(itemid_cols), ~ as.integer(is_missing(.x))))) %>%
  group_by(subject_id) %>%
  summarise(
    n_cells   = n() * n_itemid,
    n_miss    = sum(.n_miss),
    .groups   = "drop"
  ) %>%
  mutate(miss_pct = 100 * n_miss / n_cells)

## 保留缺失比例 < 80% 的患者
subject_keep <- subject_miss %>% filter(miss_pct < 80) %>% pull(subject_id)
dat2         <- dat1 %>% filter(subject_id %in% subject_keep)

n_drop <- length(unique(dat1$subject_id)) - length(subject_keep)
cat("按 80% 缺失过滤: 删除患者数 =", n_drop, "，保留患者数 =", length(subject_keep), "\n")
cat("保留行数:", nrow(dat2), "(原", nrow(dat1), ")\n")

out_wide2 <- file.path(script_dir, "0314_widedata_2.xlsx")
write_xlsx(dat2, out_wide2)
cat("结果已保存至:", out_wide2, "\n")


##############################
## 8. 删除缺失率高于 75% 的 itemid 变量，结果保存至 0314_widedata_3.xlsx
## 若未从头运行脚本，则从 0314_widedata_2.xlsx 读取 dat2
if (!exists("dat2")) {
  if (!exists("script_dir")) script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  if (!exists("is_missing")) is_missing <- function(x) is.na(x) | (is.character(x) & trimws(as.character(x)) == "")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)
  dat2 <- read_xlsx(file.path(script_dir, "0314_widedata_2.xlsx"))
}
itemid_cols2 <- names(dat2)[grepl("^itemid_", names(dat2))]
n2           <- nrow(dat2)
itemid_miss  <- data.frame(
  column    = itemid_cols2,
  n_miss    = vapply(dat2[, itemid_cols2], function(col) sum(is_missing(col)), integer(1)),
  stringsAsFactors = FALSE
) %>%
  mutate(miss_pct = 100 * n_miss / n2)
cols_drop_75 <- itemid_miss %>% filter(miss_pct > 75) %>% pull(column)
dat3         <- dat2 %>% select(-all_of(cols_drop_75))

cat("缺失率>75%% 的 itemid 变量已删除: ", length(cols_drop_75), " 个，保留 ", length(itemid_cols2) - length(cols_drop_75), " 个\n", sep = "")
cat("列数: ", ncol(dat2), " -> ", ncol(dat3), "\n", sep = "")

## 8.1 同一患者同一时间多行重复的原因与去重
## 原因：上游 0314_final_widedata 由 check_subjectid_overlap.R 生成时，
##       lab_data_wide（按 subject_id + charttime）与 baseline_icu_data 仅按 subject_id 做 left_join。
##       若同一患者在 baseline/ICU 表中有多行（如多次住院、多次 ICU stay），
##       则每条化验记录会被重复成多行，造成 (subject_id, charttime) 完全相同的重复行。
## 处理：按 (subject_id, charttime) 去重，保留每组的第一行。
key_cols  <- intersect(c("subject_id", "charttime"), names(dat3))
n_before  <- nrow(dat3)
dat3      <- dat3 %>% distinct(!!!syms(key_cols), .keep_all = TRUE)
n_after   <- nrow(dat3)
cat("按 (subject_id, charttime) 去重: ", n_before, " 行 -> ", n_after, " 行，删除 ", n_before - n_after, " 行重复\n", sep = "")

out_wide3 <- file.path(script_dir, "0314_widedata_3.xlsx")
write_xlsx(dat3, out_wide3)
cat("结果已保存至:", out_wide3, "\n")







#################################
## 9. 在 subject_id 后增加 Obstimes：按 charttime 时间顺序的纵向标记，保存至 0314_widedata_4.xlsx
if (!exists("dat3")) {
  if (!exists("script_dir")) script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)
  dat3 <- read_xlsx(file.path(script_dir, "0314_widedata_3.xlsx"))
}
dat4 <- dat3 %>%
  group_by(subject_id) %>%
  mutate(Obstimes = row_number(charttime)) %>%
  ungroup() %>%
  relocate(Obstimes, .after = subject_id)

out_wide4 <- file.path(script_dir, "0314_widedata_4.xlsx")
write_xlsx(dat4, out_wide4)
cat("已添加 Obstimes（按 subject_id + charttime 顺序），结果已保存至:", out_wide4, "\n")







########################################
## 10. 按行删除：若该行 itemid 变量缺失超过 80% 则删行，再重新计算 Obstimes，保存至 0314_widedata_5.xlsx
if (!exists("dat4")) {
  if (!exists("script_dir")) script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  if (!exists("is_missing")) is_missing <- function(x) is.na(x) | (is.character(x) & trimws(as.character(x)) == "")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)
  dat4 <- read_xlsx(file.path(script_dir, "0314_widedata_4.xlsx"))
}
itemid_cols5 <- names(dat4)[grepl("^itemid_", names(dat4))]
n_itemid5    <- length(itemid_cols5)
dat5         <- dat4 %>%
  mutate(.n_miss = rowSums(across(all_of(itemid_cols5), ~ as.integer(is_missing(.x))))) %>%
  filter(.n_miss / n_itemid5 <= 0.5) %>%
  select(-.n_miss) %>%
  group_by(subject_id) %>%
  mutate(Obstimes = row_number(charttime)) %>%
  ungroup() %>%
  relocate(Obstimes, .after = subject_id)

n_drop_row <- nrow(dat4) - nrow(dat5)
cat("按行删除 itemid 缺失>80%%: 删除 ", n_drop_row, " 行，保留 ", nrow(dat5), " 行；已重新排列 Obstimes\n", sep = "")

out_wide5 <- file.path(script_dir, "0314_widedata_5.xlsx")
write_xlsx(dat5, out_wide5)
cat("结果已保存至:", out_wide5, "\n")




#####################################
## 11. 每个患者(subject_id)数据缺失率清单（基于 0314_widedata_4.xlsx），保存至 0314_widedata4_sum.xlsx
## 缺失率 = 1 - (患者有数据的格子数 / (我选中的变量数 * 该患者记录数))
if (!exists("dat4")) {
  if (!exists("script_dir")) script_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  if (!exists("is_missing")) is_missing <- function(x) is.na(x) | (is.character(x) & trimws(as.character(x)) == "")
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  library(readxl)
  dat4 <- read_xlsx(file.path(script_dir, "0314_widedata_4.xlsx"))
}
itemid_cols_sum <- names(dat4)[grepl("^itemid_", names(dat4))]
n_var          <- length(itemid_cols_sum)   ## 我选中的变量数

patient_miss <- dat4 %>%
  mutate(.n_miss = rowSums(across(all_of(itemid_cols_sum), ~ as.integer(is_missing(.x))))) %>%
  group_by(subject_id) %>%
  summarise(
    记录数_Obstimes = n(),
    总格子数 = n_var * n(),
    有数据格子数 = sum((n_var - .n_miss)),
    .groups = "drop"
  ) %>%
  mutate(
    有数据比例 = round(有数据格子数 / 总格子数, 4),
    缺失率 = round(1 - 有数据格子数 / 总格子数, 4)
  )

out_sum4 <- file.path(script_dir, "0314_widedata4_sum.xlsx")
write_xlsx(patient_miss, out_sum4)
cat("每个患者缺失率清单已保存至:", out_sum4, "\n")
cat("变量数(itemid列数) = ", n_var, "；患者数 = ", nrow(patient_miss), "\n", sep = "")



out_wide6 <- file.path(script_dir, "0314_widedata_6.xlsx")
write_xlsx(dat6, out_wide6)
cat("结果已保存至:", out_wide6, "\n")

####################################
## 按行扫描 itemid 变量：该行缺失率 > 80% 则删除该行，结果保存至 0314_widedata_7.xlsx
script_dir_7 <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
library(readxl)
library(writexl)
library(dplyr)
dat_in <- read_xlsx(file.path(script_dir_7, "0314_widedata_4.xlsx"))
itemid_names <- names(dat_in)[grepl("^itemid_", names(dat_in))]
n_itemid     <- length(itemid_names)
if (n_itemid == 0) stop("未找到以 itemid_ 开头的列，请检查数据列名。")

## 单格是否视为缺失（统一先转字符再判空和 NA/NaN 等）
one_miss <- function(x) {
  if (is.na(x)) return(TRUE)
  if (is.numeric(x) && !is.finite(x)) return(TRUE)
  s <- trimws(as.character(x))
  (s == "") | (s == "NA") | (s == "NaN") | (s == "N/A") | (s == ".")
}
## 按行：该行在 itemid 列中有多少格为缺失
itemid_only <- dat_in[, itemid_names, drop = FALSE]
n_miss_row  <- apply(itemid_only, 1L, function(r) sum(vapply(r, one_miss, logical(1))))
miss_rate   <- n_miss_row / n_itemid
keep        <- miss_rate <= 0.8
dat7        <- dat_in[keep, , drop = FALSE]

cat("按行 itemid 缺失率>80%% 删行：总行数 ", nrow(dat_in), "，删除 ", sum(!keep), " 行，保留 ", nrow(dat7), " 行\n", sep = "")
write_xlsx(dat7, file.path(script_dir_7, "0314_widedata_7.xlsx"))
cat("结果已保存至:", file.path(script_dir_7, "0314_widedata_7.xlsx"), "\n")








###########################################################
## 对 0314_widedata_filter1.xlsx 筛选：行缺失率>70% 则删行，结果保存至 0314_widedata_filter2.xlsx（转置法）
script_dir_f <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
library(readxl)
library(writexl)
dat_f1   <- read_xlsx(file.path(script_dir_f, "0314_widedata_filter1.xlsx"))
itemid_f <- names(dat_f1)[grepl("^itemid_", names(dat_f1))]
if (length(itemid_f) == 0) itemid_f <- names(dat_f1)

one_miss_f <- function(x) {
  if (is.na(x)) return(TRUE)
  if (is.numeric(x) && !is.finite(x)) return(TRUE)
  s <- trimws(as.character(x))
  (s == "") | (s == "NA") | (s == "NaN") | (s == "N/A") | (s == ".")
}
dat_t_f    <- t(as.matrix(dat_f1))
rownames(dat_t_f) <- names(dat_f1)
colnames(dat_t_f) <- seq_len(nrow(dat_f1))
data_rows  <- rownames(dat_t_f) %in% itemid_f
n_data     <- sum(data_rows)
block_f    <- dat_t_f[data_rows, , drop = FALSE]
n_miss_f   <- apply(block_f, 2L, function(col) sum(vapply(col, one_miss_f, logical(1))))
keep_f     <- (n_miss_f / n_data) <= 0.7
dat_kept_f <- dat_t_f[, keep_f, drop = FALSE]
dat_f2_mat <- t(dat_kept_f)
colnames(dat_f2_mat) <- rownames(dat_kept_f)
dat_f2     <- as.data.frame(dat_f2_mat, stringsAsFactors = FALSE)

## 增加 Obstimes：相同 subject_id 按 charttime 时间顺序从 1 开始计数
library(dplyr)
id_subj <- "subject_id"
id_time <- "charttime"
if (!id_subj %in% names(dat_f2)) id_subj <- "subjectid"
if (!id_time %in% names(dat_f2)) id_time <- "charttime"
dat_f2 <- dat_f2 %>%
  group_by(!!sym(id_subj)) %>%
  arrange(!!sym(id_time)) %>%
  mutate(Obstimes = row_number()) %>%
  ungroup() %>%
  relocate(Obstimes, .after = all_of(id_subj))

cat("filter1 按行缺失率>70%% 删行：总行数 ", nrow(dat_f1), "，删除 ", sum(!keep_f), " 行，保留 ", nrow(dat_f2), " 行\n", sep = "")
write_xlsx(dat_f2, file.path(script_dir_f, "0314_widedata_filter2.xlsx"))
cat("已添加 Obstimes（按 subject_id + charttime 顺序），结果已覆盖保存至:", file.path(script_dir_f, "0314_widedata_filter2.xlsx"), "\n")






######################################
## 线性预测填补：同一患者(subject_id)同一变量(itemid)下，以 charttime 为 x、value 为 y 拟合直线，缺失处用预测值填补
## 结果保存至 0314_widedata_inputation1.xlsx
script_dir_imp <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
library(readxl)
library(writexl)
dat_imp   <- read_xlsx(file.path(script_dir_imp, "0314_widedata_filter2.xlsx"))
id_subj   <- "subject_id"
id_time   <- "charttime"
if (!id_subj %in% names(dat_imp)) id_subj <- "subjectid"
if (!id_time %in% names(dat_imp)) id_time <- "charttime"
itemid_imp <- names(dat_imp)[grepl("^itemid_", names(dat_imp))]
if (length(itemid_imp) == 0) stop("未找到 itemid_ 列。")

## 将 charttime 转为数值（用于 lm）
tc <- dat_imp[[id_time]]
if (inherits(tc, "POSIXt")) {
  x_num <- as.numeric(tc)
} else {
  x_num <- as.numeric(as.POSIXct(as.character(tc), tz = "UTC", optional = TRUE))
}
if (all(is.na(x_num))) stop("charttime 无法转为数值，请检查格式。")
dat_imp$.x_time <- x_num

subjects <- unique(dat_imp[[id_subj]])
for (col in itemid_imp) {
  y_raw <- dat_imp[[col]]
  if (is.character(y_raw)) y_raw <- as.numeric(as.character(y_raw))
  for (sid in subjects) {
    idx    <- dat_imp[[id_subj]] == sid
    x_all  <- dat_imp$.x_time[idx]
    y_all  <- y_raw[idx]
    ok     <- !is.na(y_all) & !is.na(x_all) & is.finite(y_all) & is.finite(x_all)
    n_ok   <- sum(ok)
    if (n_ok < 2L) next
    d_fit  <- data.frame(x = x_all[ok], y = y_all[ok])
    fit    <- lm(y ~ x, data = d_fit)
    miss      <- idx & (is.na(y_raw) | !is.finite(y_raw))
    if (!any(miss)) next
    miss_idx  <- which(miss)
    x_miss    <- dat_imp$.x_time[miss]
    valid     <- is.finite(x_miss) & !is.na(x_miss)
    if (!any(valid)) next
    pred      <- predict(fit, newdata = data.frame(x = x_miss[valid]))
    y_raw[miss_idx[valid]] <- as.numeric(pred)
  }
  dat_imp[[col]] <- y_raw
}
dat_imp$.x_time <- NULL

write_xlsx(dat_imp, file.path(script_dir_imp, "0314_widedata_inputation1.xlsx"))
cat("线性预测填补完成，已保存至:", file.path(script_dir_imp, "0314_widedata_inputation1.xlsx"), "\n")

###################################
## 总结 dat_imp：患者数(subject_id)、各患者重复测量频数
if (!exists("dat_imp")) {
  script_dir_imp <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  library(readxl)
  dat_imp <- read_xlsx(file.path(script_dir_imp, "0314_widedata_inputation1.xlsx"))
}
id_subj_sum <- "subject_id"
if (!id_subj_sum %in% names(dat_imp)) id_subj_sum <- "subjectid"
n_patients_imp <- length(unique(dat_imp[[id_subj_sum]]))
library(dplyr)
freq_imp <- dat_imp %>%
  group_by(!!sym(id_subj_sum)) %>%
  summarise(重复测量次数 = n(), .groups = "drop")
cat("dat_imp 患者数(subject_id): ", n_patients_imp, "\n", sep = "")
cat("重复测量频数汇总:\n")
print(summary(freq_imp[["重复测量次数"]]))
cat("各患者重复测量次数（前 20 例）:\n")
print(head(freq_imp, 20))

###################################
## 以 dat_imp 为基础，合并 widedata_4 中基线信息（dat_imp 中不存在的变量），按 subject_id、charttime，行数与 dat_imp 一致，结果存 data_rbind1
if (!exists("dat_imp")) {
  script_dir_imp <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
  library(readxl)
  dat_imp <- read_xlsx(file.path(script_dir_imp, "0314_widedata_inputation1.xlsx"))
}
script_dir_merge <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
wide4 <- read_xlsx(file.path(script_dir_merge, "0314_widedata_4.xlsx"))
key1 <- "subject_id"
key2 <- "charttime"
if (!key1 %in% names(wide4) && "subjectid" %in% names(wide4)) wide4 <- rename(wide4, subject_id = subjectid)
cols_base <- setdiff(names(wide4), names(dat_imp))
cols_join <- c(key1, key2, cols_base)
cols_join <- intersect(cols_join, names(wide4))
wide4_sub <- wide4[, cols_join, drop = FALSE]
wide4_sub <- wide4_sub %>% distinct(!!sym(key1), !!sym(key2), .keep_all = TRUE)
## 统一连接键类型，避免 character / double 无法匹配
dat_imp[[key1]] <- as.character(dat_imp[[key1]])
dat_imp[[key2]] <- as.character(dat_imp[[key2]])
wide4_sub[[key1]] <- as.character(wide4_sub[[key1]])
wide4_sub[[key2]] <- as.character(wide4_sub[[key2]])
data_rbind1 <- dat_imp %>%
  left_join(wide4_sub, by = c(key1, key2))
cat("合并基线信息完成: data_rbind1 行数 = ", nrow(data_rbind1), "（与 dat_imp 一致 ", nrow(data_rbind1) == nrow(dat_imp), "），新增变量数 = ", length(cols_base), "\n", sep = "")

#########################
## 将 0314_widedata_4.xlsx 与 0314_widedata_inputation1.xlsx 按 subject_id、charttime 合并
## 以 inputation 为基准（行数与其一致），重复变量以 inputation 为准，将 widedata_4 有而 inputation 没有的变量追加到合并结果
## 合并结果保存至 0314_widedata_merge.xlsx
path_merge_dir <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
dat_imp_merge  <- read_xlsx(file.path(path_merge_dir, "0314_widedata_inputation1.xlsx"))
wide4_merge    <- read_xlsx(file.path(path_merge_dir, "0314_widedata_4.xlsx"))
key1_merge <- "subject_id"
key2_merge <- "charttime"
if (!key1_merge %in% names(wide4_merge) && "subjectid" %in% names(wide4_merge)) {
  wide4_merge <- rename(wide4_merge, subject_id = subjectid)
}
## widedata_4 有而 inputation 没有的变量（不含连接键则后续用 left_join 带入）
cols_extra <- setdiff(names(wide4_merge), names(dat_imp_merge))
cols_join_merge <- c(key1_merge, key2_merge, cols_extra)
cols_join_merge <- intersect(cols_join_merge, names(wide4_merge))
wide4_merge_sub <- wide4_merge[, cols_join_merge, drop = FALSE]
wide4_merge_sub <- wide4_merge_sub %>% distinct(!!sym(key1_merge), !!sym(key2_merge), .keep_all = TRUE)
dat_imp_merge[[key1_merge]] <- as.character(dat_imp_merge[[key1_merge]])
dat_imp_merge[[key2_merge]] <- as.character(dat_imp_merge[[key2_merge]])
wide4_merge_sub[[key1_merge]] <- as.character(wide4_merge_sub[[key1_merge]])
wide4_merge_sub[[key2_merge]] <- as.character(wide4_merge_sub[[key2_merge]])
widedata_merge <- dat_imp_merge %>%
  left_join(wide4_merge_sub, by = c(key1_merge, key2_merge))
out_merge <- file.path(path_merge_dir, "0314_widedata_merge.xlsx")
write_xlsx(widedata_merge, out_merge)
cat("合并完成: 行数 = ", nrow(widedata_merge), "（与 inputation 一致 ", nrow(widedata_merge) == nrow(dat_imp_merge), "），新增变量数 = ", length(cols_extra), "\n", sep = "")
cat("结果已保存至:", out_merge, "\n")





######################################
## 仅总结用户指定的字符/分类变量，结果存数据框 data_V_summary（不输出 xlsx）
vars_cat_selected <- c(
  "itemid_admission_type",
  "itemid_admission_location",
  "itemid_discharge_location",
  "itemid_insurance",
  "itemid_language",
  "itemid_marital_status",
  "itemid_race",
  "itemid_first_careunit"
)
vars_cat <- intersect(vars_cat_selected, names(widedata_merge))
if (length(vars_cat) == 0L) {
  data_V_summary <- data.frame(variable = character(), category = character(), n = integer(), pct = numeric(), stringsAsFactors = FALSE)
  cat("未在 widedata_merge 中找到所选字符/分类变量，data_V_summary 为空。\n")
} else {
  lst_summary <- lapply(vars_cat, function(v) {
    x <- widedata_merge[[v]]
    x <- if (is.character(x)) trimws(as.character(x)) else as.character(x)
    x[is.na(x) | x == ""] <- NA_character_
    tbl <- table(x, useNA = "ifany")
    n_tot <- length(x)
    data.frame(
      variable = v,
      category = c(names(tbl)),
      n = as.integer(tbl),
      pct = round(100 * as.integer(tbl) / n_tot, 2),
      stringsAsFactors = FALSE
    )
  })
  data_V_summary <- bind_rows(lst_summary)
  data_V_summary$category[is.na(data_V_summary$category)] <- "<NA>"
  cat("字符/分类变量频数汇总完成，变量数 = ", length(vars_cat), "，data_V_summary 行数 = ", nrow(data_V_summary), "\n", sep = "")
}


#############################

## 对高基数分类变量进行合并后再汇总频数（结果存 data_V_summary1，不输出 xlsx）
## 参考常见临床研究做法：
## race -> White/Black/Asian/Hispanic/Other
## marital -> Married/Single/Divorced/Widowed/Separated/Other
## language -> English/Spanish/Chinese/Other
vars_reduce <- c("itemid_race", "itemid_marital_status", "itemid_language")
vars_reduce <- intersect(vars_reduce, names(widedata_merge))

map_race <- function(x) {
  y <- toupper(trimws(as.character(x)))
  out <- ifelse(is.na(y) | y == "", NA_character_,
                ifelse(grepl("ASIAN", y), "Asian",
                       ifelse(grepl("BLACK|AFRICAN", y), "Black",
                              ifelse(grepl("HISPANIC|LATINO", y), "Hispanic",
                                     ifelse(grepl("WHITE", y), "White", "Other")))))
  out
}

map_marital <- function(x) {
  y <- toupper(trimws(as.character(x)))
  out <- ifelse(is.na(y) | y == "", NA_character_,
                ifelse(grepl("MARRIED", y), "Married",
                       ifelse(grepl("SINGLE|NEVER", y), "Single",
                              ifelse(grepl("DIVORCED", y), "Divorced",
                                     ifelse(grepl("WIDOW", y), "Widowed",
                                            ifelse(grepl("SEPARAT", y), "Separated", "Other"))))))
  out
}

map_language <- function(x) {
  y <- toupper(trimws(as.character(x)))
  out <- ifelse(is.na(y) | y == "", NA_character_,
                ifelse(grepl("^ENGLISH$| ENGLISH|ENGLISH ", y), "English",
                       ifelse(grepl("SPANISH|ESPAN", y), "Spanish",
                              ifelse(grepl("CHINESE|MANDARIN|CANTONESE", y), "Chinese", "Other"))))
  out
}

if (length(vars_reduce) == 0L) {
  data_V_summary1 <- data.frame(variable = character(), category = character(), n = integer(), pct = numeric(), stringsAsFactors = FALSE)
  cat("未在 widedata_merge 中找到 itemid_race/itemid_marital_status/itemid_language，data_V_summary1 为空。\n")
} else {
  lst_summary1 <- lapply(vars_reduce, function(v) {
    x <- widedata_merge[[v]]
    x_new <- if (v == "itemid_race") map_race(x) else if (v == "itemid_marital_status") map_marital(x) else map_language(x)
    tbl <- table(x_new, useNA = "ifany")
    n_tot <- length(x_new)
    data.frame(
      variable = v,
      category = c(names(tbl)),
      n = as.integer(tbl),
      pct = round(100 * as.integer(tbl) / n_tot, 2),
      stringsAsFactors = FALSE
    )
  })
  data_V_summary1 <- bind_rows(lst_summary1)
  data_V_summary1$category[is.na(data_V_summary1$category)] <- "<NA>"
  cat("分类合并后频数汇总完成，变量数 = ", length(vars_reduce), "，data_V_summary1 行数 = ", nrow(data_V_summary1), "\n", sep = "")
}







#################################################
## 依据 data_V_summary1 的合并规则，对 widedata_merge 对应变量进行重编码
## 注意：这里直接覆盖原变量取值
if (exists("data_V_summary1") && nrow(data_V_summary1) > 0L) {
  ## 不覆盖原始 widedata_merge，另存为 widedata_merge1
  widedata_merge1 <- widedata_merge
  if ("itemid_race" %in% names(widedata_merge1)) {
    widedata_merge1$itemid_race <- map_race(widedata_merge1$itemid_race)
  }
  if ("itemid_marital_status" %in% names(widedata_merge1)) {
    widedata_merge1$itemid_marital_status <- map_marital(widedata_merge1$itemid_marital_status)
  }
  if ("itemid_language" %in% names(widedata_merge1)) {
    widedata_merge1$itemid_language <- map_language(widedata_merge1$itemid_language)
  }
  cat("已按 data_V_summary1 对 widedata_merge1 完成变量合并：itemid_race/itemid_marital_status/itemid_language（原 widedata_merge 未覆盖）\n")
} else {
  cat("data_V_summary1 为空，未生成 widedata_merge1。\n")
}


###################
write_xlsx(widedata_merge1,"F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314/widedata_merge1.xlsx")





#######################################################

## RSF建模（参考 cal_3_RSF）：使用 widedata_merge.xlsx 且仅取 Obstimes = 0/1
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(survival)
  library(timeROC)
  library(randomForestSRC)
})
set.seed(123)

## 1) 数据读取与结局合并
rsf_dir <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0314"
rsf_path_candidates <- c(
  file.path(rsf_dir, "widedata_merge.xlsx"),
  file.path(rsf_dir, "0314_widedata_merge.xlsx")
)
rsf_path <- rsf_path_candidates[file.exists(rsf_path_candidates)][1]
if (is.na(rsf_path)) stop("未找到 widedata_merge.xlsx 或 0314_widedata_merge.xlsx")

rsf_raw <- read_xlsx(rsf_path)

base_path <- "F:/文章_大论文/MIMIC数据库_代码/TREA代码/0311/stroke_baseline_simple.xlsx"
if (!file.exists(base_path)) stop(paste0("文件不存在: ", base_path))
base_dat <- read_xlsx(base_path) %>%
  select(hadm_id, hospital_mortality) %>%
  distinct(hadm_id, .keep_all = TRUE) %>%
  rename(hospitalmortality = hospital_mortality)

if ("hadm_id" %in% names(rsf_raw)) {
  rsf_raw <- rsf_raw %>%
    mutate(hadm_id = as.character(hadm_id)) %>%
    left_join(base_dat %>% mutate(hadm_id = as.character(hadm_id)), by = "hadm_id")
}

## 2) 调整结构：仅用 Obstimes=0/1，且每患者保留一条（优先 Obstimes=1）
id_col <- if ("subject_id" %in% names(rsf_raw)) "subject_id" else if ("subjectid" %in% names(rsf_raw)) "subjectid" else stop("未找到 subject_id/subjectid")
if (!("Obstimes" %in% names(rsf_raw))) stop("未找到 Obstimes 列")
if (!("itemid_los_hosp_days" %in% names(rsf_raw))) stop("未找到 itemid_los_hosp_days（生存时间）")
if (!("hospitalmortality" %in% names(rsf_raw))) stop("未找到 hospitalmortality（事件）")

rsf_raw$Obstimes <- suppressWarnings(as.numeric(rsf_raw$Obstimes))
rsf_raw$itemid_los_hosp_days <- suppressWarnings(as.numeric(rsf_raw$itemid_los_hosp_days))
rsf_raw$hospitalmortality <- suppressWarnings(as.numeric(rsf_raw$hospitalmortality))

rsf_data0 <- rsf_raw %>%
  filter(Obstimes %in% c(0, 1)) %>%
  mutate(.id = as.character(.data[[id_col]])) %>%
  arrange(.id, desc(Obstimes)) %>%
  distinct(.id, .keep_all = TRUE) %>%
  mutate(
    ID = as.numeric(as.factor(.id)),
    obs_time = itemid_los_hosp_days,
    event = ifelse(hospitalmortality > 0, 1, 0)
  ) %>%
  filter(is.finite(obs_time), !is.na(obs_time), obs_time > 0, !is.na(event))

## 候选预测变量：排除ID/时间/事件/明显标识字段
exclude_vars <- c(
  ".id", "ID", id_col, "subject_id", "subjectid", "hadm_id", "stay_id",
  "charttime", "Obstimes", "obs_time", "event", "hospitalmortality",
  "itemid_los_hosp_days", "itemid_hadm_id_stroke", "itemid_hadm_id_icu"
)
pred_vars <- setdiff(names(rsf_data0), exclude_vars)

## 仅保留有信息的变量
keep_pred <- pred_vars[vapply(pred_vars, function(v) {
  x <- rsf_data0[[v]]
  sum(!is.na(x)) > 0 && length(unique(x[!is.na(x)])) > 1
}, logical(1))]
if (length(keep_pred) == 0L) stop("无可用预测变量，请检查数据。")

## 字符变量转因子；高基数分类压缩到前15类+Others
for (v in keep_pred) {
  x <- rsf_data0[[v]]
  if (is.character(x)) {
    x <- trimws(x)
    x[x == ""] <- NA_character_
    lv <- names(sort(table(x), decreasing = TRUE))
    if (length(lv) > 15L) {
      keep <- lv[1:14]
      x <- ifelse(is.na(x), NA_character_, ifelse(x %in% keep, x, "Others"))
    }
    rsf_data0[[v]] <- as.factor(x)
  }
}

## 3) 模型构建 + 结果计算（AUC/CINDEX/BS）
cal_3_RSF_mimic <- function(data, t0) {
  rsf_df <- data[, c(keep_pred, "obs_time", "event"), drop = FALSE]
  surv_time <- rsf_df$obs_time
  surv_status <- rsf_df$event

  mtry_val <- max(1, floor(length(keep_pred) / 3))
  fit <- rfsrc(
    Surv(obs_time, event) ~ .,
    data = rsf_df,
    ntree = 1000,
    mtry = mtry_val,
    nodesize = 10,
    importance = TRUE,
    proximity = FALSE,
    seed = 123
  )

  pred <- predict(fit, newdata = rsf_df)
  t_idx <- which.min(abs(pred$time.interest - t0))
  survival_probs <- pred$survival[, t_idx]
  risk_marker <- 1 - survival_probs

  roc_obj1 <- timeROC(T = surv_time, delta = surv_status, marker = risk_marker, cause = 1, times = t0)
  roc_obj2 <- timeROC(T = surv_time, delta = surv_status, marker = -risk_marker, cause = 1, times = t0)
  auc1 <- if (length(roc_obj1$AUC) >= 2) roc_obj1$AUC[2] else roc_obj1$AUC[1]
  auc2 <- if (length(roc_obj2$AUC) >= 2) roc_obj2$AUC[2] else roc_obj2$AUC[1]
  AUC <- max(as.numeric(auc1), as.numeric(auc2), na.rm = TRUE)

  CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  if (!is.na(CINDEX) && CINDEX < 0.5) {
    risk_marker <- -risk_marker
    CINDEX <- as.numeric(survival::concordance(survival::Surv(surv_time, surv_status) ~ risk_marker)$concordance)
  }

  Y_obs <- as.numeric(surv_time > t0 | (surv_time <= t0 & surv_status == 0))
  censoring_model <- survfit(Surv(obs_time, 1 - event) ~ 1, data = rsf_df)
  get_weights <- function(time, event, censoring_model, t0) {
    cens_probs <- summary(censoring_model, times = pmin(time, t0))$surv
    if (length(cens_probs) != length(time)) cens_probs <- rep(summary(censoring_model, times = t0)$surv, length(time))
    if (is.null(cens_probs) || length(cens_probs) == 0) cens_probs <- rep(1, length(time))
    ifelse(time <= t0 & event == 1, 1 / pmax(cens_probs, 1e-6),
           ifelse(time > t0, 1 / pmax(summary(censoring_model, times = t0)$surv, 1e-6), 0))
  }
  weights <- get_weights(surv_time, surv_status, censoring_model, t0)
  BS <- mean(weights * (survival_probs - Y_obs)^2, na.rm = TRUE)

  list(
    fit = fit,
    metrics = data.frame(AUC = round(AUC, 4), CINDEX = round(CINDEX, 4), BS = round(BS, 4))
  )
}

t0_rsf <- median(rsf_data0$obs_time, na.rm = TRUE)
rsf_res <- cal_3_RSF_mimic(rsf_data0, t0 = t0_rsf)
rsf_metrics <- rsf_res$metrics

rsf_structure <- list(
  input_file = rsf_path,
  n_rows_raw = nrow(rsf_raw),
  n_rows_model = nrow(rsf_data0),
  n_patients = length(unique(rsf_data0$ID)),
  n_predictors = length(keep_pred),
  t0 = t0_rsf
)

cat("\nRSF建模完成（Obstimes=0/1，单条记录/患者）。\n")
print(rsf_metrics)
cat("\nRSF输入结构摘要：\n")
cat("- 输入文件: ", rsf_structure$input_file, "\n", sep = "")
cat("- 原始行数: ", rsf_structure$n_rows_raw, "\n", sep = "")
cat("- 建模行数: ", rsf_structure$n_rows_model, "\n", sep = "")
cat("- 患者数: ", rsf_structure$n_patients, "\n", sep = "")
cat("- 预测变量数: ", rsf_structure$n_predictors, "\n", sep = "")
cat("- t0: ", round(rsf_structure$t0, 6), "\n", sep = "")









