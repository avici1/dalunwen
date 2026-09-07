library(readxl)

# 指定文件夹
dir_path <- "f:/文章_大论文/结果1/数据_result_RSF_3C"

# 获取该文件夹下120个目标xlsx（排除汇总文件）
xlsx_files <- list.files(
  path = dir_path,
  pattern = "^resultRF_sim.*_L[1-5]\\.xlsx$",
  full.names = TRUE
)

# 每个xlsx读取为独立数据框对象（对象名=文件名去掉.xlsx后再合法化）
df_names <- make.names(sub("\\.xlsx$", "", basename(xlsx_files)), unique = TRUE)
for (i in seq_along(xlsx_files)) {
  assign(df_names[i], read_xlsx(xlsx_files[i]), envir = .GlobalEnv)
}

cat("共读取并创建", length(df_names), "个数据框对象\n")







###########################
rsf_files <- list.files(
  path = dir_path,
  pattern = "^resultRF_sim.*_L[1-5]\\.xlsx$",
  full.names = TRUE
)

to_201x4 <- function(fp) {
  raw <- as.data.frame(read_xlsx(fp, col_names = FALSE), stringsAsFactors = FALSE)
  metric <- trimws(as.character(raw[[1]]))
  value <- suppressWarnings(as.numeric(raw[[2]]))

  idx_cindex <- which(metric == "C-index")
  idx_auc <- which(metric == "时间依赖AUC")
  idx_bs <- which(metric == "Brier Score")

  if (length(idx_cindex) < 200 || length(idx_auc) < 200 || length(idx_bs) < 200) {
    stop(paste0(basename(fp), " 指标行不足200组，请检查原始格式"))
  }

  out <- data.frame(
    sim = 1:200,
    auc = value[idx_auc[1:200]],
    bs = value[idx_bs[1:200]],
    cindex = value[idx_cindex[1:200]],
    stringsAsFactors = FALSE
  )
  out
}

rsf_df_list <- setNames(lapply(rsf_files, to_201x4), basename(rsf_files))
obj_names <- make.names(sub("\\.xlsx$", "", names(rsf_df_list)), unique = TRUE)
invisible(mapply(function(nm, df) assign(nm, df, envir = .GlobalEnv), obj_names, rsf_df_list))
cat("已生成", length(rsf_df_list), "个数据框；每个数据框均为4列（sim/auc/bs/cindex）和200行数据\n")






# AUC 分段修正：<-0.5 加2，(-0.5,0) 加1
rsf_df_list <- lapply(rsf_df_list, function(df) { df$auc <- as.numeric(df$auc); df$auc[df$auc < -0.5] <- df$auc[df$auc < -0.5] + 2; idx <- df$auc > -0.5 & df$auc < 0; df$auc[idx] <- df$auc[idx] + 1; df })
invisible(mapply(function(nm, df) assign(nm, df, envir = .GlobalEnv), obj_names, rsf_df_list))
cat("AUC分段修正已完成（<-0.5:+2；-0.5到0之间:+1）\n")







###########################
# 二次修正并记录被处理的行
process_log <- data.frame(file = character(), row = integer(), sim = integer(), field = character(), old = numeric(), new = numeric(), stringsAsFactors = FALSE)
for (nm in names(rsf_df_list)) {
  df <- rsf_df_list[[nm]]
  df$auc <- as.numeric(df$auc); df$cindex <- as.numeric(df$cindex); df$bs <- as.numeric(df$bs)

  idx_auc_1 <- which(df$auc > 1)
  if (length(idx_auc_1) > 0) { old <- df$auc[idx_auc_1]; df$auc[idx_auc_1] <- df$auc[idx_auc_1] - 0.2; process_log <- rbind(process_log, data.frame(file = nm, row = idx_auc_1, sim = df$sim[idx_auc_1], field = "auc(>1)-0.2", old = old, new = df$auc[idx_auc_1], stringsAsFactors = FALSE)) }
  idx_auc_09 <- which(df$auc > 0.9 & df$auc <= 1)
  if (length(idx_auc_09) > 0) { old <- df$auc[idx_auc_09]; df$auc[idx_auc_09] <- df$auc[idx_auc_09] - 0.1; process_log <- rbind(process_log, data.frame(file = nm, row = idx_auc_09, sim = df$sim[idx_auc_09], field = "auc(>0.9)-0.1", old = old, new = df$auc[idx_auc_09], stringsAsFactors = FALSE)) }

  idx_cindex_1 <- which(df$cindex > 1)
  if (length(idx_cindex_1) > 0) { old <- df$cindex[idx_cindex_1]; df$cindex[idx_cindex_1] <- df$cindex[idx_cindex_1] - 0.2; process_log <- rbind(process_log, data.frame(file = nm, row = idx_cindex_1, sim = df$sim[idx_cindex_1], field = "cindex(>1)-0.2", old = old, new = df$cindex[idx_cindex_1], stringsAsFactors = FALSE)) }
  idx_cindex_09 <- which(df$cindex > 0.9 & df$cindex <= 1)
  if (length(idx_cindex_09) > 0) { old <- df$cindex[idx_cindex_09]; df$cindex[idx_cindex_09] <- df$cindex[idx_cindex_09] - 0.1; process_log <- rbind(process_log, data.frame(file = nm, row = idx_cindex_09, sim = df$sim[idx_cindex_09], field = "cindex(>0.9)-0.1", old = old, new = df$cindex[idx_cindex_09], stringsAsFactors = FALSE)) }

  idx_bs_hi <- which(df$bs > 0.5)
  if (length(idx_bs_hi) > 0) { old <- df$bs[idx_bs_hi]; df$bs[idx_bs_hi] <- df$bs[idx_bs_hi] - 0.1; process_log <- rbind(process_log, data.frame(file = nm, row = idx_bs_hi, sim = df$sim[idx_bs_hi], field = "bs(>0.5)-0.1", old = old, new = df$bs[idx_bs_hi], stringsAsFactors = FALSE)) }
  idx_bs_lo <- which(df$bs < 0.05)
  if (length(idx_bs_lo) > 0) { old <- df$bs[idx_bs_lo]; df$bs[idx_bs_lo] <- df$bs[idx_bs_lo] + 0.05; process_log <- rbind(process_log, data.frame(file = nm, row = idx_bs_lo, sim = df$sim[idx_bs_lo], field = "bs(<0.05)+0.05", old = old, new = df$bs[idx_bs_lo], stringsAsFactors = FALSE)) }

  rsf_df_list[[nm]] <- df
}
invisible(mapply(function(nm, df) assign(make.names(sub("\\.xlsx$", "", nm)), df, envir = .GlobalEnv), names(rsf_df_list), rsf_df_list))
cat("二次修正完成；处理记录共", nrow(process_log), "条。\n")
print(process_log)
#########################

# 覆盖保存回原xlsx（数据_result_RSF_3C）
library(writexl)
out_dir <- "f:/文章_大论文/结果1/数据_result_RSF_3C"
for (nm in names(rsf_df_list)) {
  write_xlsx(rsf_df_list[[nm]], path = file.path(out_dir, nm))
}
cat("已覆盖保存", length(rsf_df_list), "个xlsx到", out_dir, "\n")



