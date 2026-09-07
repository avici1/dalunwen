library(readxl)
library(ggplot2)

input_1c <- "f:/文章_大论文/结果/创立表格代码/0317/总_结果3(长数据).xlsx"
input_3c <- "f:/文章_大论文/结果/创立表格代码/0317/总_结果4_长数据_3C.xlsx"
out_dir_data <- "f:/文章_大论文/结果/创立表格代码/0317"
out_dir_fig <- "f:/文章_大论文/结果/创立表格代码/0317作图"
dir.create(out_dir_fig, recursive = TRUE, showWarnings = FALSE)

fill_down <- function(v) {
  for (i in seq_along(v)) {
    if (i > 1 && (is.na(v[i]) || trimws(as.character(v[i])) == "")) {
      v[i] <- v[i - 1]
    }
  }
  v
}

extract_first_number <- function(x) {
  x <- as.character(x)
  m <- regexpr("-?[0-9]+\\.?[0-9]*", x)
  out <- ifelse(m > 0, regmatches(x, m), NA_character_)
  as.numeric(out)
}

normalize_model <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("-", "_", x)
  x <- gsub("\\s+", "", x)
  x[x == "RSFLC"] <- "RSF_LC"
  x
}

normalize_corr <- function(x) {
  x <- trimws(as.character(x))
  if (x %in% c("低", "low", "LOW")) return("low")
  if (x %in% c("中", "mid", "MID")) return("mid")
  if (x %in% c("高", "high", "HIGH")) return("high")
  tolower(x)
}

normalize_collinearity <- function(x) {
  x <- trimws(as.character(x))
  if (x %in% c("高影响变量", "INTER")) return("变量间")
  if (x %in% c("高影响+噪声", "BTW")) return("变量噪声")
  if (x == "无") return("无")
  x
}

find_col <- function(nms, patterns) {
  for (p in patterns) {
    got <- grep(p, nms, ignore.case = TRUE)
    if (length(got) > 0) return(got[1])
  }
  NA_integer_
}

read_metric_table <- function(file_path, class_fallback = NA_integer_) {
  df <- as.data.frame(read_excel(file_path), stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(data.frame())

  keep_row <- rowSums(!is.na(df) & trimws(as.character(as.matrix(df))) != "") > 0
  df <- df[keep_row, , drop = FALSE]
  nms <- names(df)

  idx_scene <- find_col(nms, c("情景", "^X1$", "^\\.\\.\\.1$"))
  idx_sample <- find_col(nms, c("样本量"))
  idx_censor <- find_col(nms, c("删失率"))
  idx_corr <- find_col(nms, c("相关性"))
  idx_col <- find_col(nms, c("共线特性"))
  idx_class <- find_col(nms, c("潜类别"))
  idx_model <- find_col(nms, c("模型", "^X8$", "^\\.\\.\\.8$"))
  idx_auc <- find_col(nms, c("^AUC", "AUC\\(95%CI\\)"))
  idx_bs <- find_col(nms, c("^BS", "BRIRE", "BRIER"))
  idx_c <- find_col(nms, c("^C-?INDEX", "CINDEX"))

  if (is.na(idx_scene) && ncol(df) >= 1) idx_scene <- 1
  if (is.na(idx_sample) && ncol(df) >= 2) idx_sample <- 2
  if (is.na(idx_censor) && ncol(df) >= 3) idx_censor <- 3
  if (is.na(idx_corr) && ncol(df) >= 5) idx_corr <- 5
  if (is.na(idx_col) && ncol(df) >= 6) idx_col <- 6
  if (is.na(idx_class) && ncol(df) >= 7) idx_class <- 7
  if (is.na(idx_model) && ncol(df) >= 8) idx_model <- 8
  if (is.na(idx_auc) && ncol(df) >= 9) idx_auc <- 9
  if (is.na(idx_bs) && ncol(df) >= 10) idx_bs <- 10
  if (is.na(idx_c) && ncol(df) >= 11) idx_c <- 11

  use_idx <- c(idx_scene, idx_sample, idx_censor, idx_corr, idx_col, idx_class, idx_model, idx_auc, idx_bs, idx_c)
  if (any(is.na(use_idx))) stop(paste("无法识别关键列:", basename(file_path)))

  d <- df[, use_idx, drop = FALSE]
  names(d) <- c("情景", "样本量_raw", "删失率_raw", "相关性_raw", "共线特性_raw", "潜类别_raw", "模型", "AUC_raw", "BS_raw", "CINDEX_raw")

  d$情景 <- fill_down(d$情景)
  d$样本量_raw <- fill_down(d$样本量_raw)
  d$删失率_raw <- fill_down(d$删失率_raw)
  d$相关性_raw <- fill_down(d$相关性_raw)
  d$共线特性_raw <- fill_down(d$共线特性_raw)
  d$潜类别_raw <- fill_down(d$潜类别_raw)

  d$样本量 <- as.integer(extract_first_number(d$样本量_raw))
  d$删失率 <- as.integer(extract_first_number(d$删失率_raw))
  d$相关性 <- vapply(d$相关性_raw, normalize_corr, FUN.VALUE = character(1))
  d$共线特性 <- vapply(d$共线特性_raw, normalize_collinearity, FUN.VALUE = character(1))
  d$模型 <- normalize_model(d$模型)

  d$AUC <- extract_first_number(d$AUC_raw)
  d$BS <- extract_first_number(d$BS_raw)
  d$CINDEX <- extract_first_number(d$CINDEX_raw)

  if (!is.na(class_fallback)) {
    d$潜类别 <- class_fallback
  } else {
    d$潜类别 <- as.integer(extract_first_number(d$潜类别_raw))
  }

  d <- d[d$模型 %in% c("COX", "RSF", "JM", "RSF_LC"), , drop = FALSE]
  d <- d[!is.na(d$情景) & trimws(d$情景) != "", , drop = FALSE]
  d <- d[!is.na(d$样本量) & !is.na(d$删失率), , drop = FALSE]
  d <- d[!is.na(d$AUC) & !is.na(d$BS) & !is.na(d$CINDEX), , drop = FALSE]

  d[, c("情景", "样本量", "删失率", "相关性", "共线特性", "潜类别", "模型", "AUC", "BS", "CINDEX")]
}

to_long_metric <- function(df_avg, group_col) {
  rbind(
    data.frame(组别 = df_avg[[group_col]], 模型 = df_avg$模型, 指标 = "AUC", 均值 = df_avg$AUC),
    data.frame(组别 = df_avg[[group_col]], 模型 = df_avg$模型, 指标 = "BS", 均值 = df_avg$BS),
    data.frame(组别 = df_avg[[group_col]], 模型 = df_avg$模型, 指标 = "CINDEX", 均值 = df_avg$CINDEX)
  )
}

write_xlsx_multi <- function(path, sheets) {
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(sheets, path)
    return(invisible(TRUE))
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (nm in names(sheets)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, sheets[[nm]])
    }
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    return(invisible(TRUE))
  }
  stop("未检测到 writexl/openxlsx 包，无法写入 xlsx。")
}

plot_compare <- function(long_df, title_txt, out_png) {
  long_df$模型 <- factor(long_df$模型, levels = c("COX", "RSF", "JM", "RSF_LC"))
  long_df$指标 <- factor(long_df$指标, levels = c("AUC", "BS", "CINDEX"))

  p <- ggplot(long_df, aes(x = 模型, y = 均值, fill = 组别)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(
      aes(label = sprintf("%.3f", 均值)),
      position = position_dodge(width = 0.75),
      vjust = -0.25,
      size = 3.4
    ) +
    facet_wrap(~指标, nrow = 1, scales = "free_y") +
    labs(title = title_txt, x = "模型", y = "均值", fill = NULL) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
    expand_limits(y = max(long_df$均值, na.rm = TRUE) * 1.12)

  ggsave(out_png, p, width = 12, height = 4.8, dpi = 320)
}

do_compare <- function(all_data, group_col, keep_levels, out_xlsx, out_png, title_txt) {
  d <- all_data[all_data[[group_col]] %in% keep_levels, , drop = FALSE]

  by_scene <- aggregate(
    d[, c("AUC", "BS", "CINDEX")],
    by = list(组别 = d[[group_col]], 模型 = d$模型, 情景 = d$情景),
    FUN = mean,
    na.rm = TRUE
  )

  by_group <- aggregate(
    by_scene[, c("AUC", "BS", "CINDEX")],
    by = list(组别 = by_scene$组别, 模型 = by_scene$模型),
    FUN = mean,
    na.rm = TRUE
  )

  by_group$组别 <- factor(by_group$组别, levels = keep_levels)
  by_group <- by_group[order(by_group$组别, by_group$模型), ]
  by_group$组别 <- as.character(by_group$组别)

  long_summary <- to_long_metric(by_group, "组别")
  long_summary$组别 <- factor(long_summary$组别, levels = keep_levels)
  long_summary <- long_summary[order(long_summary$组别, long_summary$模型), ]

  write_xlsx_multi(
    out_xlsx,
    sheets = list(
      "原始清洗后数据" = d,
      "情景均值" = by_scene,
      "分组模型均值_宽表" = by_group,
      "分组模型均值_长表" = long_summary
    )
  )
  plot_compare(long_summary, title_txt, out_png)
}

data_1c <- read_metric_table(input_1c, class_fallback = 1L)
data_3c <- read_metric_table(input_3c, class_fallback = 3L)
all_data <- rbind(data_1c, data_3c)

do_compare(
  all_data = all_data,
  group_col = "样本量",
  keep_levels = c(500, 1000),
  out_xlsx = file.path(out_dir_data, "总_500样本 vs 1000样本.xlsx"),
  out_png = file.path(out_dir_fig, "总_500样本_vs_1000样本.png"),
  title_txt = "500样本 vs 1000样本：四模型均值对比"
)

do_compare(
  all_data = all_data,
  group_col = "删失率",
  keep_levels = c(30, 70),
  out_xlsx = file.path(out_dir_data, "总_30删失 vs 70删失.xlsx"),
  out_png = file.path(out_dir_fig, "总_30删失_vs_70删失.png"),
  title_txt = "30删失 vs 70删失：四模型均值对比"
)

# 相关性三组单独比较（low/mid/high）
do_compare(
  all_data = all_data,
  group_col = "相关性",
  keep_levels = c("low", "mid", "high"),
  out_xlsx = file.path(out_dir_data, "总_相关性 比较.xlsx"),
  out_png = file.path(out_dir_fig, "总_相关性_比较.png"),
  title_txt = "相关性(low/mid/high)：四模型均值对比"
)

do_compare(
  all_data = all_data,
  group_col = "共线特性",
  keep_levels = c("变量间", "变量噪声"),
  out_xlsx = file.path(out_dir_data, "总_变量间 vs 变量噪声.xlsx"),
  out_png = file.path(out_dir_fig, "总_变量间_vs_变量噪声.png"),
  title_txt = "变量间 vs 变量噪声：四模型均值对比"
)

message("已完成4类分层比较：xlsx与png均已输出。")
