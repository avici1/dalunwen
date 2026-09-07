library(readxl)
library(ggplot2)

input_1c <- "f:/文章_大论文/结果/创立表格代码/0317/总_结果3(长数据).xlsx"
input_3c <- "f:/文章_大论文/结果/创立表格代码/0317/总_结果4_长数据_3C.xlsx"
output_xlsx <- "f:/文章_大论文/结果/创立表格代码/0317/总_1C vs 3C.xlsx"
output_png <- "f:/文章_大论文/结果/创立表格代码/0317作图/总_1C_vs_3C_四模型对比.png"

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
  idx_class <- find_col(nms, c("潜类别"))
  idx_model <- find_col(nms, c("模型", "^X8$", "^\\.\\.\\.8$"))
  idx_auc <- find_col(nms, c("^AUC", "AUC\\(95%CI\\)"))
  idx_bs <- find_col(nms, c("^BS", "BRIRE", "BRIER"))
  idx_c <- find_col(nms, c("^C-?INDEX", "CINDEX"))

  if (is.na(idx_scene) && ncol(df) >= 1) idx_scene <- 1
  if (is.na(idx_class) && ncol(df) >= 7) idx_class <- 7
  if (is.na(idx_model) && ncol(df) >= 8) idx_model <- 8
  if (is.na(idx_auc) && ncol(df) >= 9) idx_auc <- 9
  if (is.na(idx_bs) && ncol(df) >= 10) idx_bs <- 10
  if (is.na(idx_c) && ncol(df) >= 11) idx_c <- 11

  use_idx <- c(idx_scene, idx_class, idx_model, idx_auc, idx_bs, idx_c)
  if (any(is.na(use_idx))) stop(paste("无法识别关键列:", basename(file_path)))

  d <- df[, use_idx, drop = FALSE]
  names(d) <- c("情景", "潜类别_raw", "模型", "AUC_raw", "BS_raw", "CINDEX_raw")

  d$情景 <- fill_down(d$情景)
  d$潜类别_raw <- fill_down(d$潜类别_raw)
  d$模型 <- normalize_model(d$模型)

  d <- d[d$模型 %in% c("COX", "RSF", "JM", "RSF_LC"), , drop = FALSE]
  d <- d[!is.na(d$情景) & trimws(d$情景) != "", , drop = FALSE]

  d$AUC <- extract_first_number(d$AUC_raw)
  d$BS <- extract_first_number(d$BS_raw)
  d$CINDEX <- extract_first_number(d$CINDEX_raw)

  if (!is.na(class_fallback)) {
    d$潜类别 <- class_fallback
  } else {
    d$潜类别 <- extract_first_number(d$潜类别_raw)
  }

  d <- d[!is.na(d$AUC) & !is.na(d$BS) & !is.na(d$CINDEX) & !is.na(d$潜类别), , drop = FALSE]
  d$潜类别 <- as.integer(d$潜类别)

  d[, c("情景", "潜类别", "模型", "AUC", "BS", "CINDEX")]
}

to_long_metric <- function(df_avg) {
  rbind(
    data.frame(潜类别 = df_avg$潜类别, 模型 = df_avg$模型, 指标 = "AUC", 均值 = df_avg$AUC),
    data.frame(潜类别 = df_avg$潜类别, 模型 = df_avg$模型, 指标 = "BS", 均值 = df_avg$BS),
    data.frame(潜类别 = df_avg$潜类别, 模型 = df_avg$模型, 指标 = "CINDEX", 均值 = df_avg$CINDEX)
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

data_1c <- read_metric_table(input_1c, class_fallback = 1L)
data_3c <- read_metric_table(input_3c, class_fallback = 3L)
all_data <- rbind(data_1c, data_3c)

# 防止同一情景-模型重复，先按情景粒度求均值
by_scene <- aggregate(cbind(AUC, BS, CINDEX) ~ 潜类别 + 模型 + 情景, data = all_data, FUN = mean, na.rm = TRUE)
# 再按潜类别+模型求“所有情景均值”
by_class_model <- aggregate(cbind(AUC, BS, CINDEX) ~ 潜类别 + 模型, data = by_scene, FUN = mean, na.rm = TRUE)

long_summary <- to_long_metric(by_class_model)
long_summary$潜类别 <- ifelse(long_summary$潜类别 == 1, "1C", "3C")
long_summary$模型 <- factor(long_summary$模型, levels = c("COX", "RSF", "JM", "RSF_LC"))
long_summary$指标 <- factor(long_summary$指标, levels = c("AUC", "BS", "CINDEX"))

wide_summary <- by_class_model
wide_summary$潜类别 <- ifelse(wide_summary$潜类别 == 1, "1C", "3C")

write_xlsx_multi(
  output_xlsx,
  sheets = list(
    "原始清洗后数据" = all_data,
    "情景均值" = by_scene,
    "潜类别模型均值_宽表" = wide_summary,
    "潜类别模型均值_长表" = long_summary
  )
)

p <- ggplot(long_summary, aes(x = 模型, y = 均值, fill = 潜类别)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = sprintf("%.3f", 均值)),
    position = position_dodge(width = 0.75),
    vjust = -0.25,
    size = 3.5
  ) +
  facet_wrap(~指标, nrow = 1, scales = "free_y") +
  labs(
    title = "所有1C vs 所有3C：四模型性能均值对比",
    x = "模型",
    y = "均值",
    fill = "潜类别"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  ) +
  expand_limits(y = max(long_summary$均值, na.rm = TRUE) * 1.12)

ggsave(output_png, p, width = 12, height = 4.8, dpi = 320)

message("已输出Excel: ", output_xlsx)
message("已输出图像: ", output_png)
