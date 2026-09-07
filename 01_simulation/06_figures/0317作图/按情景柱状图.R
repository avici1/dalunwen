library(readxl)
library(ggplot2)

input_files <- c(
  "f:/文章_大论文/结果/创立表格代码/0317/总_结果3(长数据).xlsx",
  "f:/文章_大论文/结果/创立表格代码/0317/总_结果4_长数据_3C.xlsx"
)

output_dir <- "f:/文章_大论文/结果/创立表格代码/0317作图"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

extract_first_number <- function(x) {
  x <- as.character(x)
  m <- regexpr("-?[0-9]+\\.?[0-9]*", x)
  out <- ifelse(m > 0, regmatches(x, m), NA_character_)
  as.numeric(out)
}

fill_down <- function(v) {
  for (i in seq_along(v)) {
    if (i > 1 && (is.na(v[i]) || trimws(as.character(v[i])) == "")) {
      v[i] <- v[i - 1]
    }
  }
  v
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

normalize_varnum <- function(x) {
  x <- trimws(as.character(x))
  if (x == "6+4") return("10V")
  if (x == "2+2") return("4V")
  toupper(x)
}

normalize_collinearity <- function(x) {
  x <- trimws(as.character(x))
  if (x == "高影响变量") return("INTER")
  if (x == "高影响+噪声") return("BTW")
  if (x == "无") return("NONE")
  toupper(x)
}

normalize_class <- function(x) {
  x <- trimws(as.character(x))
  if (x == "" || is.na(x)) return(NA_character_)
  x <- gsub("\\.0+$", "", x)
  paste0(x, "C")
}

format_int_like <- function(x) {
  x <- as.character(x)
  gsub("\\.0+$", "", x)
}

find_col <- function(nms, patterns) {
  idx <- integer(0)
  for (p in patterns) {
    got <- grep(p, nms, ignore.case = TRUE)
    if (length(got) > 0) {
      idx <- c(idx, got[1])
      break
    }
  }
  if (length(idx) == 0) return(NA_integer_)
  idx[1]
}

plot_one_file <- function(file_path) {
  df <- as.data.frame(read_excel(file_path), stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(0L)

  # Remove completely empty rows
  keep_row <- rowSums(!is.na(df) & trimws(as.character(as.matrix(df))) != "") > 0
  df <- df[keep_row, , drop = FALSE]

  nms <- names(df)
  scenario_idx <- find_col(nms, c("情景", "^X1$", "^\\.\\.\\.1$"))
  sample_idx <- find_col(nms, c("样本量"))
  censor_idx <- find_col(nms, c("删失率"))
  varnum_idx <- find_col(nms, c("变量数"))
  corr_idx <- find_col(nms, c("相关性"))
  collin_idx <- find_col(nms, c("共线特性"))
  class_idx <- find_col(nms, c("潜类别"))
  model_idx <- find_col(nms, c("模型", "^X8$", "^\\.\\.\\.8$"))
  auc_idx <- find_col(nms, c("^AUC", "AUC\\(95%CI\\)"))
  bs_idx <- find_col(nms, c("^BS", "BRIRE", "BRIER"))
  c_idx <- find_col(nms, c("^C-?INDEX", "CINDEX"))

  # Fallback by position for common long-table layout
  if (is.na(scenario_idx) && ncol(df) >= 1) scenario_idx <- 1
  if (is.na(sample_idx) && ncol(df) >= 2) sample_idx <- 2
  if (is.na(censor_idx) && ncol(df) >= 3) censor_idx <- 3
  if (is.na(varnum_idx) && ncol(df) >= 4) varnum_idx <- 4
  if (is.na(corr_idx) && ncol(df) >= 5) corr_idx <- 5
  if (is.na(collin_idx) && ncol(df) >= 6) collin_idx <- 6
  if (is.na(class_idx) && ncol(df) >= 7) class_idx <- 7
  if (is.na(model_idx) && ncol(df) >= 8) model_idx <- 8
  if (is.na(auc_idx) && ncol(df) >= 9) auc_idx <- 9
  if (is.na(bs_idx) && ncol(df) >= 10) bs_idx <- 10
  if (is.na(c_idx) && ncol(df) >= 11) c_idx <- 11

  use_idx <- c(
    scenario_idx, sample_idx, censor_idx, varnum_idx, corr_idx, collin_idx, class_idx,
    model_idx, auc_idx, bs_idx, c_idx
  )
  if (any(is.na(use_idx))) {
    stop(paste("无法识别关键列：", basename(file_path)))
  }

  d <- df[, use_idx, drop = FALSE]
  names(d) <- c("情景", "样本量", "删失率", "变量数", "相关性", "共线特性", "潜类别", "模型", "AUC_raw", "BS_raw", "CINDEX_raw")

  # Fill down merged-cell blanks
  d$情景 <- fill_down(d$情景)
  d$样本量 <- fill_down(d$样本量)
  d$删失率 <- fill_down(d$删失率)
  d$变量数 <- fill_down(d$变量数)
  d$相关性 <- fill_down(d$相关性)
  d$共线特性 <- fill_down(d$共线特性)
  d$潜类别 <- fill_down(d$潜类别)

  d$模型 <- normalize_model(d$模型)
  d <- d[d$模型 %in% c("COX", "RSF", "JM", "RSF_LC"), , drop = FALSE]
  d <- d[!is.na(d$情景) & trimws(d$情景) != "", , drop = FALSE]

  d$AUC <- extract_first_number(d$AUC_raw)
  d$BS <- extract_first_number(d$BS_raw)
  d$CINDEX <- extract_first_number(d$CINDEX_raw)

  d <- d[!is.na(d$AUC) & !is.na(d$BS) & !is.na(d$CINDEX), , drop = FALSE]
  if (nrow(d) == 0) return(0L)

  d$模型 <- factor(d$模型, levels = c("COX", "RSF", "JM", "RSF_LC"))
  scenarios <- unique(d$情景)
  file_tag <- tools::file_path_sans_ext(basename(file_path))

  count <- 0L
  for (sc in scenarios) {
    one <- d[d$情景 == sc, , drop = FALSE]
    if (nrow(one) == 0) next
    sample_txt <- format_int_like(one$样本量[1])
    censor_txt <- format_int_like(one$删失率[1])
    var_txt <- normalize_varnum(one$变量数[1])
    corr_txt <- normalize_corr(one$相关性[1])
    collin_txt <- normalize_collinearity(one$共线特性[1])
    class_txt <- normalize_class(one$潜类别[1])
    title_txt <- paste0(
      sc, "-", sample_txt, "样本_", censor_txt, "删失_",
      var_txt, "_", corr_txt, "_", collin_txt, "_", class_txt
    )

    long <- rbind(
      data.frame(模型 = one$模型, 指标 = "AUC", 数值 = one$AUC),
      data.frame(模型 = one$模型, 指标 = "BS", 数值 = one$BS),
      data.frame(模型 = one$模型, 指标 = "CINDEX", 数值 = one$CINDEX)
    )
    long$指标 <- factor(long$指标, levels = c("AUC", "BS", "CINDEX"))

    p <- ggplot(long, aes(x = 指标, y = 数值, fill = 模型)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = sprintf("%.3f", 数值)),
        position = position_dodge(width = 0.8),
        vjust = -0.25,
        size = 3
      ) +
      labs(
        title = title_txt,
        x = "模型性能指标",
        y = "数值"
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "top"
      ) +
      expand_limits(y = max(long$数值, na.rm = TRUE) * 1.10)

    out_png <- file.path(output_dir, paste0(file_tag, "_", sc, ".png"))
    ggsave(out_png, p, width = 8, height = 5, dpi = 300)
    count <- count + 1L
  }
  count
}

total <- 0L
for (f in input_files) {
  n <- plot_one_file(f)
  message("已生成图数量（", basename(f), "）: ", n)
  total <- total + n
}
message("总图数量: ", total)
