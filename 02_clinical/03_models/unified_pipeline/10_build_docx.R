suppressPackageStartupMessages({
  library(officer)
  library(flextable)
  library(dplyr)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("DOCX_START", "assemble rerun Chapter 5")

read_tab <- function(name) {
  read.csv(file.path(artifact_dir, name), check.names = FALSE, fileEncoding = "UTF-8")
}
fmt_num <- function(x, digits = 3L) {
  ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))
}

static_perf <- read_tab("\u88685-4A_\u9759\u6001\u4efb\u52a1\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6027\u80fd.csv")
dynamic_perf <- read_tab("\u88685-4B_\u52a8\u6001\u4efb\u52a1\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6027\u80fd.csv")
audit <- read_tab("00_cohort_audit.csv")

lookup_audit <- function(key) {
  z <- audit$value[audit$item == key]
  if (!length(z)) NA_character_ else as.character(z[1L])
}

doc <- read_docx()
sec <- prop_section(
  page_size = page_size(orient = "portrait", width = 8.27, height = 11.69),
  page_margins = page_mar(top = 0.75, bottom = 0.75, left = 0.85, right = 0.75)
)

normal_fp <- fp_text(font.family = "SimSun", font.size = 10.5)
caption_fp <- fp_text(font.family = "SimSun", font.size = 9.5)

add_text <- function(doc, text, align = "justify", bold = FALSE, size = 10.5,
                     first_line = 0.29, after = 4) {
  body_add_fpar(doc, fpar(
    ftext(text, fp_text(font.family = "SimSun", font.size = size, bold = bold)),
    fp_p = fp_par(text.align = align, first_line = first_line,
                  padding.bottom = after, line_spacing = 1.25)
  ))
}

add_heading <- function(doc, text, level = 1L) {
  size <- c(16, 14, 12)[pmin(level, 3L)]
  body_add_fpar(doc, fpar(
    ftext(text, fp_text(font.family = "SimHei", font.size = size, bold = TRUE)),
    fp_p = fp_par(text.align = if (level == 1L) "center" else "left",
                  padding.top = if (level == 1L) 12 else 7,
                  padding.bottom = 6)
  ))
}

clean_table <- function(x) {
  x[] <- lapply(x, function(z) {
    if (is.numeric(z) && all(is.na(z) | abs(z - round(z)) < 1e-10)) {
      ifelse(is.na(z), "NA", formatC(z, digits = 0, format = "f"))
    } else if (is.numeric(z)) fmt_num(z) else as.character(z)
  })
  names(x) <- gsub("_", " ", names(x), fixed = TRUE)
  x
}

make_ft <- function(x, font_size = 8.5) {
  x <- clean_table(x)
  ft <- flextable(x)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = "SimSun", part = "all")
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- bold(ft, part = "header")
  ft <- bg(ft, bg = "#D9EAF7", part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, align = "left", part = "body")
  ft <- padding(ft, padding = 2, part = "all")
  ft <- autofit(ft)
  ft <- set_table_properties(ft, layout = "autofit", width = 1,
                             opts_word = list(repeat_headers = TRUE, split = TRUE))
  ft
}

add_table_block <- function(doc, title, x, note = NULL, font_size = 8.5) {
  doc <- body_add_fpar(doc, fpar(
    ftext(title, fp_text(font.family = "SimSun", font.size = 10, bold = TRUE)),
    fp_p = fp_par(text.align = "center", padding.top = 5, padding.bottom = 3,
                  keep_with_next = TRUE)
  ))
  doc <- body_add_flextable(doc, make_ft(x, font_size))
  if (!is.null(note)) doc <- add_text(doc, note, align = "left", size = 8.5, first_line = 0, after = 5)
  doc
}

add_figure <- function(doc, filename, caption, width = 6.5, height = 4.8,
                       page_break = FALSE) {
  if (page_break) doc <- body_add_break(doc)
  path <- file.path(artifact_dir, filename)
  stopifnot(file.exists(path))
  doc <- body_add_img(doc, src = path, width = width, height = height)
  doc <- body_add_fpar(doc, fpar(
    ftext(caption, caption_fp),
    fp_p = fp_par(text.align = "center", padding.top = 2, padding.bottom = 7)
  ))
  doc
}

doc <- add_heading(doc, "\u7b2c\u4e94\u7ae0  \u9759\u6001\u4e0e\u52a8\u6001\u751f\u5b58\u9884\u6d4b\u6a21\u578b\u6bd4\u8f83\u7684\u5b9e\u4f8b\u7814\u7a76", 1)
doc <- add_text(doc, "\u672c\u7ae0\u5728\u5df2\u5b8c\u6210\u7684MIMIC\u5352\u4e2d\u961f\u5217\u7b5b\u9009\u57fa\u7840\u4e0a\uff0c\u4ece\u539f\u59cb\u5206\u6790\u6570\u636e\u91cd\u65b0\u6784\u5efa\u961f\u5217\u3001\u62df\u5408\u6a21\u578b\u5e76\u751f\u6210\u6240\u6709\u56fe\u8868\u3002\u5206\u6790\u4e25\u683c\u9501\u5b9a\u9759\u6001\u6a21\u578b31\u4e2a\u9884\u6d4b\u53d8\u91cf\uff0c\u52a8\u6001\u6a21\u578b3\u4e2a\u7eb5\u5411\u8f68\u8ff9\u52a028\u4e2a\u56fa\u5b9a\u534f\u53d8\u91cf\uff0c\u5e76\u5c06\u65f6\u95f4\u7edf\u4e00\u6807\u51c6\u5316\u4e3au u=t/28\u3002")
doc <- add_text(doc, "\u672c\u6587\u4e2d\u7684\u201c\u9759\u6001\u4efb\u52a1\u201d\u4f7f\u7528\u57fa\u7ebf\u4e0e\u7b2c1\u65e5\u7279\u5f81\u9884\u6d4b\u5165\u9662\u540e28\u65e5\u5168\u56e0\u6b7b\u4ea1\uff1b\u201c\u52a8\u6001\u4efb\u52a1\u201d\u4ee5\u7b2c5\u65e5\u4e3alandmark\uff0c\u4f7f\u7528\u622a\u81f3\u7b2c5\u65e5\u7684\u7eb5\u5411\u5386\u53f2\u9884\u6d4b\u7b2c5\u65e5\u540e\u81f3\u7b2c28\u65e5\u7684\u6761\u4ef6\u6b7b\u4ea1\u98ce\u9669\u3002\u4e24\u7c7b\u4efb\u52a1\u7684\u8d77\u70b9\u3001\u98ce\u9669\u96c6\u4e0e\u6307\u6807\u5b9a\u4e49\u4e0d\u540c\uff0c\u56e0\u6b64\u4e0d\u5c06\u56db\u4e2a\u6a21\u578b\u5f3a\u5236\u7f6e\u4e8e\u5355\u4e00\u6392\u540d\u4e2d\u3002")

doc <- add_heading(doc, "5.1  \u7814\u7a76\u961f\u5217\u4e0e\u6570\u636e\u5904\u7406", 2)
doc <- add_heading(doc, "5.1.1  \u7814\u7a76\u5bf9\u8c61\u7684\u7b5b\u9009", 3)
doc <- add_text(doc, "\u4fdd\u7559\u539f\u7814\u7a76\u4eceMIMIC\u6570\u636e\u5e93\u8bc6\u522b\u5352\u4e2d\u76f8\u5173\u4f4f\u9662\u3001\u9996\u6b21ICU\u4f4f\u9662\u4ee5\u53caICU\u65f6\u957f\u7b5b\u9009\u7684\u4e0a\u534a\u90e8\u5206\u3002\u5728\u5206\u6790\u6570\u636e\u5c42\u9762\uff0c\u4ee5\u7cbe\u786e\u6b7b\u4ea1\u65f6\u95f4\u6784\u902028\u65e5\u751f\u5b58\u7ed3\u5c40\uff0c\u6392\u9664337\u6761\u65f6\u95f4\u65e0\u6548\u7684\u4f4f\u9662\u8bb0\u5f55\uff1a334\u6761\u6b7b\u4ea1\u65f6\u95f4\u7f3a\u5931\u30011\u6761\u4e8b\u4ef6\u65f6\u95f4\u975e\u6b63\u30012\u6761\u4e8b\u4ef6\u65f6\u95f4\u8d85\u8fc728\u65e5\u3002\u5b8c\u6574\u7684\u961f\u5217\u6d41\u7a0b\u5982\u56fe5-1\u6240\u793a\uff0c\u6bcf\u6761\u6392\u9664\u8bb0\u5f55\u5747\u4fdd\u5b58\u4e8e\u53ef\u590d\u73b0\u65e5\u5fd7\u3002")
doc <- add_figure(doc, "\u56fe5-1_\u7814\u7a76\u961f\u5217\u7b5b\u9009\u4e0e\u5206\u6790\u6570\u636e\u6784\u5efa\u6d41\u7a0b.png", "\u56fe5-1  \u7814\u7a76\u961f\u5217\u7b5b\u9009\u4e0e\u5206\u6790\u6570\u636e\u6784\u5efa\u6d41\u7a0b", 6.25, 8.2, TRUE)

doc <- add_heading(doc, "5.1.2  \u961f\u5217\u7279\u5f81", 3)
doc <- add_text(doc, "\u6392\u9664\u7ed3\u5c40\u65f6\u95f4\u65e0\u6548\u8bb0\u5f55\u540e\uff0c\u53ef\u7528\u751f\u5b58\u961f\u5217\u5305\u542b5962\u6b21\u4f4f\u9662\uff085771\u540d\u60a3\u8005\uff09\uff0c\u5176\u4e2d1037\u6b21\u4f4f\u9662\u572828\u65e5\u5185\u6b7b\u4ea1\u3002\u9759\u6001\u5efa\u6a21\u8fd8\u8981\u6c42\u5b58\u5728\u7b2c1\u65e5\u7279\u5f81\uff0c\u6700\u7ec8\u4e3a5839\u6b21\u4f4f\u9662\uff1b\u52a8\u6001\u5efa\u6a21\u5728\u7b2c5\u65e5landmark\u4e4b\u540e\u4ecd\u5904\u4e8e\u98ce\u9669\u96c6\uff0c\u4e14\u6bcf\u4e2a\u8f68\u8ff9\u81f3\u5c11\u67092\u6b21\u89c2\u6d4b\uff0c\u6700\u7ec8\u4e3a5293\u6b21\u4f4f\u9662\u3002")

t1a0 <- read_tab("\u88685-1a_\u7814\u7a76\u961f\u5217\u57fa\u7ebf\u8fde\u7eed\u53d8\u91cf\u7279\u5f81.csv")
t1a <- data.frame(t1a0$variable, t1a0$n, t1a0$mean_sd, fmt_num(t1a0$median, 2),
                  fmt_num(t1a0$q1, 2), fmt_num(t1a0$q3, 2), check.names = FALSE)
names(t1a) <- c("\u53d8\u91cf", "n", "\u5747\u503c\u00b1\u6807\u51c6\u5dee", "\u4e2d\u4f4d\u6570", "P25", "P75")
doc <- add_table_block(doc, "\u88685-1A  \u7814\u7a76\u961f\u5217\u8fde\u7eed\u53d8\u91cf\u7279\u5f81", t1a, "\u6ce8\uff1aICU\u524d\u4f4f\u9662\u65f6\u957f\u7684\u5355\u4f4d\u4e3a\u5c0f\u65f6\uff1b\u5176\u4f59\u53d8\u91cf\u5355\u4f4d\u6309MIMIC\u539f\u59cb\u5b9a\u4e49\u3002", 8.0)
t1b0 <- read_tab("\u88685-1b_\u7814\u7a76\u961f\u5217\u57fa\u7ebf\u5206\u7c7b\u53d8\u91cf\u7279\u5f81.csv")
t1b <- t1b0[, c("section", "variable", "level", "n_percent")]
names(t1b) <- c("\u7c7b\u522b", "\u53d8\u91cf", "\u6c34\u5e73", "n (%)")
doc <- add_table_block(doc, "\u88685-1B  \u7814\u7a76\u961f\u5217\u5206\u7c7b\u53d8\u91cf\u7279\u5f81", t1b, NULL, 8.5)
t20 <- read_tab("\u88685-2_\u7814\u7a76\u961f\u5217\u7eb5\u5411\u6570\u636e\u57fa\u672c\u7279\u5f81.csv")
t2 <- t20[, c("variable", "admissions", "day1_mean_sd", "admission_mean_sd", "worst_mean_sd")]
names(t2) <- c("\u7eb5\u5411\u53d8\u91cf", "\u4f4f\u9662\u6b21\u6570", "\u7b2c1\u65e5 \u5747\u503c\u00b1SD",
               "\u4f4f\u9662\u5747\u503c \u5747\u503c\u00b1SD", "\u6700\u5dee\u503c \u5747\u503c\u00b1SD")
doc <- add_table_block(doc, "\u88685-2  \u7eb5\u5411\u6570\u636e\u7684\u57fa\u672c\u7279\u5f81", t2, "\u6ce8\uff1a\u6700\u5dee\u503c\u65b9\u5411\u4f9d\u53d8\u91cf\u7684\u4e34\u5e8a\u610f\u4e49\u786e\u5b9a\u3002", 7.5)

doc <- add_heading(doc, "5.2  \u5efa\u6a21\u4e0e\u8bc4\u4ef7\u65b9\u6cd5", 2)
doc <- add_heading(doc, "5.2.1  \u65f6\u95f4\u5c3a\u5ea6\u3001\u9884\u6d4b\u7a97\u53e3\u4e0e\u6570\u636e\u5212\u5206", 3)
doc <- add_text(doc, "\u4ee528\u65e5\u4e3a\u7edf\u4e00\u65f6\u95f4\u57fa\u51c6\uff0c\u5b9a\u4e49 u=t/28\uff0c\u56e0\u800c\u7b2c5\u65e5landmark\u4e3a uL=5/28\uff0c\u9884\u6d4b\u7ec8\u70b9\u4e3a u=1\u3002\u8bad\u7ec3\u96c6\u4e0e\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u96c6\u6cbf\u7528\u6570\u636e\u4e2d\u7684group\u5212\u5206\uff0c\u5e76\u6309subject_id\u6838\u67e5\u65e0\u60a3\u8005\u8de8\u96c6\u6cc4\u6f0f\u3002\u540c\u4e00\u60a3\u8005\u591a\u6b21\u4f4f\u9662\u60c5\u5f62\u5728\u7f6e\u4fe1\u533a\u95f4\u4e2d\u7528\u60a3\u8005\u7c07bootstrap\u5904\u7406\u3002")
doc <- add_heading(doc, "5.2.2  \u6a21\u578b\u8bbe\u5b9a", 3)
doc <- add_text(doc, "\u9759\u6001\u4efb\u52a1\u540c\u65f6\u62df\u5408Cox\u6bd4\u4f8b\u98ce\u9669\u6a21\u578b\u4e0e\u968f\u673a\u751f\u5b58\u68ee\u6797\uff08RSF\uff09\uff0c\u5747\u4f7f\u7528\u9501\u5b9a\u768431\u4e2a\u9884\u6d4b\u53d8\u91cf\u3002\u52a8\u6001\u4efb\u52a1\u6bd4\u8f83\u8054\u5408\u6a21\u578b\uff08JM\uff09\u4e0e\u7eb5\u5411\u534f\u53d8\u91cf\u968f\u673a\u751f\u5b58\u68ee\u6797\uff08RSFLC\uff09\u3002\u4e24\u8005\u90fd\u4f7f\u7528GCS\u3001SOFA\u603b\u5206\u548cCNS\u8bc4\u52063\u6761\u8f68\u8ff9\uff0c\u53e6\u7eb3\u516528\u4e2a\u56fa\u5b9a\u534f\u53d8\u91cf\u3002\u7eb5\u5411\u56fa\u5b9a\u6548\u5e94\u7edf\u4e00\u4e3a Y~time\uff0c\u4e0d\u5f15\u5165\u66f2\u7ebf\u6216\u66f4\u9ad8\u9636\u9879\u3002")
doc <- add_text(doc, "JM\u7684\u7eb5\u5411\u5b50\u6a21\u578b\u91c7\u7528\u968f\u673a\u622a\u8ddd\u3002DynForest 1.3.2\u5728RSFLC\u4e2d\u5bf9random=~1\u89e6\u53d1\u5305\u5185\u90e8\u9519\u8bef\uff0c\u56e0\u6b64\u4f9d\u7167\u9884\u5148\u5141\u8bb8\u7684\u7b80\u5316\u7ed3\u6784\u6539\u4e3arandom=~time\u3002\u8be5time\u5217\u7684\u5b9e\u9645\u6570\u503c\u4ecd\u4e3a u=t/28\uff0c\u56fa\u5b9a\u6548\u5e94\u4fdd\u6301Y~time\u4e0d\u53d8\u3002")
doc <- add_heading(doc, "5.2.3  \u8d85\u53c2\u6570\u4e0e\u8bc4\u4ef7\u6307\u6807", 3)
doc <- add_text(doc, "RSF\u4e0eRSFLC\u7684\u6700\u7ec8\u8d85\u53c2\u6570\u6cbf\u7528\u5f53\u524d\u6587\u7ae0\u4e2d\u5df2\u9009\u5b9a\u7684\u7ec4\u5408\u3002\u4e94\u6298\u4ea4\u53c9\u9a8c\u8bc1\u7f51\u683c\u641c\u7d22\u811a\u672c\u5b8c\u6574\u4fdd\u7559\uff0c\u4f46\u672c\u6b21\u4e0d\u91cd\u590d\u8fd0\u884c\u9ad8\u8017\u65f6\u641c\u7d22\u3002\u9a8c\u8bc1\u96c6\u62a5\u544aHarrell C-index\u3001\u65f6\u95f4\u4f9d\u8d56AUC\u3001IPCW Brier\u8bc4\u5206\u3001\u79ef\u5206Brier\u8bc4\u5206\uff08IBS\uff09\u3001\u6821\u51c6\u66f2\u7ebf\u4e0e\u51b3\u7b56\u66f2\u7ebf\u3002")
hp0 <- bind_rows(read_tab("\u88685-3A_\u9759\u6001\u4efb\u52a1RSF\u8d85\u53c2\u6570\u7ec4\u5408.csv"),
                 read_tab("\u88685-3B_\u52a8\u6001\u4efb\u52a1RSFLC\u8d85\u53c2\u6570\u7ec4\u5408.csv"))
hp <- hp0[, c("model", "search_space", "selected_parameters", "fivefold_C_index_mean_sd",
              "fivefold_AUC_mean_sd", "fivefold_IPCW_Brier_mean_sd")]
names(hp) <- c("\u6a21\u578b", "\u641c\u7d22\u7a7a\u95f4", "\u6700\u7ec8\u53c2\u6570", "5\u6298C-index", "5\u6298AUC", "5\u6298Brier")
doc <- add_table_block(doc, "\u88685-3  RSF\u4e0eRSFLC\u7684\u8d85\u53c2\u6570\u7ec4\u5408", hp,
                       "\u6ce8\uff1a\u4e94\u6298\u6307\u6807\u4e3a\u5f53\u524d\u6587\u7ae0\u5df2\u6709\u9009\u53c2\u7ed3\u679c\uff1b\u672c\u6b21\u4f7f\u7528\u5176\u9009\u5b9a\u53c2\u6570\u91cd\u65b0\u62df\u5408\u6a21\u578b\u3002", 7.0)

doc <- add_heading(doc, "5.3  \u9759\u6001\u751f\u5b58\u9884\u6d4b\u7ed3\u679c", 2)
doc <- add_heading(doc, "5.3.1  RSF\u6536\u655b\u6027\u3001\u53d8\u91cf\u91cd\u8981\u6027\u4e0e\u5c40\u90e8\u89e3\u91ca", 3)
doc <- add_text(doc, "RSF\u7684OOB\u8bef\u5dee\u968f\u6811\u6570\u589e\u52a0\u9010\u6e10\u7a33\u5b9a\uff0c\u8868\u660e500\u68f5\u6811\u5df2\u53ef\u63d0\u4f9b\u7a33\u5b9a\u9884\u6d4b\u3002\u7f6e\u6362\u53d8\u91cf\u91cd\u8981\u6027\u3001\u6700\u5c0f\u6df1\u5ea6\u3001PDP\u548cSHAP\u4ece\u4e0d\u540c\u89d2\u5ea6\u8bf4\u660e\u6a21\u578b\u51b3\u7b56\u4f9d\u636e\u3002\u4f9d\u7167\u6307\u5b9a\u8981\u6c42\uff0c\u56fe5-5\u4fdd\u7559\u4e00\u4f8b\u9ad8\u98ce\u9669\u6b7b\u4ea1\u60a3\u8005\u4e0e\u4e00\u4f8b\u4f4e\u98ce\u9669\u5b58\u6d3b\u60a3\u8005\u7684\u5c40\u90e8SHAP\u7011\u5e03\u56fe\uff0c\u7528\u4e8e\u663e\u793a\u5355\u4e2a\u60a3\u8005\u4e2d\u5404\u53d8\u91cf\u5c06\u9884\u6d4b\u4ece\u57fa\u51c6\u98ce\u9669\u63a8\u5411\u6700\u7ec8\u98ce\u9669\u7684\u65b9\u5411\u548c\u5e45\u5ea6\u3002")
doc <- add_figure(doc, "\u56fe5-2_RSF_OOB\u6536\u655b\u4e0e\u7f6e\u6362\u53d8\u91cf\u91cd\u8981\u6027.png", "\u56fe5-2  RSF OOB\u6536\u655b\u4e0e\u7f6e\u6362\u53d8\u91cf\u91cd\u8981\u6027", 6.4, 5.1, TRUE)
doc <- add_figure(doc, "\u56fe5-3_RSF\u4e3b\u8981\u53d8\u91cfu1\u6b7b\u4ea1\u98ce\u9669PDP.png", "\u56fe5-3  RSF\u4e3b\u8981\u53d8\u91cf\u4e0e28\u65e5\u6b7b\u4ea1\u98ce\u9669\u7684\u90e8\u5206\u4f9d\u8d56", 6.4, 5.1)
doc <- add_figure(doc, "\u56fe5-4_RSF\u9a8c\u8bc1\u96c6\u5168\u5c40SHAP\u5206\u5e03.png", "\u56fe5-4  RSF\u9a8c\u8bc1\u96c6\u5168\u5c40SHAP\u5206\u5e03", 6.4, 5.2, TRUE)
doc <- add_figure(doc, "\u56fe5-5_RSF\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca.png", "\u56fe5-5  RSF\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca", 6.2, 7.3, TRUE)
rsf_interp <- head(read_tab("\u88685-5A_RSF\u4e3b\u8981\u53d8\u91cf\u89e3\u91ca\u7ed3\u679c.csv"), 15L)
doc <- add_table_block(doc, "\u88685-5A  RSF\u4e3b\u8981\u53d8\u91cf\u7684\u591a\u65b9\u6cd5\u89e3\u91ca\u7ed3\u679c", rsf_interp,
                       "\u6ce8\uff1a\u8868\u4e2d\u5217\u51faOOB\u7f6e\u6362\u91cd\u8981\u6027\u6392\u5e8f\u524d15\u4e2a\u53d8\u91cf\uff1b\u6700\u5c0f\u6df1\u5ea6\u8d8a\u5c0f\u901a\u5e38\u8868\u660e\u53d8\u91cf\u8d8a\u65e9\u53c2\u4e0e\u5206\u88c2\u3002", 7.5)

doc <- add_heading(doc, "5.3.2  \u9759\u6001\u6a21\u578b\u533a\u5206\u5ea6\u3001\u6821\u51c6\u5ea6\u4e0e\u4e34\u5e8a\u51c0\u53d7\u76ca", 3)
sp <- data.frame(static_perf$model, static_perf$setting, fmt_num(static_perf$train_C_index),
                 fmt_num(static_perf$validation_C_index), fmt_num(static_perf$AUC_u1),
                 fmt_num(static_perf$Brier_u1), fmt_num(static_perf$IBS_0_1), check.names = FALSE)
names(sp) <- c("\u6a21\u578b", "\u8bbe\u5b9a", "\u8bad\u7ec3C-index", "\u9a8c\u8bc1C-index",
               "\u9a8c\u8bc1AUC(u=1)", "\u9a8c\u8bc1Brier(u=1)", "\u9a8c\u8bc1IBS")
doc <- add_table_block(doc, "\u88685-4A  \u9759\u6001\u4efb\u52a1\u7684\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u6027\u80fd", sp,
                       "\u6ce8\uff1au=1\u5bf9\u5e94\u5165\u9662\u540e28\u65e5\uff1bBrier\u4e0eIBS\u8d8a\u5c0f\u8d8a\u597d\u3002", 7.5)
cox_v <- read_tab("\u88685-5_COX\u6a21\u578b\u534f\u53d8\u91cf\u98ce\u9669\u6bd4.csv")
doc <- add_table_block(doc, "\u88685-5  Cox\u6a21\u578b\u534f\u53d8\u91cf\u98ce\u9669\u6bd4", cox_v,
                       "\u6ce8\uff1aHR\u4e3a\u5176\u4ed6\u534f\u53d8\u91cf\u8c03\u6574\u540e\u7684\u4f30\u8ba1\uff0c\u5206\u7c7b\u53d8\u91cf\u4ee5\u6570\u636e\u4e2d\u53c2\u8003\u6c34\u5e73\u4e3a\u57fa\u51c6\u3002", 7.5)
doc <- add_text(doc, sprintf("\u5728\u7559\u51fa\u9a8c\u8bc1\u96c6\u4e2d\uff0cCox\u4e0eRSF\u7684C-index\u5206\u522b\u4e3a%s\u548c%s\uff0c28\u65e5AUC\u5206\u522b\u4e3a%s\u548c%s\u3002RSF\u540c\u65f6\u83b7\u5f97\u66f4\u4f4e\u7684Brier\u8bc4\u5206\uff08%s vs %s\uff09\u548cIBS\uff08%s vs %s\uff09\uff0c\u663e\u793a\u5176\u5728\u9759\u6001\u4efb\u52a1\u4e2d\u5177\u6709\u8f83\u597d\u7684\u7efc\u5408\u9884\u6d4b\u8868\u73b0\u3002",
                               fmt_num(static_perf$validation_C_index[static_perf$model == "COX"]),
                               fmt_num(static_perf$validation_C_index[static_perf$model == "RSF"]),
                               fmt_num(static_perf$AUC_u1[static_perf$model == "COX"]),
                               fmt_num(static_perf$AUC_u1[static_perf$model == "RSF"]),
                               fmt_num(static_perf$Brier_u1[static_perf$model == "RSF"]),
                               fmt_num(static_perf$Brier_u1[static_perf$model == "COX"]),
                               fmt_num(static_perf$IBS_0_1[static_perf$model == "RSF"]),
                               fmt_num(static_perf$IBS_0_1[static_perf$model == "COX"])))
doc <- add_figure(doc, "\u56fe5-10_\u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u6821\u51c6\u56fe.png", "\u56fe5-10  \u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c628\u65e5\u98ce\u9669\u6821\u51c6\u56fe", 6.0, 4.6, FALSE)
doc <- add_figure(doc, "\u56fe5-11_\u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u51b3\u7b56\u66f2\u7ebf.png", "\u56fe5-11  \u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c628\u65e5\u51b3\u7b56\u66f2\u7ebf", 6.2, 4.7)

doc <- add_heading(doc, "5.4  \u52a8\u6001\u751f\u5b58\u9884\u6d4b\u7ed3\u679c", 2)
doc <- add_heading(doc, "5.4.1  RSFLC\u7684\u53d8\u91cf\u91cd\u8981\u6027\u4e0eSHAP\u89e3\u91ca", 3)
doc <- add_text(doc, "RSFLC\u540c\u65f6\u5229\u7528\u7b2c5\u65e5\u524d3\u4e2a\u751f\u7406/\u4e25\u91cd\u5ea6\u8f68\u8ff9\u4e0e28\u4e2a\u56fa\u5b9a\u534f\u53d8\u91cf\u3002\u56fe5-6\u7ed9\u51fa\u539f\u751fOOB\u7f6e\u6362\u53d8\u91cf\u91cd\u8981\u6027\uff1b\u56fe5-7\u7684SHAP\u4ec5\u9488\u5bf9\u56fa\u5b9a\u534f\u53d8\u91cf\uff0c\u56e0\u6b64\u5e94\u89e3\u8bfb\u4e3a\u201c\u5728\u5df2\u7ed9\u5b9a\u7eb5\u5411\u5386\u53f2\u7684\u60c5\u51b5\u4e0b\u201d\u5404\u56fa\u5b9a\u53d8\u91cf\u5bf9\u6761\u4ef6\u98ce\u9669\u7684\u5c40\u90e8\u8d21\u732e\u3002\u56fe5-8\u4fdd\u7559\u4e24\u4f8b\u5178\u578b\u60a3\u8005\u7684\u5c40\u90e8SHAP\u89e3\u91ca\u3002")
doc <- add_figure(doc, "\u56fe5-6_RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027.png", "\u56fe5-6  RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027", 6.2, 5.2, FALSE)
rsflc_vimp <- head(read_tab("\u88685-7_RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027.csv"), 20L)
doc <- add_table_block(doc, "\u88685-7  RSFLC\u539f\u751fOOB\u53d8\u91cf\u91cd\u8981\u6027", rsflc_vimp,
                       "\u6ce8\uff1a\u5217\u51faOOB VIMP\u6392\u5e8f\u524d20\u4e2a\u8f93\u5165\u3002", 8.0)
doc <- add_figure(doc, "\u56fe5-7_RSFLC\u56fa\u5b9a\u534f\u53d8\u91cf\u5168\u5c40SHAP\u5206\u5e03.png", "\u56fe5-7  RSFLC\u56fa\u5b9a\u534f\u53d8\u91cf\u5168\u5c40SHAP\u5206\u5e03", 6.2, 5.1, FALSE)
doc <- add_figure(doc, "\u56fe5-8_RSFLC\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca.png", "\u56fe5-8  RSFLC\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca", 6.1, 7.2, FALSE)

doc <- add_heading(doc, "5.4.2  \u8054\u5408\u6a21\u578b\u4e0eMCMC\u8bca\u65ad", 3)
jm_tab <- read_tab("\u88685-6_JM\u5f53\u524d\u503c\u5173\u8054\u53c2\u6570.csv")
doc <- add_table_block(doc, "\u88685-6  \u8054\u5408\u6a21\u578b\u5f53\u524d\u503c\u5173\u8054\u53c2\u6570", jm_tab,
                       "\u6ce8\uff1aRhat\u7528\u4e8e\u8bc4\u4f30\u94fe\u95f4\u6536\u655b\uff0cESS\u4e3a\u6709\u6548\u6837\u672c\u91cf\u3002", 8.0)
if (max(jm_tab$Rhat, na.rm = TRUE) > 1.05) {
  doc <- add_text(doc, sprintf("\u5c3d\u7ba1\u8fed\u4ee3\u6b21\u6570\u5df2\u81ea\u52a8\u6269\u5c55\uff0c\u4ecd\u6709\u5173\u8054\u53c2\u6570\u7684Rhat\u4e3a%.3f\uff08>1.05\uff09\u3002\u56e0\u6b64JM\u7684\u5173\u8054\u53c2\u6570\u4e0e\u9884\u6d4b\u6027\u80fd\u5e94\u89c6\u4e3a\u63a2\u7d22\u6027\u7ed3\u679c\uff0c\u4e0d\u5bf9\u8be5\u53c2\u6570\u4f5c\u786e\u8bc1\u6027\u56e0\u679c\u89e3\u91ca\u3002",
                               max(jm_tab$Rhat, na.rm = TRUE)))
} else {
  doc <- add_text(doc, sprintf("\u6240\u6709\u62a5\u544a\u7684\u5173\u8054\u53c2\u6570Rhat\u5747\u4e0d\u9ad8\u4e8e1.05\uff08\u6700\u5927%.3f\uff09\uff0c\u94fe\u95f4\u6536\u655b\u53ef\u63a5\u53d7\u3002",
                               max(jm_tab$Rhat, na.rm = TRUE)))
}
doc <- add_figure(doc, "\u56fe5-9_JM\u5173\u8054\u53c2\u6570MCMC\u8f68\u8ff9\u4e0e\u540e\u9a8c\u5bc6\u5ea6.png", "\u56fe5-9  \u8054\u5408\u6a21\u578b\u5173\u8054\u53c2\u6570MCMC\u8f68\u8ff9\u4e0e\u540e\u9a8c\u5bc6\u5ea6", 6.6, 5.2, TRUE)

doc <- add_heading(doc, "5.4.3  \u52a8\u6001\u6a21\u578b\u7684\u9a8c\u8bc1\u6027\u80fd", 3)
dp <- data.frame(dynamic_perf$model, dynamic_perf$setting, fmt_num(dynamic_perf$train_C_index),
                 fmt_num(dynamic_perf$validation_C_index), fmt_num(dynamic_perf$AUC_u1_given_uL),
                 fmt_num(dynamic_perf$Brier_u1_given_uL), fmt_num(dynamic_perf$IBS_uL_1),
                 dynamic_perf$validation_n, check.names = FALSE)
names(dp) <- c("\u6a21\u578b", "\u8bbe\u5b9a", "\u8bad\u7ec3C-index", "\u9a8c\u8bc1C-index",
               "\u9a8c\u8bc1AUC(u=1|uL)", "\u9a8c\u8bc1Brier(u=1|uL)", "\u9a8c\u8bc1IBS", "\u5171\u540c\u9a8c\u8bc1n")
doc <- add_table_block(doc, "\u88685-4B  \u52a8\u6001\u4efb\u52a1\u7684\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\u6027\u80fd", dp,
                       "\u6ce8\uff1a\u4e24\u6a21\u578b\u5747\u5728\u53ef\u751f\u6210\u8054\u5408\u6a21\u578b\u9884\u6d4b\u7684\u5171\u540c\u9a8c\u8bc1\u961f\u5217\u4e0a\u91cd\u7b97\u3002", 7.0)
doc <- add_text(doc, sprintf("\u5728\u7b2c5\u65e5landmark\u7684\u5171\u540c\u9a8c\u8bc1\u961f\u5217\u4e2d\uff0cJM\u4e0eRSFLC\u7684C-index\u5206\u522b\u4e3a%s\u548c%s\uff0c\u7b2c28\u65e5\u6761\u4ef6AUC\u5206\u522b\u4e3a%s\u548c%s\uff0cBrier\u8bc4\u5206\u5206\u522b\u4e3a%s\u548c%s\u3002\u8fd9\u4e9b\u7ed3\u679c\u5e94\u7ed3\u5408\u6821\u51c6\u548c\u51b3\u7b56\u66f2\u7ebf\u7efc\u5408\u89e3\u8bfb\uff0c\u800c\u4e0d\u5e94\u53ea\u636e\u5355\u4e00\u533a\u5206\u5ea6\u6307\u6807\u9009\u62e9\u6a21\u578b\u3002",
                               fmt_num(dynamic_perf$validation_C_index[dynamic_perf$model == "JM"]),
                               fmt_num(dynamic_perf$validation_C_index[dynamic_perf$model == "RSFLC"]),
                               fmt_num(dynamic_perf$AUC_u1_given_uL[dynamic_perf$model == "JM"]),
                               fmt_num(dynamic_perf$AUC_u1_given_uL[dynamic_perf$model == "RSFLC"]),
                               fmt_num(dynamic_perf$Brier_u1_given_uL[dynamic_perf$model == "JM"]),
                               fmt_num(dynamic_perf$Brier_u1_given_uL[dynamic_perf$model == "RSFLC"])))
doc <- add_figure(doc, "\u56fe5-12_\u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u6761\u4ef6\u98ce\u9669\u6821\u51c6\u56fe.png", "\u56fe5-12  \u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c628\u65e5\u6761\u4ef6\u98ce\u9669\u6821\u51c6\u56fe", 6.0, 4.7, TRUE)
doc <- add_figure(doc, "\u56fe5-13_\u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6\u51b3\u7b56\u66f2\u7ebf.png", "\u56fe5-13  \u52a8\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6\u51b3\u7b56\u66f2\u7ebf", 6.2, 4.7)

doc <- add_heading(doc, "5.5  \u5206\u4efb\u52a1\u7684\u65f6\u95f4\u4f9d\u8d56\u6027\u6bd4\u8f83", 2)
doc <- add_text(doc, "\u56fe5-14\u5206\u5f00\u5c55\u793a\u9759\u6001\u4efb\u52a1\u4e0e\u7b2c5\u65e5landmark\u52a8\u6001\u4efb\u52a1\u7684\u65f6\u95f4\u4f9d\u8d56AUC\u548cBrier\u66f2\u7ebf\u3002\u8fd9\u79cd\u5c55\u793a\u4fdd\u7559\u4e86\u6bcf\u4e2a\u6a21\u578b\u968f\u9884\u6d4b\u65f6\u70b9\u53d8\u5316\u7684\u4fe1\u606f\uff0c\u540c\u65f6\u907f\u514d\u5c06\u5165\u9662\u65f6\u8d77\u7b97\u7684\u65e0\u6761\u4ef6\u98ce\u9669\u4e0e\u7b2c5\u65e5\u540e\u6761\u4ef6\u98ce\u9669\u76f4\u63a5\u6df7\u5408\u6392\u540d\u3002")
doc <- add_figure(doc, "\u56fe5-14_\u4e24\u7c7b\u4efb\u52a1\u65f6\u95f4\u4f9d\u8d56AUC\u4e0eBrier\u66f2\u7ebf.png", "\u56fe5-14  \u9759\u6001\u4e0e\u7b2c5\u65e5landmark\u52a8\u6001\u4efb\u52a1\u7684\u65f6\u95f4\u4f9d\u8d56AUC\u4e0eBrier\u66f2\u7ebf", 6.5, 5.9, FALSE)

doc <- add_heading(doc, "5.6  \u672c\u7ae0\u5c0f\u7ed3\u4e0e\u56fe\u8868\u53d6\u820d", 2)
doc <- add_text(doc, "\u672c\u6b21\u91cd\u8dd1\u5728\u6570\u636e\u5c42\u9762\u7cfb\u7edf\u8bb0\u5f55337\u6761\u7ed3\u5c40\u65f6\u95f4\u65e0\u6548\u8bb0\u5f55\u7684\u6392\u9664\uff0c\u5728\u6a21\u578b\u5c42\u9762\u56fa\u5b9a\u53d8\u91cf\u96c6\u3001\u65f6\u95f4\u5c3a\u5ea6\u548c\u8d85\u53c2\u6570\uff0c\u5e76\u5c06\u9759\u6001\u4e0e\u52a8\u6001\u4efb\u52a1\u5206\u5f00\u8bc4\u4ef7\u3002\u7ed3\u679c\u8868\u660e\uff0c\u9759\u6001\u4efb\u52a1\u4e2dRSF\u7684\u7efc\u5408\u9884\u6d4b\u6027\u80fd\u4f18\u4e8eCox\uff1b\u52a8\u6001\u4efb\u52a1\u4e2dJM\u4e0eRSFLC\u5219\u9700\u8981\u540c\u65f6\u7ed3\u5408\u533a\u5206\u5ea6\u3001\u6821\u51c6\u5ea6\u4e0e\u51c0\u53d7\u76ca\u5224\u65ad\u3002")
doc <- add_text(doc, "\u672c\u7ae0\u5220\u9664\u4e86\u4e24\u7c7b\u56fe\uff1a\uff081\uff09SHAP\u4f9d\u8d56\u56fe\uff0c\u56e0\u5176\u4e0ePDP\u548cSHAP\u8702\u7fa4\u56fe\u5728\u53d8\u91cf\u6548\u5e94\u4fe1\u606f\u4e0a\u9ad8\u5ea6\u91cd\u590d\uff0c\u4e0d\u4f1a\u6539\u53d8\u6a21\u578b\u6027\u80fd\u6bd4\u8f83\u7684\u7ed3\u8bba\uff1b\uff082\uff09RSFLC\u5728\u6781\u5c0f\u9a8c\u8bc1\u62bd\u6837\u4e0a\u7684\u91cd\u590d\u7f6e\u6362\u91cd\u8981\u6027\u56fe\uff0c\u56e0\u5176\u6837\u672c\u5c11\u3001\u65b9\u5dee\u5927\uff0c\u4e0d\u5982\u539f\u751fOOB VIMP\u7a33\u5b9a\u3002\u6559\u5e08\u6307\u5b9a\u7684\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u56fe\u5df2\u5728\u9759\u6001\u548cRSFLC\u90e8\u5206\u4fdd\u7559\u3002")
doc <- add_text(doc, "\u5c40\u9650\u6027\u5305\u62ec\uff1a\u672c\u7814\u7a76\u4e3a\u5355\u6570\u636e\u5e93\u7684\u5185\u90e8\u7559\u51fa\u9a8c\u8bc1\uff0c\u672a\u8fdb\u884c\u5916\u90e8\u9a8c\u8bc1\uff1b\u9ad8\u8017\u65f6\u4e94\u6298\u8d85\u53c2\u6570\u641c\u7d22\u672c\u6b21\u672a\u91cd\u590d\u8fd0\u884c\uff1bRSFLC\u7684SHAP\u56e0\u7b97\u529b\u9650\u5236\u4ec5\u8bc4\u4f30\u56fa\u5b9a\u534f\u53d8\u91cf\uff0c\u4e0d\u80fd\u66ff\u4ee3\u5bf9\u7eb5\u5411\u8f68\u8ff9\u6574\u4f53\u4f5c\u7528\u7684\u89e3\u91ca\uff1b\u6240\u6709\u7ed3\u679c\u4ecd\u9700\u5728\u72ec\u7acb\u5916\u90e8\u961f\u5217\u4e2d\u9a8c\u8bc1\u3002")

doc <- body_set_default_section(doc, value = sec)
out_path <- file.path(project_dir, "codex\u7b2c\u4e94\u7ae0.docx")
print(doc, target = out_path)
artifact_files <- list.files(artifact_dir, full.names = TRUE, recursive = FALSE)
manifest <- data.frame(
  filename = basename(artifact_files),
  bytes = file.info(artifact_files)$size,
  md5 = unname(tools::md5sum(artifact_files)),
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  row.names = NULL
)
write_utf8(manifest, file.path(artifact_dir, "00_output_manifest.csv"))
log_progress("DOCX_OK", out_path)
cat(out_path, "\n")
