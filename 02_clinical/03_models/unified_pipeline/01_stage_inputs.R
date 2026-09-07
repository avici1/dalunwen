options(stringsAsFactors = FALSE)
invisible(try(Sys.setlocale("LC_CTYPE", ".UTF-8"), silent = TRUE))

args <- commandArgs(trailingOnly = TRUE)
args <- args[args != "--args"]
if (length(args) < 3L) {
  args <- c(
    "F:/\u6587\u7ae0_\u5927\u8bba\u6587/0722/\u5b9e\u4f8b\u7814\u7a76\u4ee3\u7801/stroke_baseline_knn_0824_fold.csv",
    "F:/\u6587\u7ae0_\u5927\u8bba\u6587/0722/\u5b9e\u4f8b\u7814\u7a76\u4ee3\u7801/stroke_longitudinal_knn_0824_group_fold.csv",
    "F:/\u6587\u7ae0_\u5927\u8bba\u6587/0722/\u5927\u8bba\u6587\u7248\u672c/0831_8_\u9759\u6001\u4e0e\u52a8\u6001\u751f\u5b58\u9884\u6d4b\u6a21\u578b\u6bd4\u8f83\u7814\u7a76_\u6807\u51c6\u5316\u65f6\u95f4\u5c3a\u5ea6\u4fee\u8ba2.docx"
  )
}

script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")

src <- normalizePath(args[1:3], winslash = "/", mustWork = TRUE)
dst <- c(
  file.path(data_dir, "stroke_baseline_knn_0824_fold.csv"),
  file.path(data_dir, "stroke_longitudinal_knn_0824_group_fold.csv"),
  file.path(data_dir, "reference_article.docx")
)

ok <- mapply(function(a, b) file.copy(a, b, overwrite = TRUE, copy.date = TRUE), src, dst)
if (!all(ok)) stop("One or more input files could not be staged")

manifest <- data.frame(
  role = c("baseline", "longitudinal", "reference_article"),
  source_path = src,
  staged_path = normalizePath(dst, winslash = "/", mustWork = TRUE),
  bytes = file.info(dst)$size,
  source_md5 = unname(tools::md5sum(src)),
  staged_md5 = unname(tools::md5sum(dst)),
  identical_md5 = unname(tools::md5sum(src)) == unname(tools::md5sum(dst)),
  staged_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
write_utf8(manifest, file.path(artifact_dir, "00_input_manifest.csv"))
log_progress("STAGE_INPUTS", paste("files=3; verified=", all(manifest$identical_md5)))
cat("STAGE_INPUTS_OK\n")
