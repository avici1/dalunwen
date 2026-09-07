# Complete tuning/refit entry; see ../README.md.
if (.Platform$OS.type == "windows") invisible(try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE))
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(file_arg)) stop("Run this entry with Rscript; use --help for arguments")
model_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
source(file.path(model_dir, "..", "hyperparameter_common.R"), encoding = "UTF-8")
hp_cli("RSFLC", model_dir)
