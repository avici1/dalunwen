args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Usage: Rscript utf8_launcher.R <target.R> [target args]")
target <- args[1]
eval(parse(file = target, encoding = "UTF-8"), envir = .GlobalEnv)

