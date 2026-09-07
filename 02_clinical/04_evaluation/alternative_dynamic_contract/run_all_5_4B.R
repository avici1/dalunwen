options(stringsAsFactors = FALSE)
code_dir <- Sys.getenv("RSF5_4B_CODE", unset = getwd())
source(file.path(code_dir, "RSFLC_5_4B_corrected.R"), encoding = "UTF-8")
source(file.path(code_dir, "JM_5_4B_corrected.R"), encoding = "UTF-8")

root <- Sys.getenv("RSF5_4B_OUT", unset = "F:/文章_大论文/0830/结果/RSF5_4B")
rsflc <- read.csv(file.path(root, "RSFLC", "02_表5-4B_RSFLC指标.csv"), check.names = FALSE)
jm <- read.csv(file.path(root, "JM", "02_表5-4B_JM指标.csv"), check.names = FALSE)
write.csv(rbind(jm, rsflc), file.path(root, "表5-4B_JM_RSFLC合并.csv"), row.names = FALSE, fileEncoding = "UTF-8")
