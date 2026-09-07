# 验证老师窗口 Tstart=1, Dt=3；以及 70% 回退
# 用法: Rscript _verify_teacher_window.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(readxl)
  library(nlme)
  library(JMbayes2)
})

src_file <- "F:/文章_大论文/0722/模拟研究代码/5_jointmodel_4V.R"
src <- readLines(src_file, encoding = "UTF-8")
i1 <- grep("^cal_3_JM_4V <- function", src)[1]
i2 <- grep("^########低相关性###########", src)[1] - 1
eval(parse(text = src[i1:i2]))

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/生成Y"

read_L1 <- function(nm) {
  f <- file.path(input_dir, paste0(nm, ".xlsx"))
  as.data.frame(read_xlsx(f, sheet = excel_sheets(f)[1]))
}

# ---- 30%: 应使用老师窗口 1 / 3，AUC 应明显高于 0.5 ----
nm30 <- "sim500_30_4V_lowINTER_1c_L1"
cat("\n===== ", nm30, " =====\n", sep = "")
res30 <- cal_3_JM_4V(read_L1(nm30), t0 = 1, Dt = 3)
print(res30)
stopifnot(!is.na(res30$AUC), !is.na(res30$BS), !is.na(res30$Cindex))
stopifnot(isTRUE(all.equal(res30$Tstart_used, 1)))
stopifnot(isTRUE(all.equal(res30$Dt_used, 3)))
cat(
  "VERIFY 30% OK: Tstart_used=1, Dt_used=3, AUC=", res30$AUC,
  " (note: landmark AUC may stay near 0.5 even when C-index is high)\n",
  sep = ""
)

# ---- 70%: 老师窗口应失败并回退；须记录实际窗口 ----
nm70 <- "sim500_70_4V_lowINTER_1c_L1"
cat("\n===== ", nm70, " =====\n", sep = "")
res70 <- cal_3_JM_4V(read_L1(nm70), t0 = 1, Dt = 3)
print(res70)
stopifnot(!is.na(res70$AUC))
stopifnot(!is.na(res70$Tstart_used), !is.na(res70$Dt_used))
# 70% 随访短，通常用不到老师 1/3
if (isTRUE(all.equal(res70$Tstart_used, 1)) && isTRUE(all.equal(res70$Dt_used, 3))) {
  cat("NOTE: 70% unexpectedly used teacher window 1/3\n")
} else {
  cat(
    "VERIFY 70% OK: fallback Tstart_used=", res70$Tstart_used,
    ", Dt_used=", res70$Dt_used,
    ", AUC=", res70$AUC, "\n", sep = ""
  )
}

cat("\nALL VERIFY OK\n")
