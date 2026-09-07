# 验证 landmark 回退后，原先失败的 BTW 70% 能否出结果
# 用法: Rscript _verify_70_fix.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(readxl)
  library(nlme)
  library(JMbayes2)
})

# 从主脚本抽出 cal_3_JM（不跑全量）
src <- readLines("f:/文章_大论文/0722/模拟研究代码/5_jointmodel_10V1C.R", encoding = "UTF-8")
i1 <- grep("^cal_3_JM <- function", src)[1]
i2 <- grep("^########低相关性###########", src)[1] - 1
eval(parse(text = src[i1:i2]))

ncores <- 1L
options(mc.cores = ncores)

path <- "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_70_10V_lowBTW_1c_L1.xlsx"
dat <- as.data.frame(read_xlsx(path, sheet = 1))
cat("rows:", nrow(dat), " max_obs:", max(dat$obs_time), "\n")

# 原先失败点：t0=0.5；函数内会回退到 0.3/0.5
res <- cal_3_JM(dat, t0 = 0.5)
print(res)

if (anyNA(res$AUC) || anyNA(res$BS) || anyNA(res$Cindex)) {
  quit(status = 1)
}
cat("VERIFY OK\n")
