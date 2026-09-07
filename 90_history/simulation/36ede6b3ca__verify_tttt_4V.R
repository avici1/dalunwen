# 复验：写死后的 cal_3_JM_4V（tttt, t0=0.5, Dt=1）
suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(readxl)
  library(nlme)
  library(JMbayes2)
})

src <- readLines("F:/文章_大论文/0722/模拟研究代码/5_jointmodel_4V.R", encoding = "UTF-8")
i1 <- grep("^cal_3_JM_4V <- function", src)[1]
i2 <- grep("^########低相关性###########", src)[1] - 1
eval(parse(text = src[i1:i2]))

fpath <- "F:/文章/大论文/程序Trae/模拟数据_4V/生成Y/sim500_30_4V_lowINTER_1c_L1.xlsx"
dat <- as.data.frame(read_xlsx(fpath, sheet = excel_sheets(fpath)[1]))
res <- cal_3_JM_4V(dat, t0 = 0.5, Dt = 1)
print(res)
stopifnot(!is.na(res$AUC), !is.na(res$Cindex))
# 与搜索结果 tttt 对齐（允许 MCMC 微小波动）
stopifnot(abs(res$AUC - 0.5164) < 0.05)
cat("VERIFY OK: tttt + t0=0.5 Dt=1 | AUC=", res$AUC, " Cindex=", res$Cindex, "\n", sep = "")
