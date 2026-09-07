suppressPackageStartupMessages({
  library(survival)
  library(joineRML)
  library(readxl)
})
input_dir <- "F:/文章_大论文/0319大改/测试集/模拟数据_10V1C/生成Y"
path <- file.path(input_dir, "sim500_30_10V_highBTW_1c_L1.xlsx")
d <- read_xlsx(path, sheet = 1)
d <- d[d$t <= d$obs_time, ]
long_fixed_formula <- Y ~ t + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10
# 取前 80 个受试者加速
ids <- unique(d$ID)[1:80]
d <- d[d$ID %in% ids, ]
message("nrow=", nrow(d), " n_subj=", length(unique(d$ID)))
fit <- mjoint(
  formLongFixed = list("Y" = long_fixed_formula),
  formLongRandom = list("Y" = ~ t | ID),
  formSurv = Surv(obs_time, event) ~ 1,
  data = d,
  timeVar = "t",
  pfs = TRUE,
  control = list(type = "sobol", burnin = 3, nMCscale = 2)
)
re <- ranef(fit)
message("str(ranef(fit)):")
str(re)
message("class: ", paste(class(re), collapse = ","))
message("dim: ", paste(dim(re), collapse = "x"))
if (is.matrix(re) || is.data.frame(re)) {
  message("colnames: ", paste(colnames(re), collapse = ","))
}
