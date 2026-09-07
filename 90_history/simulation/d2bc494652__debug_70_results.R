library(readxl)
library(dplyr)

r1 <- as.data.frame(read_xlsx("F:/文章_大论文/0722/模拟研究代码/10V1Cresult.xlsx"))
r3 <- as.data.frame(read_xlsx("F:/文章_大论文/0722/模拟研究代码/10V3Cresult.xlsx"))

cat("=== 10V1C _70_ rows ===\n")
print(r1[grepl("_70_", r1$dataset), ], row.names = FALSE)
cat("\n=== 10V3C _70_ rows ===\n")
print(r3[grepl("_70_", r3$dataset), ], row.names = FALSE)

cat("\n=== 10V1C NA summary ===\n")
cat("n_NA_AUC:", sum(is.na(r1$AUC)), "/", nrow(r1), "\n")
cat("NA datasets:\n"); print(r1$dataset[is.na(r1$AUC)])

cat("\n=== 10V3C NA summary ===\n")
cat("n_NA_AUC:", sum(is.na(r3$AUC)), "/", nrow(r3), "\n")
cat("NA datasets:\n"); print(r3$dataset[is.na(r3$AUC)])

# Check event windows for failed BTW 70 examples
check_one <- function(path, label) {
  cat("\n----", label, "----\n")
  d <- as.data.frame(read_xlsx(path, sheet = excel_sheets(path)[1]))
  s <- d[!duplicated(d$ID), ]
  cat("event rate:", round(mean(s$event), 3), " n_event:", sum(s$event),
      " max(obs_time):", round(max(s$obs_time), 4),
      " median:", round(median(s$obs_time), 4), "\n")
  for (t0 in c(0.2, 0.3, 0.4, 0.5)) {
    for (Dt in c(0.2, 0.3, 0.5, 1)) {
      Th <- t0 + Dt
      n_int <- sum(s$event == 1 & s$obs_time >= t0 & s$obs_time < Th)
      ids_long <- unique(d$ID[d$t <= t0])
      ids_evt <- unique(s$ID[s$event == 1 & s$obs_time > t0])
      n_cross <- length(intersect(ids_long, ids_evt))
      if (n_int > 0 && n_cross > 0) {
        cat(sprintf("  OK Tstart=%.1f Dt=%.1f | events=%d cross=%d\n", t0, Dt, n_int, n_cross))
      }
    }
  }
}

dir1 <- "F:/文章/大论文/程序Trae/模拟数据_添加Y"
check_one(file.path(dir1, "sim500_70_10V_lowBTW_1c_L1.xlsx"), "500_70_lowBTW 1c")
check_one(file.path(dir1, "sim500_70_10V_lowINTER_1c_L1.xlsx"), "500_70_lowINTER 1c")
check_one(file.path(dir1, "sim1000_70_10V_midBTW_1c_L1.xlsx"), "1000_70_midBTW 1c")

dir3l <- "F:/文章/大论文/程序/模拟数据/数据3_withY/低相关"
check_one(file.path(dir3l, "sim500_70_10V_lowBTW_3c_L1.xlsx"), "500_70_lowBTW 3c")
check_one(file.path(dir3l, "sim500_70_10V_lowINTER_3c_L1.xlsx"), "500_70_lowINTER 3c")
