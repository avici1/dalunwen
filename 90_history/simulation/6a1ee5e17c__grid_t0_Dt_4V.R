# 在同一 jm 上网格搜索 (t0, Dt)，寻找合理 tvAUC
# 用法: Rscript _grid_t0_Dt_4V.R

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(readxl)
  library(nlme)
  library(JMbayes2)
  library(writexl)
})

set.seed(123)

input_dir <- "F:/文章/大论文/程序Trae/模拟数据_4V/生成Y"
nm <- "sim500_30_4V_lowINTER_1c_L1"
fpath <- file.path(input_dir, paste0(nm, ".xlsx"))
data_raw <- as.data.frame(read_xlsx(fpath, sheet = excel_sheets(fpath)[1]))
data_clean <- data_raw[data_raw$t <= data_raw$obs_time, , drop = FALSE]
survData <- data_clean[!duplicated(data_clean$ID), , drop = FALSE]
max_obs <- max(survData$obs_time, na.rm = TRUE)

cat("dataset:", nm, "\n")
cat("n_id:", nrow(survData),
    " n_event:", sum(survData$event),
    " max_obs:", max_obs, "\n")

# ---- 拟合一次 jm（与 cal_3_JM_4V 一致）----
cat("Fitting jm once...\n")
t_fit0 <- Sys.time()
fm1 <- lme(V1 ~ t, data = data_clean, random = ~ 1 | ID)
fm2 <- lme(V2 ~ t, data = data_clean, random = ~ 1 | ID)
fm3 <- lme(V3 ~ t, data = data_clean, random = ~ 1 | ID)
fm4 <- lme(V4 ~ t, data = data_clean, random = ~ 1 | ID)
coxFit <- coxph(Surv(obs_time, event) ~ 1, data = survData, x = TRUE)
jmFit <- jm(coxFit, list(fm1, fm2, fm3, fm4), time_var = "t")
cat("jm elapsed:", round(as.numeric(difftime(Sys.time(), t_fit0, units = "secs")), 1), "sec\n")

# ---- C-index 对照 ----
pred <- predict(jmFit, newdata = data_clean, process = "event")
pred_patient <- data.frame(ID = pred$id, time = pred$times, risk = pred$pred)
risk_last <- pred_patient %>%
  group_by(ID) %>%
  summarise(risk = last(risk), .groups = "drop")
cdata <- merge(survData, risk_last, by = "ID")
Cindex <- as.numeric(
  concordance(Surv(obs_time, event) ~ risk, reverse = TRUE, data = cdata)$concordance
)
cat("Cindex:", round(Cindex, 4), "\n")

# ---- 网格 ----
t0_grid <- c(0.5, 1, 1.5, 2, 2.5, 3)
Dt_grid <- c(0.5, 1, 1.5, 2, 3, 4)
grid <- expand.grid(t0 = t0_grid, Dt = Dt_grid, KEEP.OUT.ATTRS = FALSE)

rows <- lapply(seq_len(nrow(grid)), function(i) {
  t0 <- grid$t0[i]
  Dt <- grid$Dt[i]
  th <- t0 + Dt
  at_risk <- survData$obs_time > t0
  n_event_window <- sum(
    survData$event == 1 & survData$obs_time > t0 & survData$obs_time <= th
  )
  base <- data.frame(
    t0 = t0,
    Dt = Dt,
    Thoriz = th,
    n_at_risk = sum(at_risk),
    n_event_window = n_event_window,
    AUC = NA_real_,
    BS = NA_real_,
    nr = NA_real_,
    ok = FALSE,
    err = NA_character_,
    stringsAsFactors = FALSE
  )
  if (!(th < max_obs)) {
    base$err <- "Thoriz >= max(obs_time)"
    cat(sprintf("SKIP t0=%.1f Dt=%.1f: %s\n", t0, Dt, base$err))
    return(base)
  }
  res <- tryCatch({
    auc_obj <- tvAUC(
      object = jmFit, newdata = data_clean,
      Tstart = t0, Dt = Dt, cores = 1L
    )
    bs_obj <- tvBrier(
      object = jmFit, newdata = data_clean,
      Tstart = t0, Dt = Dt, cores = 1L
    )
    list(
      ok = TRUE,
      auc = as.numeric(auc_obj$auc),
      bs = as.numeric(bs_obj$Brier),
      nr = as.numeric(auc_obj$nr)
    )
  }, error = function(e) {
    list(ok = FALSE, err = conditionMessage(e))
  })
  if (isTRUE(res$ok)) {
    base$ok <- TRUE
    base$AUC <- round(res$auc, 4)
    base$BS <- res$bs
    base$nr <- res$nr
    cat(sprintf(
      "OK   t0=%.1f Dt=%.1f AUC=%.4f BS=%.4f nr=%s events=%d\n",
      t0, Dt, base$AUC, base$BS, as.character(base$nr), n_event_window
    ))
  } else {
    base$err <- res$err
    cat(sprintf("FAIL t0=%.1f Dt=%.1f: %s\n", t0, Dt, base$err))
  }
  base
})

tab <- do.call(rbind, rows)
rownames(tab) <- NULL
tab$Cindex <- Cindex

out_csv <- "F:/文章_大论文/0722/模拟研究代码/_grid_t0_Dt_4V_result.csv"
out_xlsx <- "F:/文章_大论文/0722/模拟研究代码/_grid_t0_Dt_4V_result.xlsx"
write.csv(tab, out_csv, row.names = FALSE)
write_xlsx(tab, out_xlsx)
cat("\nSaved:", out_csv, "\n")
print(tab[order(-as.integer(tab$ok), -ifelse(is.na(tab$AUC), -1, tab$AUC)), ])

# ---- 选窗：AUC>=0.70 且 nr>=50 且 n_event_window>=20 ----
cand <- tab[
  tab$ok &
    !is.na(tab$AUC) & tab$AUC >= 0.70 &
    !is.na(tab$nr) & tab$nr >= 50 &
    tab$n_event_window >= 20,
  ,
  drop = FALSE
]

pick_path <- "F:/文章_大论文/0722/模拟研究代码/_grid_t0_Dt_4V_pick.txt"
if (nrow(cand) == 0) {
  msg <- paste0(
    "NO_MATCH: no cell with AUC>=0.70 & nr>=50 & n_event_window>=20.\n",
    "Best successful AUC cells (top 5):\n"
  )
  ok_tab <- tab[tab$ok & !is.na(tab$AUC), , drop = FALSE]
  if (nrow(ok_tab) > 0) {
    ok_tab <- ok_tab[order(-ok_tab$AUC, -ok_tab$n_event_window), ]
    msg <- paste0(msg, paste(capture.output(print(head(ok_tab, 5))), collapse = "\n"))
  } else {
    msg <- paste0(msg, "(none succeeded)\n")
  }
  writeLines(msg, pick_path)
  cat("\n", msg, "\n", sep = "")
  quit(status = 2)
}

# 优先更接近老师 (1,3)：距离 (t0-1)^2+(Dt-3)^2 最小，再比 AUC、事件数
cand$dist_teacher <- (cand$t0 - 1)^2 + (cand$Dt - 3)^2
cand <- cand[order(cand$dist_teacher, -cand$AUC, -cand$n_event_window), ]
pick <- cand[1, , drop = FALSE]

msg <- sprintf(
  "PICK t0=%.1f Dt=%.1f AUC=%.4f BS=%g nr=%s n_event_window=%d Cindex=%.4f\n",
  pick$t0, pick$Dt, pick$AUC, pick$BS,
  as.character(pick$nr), pick$n_event_window, Cindex
)
writeLines(msg, pick_path)
cat("\n", msg, sep = "")
cat("Pick file:", pick_path, "\n")
