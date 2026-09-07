# V1–V4 固定效应 ~1 / ~t 共 16 种组合；随机效应统一 ~ 1 | ID
# 评估写死 t0=0.5, Dt=1
# 用法: Rscript _search_lme_form_4V.R

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

t0 <- 0.5
Dt <- 1

cat("dataset:", nm, "\n")
cat("n_id:", nrow(survData), " n_event:", sum(survData$event), "\n")
cat("eval: t0=", t0, " Dt=", Dt, "\n", sep = "")

# 16 种：每位 V 为 "1" 或 "t"，label 如 tttt / 1t1t
forms_grid <- expand.grid(
  V1 = c("1", "t"),
  V2 = c("1", "t"),
  V3 = c("1", "t"),
  V4 = c("1", "t"),
  stringsAsFactors = FALSE
)
forms_grid$label <- paste0(forms_grid$V1, forms_grid$V2, forms_grid$V3, forms_grid$V4)

fit_lme_one <- function(var, form_fixed, data) {
  fml <- if (form_fixed == "1") {
    as.formula(paste(var, "~ 1"))
  } else {
    as.formula(paste(var, "~ t"))
  }
  lme(fml, data = data, random = ~ 1 | ID)
}

run_one <- function(row) {
  label <- row$label
  cat("\n===== ", label, " (V1~", row$V1, ", V2~", row$V2,
      ", V3~", row$V3, ", V4~", row$V4, ") =====\n", sep = "")
  t0_run <- Sys.time()
  out <- tryCatch({
    fm1 <- fit_lme_one("V1", row$V1, data_clean)
    fm2 <- fit_lme_one("V2", row$V2, data_clean)
    fm3 <- fit_lme_one("V3", row$V3, data_clean)
    fm4 <- fit_lme_one("V4", row$V4, data_clean)
    coxFit <- coxph(Surv(obs_time, event) ~ 1, data = survData, x = TRUE)
    jmFit <- jm(coxFit, list(fm1, fm2, fm3, fm4), time_var = "t")

    pred <- predict(jmFit, newdata = data_clean, process = "event")
    pred_patient <- data.frame(
      ID = pred$id, time = pred$times, risk = pred$pred,
      stringsAsFactors = FALSE
    )
    risk_last <- pred_patient %>%
      group_by(ID) %>%
      summarise(risk = last(risk), .groups = "drop")
    cdata <- merge(survData, risk_last, by = "ID")
    Cindex <- as.numeric(
      concordance(Surv(obs_time, event) ~ risk, reverse = TRUE, data = cdata)$concordance
    )

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
      AUC = round(as.numeric(auc_obj$auc), 4),
      BS = as.numeric(bs_obj$Brier),
      Cindex = Cindex,
      err = NA_character_
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      AUC = NA_real_,
      BS = NA_real_,
      Cindex = NA_real_,
      err = conditionMessage(e)
    )
  })
  elapsed <- as.numeric(difftime(Sys.time(), t0_run, units = "secs"))
  if (isTRUE(out$ok)) {
    cat(sprintf(
      "OK AUC=%.4f BS=%.4f Cindex=%.4f Time=%.1fs\n",
      out$AUC, out$BS, out$Cindex, elapsed
    ))
  } else {
    cat("FAIL:", out$err, "\n")
  }
  data.frame(
    label = label,
    form_V1 = row$V1,
    form_V2 = row$V2,
    form_V3 = row$V3,
    form_V4 = row$V4,
    AUC = out$AUC,
    BS = out$BS,
    Cindex = out$Cindex,
    Time = elapsed,
    ok = isTRUE(out$ok),
    err = if (is.null(out$err)) NA_character_ else out$err,
    stringsAsFactors = FALSE
  )
}

rows <- lapply(seq_len(nrow(forms_grid)), function(i) {
  run_one(forms_grid[i, , drop = FALSE])
})
tab <- do.call(rbind, rows)
rownames(tab) <- NULL

out_csv <- "F:/文章_大论文/0722/模拟研究代码/_search_lme_form_4V_result.csv"
out_xlsx <- "F:/文章_大论文/0722/模拟研究代码/_search_lme_form_4V_result.xlsx"
write.csv(tab, out_csv, row.names = FALSE)
write_xlsx(tab, out_xlsx)
cat("\nSaved:", out_csv, "\n")
print(tab[order(-as.integer(tab$ok), -ifelse(is.na(tab$AUC), -Inf, tab$AUC),
                  -ifelse(is.na(tab$Cindex), -Inf, tab$Cindex),
                  ifelse(is.na(tab$BS), Inf, tab$BS)), ])

# 选优：AUC 最大 → Cindex 最大 → BS 最小 → 优先 tttt
cand <- tab[tab$ok & !is.na(tab$AUC), , drop = FALSE]
pick_path <- "F:/文章_大论文/0722/模拟研究代码/_search_lme_form_4V_pick.txt"
if (nrow(cand) == 0) {
  msg <- "NO_OK: all 16 combinations failed.\n"
  writeLines(msg, pick_path)
  cat(msg)
  quit(status = 2)
}

cand$prefer_tttt <- as.integer(cand$label == "tttt")
cand <- cand[order(
  -cand$AUC,
  -cand$Cindex,
  cand$BS,
  -cand$prefer_tttt
), ]
pick <- cand[1, , drop = FALSE]

msg <- sprintf(
  paste0(
    "PICK label=%s form=V1~%s V2~%s V3~%s V4~%s\n",
    "AUC=%.4f BS=%g Cindex=%.4f Time=%.1f\n",
    "eval t0=%.1f Dt=%.1f\n"
  ),
  pick$label, pick$form_V1, pick$form_V2, pick$form_V3, pick$form_V4,
  pick$AUC, pick$BS, pick$Cindex, pick$Time,
  t0, Dt
)
writeLines(msg, pick_path)
cat("\n", msg, sep = "")
cat("Pick file:", pick_path, "\n")
