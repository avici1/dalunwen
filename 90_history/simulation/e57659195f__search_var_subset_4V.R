# V1–V4 全部非空子集（15 种）；纵向固定 Vi ~ t, random ~ 1 | ID
# 评估写死 t0=0.5, Dt=1
# 用法: Rscript _search_var_subset_4V.R

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
all_vars <- c("V1", "V2", "V3", "V4")

cat("dataset:", nm, "\n")
cat("n_id:", nrow(survData), " n_event:", sum(survData$event), "\n")
cat("eval: t0=", t0, " Dt=", Dt, "\n", sep = "")

# 15 个非空子集
mask <- expand.grid(
  V1 = c(FALSE, TRUE),
  V2 = c(FALSE, TRUE),
  V3 = c(FALSE, TRUE),
  V4 = c(FALSE, TRUE)
)
mask <- mask[rowSums(mask) > 0, , drop = FALSE]
rownames(mask) <- NULL

fit_lme_var <- function(var, data) {
  fml <- as.formula(paste(var, "~ t"))
  lme(fml, data = data, random = ~ 1 | ID)
}

run_one <- function(use_vars) {
  label <- paste(use_vars, collapse = "")
  n_var <- length(use_vars)
  cat("\n===== ", label, " (k=", n_var, ") =====\n", sep = "")
  t_run <- Sys.time()
  out <- tryCatch({
    fms <- lapply(use_vars, fit_lme_var, data = data_clean)
    coxFit <- coxph(Surv(obs_time, event) ~ 1, data = survData, x = TRUE)
    jmFit <- jm(coxFit, fms, time_var = "t")

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
  elapsed <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))
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
    n_var = n_var,
    use_V1 = "V1" %in% use_vars,
    use_V2 = "V2" %in% use_vars,
    use_V3 = "V3" %in% use_vars,
    use_V4 = "V4" %in% use_vars,
    AUC = out$AUC,
    BS = out$BS,
    Cindex = out$Cindex,
    Time = elapsed,
    ok = isTRUE(out$ok),
    err = if (is.null(out$err)) NA_character_ else out$err,
    stringsAsFactors = FALSE
  )
}

rows <- lapply(seq_len(nrow(mask)), function(i) {
  use_vars <- all_vars[as.logical(unlist(mask[i, ]))]
  run_one(use_vars)
})
tab <- do.call(rbind, rows)
rownames(tab) <- NULL

out_csv <- "F:/文章_大论文/0722/模拟研究代码/_search_var_subset_4V_result.csv"
out_xlsx <- "F:/文章_大论文/0722/模拟研究代码/_search_var_subset_4V_result.xlsx"
write.csv(tab, out_csv, row.names = FALSE)
write_xlsx(tab, out_xlsx)
cat("\nSaved:", out_csv, "\n")
ord <- order(
  -as.integer(tab$ok),
  -ifelse(is.na(tab$AUC), -Inf, tab$AUC),
  -ifelse(is.na(tab$Cindex), -Inf, tab$Cindex),
  ifelse(is.na(tab$BS), Inf, tab$BS),
  tab$n_var
)
print(tab[ord, ])

cand <- tab[tab$ok & !is.na(tab$AUC), , drop = FALSE]
pick_path <- "F:/文章_大论文/0722/模拟研究代码/_search_var_subset_4V_pick.txt"
if (nrow(cand) == 0) {
  msg <- "NO_OK: all 15 subsets failed.\n"
  writeLines(msg, pick_path)
  cat(msg)
  quit(status = 2)
}

cand <- cand[order(-cand$AUC, -cand$Cindex, cand$BS, cand$n_var), ]
pick <- cand[1, , drop = FALSE]
vars_str <- paste(all_vars[c(pick$use_V1, pick$use_V2, pick$use_V3, pick$use_V4)], collapse = ",")

msg <- sprintf(
  paste0(
    "PICK label=%s vars=%s n_var=%d\n",
    "AUC=%.4f BS=%g Cindex=%.4f Time=%.1f\n",
    "eval t0=%.1f Dt=%.1f\n"
  ),
  pick$label, vars_str, pick$n_var,
  pick$AUC, pick$BS, pick$Cindex, pick$Time,
  t0, Dt
)
writeLines(msg, pick_path)
cat("\n", msg, sep = "")
cat("Pick file:", pick_path, "\n")
