library(readxl)
library(dplyr)
library(survival)
library(nlme)
library(JMbayes2)

# Failing case from user: sim500_70_10V_lowBTW with t0=0.5
path <- "F:/文章/大论文/程序Trae/模拟数据_添加Y/sim500_70_10V_lowBTW_1c_L1.xlsx"
data <- as.data.frame(read_xlsx(path, sheet = 1))
data_clean <- data[data$t <= data$obs_time, , drop = FALSE]
survData <- data_clean[!duplicated(data_clean$ID), ]

cat("clean n_id:", nrow(survData), " n_event:", sum(survData$event),
    " max_obs:", max(survData$obs_time), "\n")

# JMbayes2 tvAUC requirements more carefully
t0 <- 0.5; Dt <- 0.5
# at risk at Tstart: obs_time > Tstart
at_risk <- survData$obs_time > t0
# had long meas before/at Tstart
ids_long <- unique(data_clean$ID[data_clean$t <= t0])
# event in (Tstart, Thoriz]
evt_win <- survData$event == 1 & survData$obs_time > t0 & survData$obs_time <= (t0 + Dt)
evt_win2 <- survData$event == 1 & survData$obs_time >= t0 & survData$obs_time < (t0 + Dt)

cat("at_risk after Tstart:", sum(at_risk), "\n")
cat("IDs with long <=Tstart:", length(ids_long), "\n")
cat("events in (Tstart, Thoriz]:", sum(evt_win), "\n")
cat("events in [Tstart, Thoriz):", sum(evt_win2), "\n")
cat("at_risk & long before:", length(intersect(survData$ID[at_risk], ids_long)), "\n")

# After clean, how many long meas strictly before Tstart for at-risk subjects?
dc <- data_clean
for (tt in c(0.3, 0.4, 0.5)) {
  ar <- survData$ID[survData$obs_time > tt]
  long_b <- unique(dc$ID[dc$t < tt])  # strictly before
  long_be <- unique(dc$ID[dc$t <= tt])
  n_evt <- sum(survData$event == 1 & survData$obs_time > tt & survData$obs_time <= tt + 0.5)
  cat(sprintf("t0=%.1f Dt=0.5 | at_risk=%d long<t0=%d long<=t0=%d intersect_le=%d events_win=%d\n",
              tt, length(ar), length(long_b), length(long_be),
              length(intersect(ar, long_be)), n_evt))
}

cat("\nFitting short jm...\n")
fm1 <- lme(V1 ~ t, data = data_clean, random = ~ 1 | ID)
fm2 <- lme(V2 ~ t, data = data_clean, random = ~ 1 | ID)
fm3 <- lme(V3 ~ t, data = data_clean, random = ~ 1 | ID)
fm4 <- lme(V4 ~ t, data = data_clean, random = ~ 1 | ID)
fm5 <- lme(V5 ~ t, data = data_clean, random = ~ 1 | ID)
fm6 <- lme(V6 ~ t, data = data_clean, random = ~ 1 | ID)
coxFit <- coxph(Surv(obs_time, event) ~ 1, data = survData, x = TRUE)
jmFit <- jm(coxFit, list(fm1, fm2, fm3, fm4, fm5, fm6), time_var = "t",
            n_iter = 500L, n_burnin = 150L)
cat("jm OK\n")

try_tv <- function(t0, Dt) {
  tryCatch({
    a <- tvAUC(jmFit, newdata = data_clean, Tstart = t0, Dt = Dt, cores = 1L)
    b <- tvBrier(jmFit, newdata = data_clean, Tstart = t0, Dt = Dt, cores = 1L)
    cat(sprintf("SUCCESS t0=%.2f Dt=%.2f AUC=%.4f BS=%.4f\n", t0, Dt, a$auc, b$Brier))
  }, error = function(e) {
    cat(sprintf("FAIL t0=%.2f Dt=%.2f : %s\n", t0, Dt, conditionMessage(e)))
  })
}

try_tv(0.5, 0.5)
try_tv(0.4, 0.5)
try_tv(0.3, 0.5)
try_tv(0.2, 0.5)
try_tv(0.3, 0.3)
