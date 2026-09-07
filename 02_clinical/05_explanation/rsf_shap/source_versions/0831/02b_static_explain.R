suppressPackageStartupMessages({
  library(dplyr); library(survival); library(randomForestSRC); library(riskRegression)
  library(ggplot2); library(shapviz); library(patchwork); library(tidyr)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
d <- readRDS(file.path(out_dir, "models", "static_data.rds"))
train0 <- d %>% filter(group == 1L); valid0 <- d %>% filter(group == 2L)
train <- train0 %>% select(-hadm_id, -group) %>% as.data.frame()
valid <- valid0 %>% select(-hadm_id, -group) %>% as.data.frame()
for (nm in intersect(factor_vars, names(train))) {
  train[[nm]] <- factor(train[[nm]])
  valid[[nm]] <- factor(valid[[nm]], levels = levels(train[[nm]]))
}
cox_fit <- readRDS(file.path(out_dir, "models", "cox_static.rds"))
rsf_fit <- readRDS(file.path(out_dir, "models", "rsf_static.rds"))
predict_rsf_28 <- function(object, newdata) {
  pr <- predict(object, newdata = newdata, na.action = "na.impute")
  j <- which.min(abs(pr$time.interest - horizon)); 1 - pr$survival[, j]
}
predict_rsf_matrix <- function(newdata, times) {
  pr <- predict(rsf_fit, newdata = newdata, na.action = "na.impute")
  t(vapply(seq_len(nrow(newdata)), function(i) 1 - step_at(pr$time.interest, pr$survival[i, ], times, initial = 1), numeric(length(times))))
}

train_x <- train[, static31, drop = FALSE]; valid_x <- valid[, static31, drop = FALSE]
base_valid_risk <- predict_rsf_28(rsf_fit, valid_x)
hi <- which.max(ifelse(valid$status28 == 1L, base_valid_risk, -Inf))
lo <- which.min(ifelse(valid$status28 == 0L, base_valid_risk, Inf))
set.seed(seed_value); remain <- setdiff(seq_len(nrow(valid_x)), c(hi, lo))
idx <- unique(c(hi, lo, sample(remain, min(shap_n - 2L, length(remain)))))
shap_x <- valid_x[idx, , drop = FALSE]
set.seed(seed_value); bg <- train_x[sample(seq_len(nrow(train_x)), min(shap_background_n, nrow(train_x))), , drop = FALSE]

# Permutation-path Monte Carlo SHAP. For each patient and simulation, a random
# feature ordering is traversed from a sampled background row to the patient.
# All 2*p*n hybrid rows are predicted in one vectorized forest call.
vectorized_mc_shap <- function(object, newdata, background, nsim) {
  n <- nrow(newdata); p <- ncol(newdata); vars <- names(newdata)
  acc <- matrix(0, n, p, dimnames = list(rownames(newdata), vars))
  set.seed(seed_value)
  for (s in seq_len(nsim)) {
    blocks <- vector("list", n)
    orders <- vector("list", n)
    for (i in seq_len(n)) {
      current <- background[sample.int(nrow(background), 1L), , drop = FALSE]
      ord <- sample.int(p); orders[[i]] <- ord
      rows <- current[rep(1L, 2L * p), , drop = FALSE]
      for (k in seq_len(p)) {
        j <- ord[k]
        rows[2L * k - 1L, ] <- current
        current[[j]] <- newdata[[j]][i]
        rows[2L * k, ] <- current
      }
      blocks[[i]] <- rows
    }
    z <- bind_rows(blocks)
    pr <- predict_rsf_28(object, z)
    cursor <- 0L
    for (i in seq_len(n)) {
      vals <- pr[cursor + seq_len(2L * p)]
      delta <- vals[seq(2L, 2L * p, by = 2L)] - vals[seq(1L, 2L * p - 1L, by = 2L)]
      acc[i, orders[[i]]] <- acc[i, orders[[i]]] + delta
      cursor <- cursor + 2L * p
    }
    message("RSF vectorized SHAP simulation ", s, "/", nsim)
    rm(z, blocks, pr)
    invisible(gc(FALSE))
  }
  acc / nsim
}

shap_chunks <- split(seq_len(nrow(shap_x)), ceiling(seq_len(nrow(shap_x)) / 20L))
shap_mat <- do.call(rbind, lapply(seq_along(shap_chunks), function(k) {
  message("RSF SHAP patient chunk ", k, "/", length(shap_chunks))
  vectorized_mc_shap(rsf_fit, shap_x[shap_chunks[[k]], , drop = FALSE], bg, shap_nsim)
}))
sv <- shapviz::shapviz(shap_mat, X = shap_x, baseline = mean(predict_rsf_28(rsf_fit, bg)))
shap_imp <- data.frame(variable = colnames(shap_mat), mean_abs_SHAP = colMeans(abs(shap_mat)), mean_SHAP = colMeans(shap_mat)) %>% arrange(desc(mean_abs_SHAP))
write_utf8(shap_imp, file.path(out_dir, "tables", "rsf_SHAP_importance.csv"))
saveRDS(list(values = shap_mat, X = shap_x, indices = idx), file.path(out_dir, "models", "rsf_SHAP.rds"))
write_utf8(data.frame(patient = c("high_risk_death", "low_risk_survivor"),
                      hadm_id = valid0$hadm_id[c(hi, lo)], observed_time = valid$time28[c(hi, lo)],
                      status = valid$status28[c(hi, lo)], risk28 = base_valid_risk[c(hi, lo)]),
           file.path(out_dir, "tables", "rsf_typical_patients.csv"))

vimp_tbl <- read.csv(file.path(out_dir, "tables", "rsf_OOB_VIMP.csv"))
md_tbl <- read.csv(file.path(out_dir, "tables", "rsf_minimal_depth.csv"))
perm_summary <- read.csv(file.path(out_dir, "tables", "rsf_validation_permutation_VIMP.csv"))
top4 <- head(vimp_tbl$variable, 4)
pdp_tbl <- bind_rows(lapply(top4, function(nm) {
  grid <- if (is.numeric(valid_x[[nm]])) unique(as.numeric(quantile(valid_x[[nm]], seq(.05,.95,length.out=20), na.rm=TRUE))) else levels(valid_x[[nm]])
  bind_rows(lapply(grid, function(z) {
    x <- valid_x
    x[[nm]] <- if (is.factor(x[[nm]])) factor(z, levels=levels(x[[nm]])) else as.numeric(z)
    data.frame(variable=nm, value=as.character(z), risk28=mean(predict_rsf_28(rsf_fit,x)))
  }))
}))
write_utf8(pdp_tbl, file.path(out_dir, "tables", "rsf_PDP.csv"))

theme_set(theme_minimal(base_size=11))
ggsave(file.path(out_dir,"figures","RSF_01_OOB_convergence.png"),
       ggplot(data.frame(tree=seq_along(rsf_fit$err.rate),C=1-as.numeric(rsf_fit$err.rate)),aes(tree,C))+geom_line(colour="#2F75B5")+labs(x="Number of trees",y="OOB C-index",title="RSF OOB convergence"),width=7,height=4.6,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_02_OOB_VIMP.png"),ggplot(head(vimp_tbl,20),aes(reorder(variable,OOB_VIMP),OOB_VIMP))+geom_col(fill="#2F75B5")+coord_flip()+labs(x=NULL,y="OOB permutation VIMP",title="RSF OOB variable importance"),width=7,height=5.6,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_03_validation_permutation_VIMP.png"),ggplot(head(perm_summary,20),aes(reorder(variable,mean_delta_C),mean_delta_C))+geom_col(fill="#70AD47")+coord_flip()+labs(x=NULL,y="Mean decrease in validation C-index",title="RSF validation permutation importance"),width=7,height=5.6,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_04_minimal_depth.png"),ggplot(head(md_tbl,20),aes(reorder(variable,-minimal_depth),minimal_depth))+geom_col(fill="#A5A5A5")+coord_flip()+labs(x=NULL,y="Minimal depth",title="RSF minimal depth"),width=7,height=5.6,dpi=300)
pdp_plots <- lapply(split(pdp_tbl,pdp_tbl$variable),function(z){z$value_num<-suppressWarnings(as.numeric(z$value));if(all(is.finite(z$value_num)))ggplot(z,aes(value_num,risk28))+geom_line(colour="#2F75B5")+geom_point(colour="#2F75B5")+labs(x=z$variable[1],y="Mean 28-day risk") else ggplot(z,aes(value,risk28))+geom_col(fill="#2F75B5")+labs(x=z$variable[1],y="Mean 28-day risk")})
ggsave(file.path(out_dir,"figures","RSF_05_PDP.png"),wrap_plots(pdp_plots,ncol=2)+plot_annotation(title="RSF partial dependence for 28-day mortality"),width=8,height=6.5,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_06_SHAP_beeswarm.png"),sv_importance(sv,kind="beeswarm",max_display=20)+ggtitle(sprintf("RSF SHAP (n=%d, nsim=%d)",length(idx),shap_nsim)),width=7.3,height=6,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_07_typical_patient_SHAP.png"),sv_waterfall(sv,row_id=1,max_display=12)/sv_waterfall(sv,row_id=2,max_display=12),width=7.3,height=9,dpi=300)
ggsave(file.path(out_dir,"figures","RSF_08_SHAP_dependence.png"),sv_dependence(sv,v=shap_imp$variable[1],color_var="auto")+ggtitle("RSF SHAP dependence"),width=7,height=5,dpi=300)

cox_risk <- as.matrix(riskRegression::predictRisk(cox_fit,newdata=valid,times=static_times))
rsf_risk <- predict_rsf_matrix(valid,static_times)
cal <- bind_rows(transform(calibration_table(cox_risk[,ncol(cox_risk)],valid$status28),model="COX"),transform(calibration_table(rsf_risk[,ncol(rsf_risk)],valid$status28),model="RSF"))
write_utf8(cal,file.path(out_dir,"tables","static_calibration.csv"))
ggsave(file.path(out_dir,"figures","STATIC_09_calibration.png"),ggplot(cal,aes(predicted,observed,colour=model))+geom_abline(slope=1,intercept=0,linetype=2)+geom_line()+geom_point()+coord_equal()+labs(x="Predicted 28-day risk",y="Observed 28-day mortality",title="Static-task validation calibration"),width=7,height=5.5,dpi=300)
dca <- bind_rows(transform(dca_table(cox_risk[,ncol(cox_risk)],valid$status28),model_name="COX"),transform(dca_table(rsf_risk[,ncol(rsf_risk)],valid$status28),model_name="RSF"))
write_utf8(dca,file.path(out_dir,"tables","static_DCA.csv"))
ggsave(file.path(out_dir,"figures","STATIC_10_DCA.png"),ggplot(dca,aes(threshold,model,colour=model_name))+geom_line()+geom_line(aes(y=treat_all),colour="grey40",linetype=2)+geom_hline(yintercept=0,linetype=3)+labs(x="Threshold probability",y="Net benefit",title="Static-task decision curve analysis"),width=7,height=5.5,dpi=300)
curve_tbl <- read.csv(file.path(out_dir,"tables","static_time_metrics.csv"))
long_curve <- pivot_longer(curve_tbl,c(AUC,Brier),names_to="metric",values_to="value")
ggsave(file.path(out_dir,"figures","STATIC_11_time_metrics.png"),ggplot(long_curve,aes(time,value,colour=model))+geom_line()+facet_wrap(~metric,scales="free_y")+labs(x="Days",y=NULL,title="Static-task time-dependent performance"),width=8,height=4.8,dpi=300)
cat("STATIC_EXPLAIN_OK\n")
