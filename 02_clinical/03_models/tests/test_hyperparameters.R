invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
args <- commandArgs(trailingOnly = TRUE)
model_root <- args[1]; test_root <- args[2]
source(file.path(model_root, "hyperparameter_common.R"), encoding = "UTF-8")
cfg <- hp_design(file.path(model_root, "unified_pipeline", "00_config.R"))
stopifnot(nrow(cfg$rsf_grid) == 108, nrow(cfg$rsflc_grid) == 36)
must_fail <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))

# Incomplete high-scoring combinations cannot win.
grid <- data.frame(ntree=c(10L,20L),mtry=1L,nodesize=10L,nsplit=2L)
raw <- data.frame(grid_id=rep(1:2,each=5), fold=rep(1:5,2),
  C_index=c(rep(.99,5),rep(.6,5)),AUC=.7,Brier=.2,status=c("failed",rep("ok",9)))
rank <- hp_rank(raw,grid); stopifnot(rank$grid_id[1]==2, !rank$eligible[2])
stopifnot(identical(hp_step_risk(c(.2,.8),matrix(c(.1,.9),nrow=1),.5),.1))
pp <- hp_preprocess(data.frame(x=c(1,NA,3)), data.frame(x=c(NA,1000)), "x", character())
stopifnot(pp$valid$x[1]==2,pp$rules$x$fill==2)

set.seed(46)
n <- 100L
d <- data.frame(subject_id=10000L+seq_len(n),hadm_id=20000L+seq_len(n),group=1L,
  fold=rep(1:5,length.out=n))
for (v in cfg$static31) d[[v]] <- stats::rnorm(n)
for (v in cfg$factor_vars) d[[v]] <- factor(rep(c(0,1),length.out=n))
d$status28 <- rep(c(0L,1L),length.out=n)
d$time_u <- ifelse(d$status28==0L,1,stats::runif(n,.3,.95))
d$lm_status <- d$status28; d$lm_time_u <- d$time_u
d$age[c(3,14,25)] <- NA_real_
bad <- d; bad$subject_id[2] <- bad$subject_id[1]
must_fail(hp_training_data(bad,cfg$static31,"time_u","status28"))
bad <- d; bad$group[2] <- 2L; bad$subject_id[2] <- bad$subject_id[1]
must_fail(hp_training_data(bad,cfg$static31,"time_u","status28"))
# Holdout outcomes/features are deliberately invalid and must never be used.
held <- d[1:5,]; held$hadm_id <- held$hadm_id+1000L; held$subject_id <- held$subject_id+1000L
held$group <- 2L;held$time_u <- NA_real_; held$lm_time_u <- NA_real_
base <- rbind(d,held)
long <- data.frame(hadm_id=rep(d$hadm_id,each=3),time_u=rep(c(1,3,5)/28,n))
for (v in cfg$traj3) long[[v]] <- rep(stats::rnorm(n),each=3)+long$time_u+stats::rnorm(3*n,sd=.2)
cache <- file.path(test_root,"synthetic_cache");dir.create(cache,recursive=TRUE,showWarnings=FALSE)
saveRDS(base,file.path(cache,"static_data.rds"))
saveRDS(base,file.path(cache,"dynamic_base.rds"))
saveRDS(long,file.path(cache,"dynamic_long.rds"))
rsf_best <- hp_run("RSF",cache,file.path(test_root,"rsf_run"),cfg,cores=1L,grid=grid[1,,drop=FALSE])
rsflc_grid <- data.frame(ntree=5L,mtry=1L,nodesize=15L)
rsflc_best <- hp_run("RSFLC",cache,file.path(test_root,"rsflc_run"),cfg,cores=1L,grid=rsflc_grid)
for (model in c("rsf","rsflc")) {
  run <- file.path(test_root,paste0(model,"_run"))
  tab <- read.csv(file.path(run,"cv_folds.csv"))
  stopifnot(nrow(tab)==5,all(tab$status=="ok"))
  b <- readRDS(file.path(run,"best_model_bundle.rds"))
  stopifnot(b$training_group==1L, nrow(b$selected_parameters)==1L)
  expected <- if (model=="rsf") 10L else 5L
  stopifnot(b$selected_parameters$ntree==expected)
}
must_fail(hp_run("RSF",cache,file.path(test_root,"rsf_run"),cfg,cores=1L,grid=grid[1,,drop=FALSE]))
cat("PASS: grid counts; incomplete-fold rejection; step lookup; fold-local imputation; subject leakage guards; both real-package fivefold/refit smoke tests; output collision guard\n")
