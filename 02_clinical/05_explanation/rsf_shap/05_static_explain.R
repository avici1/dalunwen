suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(randomForestSRC)
  library(ggplot2)
  library(shapviz)
  library(patchwork)
  library(tidyr)
})
script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = getwd())
source(file.path(script_dir, "00_config.R"), encoding = "UTF-8")
log_progress("STATIC_EXPLAIN_START", "RSF SHAP, typical patients, PDP, calibration and DCA")

d <- readRDS(file.path(cache_dir, "static_data.rds"))
preds <- readRDS(file.path(cache_dir, "static_predictions.rds"))
rsf_fit <- readRDS(file.path(cache_dir, "rsf_static_31.rds"))
train0 <- d %>% filter(group == 1L)
valid0 <- d %>% filter(group == 2L)
train_x <- train0[, static31, drop = FALSE] %>% as.data.frame()
valid_x <- valid0[, static31, drop = FALSE] %>% as.data.frame()
for (nm in intersect(factor_vars, static31)) {
  train_x[[nm]] <- factor(train_x[[nm]])
  valid_x[[nm]] <- factor(valid_x[[nm]], levels = levels(train_x[[nm]]))
}

predict_rsf_u1 <- function(object, newdata) {
  pr <- predict(object, newdata = newdata, na.action = "na.impute")
  j <- which.min(abs(pr$time.interest - 1))
  1 - pr$survival[, j]
}

base_valid_risk <- predict_rsf_u1(rsf_fit, valid_x)
hi <- which.max(ifelse(valid0$status28 == 1L, base_valid_risk, -Inf))
lo <- which.min(ifelse(valid0$status28 == 0L, base_valid_risk, Inf))
set.seed(seed_value)
remain <- setdiff(seq_len(nrow(valid_x)), c(hi, lo))
idx <- unique(c(hi, lo, sample(remain, min(rsf_shap_n - 2L, length(remain)))))
shap_x <- valid_x[idx, , drop = FALSE]
set.seed(seed_value)
bg <- train_x[sample(seq_len(nrow(train_x)), min(rsf_shap_background_n, nrow(train_x))), , drop = FALSE]

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
    pr <- predict_rsf_u1(object, z)
    cursor <- 0L
    for (i in seq_len(n)) {
      vals <- pr[cursor + seq_len(2L * p)]
      delta <- vals[seq(2L, 2L * p, by = 2L)] - vals[seq(1L, 2L * p - 1L, by = 2L)]
      acc[i, orders[[i]]] <- acc[i, orders[[i]]] + delta
      cursor <- cursor + 2L * p
    }
    if (s %% 5L == 0L || s == nsim) log_progress("STATIC_SHAP", sprintf("simulation %d/%d", s, nsim))
    rm(z, blocks, pr); invisible(gc(FALSE))
  }
  acc / nsim
}

shap_path <- file.path(cache_dir, "rsf_SHAP_200x50.rds")
if (file.exists(shap_path)) {
  shap_obj <- readRDS(shap_path)
  shap_mat <- shap_obj$values
  shap_x <- shap_obj$X
} else {
  shap_chunks <- split(seq_len(nrow(shap_x)), ceiling(seq_len(nrow(shap_x)) / 20L))
  chunk_dir <- file.path(cache_dir, "rsf_shap_chunks")
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
  shap_mat <- do.call(rbind, lapply(seq_along(shap_chunks), function(k) {
    chunk_path <- file.path(chunk_dir, sprintf("chunk_%02d.rds", k))
    if (file.exists(chunk_path)) {
      log_progress("STATIC_SHAP", sprintf("load checkpoint chunk %d/%d", k, length(shap_chunks)))
      return(readRDS(chunk_path))
    }
    log_progress("STATIC_SHAP", sprintf("patient chunk %d/%d", k, length(shap_chunks)))
    z <- vectorized_mc_shap(rsf_fit, shap_x[shap_chunks[[k]], , drop = FALSE], bg, rsf_shap_nsim)
    saveRDS(z, chunk_path)
    z
  }))
  saveRDS(list(values = shap_mat, X = shap_x, indices = idx), shap_path)
}

sv <- shapviz(shap_mat, X = shap_x, baseline = mean(predict_rsf_u1(rsf_fit, bg)))
shap_imp <- data.frame(
  variable = colnames(shap_mat), mean_abs_SHAP = colMeans(abs(shap_mat)),
  mean_SHAP = colMeans(shap_mat)
) %>% arrange(desc(mean_abs_SHAP))
write_utf8(shap_imp, file.path(artifact_dir, "RSF_SHAP\u5168\u5c40\u91cd\u8981\u6027.csv"))

typical <- data.frame(
  patient = c("high_risk_death", "low_risk_survivor"),
  subject_id = valid0$subject_id[c(hi, lo)], hadm_id = valid0$hadm_id[c(hi, lo)],
  observed_days = valid0$time_days[c(hi, lo)], status = valid0$status28[c(hi, lo)],
  predicted_risk_u1 = base_valid_risk[c(hi, lo)]
)
write_utf8(typical, file.path(artifact_dir, "RSF_\u5178\u578b\u60a3\u8005.csv"))

vimp_tbl <- read.csv(file.path(artifact_dir, "RSF_OOB_VIMP.csv"), check.names = FALSE)
md_tbl <- read.csv(file.path(artifact_dir, "RSF_\u6700\u5c0f\u6df1\u5ea6.csv"), check.names = FALSE)
top4 <- head(vimp_tbl$variable, 4L)
pdp_tbl <- bind_rows(lapply(top4, function(nm) {
  grid <- if (is.numeric(valid_x[[nm]])) {
    unique(as.numeric(quantile(valid_x[[nm]], seq(0.05, 0.95, length.out = 20L), na.rm = TRUE)))
  } else levels(valid_x[[nm]])
  bind_rows(lapply(grid, function(z) {
    x <- valid_x
    x[[nm]] <- if (is.factor(x[[nm]])) factor(z, levels = levels(x[[nm]])) else as.numeric(z)
    data.frame(variable = nm, value = as.character(z), risk_u1 = mean(predict_rsf_u1(rsf_fit, x)))
  }))
}))
write_utf8(pdp_tbl, file.path(artifact_dir, "RSF_PDP_u1.csv"))

table_5_5a <- full_join(vimp_tbl, md_tbl, by = "variable") %>%
  full_join(shap_imp, by = "variable") %>%
  mutate(variable_label = unname(cn_labels[variable])) %>%
  arrange(desc(OOB_VIMP)) %>%
  select(variable, variable_label, OOB_VIMP, minimal_depth, mean_abs_SHAP, mean_SHAP)
write_utf8(table_5_5a, file.path(artifact_dir, "\u88685-5A_RSF\u4e3b\u8981\u53d8\u91cf\u89e3\u91ca\u7ed3\u679c.csv"))

theme_set(theme_minimal(base_size = 11, base_family = "Microsoft YaHei"))
p_conv <- ggplot(data.frame(tree = seq_along(rsf_fit$err.rate), C = 1 - as.numeric(rsf_fit$err.rate)), aes(tree, C)) +
  geom_line(colour = "#2F75B5", linewidth = 0.7) +
  labs(x = "\u6811\u7684\u6570\u91cf", y = "OOB C-index", title = "A  OOB\u6536\u655b")
p_vimp <- ggplot(head(vimp_tbl, 20L), aes(reorder(variable, OOB_VIMP), OOB_VIMP)) +
  geom_col(fill = "#2F75B5") + coord_flip() +
  scale_x_discrete(labels = function(x) ifelse(is.na(cn_labels[x]), x, cn_labels[x])) +
  labs(x = NULL, y = "OOB permutation VIMP", title = "B  \u53d8\u91cf\u91cd\u8981\u6027")
ggsave(file.path(artifact_dir, "\u56fe5-2_RSF_OOB\u6536\u655b\u4e0e\u7f6e\u6362\u53d8\u91cf\u91cd\u8981\u6027.png"),
       p_conv / p_vimp, width = 8.2, height = 9.0, dpi = 320, bg = "white")

pdp_plots <- lapply(split(pdp_tbl, pdp_tbl$variable), function(z) {
  z$value_num <- suppressWarnings(as.numeric(z$value))
  xlab <- unname(cn_labels[z$variable[1]])
  if (all(is.finite(z$value_num))) {
    ggplot(z, aes(value_num, risk_u1)) + geom_line(colour = "#2F75B5") + geom_point(colour = "#2F75B5") + labs(x = xlab, y = "u=1\u6b7b\u4ea1\u98ce\u9669")
  } else {
    ggplot(z, aes(value, risk_u1)) + geom_col(fill = "#2F75B5") + labs(x = xlab, y = "u=1\u6b7b\u4ea1\u98ce\u9669")
  }
})
ggsave(file.path(artifact_dir, "\u56fe5-3_RSF\u4e3b\u8981\u53d8\u91cfu1\u6b7b\u4ea1\u98ce\u9669PDP.png"),
       wrap_plots(pdp_plots, ncol = 2L), width = 8.2, height = 6.5, dpi = 320, bg = "white")

ggsave(file.path(artifact_dir, "\u56fe5-4_RSF\u9a8c\u8bc1\u96c6\u5168\u5c40SHAP\u5206\u5e03.png"),
       sv_importance(sv, kind = "beeswarm", max_display = 20L) +
         ggtitle(sprintf("RSF SHAP\uff08n=%d\uff0cnsim=%d\uff09", nrow(shap_x), rsf_shap_nsim)),
       width = 7.5, height = 6.2, dpi = 320, bg = "white")

ggsave(file.path(artifact_dir, "\u56fe5-5_RSF\u5178\u578b\u60a3\u8005\u5c40\u90e8SHAP\u89e3\u91ca.png"),
       sv_waterfall(sv, row_id = 1L, max_display = 12L) /
         sv_waterfall(sv, row_id = 2L, max_display = 12L),
       width = 7.6, height = 9.0, dpi = 320, bg = "white")

cal <- bind_rows(
  transform(calibration_table(preds$cox_validation_risk[, ncol(preds$cox_validation_risk)], valid0$status28), model = "COX"),
  transform(calibration_table(preds$rsf_validation_risk[, ncol(preds$rsf_validation_risk)], valid0$status28), model = "RSF")
)
write_utf8(cal, file.path(artifact_dir, "\u9759\u6001\u4efb\u52a1_\u6821\u51c6\u6570\u636e.csv"))
dca <- bind_rows(
  transform(dca_table(preds$cox_validation_risk[, ncol(preds$cox_validation_risk)], valid0$status28), model_name = "COX"),
  transform(dca_table(preds$rsf_validation_risk[, ncol(preds$rsf_validation_risk)], valid0$status28), model_name = "RSF")
)
write_utf8(dca, file.path(artifact_dir, "\u9759\u6001\u4efb\u52a1_DCA\u6570\u636e.csv"))

p_cal <- ggplot(cal, aes(predicted, observed, colour = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.8) + geom_point(size = 2) + coord_equal() +
  labs(x = "\u9884\u6d4bu=1\u6b7b\u4ea1\u98ce\u9669", y = "\u89c2\u5bdf28\u65e5\u6b7b\u4ea1\u7387", colour = NULL)
ggsave(file.path(artifact_dir, "\u56fe5-10_\u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u6821\u51c6\u56fe.png"),
       p_cal, width = 7.0, height = 5.6, dpi = 320, bg = "white")

dca_long <- dca %>% pivot_longer(c(model, treat_all, treat_none), names_to = "strategy", values_to = "net_benefit")
p_dca <- ggplot(dca_long, aes(threshold, net_benefit,
                              colour = ifelse(strategy == "model", model_name, strategy),
                              linetype = strategy == "model")) +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(values = c(`TRUE` = 1, `FALSE` = 2), guide = "none") +
  coord_cartesian(xlim = c(0.01, 0.30), ylim = c(-0.02, 0.16)) +
  labs(x = "\u9608\u503c\u6982\u7387", y = "\u51c0\u53d7\u76ca", colour = NULL)
ggsave(file.path(artifact_dir, "\u56fe5-11_\u9759\u6001\u4efb\u52a1\u9a8c\u8bc1\u96c6u1\u51b3\u7b56\u66f2\u7ebf.png"),
       p_dca, width = 7.2, height = 5.6, dpi = 320, bg = "white")

log_progress("STATIC_EXPLAIN_OK", sprintf("SHAP n=%d nsim=%d", nrow(shap_x), rsf_shap_nsim))
cat("STATIC_EXPLAIN_OK\n")
