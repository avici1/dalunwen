# =============================================================================
# 查找「协变量时间 t > obs_time 被剔除」的个体示例
# 用于验证 filter(t <= obs_time) 的实际效果（与 4_补充_模拟数据_10V1C 一致）
# =============================================================================

library(tidyverse)

# 与 10V1C 中 sim_single_class 相同的纵向时间结构：t 在 [0, ~0.25, ~0.5, ~0.75, ~1]
run_mini_example <- function(seed = 12345) {
  set.seed(seed)
  n <- 500
  n_time <- 5
  t_mat <- matrix(NA, nrow = n, ncol = n_time)
  t_mat[, 1] <- 0
  t_mat[, 2] <- runif(n, 0, 0.25)
  t_mat[, 3] <- runif(n, 0.25, 0.5)
  t_mat[, 4] <- runif(n, 0.5, 0.75)
  t_mat[, 5] <- runif(n, 0.75, 1)
  t_mat <- t(apply(t_mat, 1, sort))
  long <- data.frame(
    ID = rep(1:n, each = n_time),
    t = c(t(t_mat)),
    V1 = rnorm(n * n_time, 0.5, 0.1),
    class = "c1"
  )
  # 生存：与 10V1C add_surv 类似，target_censor=0.3 时部分个体 obs_time 较小
  surv_t <- 2 * (-log(runif(n)))^1  # 简化的 Weibull
  censor_t <- runif(n, 0, quantile(surv_t, 0.8) * 1.5)  # 约 30% 删失
  obs_t <- pmin(surv_t, censor_t)
  surv_df <- data.frame(ID = 1:n, surv_time = surv_t, censor_time = censor_t,
                        obs_time = obs_t, event = as.integer(surv_t <= censor_t))
  merged <- long %>% left_join(surv_df, by = "ID")
  filtered <- merged %>% filter(t <= obs_time)
  ids_removed <- unique(merged$ID[merged$t > merged$obs_time])
  if (length(ids_removed) == 0) return(run_mini_example(seed + 1))
  ex_id <- ids_removed[1]
  list(
    before = merged %>% filter(ID == ex_id) %>% arrange(t),
    after = filtered %>% filter(ID == ex_id) %>% arrange(t),
    surv_info = surv_df %>% filter(ID == ex_id),
    ex_id = ex_id,
    n_removed_total = nrow(merged) - nrow(filtered),
    n_ids_affected = length(ids_removed)
  )
}

result <- run_mini_example()

# ========== 输出示例 ==========
cat("\n========== 协变量被剔除的个体示例 ==========\n\n")
cat("个体 ID:", result$ex_id, "\n")
cat("该个体 obs_time (删失/死亡时刻):", result$surv_info$obs_time, "\n")
cat("该个体 event (1=死亡, 0=删失):", result$surv_info$event, "\n\n")

cat("【 filter 前 】原始纵向观测（含 t > obs_time 的无效行）:\n")
print(result$before %>% dplyr::select(ID, t, obs_time, event))

cat("\n【 filter 后 】保留的观测（仅 t <= obs_time）:\n")
print(result$after %>% dplyr::select(ID, t, obs_time, event))

cat("\n【 被剔除的行 】(t > obs_time):\n")
dropped <- result$before %>% filter(t > result$surv_info$obs_time)
if (nrow(dropped) > 0) {
  print(dropped %>% dplyr::select(ID, t, obs_time))
  cat("\n说明: 以上", nrow(dropped), "行因 协变量时间 t(", 
      paste(round(dropped$t, 4), collapse = ", "), 
      ") > obs_time(", result$surv_info$obs_time, 
      ") 被 filter(t <= obs_time) 剔除，不会出现在 xlsx 中。\n")
} else {
  cat("(无 - 本示例中该个体所有 t 均 <= obs_time)\n")
}

cat("\n全数据集: 共剔除", result$n_removed_total, "行，涉及", result$n_ids_affected, "个个体。\n")
cat("============================================\n")
