# RSF / Cox / RSFLC / JM 统一评价口径参考函数
# 本文件不绑定某一种模型；模型须先提供在共同评价网格上的个体风险矩阵。

make_metric_contract <- function(landmark = 0, horizon = 28, step = 0.5) {
  stopifnot(is.numeric(landmark), is.numeric(horizon), landmark < horizon, step > 0)
  grid <- unique(c(seq(landmark, horizon, by = step), horizon))
  list(
    landmark = landmark,
    horizon = horizon,
    grid = grid,
    fixed_horizon = horizon,
    auc_definition = "cumulative/dynamic time-dependent AUC",
    brier_definition = "IPCW Brier score",
    ibs_definition = sprintf("IPCW Brier积分除以(%g-%g)", horizon, landmark),
    risk_direction = "larger value = higher event risk"
  )
}

assert_risk_matrix <- function(risk, n_subject, grid, tolerance = 1e-08) {
  risk <- as.matrix(risk)
  stopifnot(nrow(risk) == n_subject, ncol(risk) == length(grid))
  if (any(!is.finite(risk))) stop("风险矩阵含NA/Inf。")
  if (any(risk < -tolerance | risk > 1 + tolerance)) stop("风险必须位于[0,1]。")
  if (any(apply(risk, 1, diff) < -tolerance)) {
    stop("累计风险曲线不单调；请检查时间顺序或预测定义。")
  }
  invisible(TRUE)
}

make_dynamic_risk_set <- function(data, time_col, landmark = 5) {
  # 动态模型共同风险集：只保留landmark时仍存活且有随访者（T > landmark）。
  data[data[[time_col]] > landmark, , drop = FALSE]
}

# 推荐调用：
# static_contract  <- make_metric_contract(landmark = 0, horizon = 28)
# dynamic_contract <- make_metric_contract(landmark = 5, horizon = 28)
#
# 每个模型输出 risk[i, j] = P(T_i <= grid[j] | T_i > landmark, history <= landmark)。
# 随后在同一验证集、同一IPCW权重与同一网格上计算：
#   AUC(landmark, 28)、Brier(landmark, 28)、IBS(landmark–28)。
# RSF/Cox静态比较使用landmark=0；RSFLC/JM动态比较使用论文预设landmark=5。
# 禁止把单个28天风险重复成整条曲线后计算IBS。

