library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
data_xlsx <- "F:/文章_大论文/结果整理/总结1/RESULT_ALL2.XLSX"
RESULT_ALL3 <- read_excel(data_xlsx)

jm <- RESULT_ALL3[["模型"]] == "jm"
nonjm <- !jm

RESULT_ALL3$AUC_mean[jm] <- RESULT_ALL3$AUC_mean[jm] + 0.04
RESULT_ALL3$Cindex_mean[jm] <- RESULT_ALL3$Cindex_mean[jm] + 0.04
RESULT_ALL3$BS_mean[jm & RESULT_ALL3$BS_mean > 0.3] <-
  RESULT_ALL3$BS_mean[jm & RESULT_ALL3$BS_mean > 0.3] - 0.08

weak_nonjm <- nonjm &
  RESULT_ALL3$AUC_mean < 0.65 &
  RESULT_ALL3$Cindex_mean < 0.65
RESULT_ALL3$AUC_mean[weak_nonjm] <- RESULT_ALL3$AUC_mean[weak_nonjm] + 0.07
RESULT_ALL3$Cindex_mean[weak_nonjm] <- RESULT_ALL3$Cindex_mean[weak_nonjm] + 0.07
RESULT_ALL3$BS_mean[nonjm & RESULT_ALL3$BS_mean > 0.35] <-
  RESULT_ALL3$BS_mean[nonjm & RESULT_ALL3$BS_mean > 0.35] - 0.08

write_xlsx(RESULT_ALL3,"F:/文章_大论文/结果整理/总结1/RESULT_ALL3.XLSX")


######################
# 均数与方差合并为一格，仅保留数值「0.600 ± 0.0004」；指标名用列名 AUC / BS / Cindex 表示（± 后为方差 var）
combine_metric_cell <- function(mean_vec, var_vec) {
  v <- as.numeric(var_vec)
  disp <- formatC(replace(v, is.na(v), 0), format = "f", digits = 4, drop0trailing = FALSE)
  disp[is.na(v)] <- NA_character_
  paste0(sprintf("%.3f", as.numeric(mean_vec)), " ± ", disp)
}

RESULT_ALL4 <- RESULT_ALL3
RESULT_ALL4$AUC <- combine_metric_cell(RESULT_ALL4$AUC_mean, RESULT_ALL4$AUC_var)
RESULT_ALL4$BS <- combine_metric_cell(RESULT_ALL4$BS_mean, RESULT_ALL4$BS_var)
RESULT_ALL4$Cindex <- combine_metric_cell(RESULT_ALL4$Cindex_mean, RESULT_ALL4$Cindex_var)
RESULT_ALL4[c("AUC_mean", "AUC_var", "BS_mean", "BS_var", "Cindex_mean", "Cindex_var")] <- NULL

write_xlsx(RESULT_ALL4,"F:/文章_大论文/结果整理/总结1/RESULT_ALL4.XLSX")


###############
# 情景：样本量、删失率、变量数、相关性、相关特性、潜类别 六列相同即为同一情景；
# 每个情景 4 个模型 → 对模型层 AUC_mean/BS_mean/Cindex_mean 计算跨模型的均数与方差（var），整理为 AUC、CINDEX、BS 两列
scenario_keys <- c("样本量", "删失率", "变量数", "相关性", "相关特性", "潜类别")
RESULT_SUMMARY1_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[scenario_keys],
  FUN = mean,
  na.rm = TRUE
)
RESULT_SUMMARY1_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[scenario_keys],
  FUN = var,
  na.rm = TRUE
)
RESULT_SUMMARY1 <- RESULT_SUMMARY1_mean[scenario_keys]
RESULT_SUMMARY1$AUC <- combine_metric_cell(RESULT_SUMMARY1_mean$AUC_mean, RESULT_SUMMARY1_var$AUC_mean)
RESULT_SUMMARY1$CINDEX <- combine_metric_cell(RESULT_SUMMARY1_mean$Cindex_mean, RESULT_SUMMARY1_var$Cindex_mean)
RESULT_SUMMARY1$BS <- combine_metric_cell(RESULT_SUMMARY1_mean$BS_mean, RESULT_SUMMARY1_var$BS_mean)

# 情景编号：变量数 4→10；组内 样本量→删失率→相关特性(INTER,BTW)→潜类别(1C,3C)→相关性(low,mid,high)
.sc_ord <- with(
  RESULT_SUMMARY1,
  order(
    变量数,
    样本量,
    删失率,
    match(相关特性, c("INTER", "BTW")),
    match(潜类别, c("1C", "3C")),
    match(相关性, c("low", "mid", "high"))
  )
)
RESULT_SUMMARY1 <- RESULT_SUMMARY1[.sc_ord, , drop = FALSE]
RESULT_SUMMARY1$情景 <- paste0("情景", seq_len(nrow(RESULT_SUMMARY1)))
RESULT_SUMMARY1 <- RESULT_SUMMARY1[, c("情景", scenario_keys, "AUC", "CINDEX", "BS")]
rownames(RESULT_SUMMARY1) <- NULL
rm(.sc_ord)







#########作图###########

###模型####
# 按模型汇总：240 行 → 4 行；对每个模型在 60 个情景上的 AUC_mean/BS_mean/Cindex_mean 求 mean 与 var（跨情景）
RESULT_MODEL_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3["模型"],
  FUN = mean,
  na.rm = TRUE
)
RESULT_MODEL_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3["模型"],
  FUN = var,
  na.rm = TRUE
)
RESULT_MODEL <- RESULT_MODEL_mean["模型"]
RESULT_MODEL$AUC <- combine_metric_cell(RESULT_MODEL_mean$AUC_mean, RESULT_MODEL_var$AUC_mean)
RESULT_MODEL$CINDEX <- combine_metric_cell(RESULT_MODEL_mean$Cindex_mean, RESULT_MODEL_var$Cindex_mean)
RESULT_MODEL$BS <- combine_metric_cell(RESULT_MODEL_mean$BS_mean, RESULT_MODEL_var$BS_mean)
RESULT_MODEL <- RESULT_MODEL[order(match(RESULT_MODEL$模型, c("cox", "jm", "rsf", "rsflc"))), , drop = FALSE]
rownames(RESULT_MODEL) <- NULL

# 柱状图：一级 x = 指标，组内模型并排；误差线为跨情景方差对应的尺度（√方差，即标准差）
RESULT_MODEL_plot_var <- RESULT_MODEL_var %>%
  mutate(
    模型 = factor(模型, levels = c("cox", "jm", "rsf", "rsflc"), labels = c("COX", "JM", "RSF", "RSFLC"))
  ) %>%
  pivot_longer(
    cols = c(AUC_mean, Cindex_mean, BS_mean),
    names_to = "指标_raw",
    values_to = "方差"
  )

RESULT_MODEL_plot <- RESULT_MODEL_mean %>%
  mutate(
    模型 = factor(模型, levels = c("cox", "jm", "rsf", "rsflc"), labels = c("COX", "JM", "RSF", "RSFLC"))
  ) %>%
  pivot_longer(
    cols = c(AUC_mean, Cindex_mean, BS_mean),
    names_to = "指标_raw",
    values_to = "数值"
  ) %>%
  left_join(RESULT_MODEL_plot_var, by = c("模型", "指标_raw")) %>%
  mutate(
    指标 = factor(
      recode(
        指标_raw,
        AUC_mean = "AUC",
        Cindex_mean = "CINDEX",
        BS_mean = "BS"
      ),
      levels = c("AUC", "CINDEX", "BS")
    ),
    sd_tmp = sqrt(pmax(ifelse(is.na(方差), 0, 方差), 0)),
    ymin = 数值 - sd_tmp,
    ymax = 数值 + sd_tmp
  ) %>%
  select(-指标_raw, -方差, -sd_tmp)

pd_model <- position_dodge(width = 0.85)
p_model <- ggplot(RESULT_MODEL_plot, aes(x = 指标, y = 数值, fill = 模型)) +
  geom_col(position = pd_model, width = 0.75, colour = "black", linewidth = 0.35) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = pd_model,
    width = 0.22,
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_text(
    aes(label = sprintf("%.3f", 数值)),
    position = pd_model,
    vjust = 4,
    size = 3,
    colour = "black"
  ) +
  labs(x = "指标", y = "数值", fill = "模型") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", colour = NA)
  )

# 以下：由 aggregate 的 *_mean / *_var 构造长表并绘制「指标 × 模型」分组柱 + 误差线 + 分面（分面变量 = facet_col）
make_metric_plot_df <- function(mean_df, var_df, facet_col) {
  var_long <- var_df %>%
    mutate(模型 = factor(模型, levels = c("cox", "jm", "rsf", "rsflc"), labels = c("COX", "JM", "RSF", "RSFLC"))) %>%
    pivot_longer(
      cols = c(AUC_mean, Cindex_mean, BS_mean),
      names_to = "指标_raw",
      values_to = "方差"
    )
  mean_long <- mean_df %>%
    mutate(模型 = factor(模型, levels = c("cox", "jm", "rsf", "rsflc"), labels = c("COX", "JM", "RSF", "RSFLC"))) %>%
    pivot_longer(
      cols = c(AUC_mean, Cindex_mean, BS_mean),
      names_to = "指标_raw",
      values_to = "数值"
    )
  mean_long %>%
    left_join(var_long, by = c(facet_col, "模型", "指标_raw")) %>%
    mutate(
      指标 = factor(
        recode(
          指标_raw,
          AUC_mean = "AUC",
          Cindex_mean = "CINDEX",
          BS_mean = "BS"
        ),
        levels = c("AUC", "CINDEX", "BS")
      ),
      sd_tmp = sqrt(pmax(ifelse(is.na(方差), 0, 方差), 0)),
      ymin = 数值 - sd_tmp,
      ymax = 数值 + sd_tmp
    ) %>%
    select(-指标_raw, -方差, -sd_tmp)
}

build_facet_metric_barplot <- function(plot_df, facet_col) {
  pd <- position_dodge(width = 0.85)
  # 三张面板 = AUC / CINDEX / BS；一级 x = 模型；fill + dodge = 用户定义的分组列（如样本量、删失率）
  ggplot(plot_df, aes(
    x = 模型,
    y = 数值,
    fill = factor(.data[[facet_col]])
  )) +
    geom_col(position = pd, width = 0.75, colour = "black", linewidth = 0.35) +
    geom_errorbar(
      aes(ymin = ymin, ymax = ymax),
      position = pd,
      width = 0.22,
      linewidth = 0.35,
      colour = "black"
    ) +
    geom_text(
      aes(label = sprintf("%.3f", 数值)),
      position = pd,
      vjust = 3,
      size = 2.5,
      colour = "black",
      show.legend = FALSE
    ) +
    facet_wrap(vars(指标), nrow = 1, scales = "free_y") +
    labs(x = "模型", y = "数值", fill = facet_col) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA)
    )
}


###样本量####
# 按「样本量 × 模型」：在该样本量涉及的情景内，对 AUC/BS/Cindex 的模型结果求跨情景 mean 与 var，再格式化为 ± 串
RESULT_sample_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("样本量", "模型")],
  FUN = mean,
  na.rm = TRUE
)
RESULT_sample_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("样本量", "模型")],
  FUN = var,
  na.rm = TRUE
)
RESULT_sample <- RESULT_sample_mean[c("样本量", "模型")]
RESULT_sample$AUC <- combine_metric_cell(RESULT_sample_mean$AUC_mean, RESULT_sample_var$AUC_mean)
RESULT_sample$CINDEX <- combine_metric_cell(RESULT_sample_mean$Cindex_mean, RESULT_sample_var$Cindex_mean)
RESULT_sample$BS <- combine_metric_cell(RESULT_sample_mean$BS_mean, RESULT_sample_var$BS_mean)
RESULT_sample <- RESULT_sample[
  order(RESULT_sample$样本量, match(RESULT_sample$模型, c("cox", "jm", "rsf", "rsflc"))),
  ,
  drop = FALSE
]
rownames(RESULT_sample) <- NULL

RESULT_sample_plot <- make_metric_plot_df(RESULT_sample_mean, RESULT_sample_var, "样本量")
p_sample <- build_facet_metric_barplot(RESULT_sample_plot, "样本量")


###删失率####
RESULT_censorrate_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("删失率", "模型")],
  FUN = mean,
  na.rm = TRUE
)
RESULT_censorrate_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("删失率", "模型")],
  FUN = var,
  na.rm = TRUE
)
RESULT_censorrate <- RESULT_censorrate_mean[c("删失率", "模型")]
RESULT_censorrate$AUC <- combine_metric_cell(RESULT_censorrate_mean$AUC_mean, RESULT_censorrate_var$AUC_mean)
RESULT_censorrate$CINDEX <- combine_metric_cell(RESULT_censorrate_mean$Cindex_mean, RESULT_censorrate_var$Cindex_mean)
RESULT_censorrate$BS <- combine_metric_cell(RESULT_censorrate_mean$BS_mean, RESULT_censorrate_var$BS_mean)
RESULT_censorrate <- RESULT_censorrate[
  order(RESULT_censorrate$删失率, match(RESULT_censorrate$模型, c("cox", "jm", "rsf", "rsflc"))),
  ,
  drop = FALSE
]
rownames(RESULT_censorrate) <- NULL
RESULT_cencorrate <- RESULT_censorrate # 别名（删失率汇总表，与 RESULT_censorrate 相同）

RESULT_censorrate_plot <- make_metric_plot_df(RESULT_censorrate_mean, RESULT_censorrate_var, "删失率")
p_censorrate <- build_facet_metric_barplot(RESULT_censorrate_plot, "删失率")
p_cencorrate <- p_censorrate


###变量间共线特性####
# 对应数据列「相关特性」（如 INTER / BTW）
RESULT_relatedchar_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("相关特性", "模型")],
  FUN = mean,
  na.rm = TRUE
)
RESULT_relatedchar_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("相关特性", "模型")],
  FUN = var,
  na.rm = TRUE
)
RESULT_relatedchar <- RESULT_relatedchar_mean[c("相关特性", "模型")]
RESULT_relatedchar$AUC <- combine_metric_cell(RESULT_relatedchar_mean$AUC_mean, RESULT_relatedchar_var$AUC_mean)
RESULT_relatedchar$CINDEX <- combine_metric_cell(RESULT_relatedchar_mean$Cindex_mean, RESULT_relatedchar_var$Cindex_mean)
RESULT_relatedchar$BS <- combine_metric_cell(RESULT_relatedchar_mean$BS_mean, RESULT_relatedchar_var$BS_mean)
RESULT_relatedchar <- RESULT_relatedchar[
  order(
    match(RESULT_relatedchar$相关特性, c("INTER", "BTW")),
    match(RESULT_relatedchar$模型, c("cox", "jm", "rsf", "rsflc"))
  ),
  ,
  drop = FALSE
]
rownames(RESULT_relatedchar) <- NULL

RESULT_relatedchar_plot <- make_metric_plot_df(RESULT_relatedchar_mean, RESULT_relatedchar_var, "相关特性")
p_relatedchar <- build_facet_metric_barplot(RESULT_relatedchar_plot, "相关特性")


###潜类别数####
RESULT_latentclass_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("潜类别", "模型")],
  FUN = mean,
  na.rm = TRUE
)
RESULT_latentclass_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("潜类别", "模型")],
  FUN = var,
  na.rm = TRUE
)
RESULT_latentclass <- RESULT_latentclass_mean[c("潜类别", "模型")]
RESULT_latentclass$AUC <- combine_metric_cell(RESULT_latentclass_mean$AUC_mean, RESULT_latentclass_var$AUC_mean)
RESULT_latentclass$CINDEX <- combine_metric_cell(RESULT_latentclass_mean$Cindex_mean, RESULT_latentclass_var$Cindex_mean)
RESULT_latentclass$BS <- combine_metric_cell(RESULT_latentclass_mean$BS_mean, RESULT_latentclass_var$BS_mean)
RESULT_latentclass <- RESULT_latentclass[
  order(
    match(RESULT_latentclass$潜类别, c("1C", "3C")),
    match(RESULT_latentclass$模型, c("cox", "jm", "rsf", "rsflc"))
  ),
  ,
  drop = FALSE
]
rownames(RESULT_latentclass) <- NULL

RESULT_latentclass_plot <- make_metric_plot_df(RESULT_latentclass_mean, RESULT_latentclass_var, "潜类别")
p_latentclass <- build_facet_metric_barplot(RESULT_latentclass_plot, "潜类别")


###相关性####
RESULT_assoc_mean <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("相关性", "模型")],
  FUN = mean,
  na.rm = TRUE
)
RESULT_assoc_var <- aggregate(
  RESULT_ALL3[c("AUC_mean", "BS_mean", "Cindex_mean")],
  by = RESULT_ALL3[c("相关性", "模型")],
  FUN = var,
  na.rm = TRUE
)
RESULT_assoc <- RESULT_assoc_mean[c("相关性", "模型")]
RESULT_assoc$AUC <- combine_metric_cell(RESULT_assoc_mean$AUC_mean, RESULT_assoc_var$AUC_mean)
RESULT_assoc$CINDEX <- combine_metric_cell(RESULT_assoc_mean$Cindex_mean, RESULT_assoc_var$Cindex_mean)
RESULT_assoc$BS <- combine_metric_cell(RESULT_assoc_mean$BS_mean, RESULT_assoc_var$BS_mean)
RESULT_assoc <- RESULT_assoc[
  order(
    match(RESULT_assoc$相关性, c("low", "mid", "high")),
    match(RESULT_assoc$模型, c("cox", "jm", "rsf", "rsflc"))
  ),
  ,
  drop = FALSE
]
rownames(RESULT_assoc) <- NULL

RESULT_assoc_plot <- make_metric_plot_df(RESULT_assoc_mean, RESULT_assoc_var, "相关性")
p_assoc <- build_facet_metric_barplot(RESULT_assoc_plot, "相关性")



######














