library(readxl)
library(dplyr)
library(ggplot2)

#############数据录入###############
# 文件路径
in_file <- "F:/文章/大论文/程序/建模结果/一批次/建模结果_一批次.xlsx"

# 获取所有sheet名称
sheets <- excel_sheets(in_file)

# 循环读取并赋值到全局环境
for (nm in sheets) {
  assign(nm, read_excel(in_file, sheet = nm), envir = .GlobalEnv)
}


head(result_1000_30_10V_highINTER_3c)













##################汇总####################
# 定义一个函数：计算均值和95%CI
# 定义一个函数：计算均值和95%CI，并返回一行
summary_stats <- function(df, name) {
  n_AUC <- sum(!is.na(df$AUC))
  n_BS  <- sum(!is.na(df$BS))
  n_C   <- sum(!is.na(df$Cindex))
  
  tibble(
    Dataset      = name,
    
    AUC_mean     = mean(df$AUC, na.rm = TRUE),
    AUC_lower    = mean(df$AUC, na.rm = TRUE) - 1.96 * sd(df$AUC, na.rm = TRUE)/sqrt(n_AUC),
    AUC_upper    = mean(df$AUC, na.rm = TRUE) + 1.96 * sd(df$AUC, na.rm = TRUE)/sqrt(n_AUC),
    
    BS_mean      = mean(df$BS, na.rm = TRUE),
    BS_lower     = mean(df$BS, na.rm = TRUE) - 1.96 * sd(df$BS, na.rm = TRUE)/sqrt(n_BS),
    BS_upper     = mean(df$BS, na.rm = TRUE) + 1.96 * sd(df$BS, na.rm = TRUE)/sqrt(n_BS),
    
    Cindex_mean  = mean(df$Cindex, na.rm = TRUE),
    Cindex_lower = mean(df$Cindex, na.rm = TRUE) - 1.96 * sd(df$Cindex, na.rm = TRUE)/sqrt(n_C),
    Cindex_upper = mean(df$Cindex, na.rm = TRUE) + 1.96 * sd(df$Cindex, na.rm = TRUE)/sqrt(n_C)
  )
}

# 把对象和名字放到一起
result_list <- list(
  result_500_30_10V_lowINTER_3c,
  result_500_30_10V_midINTER_3c,
  result_500_30_10V_highINTER_3c,
  
  result_500_70_10V_lowINTER_3c,
  result_500_70_10V_midINTER_3c,
  result_500_70_10V_highINTER_3c,
  
  result_1000_30_10V_lowINTER_3c,
  result_1000_30_10V_midINTER_3c,
  result_1000_30_10V_highINTER_3c,
  
  result_1000_70_10V_lowINTER_3c,
  result_1000_70_10V_midINTER_3c,
  result_1000_70_10V_highINTER_3c
)

names(result_list) <- c(
  "result_500_30_10V_lowINTER_3c",
  "result_500_30_10V_midINTER_3c",
  "result_500_30_10V_highINTER_3c",
  
  "result_500_70_10V_lowINTER_3c",
  "result_500_70_10V_midINTER_3c",
  "result_500_70_10V_highINTER_3c",
  
  "result_1000_30_10V_lowINTER_3c",
  "result_1000_30_10V_midINTER_3c",
  "result_1000_30_10V_highINTER_3c",
  
  "result_1000_70_10V_lowINTER_3c",
  "result_1000_70_10V_midINTER_3c",
  "result_1000_70_10V_highINTER_3c"
)

# 汇总到一个12行数据框
result_JM_batch1 <- bind_rows(
  Map(summary_stats, result_list, names(result_list))
)

result_long <- result_JM_batch1 %>%
  pivot_longer(
    cols = -Dataset,
    names_to = c("Metric", "Stat"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Stat,
    values_from = Value
  )


















##################出图##################
# 将数据转换为长格式
png(  filename = "F:/文章/大论文/程序/建模结果/一批次/模型性能森林图.png",
  width = 3000, height = 1500, res = 300
)

ggplot(result_long, aes(x = mean, y = Dataset)) +
  geom_point(aes(color = Metric), size = 2) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper, color = Metric),
    height = 0.2
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~ Metric, scales = "free_x") +
  labs(title = "模型性能森林图",
       x = "指标值", y = "数据集") +
  theme_minimal()

dev.off()






























