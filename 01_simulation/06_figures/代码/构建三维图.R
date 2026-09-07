####模板###

library(readxl)
library(dplyr)
library(tidyr)
library(echarts4r)
library(openxlsx)

###########可视化########

# 1. 读取整合后的数据
integrated_data <- read.csv("f:\\文章_大论文\\TREA代码\\整合后的数据.csv", fileEncoding = "UTF-8")

# 2. 将数据重塑为长格式
long_data <- integrated_data %>% 
  pivot_longer(
    cols = c(BS, CI, AUC),  # 选择需要重塑的列
    names_to = '指标',       # 新列名：指标
    values_to = 'result'      # 新列名：result
  )

# 添加调试信息
cat("重塑后的数据列名：\n")
names(long_data)
cat("\n重塑后的数据前几行：\n")
head(long_data)

# 3. 保存为新的Excel文件
write.xlsx(long_data, "f:\\文章_大论文\\TREA代码\\all_data.xlsx")

# 4. 构建3D柱状图
# 检查是否有3c数据
cat("\nLatent_Class的唯一值：\n")
unique(long_data$Latent_Class)

# 筛选3C的数据
long_data_3c <- long_data %>% 
  filter(Latent_Class == "3c")

# 检查3c数据量
cat("\n3c数据量：", nrow(long_data_3c), "\n")

# 如果没有3c数据，使用1c数据
if (nrow(long_data_3c) == 0) {
  cat("\n没有3c数据，使用1c数据\n")
  long_data_3c <- long_data %>% 
    filter(Latent_Class == "1c")
}

# 计算每个组合的平均值
plot_data <- long_data_3c %>% 
  group_by(Var_Corr, `指标`) %>% 
  summarise(result = mean(result), .groups = "drop")

# 检查plot_data
cat("\nplot_data前几行：\n")
head(plot_data)
cat("\nplot_data列名：\n")
names(plot_data)

# 添加颜色映射，为每个指标分配固定颜色
plot_data <- plot_data %>% 
  mutate(color = case_when(
    `指标` == "BS" ~ "#FF6B6B",  # 红色 - BS指标
    `指标` == "CI" ~ "#4ECDC4",  # 青色 - CI指标
    `指标` == "AUC" ~ "#45B7D1"   # 蓝色 - AUC指标
  ))

# 将中文列名改为英文，避免echarts4r的中文支持问题
plot_data_eng <- plot_data %>% 
  rename(Indicator = `指标`)  # 将"指标"改为"Indicator"

# 构建3D柱状图
echart <- plot_data_eng %>% 
  e_charts(Var_Corr) %>% 
  
  # 3D柱状图，按指标使用不同颜色
  e_bar_3d(Indicator, result, name = "3D柱状图", itemStyle = list(color = ~color)) %>% 
  
  # 添加标题
  e_title("3C 3D柱状图", 
          textStyle = list(fontSize = 16)) %>% 
  
  # 添加图例
  e_legend(show = TRUE, 
           orient = "horizontal", 
           bottom = 10, 
           data = c("BS", "CI", "AUC")) %>% 
  
  # 添加提示框，显示详细信息
  e_tooltip(trigger = "item", 
            formatter = "函数: {b}<br/>指标: {c}<br/>值: {d}") %>%
  
  # 设置坐标轴样式（包括颜色）
  e_x_axis(axisLine = list(lineStyle = list(color = "#333333")),
           axisLabel = list(color = "#666666"),
           axisTick = list(lineStyle = list(color = "#999999")))
  
  # 注意：echarts4r的3D柱状图目前不直接支持通过函数设置Y轴和Z轴的颜色
  # 这是因为3D图表的坐标轴配置方式与2D图表不同
  # 如果需要更复杂的3D配置，建议使用原生ECharts JavaScript库

# 显示图表
print(echart)

cat("\n图表构建完成！\n")



############PLOT3D包####################

# 加载plot3D包（如果未安装，先运行：install.packages("plot3D")）
library(plot3D)

# 准备PLOT3D所需的数据
# 1. 查看Var_Corr的唯一值，确认所有可能的取值
cat("\nVar_Corr的唯一值：\n")
var_corr_values <- unique(plot_data$Var_Corr)
print(var_corr_values)

# 2. 将Var_Corr转换为数值型，便于3D绘图
# Low = 1, Medium = 2, High = 3
plot_data_plot3d <- plot_data %>%
  mutate(Var_Corr_num = case_when(
    Var_Corr == "Low" ~ 1,
    Var_Corr == "Medium" ~ 2,
    Var_Corr == "Mid" ~ 2,
    Var_Corr == "High" ~ 3,
    TRUE ~ NA_real_  # 处理未匹配的值
  ),
  # 将指标转换为数值型，并确保CI显示为CINDEX
  指标_num = case_when(
    `指标` == "BS" ~ 1,
    `指标` == "CI" ~ 2,
    `指标` == "AUC" ~ 3,
    TRUE ~ NA_real_  # 处理未匹配的值
  ),
  # 为Y轴准备显示的指标名称
  指标_name = case_when(
    `指标` == "BS" ~ "BS",
    `指标` == "CI" ~ "CINDEX",
    `指标` == "AUC" ~ "AUC",
    TRUE ~ as.character(`指标`)  # 保留原始值
  ))

# 检查数据转换结果
cat("\nPLOT3D数据转换结果：\n")
head(plot_data_plot3d)

# 3. 确定X轴和Y轴的范围和标签
# 获取实际存在的Var_Corr值和对应的数值映射
var_corr_mapping <- unique(plot_data_plot3d[, c("Var_Corr", "Var_Corr_num")])
var_corr_mapping <- var_corr_mapping[order(var_corr_mapping$Var_Corr_num), ]

# 获取实际存在的指标和对应的数值映射
indicator_mapping <- unique(plot_data_plot3d[, c("指标_name", "指标_num")])
indicator_mapping <- indicator_mapping[order(indicator_mapping$指标_num), ]

# 2. 创建3D散点图
png("f:/文章_大论文/TREA代码/3D散点图.png", width = 800, height = 600)

scatter3D(
  x = plot_data_plot3d$Var_Corr_num,  # X轴：变量相关性
  y = plot_data_plot3d$指标_num,       # Y轴：指标
  z = plot_data_plot3d$result,         # Z轴：结果值
  xlab = "VAR_CORR",
  ylab = "指标",
  zlab = "result",
  main = "3C 3D散点图",
  pch = 18,  # 点的形状
  cex = 2,   # 点的大小
  col = plot_data_plot3d$color,  # 使用之前定义的颜色
  ticktype = "detailed",  # 显示详细刻度
  # 设置X轴范围和标签
  xlim = c(min(plot_data_plot3d$Var_Corr_num, na.rm = TRUE) - 0.5, max(plot_data_plot3d$Var_Corr_num, na.rm = TRUE) + 0.5),
  ylim = c(min(plot_data_plot3d$指标_num, na.rm = TRUE) - 0.5, max(plot_data_plot3d$指标_num, na.rm = TRUE) + 0.5),
  # 设置X轴刻度和标签
  xticks = var_corr_mapping$Var_Corr_num,
  xticklabels = var_corr_mapping$Var_Corr,
  # 设置Y轴刻度和标签
  yticks = indicator_mapping$指标_num,
  yticklabels = indicator_mapping$指标_name
)

# 添加图例
legend("topright",
       legend = c("BS", "CINDEX", "AUC"),
       col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
       pch = 18,
       cex = 0.8,
       bty = "n")

dev.off()

# 3. 创建3D柱状图（使用hist3D函数模拟）
png("f:/文章_大论文/TREA代码/3D柱状图_plot3d.png", width = 800, height = 600)

# 准备3D柱状图的数据
# 创建一个矩阵来存储结果值
# 行：Var_Corr（1=Low, 2=Mid, 3=High）
# 列：指标（1=BS, 2=CINDEX, 3=AUC）
max_row <- max(plot_data_plot3d$Var_Corr_num, na.rm = TRUE)
max_col <- max(plot_data_plot3d$指标_num, na.rm = TRUE)
bar3d_data <- matrix(0, nrow = max_row, ncol = max_col)

# 填充数据
for (i in 1:nrow(plot_data_plot3d)) {
  row_idx <- plot_data_plot3d$Var_Corr_num[i]
  col_idx <- plot_data_plot3d$指标_num[i]
  if (!is.na(row_idx) && !is.na(col_idx)) {
    bar3d_data[row_idx, col_idx] <- plot_data_plot3d$result[i]
  }
}

# 绘制3D柱状图
hist3D(
  x = 1:max_row,  # X轴：Var_Corr
  y = 1:max_col,  # Y轴：指标
  z = bar3d_data,  # Z轴：结果值
  xlab = "VAR_CORR",
  ylab = "指标",
  zlab = "result",
  main = "3C 3D柱状图",
  col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
  ticktype = "detailed",
  # 设置X轴刻度和标签
  xticks = var_corr_mapping$Var_Corr_num,
  xticklabels = var_corr_mapping$Var_Corr,
  # 设置Y轴刻度和标签
  yticks = indicator_mapping$指标_num,
  yticklabels = indicator_mapping$指标_name,
  shade = 0.5,  # 设置阴影效果
  border = "black"  # 设置柱子边框颜色
)

dev.off()

cat("\nPLOT3D图表已保存！\n")
cat("- 3D散点图：3D散点图.png\n")
cat("- 3D柱状图：3D柱状图_plot3d.png\n")

#########ggplot#######

# 加载ggplot2包
library(ggplot2)

# 准备ggplot数据
# 确保指标显示为CINDEX
plot_data_ggplot <- plot_data %>%
  mutate(指标 = case_when(
    `指标` == "BS" ~ "BS",
    `指标` == "CI" ~ "CINDEX",
    `指标` == "AUC" ~ "AUC",
    TRUE ~ as.character(`指标`)
  ))

# 检查ggplot数据
cat("\nggplot数据：\n")
head(plot_data_ggplot)

# 创建ggplot2图表 - 使用分面表示第三个维度
# 保持xyz轴含义不变：
# x轴：Var_Corr（变量相关性）
# y轴：指标（BS, CINDEX, AUC）
# z轴：result（结果值）
# 使用分面来展示不同模型或其他维度

# 首先，我们需要确定第三个维度是什么
# 从原始数据中，我们可以使用Model作为第三个维度
# 所以我们需要重新准备数据，包括Model列

# 重新计算数据，包含Model列
plot_data_with_model <- long_data_3c %>%
  # 将CI显示为CINDEX
  mutate(指标 = case_when(
    `指标` == "BS" ~ "BS",
    `指标` == "CI" ~ "CINDEX",
    `指标` == "AUC" ~ "AUC",
    TRUE ~ as.character(`指标`)
  )) %>%
  group_by(Var_Corr, 指标, Model) %>%
  summarise(result = mean(result), .groups = "drop")

# 检查包含Model的数据
cat("\n包含Model的ggplot数据：\n")
head(plot_data_with_model)

# 1. 创建2D柱状图，使用分面表示Model维度
png("f:/文章_大论文/TREA代码/ggplot2_柱状图_分面.png", width = 1000, height = 800, res = 100)

ggplot(plot_data_with_model, aes(x = Var_Corr, y = result, fill = 指标)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  
  # 设置分面，按Model分列
  facet_wrap(~ Model, ncol = 2) +
  
  # 设置颜色映射
  scale_fill_manual(values = c("BS" = "#FF6B6B", "CINDEX" = "#4ECDC4", "AUC" = "#45B7D1")) +
  
  # 设置坐标轴标签和标题
  labs(
    title = "3C 模型结果比较 - 按指标和变量相关性",
    x = "VAR_CORR",
    y = "result",
    fill = "指标"
  ) +
  
  # 设置主题
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc")
  ) +
  
  # 设置y轴范围
  ylim(0, max(plot_data_with_model$result) * 1.1)

# 保存图表
dev.off()

# 2. 创建热图，使用分面表示Model维度
png("f:/文章_大论文/TREA代码/ggplot2_热图_分面.png", width = 1000, height = 800, res = 100)

ggplot(plot_data_with_model, aes(x = Var_Corr, y = 指标, fill = result)) +
  geom_tile(color = "white", size = 1) +
  
  # 设置分面，按Model分列
  facet_wrap(~ Model, ncol = 2) +
  
  # 设置颜色映射
  scale_fill_gradient(low = "#45B7D1", high = "#FF6B6B", name = "result") +
  
  # 添加数值标签
  geom_text(aes(label = round(result, 3)), color = "white", size = 4, fontface = "bold") +
  
  # 设置坐标轴标签和标题
  labs(
    title = "3C 模型结果热图 - 按指标和变量相关性",
    x = "VAR_CORR",
    y = "指标"
  ) +
  
  # 设置主题
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 保存图表
dev.off()

# 3. 如果不需要Model维度，我们可以使用其他方式展示
# 例如，使用不同颜色表示不同指标，y轴表示result
png("f:/文章_大论文/TREA代码/ggplot2_折线图_分面.png", width = 1000, height = 800, res = 100)

ggplot(plot_data_with_model, aes(x = Var_Corr, y = result, color = 指标, group = 指标)) +
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, fill = "white") +
  
  # 设置分面，按Model分列
  facet_wrap(~ Model, ncol = 2) +
  
  # 设置颜色映射
  scale_color_manual(values = c("BS" = "#FF6B6B", "CINDEX" = "#4ECDC4", "AUC" = "#45B7D1")) +
  
  # 设置坐标轴标签和标题
  labs(
    title = "3C 模型结果趋势 - 按指标和变量相关性",
    x = "VAR_CORR",
    y = "result",
    color = "指标"
  ) +
  
  # 设置主题
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc")
  ) +
  
  # 设置y轴范围
  ylim(0, max(plot_data_with_model$result) * 1.1)

# 保存图表
dev.off()

cat("\nggplot2图表已保存！\n")
cat("- 柱状图：ggplot2_柱状图_分面.png\n")
cat("- 热图：ggplot2_热图_分面.png\n")
cat("- 折线图：ggplot2_折线图_分面.png\n")


#####scatterplot3d#########

# 加载scatterplot3d包，如果未安装则自动安装
if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
  install.packages("scatterplot3d", dependencies = TRUE, repos = "https://cran.r-project.org")
}
library(scatterplot3d)

# 准备scatterplot3d数据
# 使用之前准备的plot_data_plot3d数据，它已经包含了数值转换

# 检查数据
cat("\nscatterplot3d数据：\n")
head(plot_data_plot3d)

# 创建3D柱状图（使用scatterplot3d模拟）
png("f:/文章_大论文/TREA代码/scatterplot3d_3d柱状图.png", width = 800, height = 600, res = 100)

# 创建基础的3D场景
sp3d <- scatterplot3d(
  x = plot_data_plot3d$Var_Corr_num,  # X轴：变量相关性
  y = plot_data_plot3d$指标_num,       # Y轴：指标
  z = plot_data_plot3d$result,         # Z轴：结果值
  xlab = "VAR_CORR",  # X轴标签
  ylab = "指标",       # Y轴标签
  zlab = "result",     # Z轴标签
  main = "3C 3D柱状图",
  # 隐藏原始散点
  pch = NA,
  # 设置合适的角度
  angle = 60,
  # 显示网格和框
  grid = TRUE,
  box = TRUE
)

# 绘制3D柱状图 - 使用垂直长方体模拟
# 设置柱子的宽度
bar_width <- 0.2

# 遍历数据，为每个点绘制一个柱子
for (i in 1:nrow(plot_data_plot3d)) {
  x <- plot_data_plot3d$Var_Corr_num[i]
  y <- plot_data_plot3d$指标_num[i]
  z <- plot_data_plot3d$result[i]
  col <- plot_data_plot3d$color[i]
  
  # 绘制柱子的8个顶点
  # 底面四个点
  x1 <- x - bar_width
  y1 <- y - bar_width
  x2 <- x + bar_width
  y2 <- y + bar_width
  
  # 绘制柱子的四个侧面
  # 前面（y固定为y2）
  sp3d$points3d(c(x1, x2, x2, x1, x1), 
                c(y2, y2, y2, y2, y2), 
                c(0, 0, z, z, 0), 
                type = "l", 
                col = col, 
                lwd = 1)
  
  # 后面（y固定为y1）
  sp3d$points3d(c(x1, x2, x2, x1, x1), 
                c(y1, y1, y1, y1, y1), 
                c(0, 0, z, z, 0), 
                type = "l", 
                col = col, 
                lwd = 1)
  
  # 左面（x固定为x1）
  sp3d$points3d(c(x1, x1, x1, x1, x1), 
                c(y1, y2, y2, y1, y1), 
                c(0, 0, z, z, 0), 
                type = "l", 
                col = col, 
                lwd = 1)
  
  # 右面（x固定为x2）
  sp3d$points3d(c(x2, x2, x2, x2, x2), 
                c(y1, y2, y2, y1, y1), 
                c(0, 0, z, z, 0), 
                type = "l", 
                col = col, 
                lwd = 1)
  
  # 顶面
  sp3d$points3d(c(x1, x2, x2, x1, x1), 
                c(y1, y1, y2, y2, y1), 
                c(z, z, z, z, z), 
                type = "l", 
                col = col, 
                lwd = 1)
}

# 添加图例
legend("topleft",
       legend = c("BS", "CINDEX", "AUC"),
       col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
       pch = 15,
       cex = 0.8,
       bty = "n")

# 保存图表
dev.off()

# 创建简化版3D柱状图 - 使用垂直线段模拟
png("f:/文章_大论文/TREA代码/scatterplot3d_3d柱状图_简化版.png", width = 800, height = 600, res = 100)

# 创建基础的3D场景
sp3d_simple <- scatterplot3d(
  x = plot_data_plot3d$Var_Corr_num,  # X轴：变量相关性
  y = plot_data_plot3d$指标_num,       # Y轴：指标
  z = plot_data_plot3d$result,         # Z轴：结果值
  xlab = "VAR_CORR",  # X轴标签
  ylab = "指标",       # Y轴标签
  zlab = "result",     # Z轴标签
  main = "3C 3D柱状图（简化版）",
  # 隐藏原始散点
  pch = NA,
  # 设置合适的角度
  angle = 60,
  # 显示网格和框
  grid = TRUE,
  box = TRUE
)

# 绘制简化版3D柱状图 - 使用垂直线段
for (i in 1:nrow(plot_data_plot3d)) {
  x <- plot_data_plot3d$Var_Corr_num[i]
  y <- plot_data_plot3d$指标_num[i]
  z <- plot_data_plot3d$result[i]
  col <- plot_data_plot3d$color[i]
  
  # 绘制从底面到顶面的垂直线段
  sp3d_simple$points3d(c(x, x), 
                      c(y, y), 
                      c(0, z), 
                      type = "l", 
                      col = col, 
                      lwd = 3)
  
  # 在顶面添加一个点，增强视觉效果
  sp3d_simple$points3d(x, y, z, 
                      pch = 16, 
                      col = col, 
                      cex = 1.5)
}

# 添加图例
legend("topleft",
       legend = c("BS", "CINDEX", "AUC"),
       col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
       pch = 16,
       cex = 0.8,
       bty = "n")

# 保存图表
dev.off()

cat("\nscatterplot3d图表已保存！\n")
cat("- 3D柱状图：scatterplot3d_3d柱状图.png\n")
cat("- 简化版3D柱状图：scatterplot3d_3d柱状图_简化版.png\n")