library(psych)
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(GPArotation)
simdata2 <- read_excel("F:/文章/大论文/程序/模拟数据/500_30删失_10V_HIGH(变量间)_3类/sim500_30_10V_highINTER_3c_2.xlsx")

head(simdata2)

########程序准备################
add_Y <- function(sim_df, k) {
  ## 检查
  if (!is.numeric(k) || length(k) != 10)
    stop("k 必须是长度为 10 的数值向量")
  
  ## 自动检测 V 变量
  v_cols <- grep("^V\\d+", names(sim_df), value = TRUE)
  if (length(v_cols) != 10)
    stop(paste0("找不到 10 个 V 变量，当前匹配到 ", length(v_cols), " 个: ", paste(v_cols, collapse = ", ")))
  
  ## 命名权重并计算
  k_vec <- setNames(k, v_cols)
  V_mat <- sim_df %>% dplyr::select(all_of(v_cols)) %>% as.matrix()
  Y <- V_mat %*% k_vec
  
  ## 返回结果
  sim_df %>% dplyr::mutate(Y = as.numeric(Y))
}

## 使用示例
#假设专家先验权重：V1-V6 各 5，V7-V10 各 0.025（总和 = 1）
set.seed(123)
k <- c(runif(6, min = 5.1, max = 10), rep(0.025, 4))
k

add_Y_to_list <- function(data_list, k) {
  lapply(data_list, function(df) add_Y(df, k))
}

##########读取数据#################


# 加载必要的包
library(readxl)
library(writexl)

# 设置数据目录路径
data_dir <- "F:/文章/大论文/程序/模拟数据/数据2_生存"

# 设置各相关性级别的子目录
out_dir_low <- file.path(data_dir, "低相关")
out_dir_mid <- file.path(data_dir, "中相关")  
out_dir_high <- file.path(data_dir, "高相关")

################## 低相关性数据导入 ##################
library(readxl)
library(purrr)

# 样本量500，变量30
sim500_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_lowINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_lowBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_lowBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx"), sheet = .x))

################## 中相关性数据导入 ##################
# 样本量500，变量30
sim500_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx")),
                                           excel_sheets(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx"))),
                                  ~ read_xlsx(file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_midINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_midBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_midBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx"), sheet = .x))



################## 高相关性数据导入 ##################
library(readxl)
library(purrr)

# 样本量500，变量30
sim500_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim500_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))


# 样本量500，变量70
sim500_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx")),
                                              excel_sheets(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx"))),
                                     ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim500_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim500_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx")),
                                            excel_sheets(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx"))),
                                   ~ read_xlsx(file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))


# 样本量1000，变量30
sim1000_30_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim1000_30_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_30_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_highINTER_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highINTER_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx")),
                                               excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx"))),
                                      ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx"), sheet = .x))

sim1000_70_10V_highBTW_3c_L1 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L2 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L3 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L4 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx"), sheet = .x))
sim1000_70_10V_highBTW_3c_L5 <- map(setNames(excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx")),
                                             excel_sheets(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx"))),
                                    ~ read_xlsx(file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx"), sheet = .x))

########

########### 添加Y ######################

######低相关性######
sim500_30_10V_lowINTER_3c_L1 <-add_Y_to_list(sim500_30_10V_lowINTER_3c_L1,  k)
sim500_30_10V_lowINTER_3c_L2 <-add_Y_to_list(sim500_30_10V_lowINTER_3c_L2,  k)
sim500_30_10V_lowINTER_3c_L3 <-add_Y_to_list(sim500_30_10V_lowINTER_3c_L3,  k)
sim500_30_10V_lowINTER_3c_L4 <-add_Y_to_list(sim500_30_10V_lowINTER_3c_L4,  k)
sim500_30_10V_lowINTER_3c_L5 <-add_Y_to_list(sim500_30_10V_lowINTER_3c_L5,  k)

sim500_30_10V_lowBTW_3c_L1 <-add_Y_to_list(sim500_30_10V_lowBTW_3c_L1,  k)
sim500_30_10V_lowBTW_3c_L2 <-add_Y_to_list(sim500_30_10V_lowBTW_3c_L2,  k)
sim500_30_10V_lowBTW_3c_L3 <-add_Y_to_list(sim500_30_10V_lowBTW_3c_L3,  k)
sim500_30_10V_lowBTW_3c_L4 <-add_Y_to_list(sim500_30_10V_lowBTW_3c_L4,  k)
sim500_30_10V_lowBTW_3c_L5 <-add_Y_to_list(sim500_30_10V_lowBTW_3c_L5,  k)

sim500_70_10V_lowINTER_3c_L1 <-add_Y_to_list(sim500_70_10V_lowINTER_3c_L1,  k)
sim500_70_10V_lowINTER_3c_L2 <-add_Y_to_list(sim500_70_10V_lowINTER_3c_L2,  k)
sim500_70_10V_lowINTER_3c_L3 <-add_Y_to_list(sim500_70_10V_lowINTER_3c_L3,  k)
sim500_70_10V_lowINTER_3c_L4 <-add_Y_to_list(sim500_70_10V_lowINTER_3c_L4,  k)
sim500_70_10V_lowINTER_3c_L5 <-add_Y_to_list(sim500_70_10V_lowINTER_3c_L5,  k)

sim500_70_10V_lowBTW_3c_L1 <-add_Y_to_list(sim500_70_10V_lowBTW_3c_L1,  k)
sim500_70_10V_lowBTW_3c_L2 <-add_Y_to_list(sim500_70_10V_lowBTW_3c_L2,  k)
sim500_70_10V_lowBTW_3c_L3 <-add_Y_to_list(sim500_70_10V_lowBTW_3c_L3,  k)
sim500_70_10V_lowBTW_3c_L4 <-add_Y_to_list(sim500_70_10V_lowBTW_3c_L4,  k)
sim500_70_10V_lowBTW_3c_L5 <-add_Y_to_list(sim500_70_10V_lowBTW_3c_L5,  k)


sim1000_30_10V_lowINTER_3c_L1 <-add_Y_to_list(sim1000_30_10V_lowINTER_3c_L1,  k)
sim1000_30_10V_lowINTER_3c_L2 <-add_Y_to_list(sim1000_30_10V_lowINTER_3c_L2,  k)
sim1000_30_10V_lowINTER_3c_L3 <-add_Y_to_list(sim1000_30_10V_lowINTER_3c_L3,  k)
sim1000_30_10V_lowINTER_3c_L4 <-add_Y_to_list(sim1000_30_10V_lowINTER_3c_L4,  k)
sim1000_30_10V_lowINTER_3c_L5 <-add_Y_to_list(sim1000_30_10V_lowINTER_3c_L5,  k)

sim1000_30_10V_lowBTW_3c_L1 <-add_Y_to_list(sim1000_30_10V_lowBTW_3c_L1,  k)
sim1000_30_10V_lowBTW_3c_L2 <-add_Y_to_list(sim1000_30_10V_lowBTW_3c_L2,  k)
sim1000_30_10V_lowBTW_3c_L3 <-add_Y_to_list(sim1000_30_10V_lowBTW_3c_L3,  k)
sim1000_30_10V_lowBTW_3c_L4 <-add_Y_to_list(sim1000_30_10V_lowBTW_3c_L4,  k)
sim1000_30_10V_lowBTW_3c_L5 <-add_Y_to_list(sim1000_30_10V_lowBTW_3c_L5,  k)

sim1000_70_10V_lowINTER_3c_L1 <-add_Y_to_list(sim1000_70_10V_lowINTER_3c_L1,  k)
sim1000_70_10V_lowINTER_3c_L2 <-add_Y_to_list(sim1000_70_10V_lowINTER_3c_L2,  k)
sim1000_70_10V_lowINTER_3c_L3 <-add_Y_to_list(sim1000_70_10V_lowINTER_3c_L3,  k)
sim1000_70_10V_lowINTER_3c_L4 <-add_Y_to_list(sim1000_70_10V_lowINTER_3c_L4,  k)
sim1000_70_10V_lowINTER_3c_L5 <-add_Y_to_list(sim1000_70_10V_lowINTER_3c_L5,  k)

sim1000_70_10V_lowBTW_3c_L1 <-add_Y_to_list(sim1000_70_10V_lowBTW_3c_L1,  k)
sim1000_70_10V_lowBTW_3c_L2 <-add_Y_to_list(sim1000_70_10V_lowBTW_3c_L2,  k)
sim1000_70_10V_lowBTW_3c_L3 <-add_Y_to_list(sim1000_70_10V_lowBTW_3c_L3,  k)
sim1000_70_10V_lowBTW_3c_L4 <-add_Y_to_list(sim1000_70_10V_lowBTW_3c_L4,  k)
sim1000_70_10V_lowBTW_3c_L5 <-add_Y_to_list(sim1000_70_10V_lowBTW_3c_L5,  k)


#######中相关性#####
#中相关性
sim500_30_10V_midINTER_3c_L1 <-add_Y_to_list(sim500_30_10V_midINTER_3c_L1,  k)
sim500_30_10V_midINTER_3c_L2 <-add_Y_to_list(sim500_30_10V_midINTER_3c_L2,  k)
sim500_30_10V_midINTER_3c_L3 <-add_Y_to_list(sim500_30_10V_midINTER_3c_L3,  k)
sim500_30_10V_midINTER_3c_L4 <-add_Y_to_list(sim500_30_10V_midINTER_3c_L4,  k)
sim500_30_10V_midINTER_3c_L5 <-add_Y_to_list(sim500_30_10V_midINTER_3c_L5,  k)

sim500_30_10V_midBTW_3c_L1 <-add_Y_to_list(sim500_30_10V_midBTW_3c_L1,  k)
sim500_30_10V_midBTW_3c_L2 <-add_Y_to_list(sim500_30_10V_midBTW_3c_L2,  k)
sim500_30_10V_midBTW_3c_L3 <-add_Y_to_list(sim500_30_10V_midBTW_3c_L3,  k)
sim500_30_10V_midBTW_3c_L4 <-add_Y_to_list(sim500_30_10V_midBTW_3c_L4,  k)
sim500_30_10V_midBTW_3c_L5 <-add_Y_to_list(sim500_30_10V_midBTW_3c_L5,  k)

sim500_70_10V_midINTER_3c_L1 <-add_Y_to_list(sim500_70_10V_midINTER_3c_L1,  k)
sim500_70_10V_midINTER_3c_L2 <-add_Y_to_list(sim500_70_10V_midINTER_3c_L2,  k)
sim500_70_10V_midINTER_3c_L3 <-add_Y_to_list(sim500_70_10V_midINTER_3c_L3,  k)
sim500_70_10V_midINTER_3c_L4 <-add_Y_to_list(sim500_70_10V_midINTER_3c_L4,  k)
sim500_70_10V_midINTER_3c_L5 <-add_Y_to_list(sim500_70_10V_midINTER_3c_L5,  k)

sim500_70_10V_midBTW_3c_L1 <-add_Y_to_list(sim500_70_10V_midBTW_3c_L1,  k)
sim500_70_10V_midBTW_3c_L2 <-add_Y_to_list(sim500_70_10V_midBTW_3c_L2,  k)
sim500_70_10V_midBTW_3c_L3 <-add_Y_to_list(sim500_70_10V_midBTW_3c_L3,  k)
sim500_70_10V_midBTW_3c_L4 <-add_Y_to_list(sim500_70_10V_midBTW_3c_L4,  k)
sim500_70_10V_midBTW_3c_L5 <-add_Y_to_list(sim500_70_10V_midBTW_3c_L5,  k)


sim1000_30_10V_midINTER_3c_L1 <-add_Y_to_list(sim1000_30_10V_midINTER_3c_L1,  k)
sim1000_30_10V_midINTER_3c_L2 <-add_Y_to_list(sim1000_30_10V_midINTER_3c_L2,  k)
sim1000_30_10V_midINTER_3c_L3 <-add_Y_to_list(sim1000_30_10V_midINTER_3c_L3,  k)
sim1000_30_10V_midINTER_3c_L4 <-add_Y_to_list(sim1000_30_10V_midINTER_3c_L4,  k)
sim1000_30_10V_midINTER_3c_L5 <-add_Y_to_list(sim1000_30_10V_midINTER_3c_L5,  k)

sim1000_30_10V_midBTW_3c_L1 <-add_Y_to_list(sim1000_30_10V_midBTW_3c_L1,  k)
sim1000_30_10V_midBTW_3c_L2 <-add_Y_to_list(sim1000_30_10V_midBTW_3c_L2,  k)
sim1000_30_10V_midBTW_3c_L3 <-add_Y_to_list(sim1000_30_10V_midBTW_3c_L3,  k)
sim1000_30_10V_midBTW_3c_L4 <-add_Y_to_list(sim1000_30_10V_midBTW_3c_L4,  k)
sim1000_30_10V_midBTW_3c_L5 <-add_Y_to_list(sim1000_30_10V_midBTW_3c_L5,  k)

sim1000_70_10V_midINTER_3c_L1 <-add_Y_to_list(sim1000_70_10V_midINTER_3c_L1,  k)
sim1000_70_10V_midINTER_3c_L2 <-add_Y_to_list(sim1000_70_10V_midINTER_3c_L2,  k)
sim1000_70_10V_midINTER_3c_L3 <-add_Y_to_list(sim1000_70_10V_midINTER_3c_L3,  k)
sim1000_70_10V_midINTER_3c_L4 <-add_Y_to_list(sim1000_70_10V_midINTER_3c_L4,  k)
sim1000_70_10V_midINTER_3c_L5 <-add_Y_to_list(sim1000_70_10V_midINTER_3c_L5,  k)

sim1000_70_10V_midBTW_3c_L1 <-add_Y_to_list(sim1000_70_10V_midBTW_3c_L1,  k)
sim1000_70_10V_midBTW_3c_L2 <-add_Y_to_list(sim1000_70_10V_midBTW_3c_L2,  k)
sim1000_70_10V_midBTW_3c_L3 <-add_Y_to_list(sim1000_70_10V_midBTW_3c_L3,  k)
sim1000_70_10V_midBTW_3c_L4 <-add_Y_to_list(sim1000_70_10V_midBTW_3c_L4,  k)
sim1000_70_10V_midBTW_3c_L5 <-add_Y_to_list(sim1000_70_10V_midBTW_3c_L5,  k)



######高相关性######
sim500_30_10V_highINTER_3c_L1 <-add_Y_to_list(sim500_30_10V_highINTER_3c_L1,  k)
sim500_30_10V_highINTER_3c_L2 <-add_Y_to_list(sim500_30_10V_highINTER_3c_L2,  k)
sim500_30_10V_highINTER_3c_L3 <-add_Y_to_list(sim500_30_10V_highINTER_3c_L3,  k)
sim500_30_10V_highINTER_3c_L4 <-add_Y_to_list(sim500_30_10V_highINTER_3c_L4,  k)
sim500_30_10V_highINTER_3c_L5 <-add_Y_to_list(sim500_30_10V_highINTER_3c_L5,  k)

sim500_30_10V_highBTW_3c_L1 <-add_Y_to_list(sim500_30_10V_highBTW_3c_L1,  k)
sim500_30_10V_highBTW_3c_L2 <-add_Y_to_list(sim500_30_10V_highBTW_3c_L2,  k)
sim500_30_10V_highBTW_3c_L3 <-add_Y_to_list(sim500_30_10V_highBTW_3c_L3,  k)
sim500_30_10V_highBTW_3c_L4 <-add_Y_to_list(sim500_30_10V_highBTW_3c_L4,  k)
sim500_30_10V_highBTW_3c_L5 <-add_Y_to_list(sim500_30_10V_highBTW_3c_L5,  k)

sim500_70_10V_highINTER_3c_L1 <-add_Y_to_list(sim500_70_10V_highINTER_3c_L1,  k)
sim500_70_10V_highINTER_3c_L2 <-add_Y_to_list(sim500_70_10V_highINTER_3c_L2,  k)
sim500_70_10V_highINTER_3c_L3 <-add_Y_to_list(sim500_70_10V_highINTER_3c_L3,  k)
sim500_70_10V_highINTER_3c_L4 <-add_Y_to_list(sim500_70_10V_highINTER_3c_L4,  k)
sim500_70_10V_highINTER_3c_L5 <-add_Y_to_list(sim500_70_10V_highINTER_3c_L5,  k)

sim500_70_10V_highBTW_3c_L1 <-add_Y_to_list(sim500_70_10V_highBTW_3c_L1,  k)
sim500_70_10V_highBTW_3c_L2 <-add_Y_to_list(sim500_70_10V_highBTW_3c_L2,  k)
sim500_70_10V_highBTW_3c_L3 <-add_Y_to_list(sim500_70_10V_highBTW_3c_L3,  k)
sim500_70_10V_highBTW_3c_L4 <-add_Y_to_list(sim500_70_10V_highBTW_3c_L4,  k)
sim500_70_10V_highBTW_3c_L5 <-add_Y_to_list(sim500_70_10V_highBTW_3c_L5,  k)


sim1000_30_10V_highINTER_3c_L1 <-add_Y_to_list(sim1000_30_10V_highINTER_3c_L1,  k)
sim1000_30_10V_highINTER_3c_L2 <-add_Y_to_list(sim1000_30_10V_highINTER_3c_L2,  k)
sim1000_30_10V_highINTER_3c_L3 <-add_Y_to_list(sim1000_30_10V_highINTER_3c_L3,  k)
sim1000_30_10V_highINTER_3c_L4 <-add_Y_to_list(sim1000_30_10V_highINTER_3c_L4,  k)
sim1000_30_10V_highINTER_3c_L5 <-add_Y_to_list(sim1000_30_10V_highINTER_3c_L5,  k)

sim1000_30_10V_highBTW_3c_L1 <-add_Y_to_list(sim1000_30_10V_highBTW_3c_L1,  k)
sim1000_30_10V_highBTW_3c_L2 <-add_Y_to_list(sim1000_30_10V_highBTW_3c_L2,  k)
sim1000_30_10V_highBTW_3c_L3 <-add_Y_to_list(sim1000_30_10V_highBTW_3c_L3,  k)
sim1000_30_10V_highBTW_3c_L4 <-add_Y_to_list(sim1000_30_10V_highBTW_3c_L4,  k)
sim1000_30_10V_highBTW_3c_L5 <-add_Y_to_list(sim1000_30_10V_highBTW_3c_L5,  k)

sim1000_70_10V_highINTER_3c_L1 <-add_Y_to_list(sim1000_70_10V_highINTER_3c_L1,  k)
sim1000_70_10V_highINTER_3c_L2 <-add_Y_to_list(sim1000_70_10V_highINTER_3c_L2,  k)
sim1000_70_10V_highINTER_3c_L3 <-add_Y_to_list(sim1000_70_10V_highINTER_3c_L3,  k)
sim1000_70_10V_highINTER_3c_L4 <-add_Y_to_list(sim1000_70_10V_highINTER_3c_L4,  k)
sim1000_70_10V_highINTER_3c_L5 <-add_Y_to_list(sim1000_70_10V_highINTER_3c_L5,  k)

sim1000_70_10V_highBTW_3c_L1 <-add_Y_to_list(sim1000_70_10V_highBTW_3c_L1,  k)
sim1000_70_10V_highBTW_3c_L2 <-add_Y_to_list(sim1000_70_10V_highBTW_3c_L2,  k)
sim1000_70_10V_highBTW_3c_L3 <-add_Y_to_list(sim1000_70_10V_highBTW_3c_L3,  k)
sim1000_70_10V_highBTW_3c_L4 <-add_Y_to_list(sim1000_70_10V_highBTW_3c_L4,  k)
sim1000_70_10V_highBTW_3c_L5 <-add_Y_to_list(sim1000_70_10V_highBTW_3c_L5,  k)

######






################保存数据##################

# 设置输出目录
out_dir_low <- "F:/文章/大论文/程序/模拟数据/数据3_withY/低相关"
out_dir_mid <- "F:/文章/大论文/程序/模拟数据/数据3_withY/中相关"  
out_dir_high <- "F:/文章/大论文/程序/模拟数据/数据3_withY/高相关"

# 确保目录存在
dir.create(out_dir_low, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_mid, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_high, recursive = TRUE, showWarnings = FALSE)

################## 低相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_lowINTER_3c_L1, path = file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L2, path = file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L3, path = file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L4, path = file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowINTER_3c_L5, path = file.path(out_dir_low, "sim500_30_10V_lowINTER_3c_L5.xlsx"))

write_xlsx(sim500_30_10V_lowBTW_3c_L1, path = file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L2, path = file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L3, path = file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L4, path = file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_lowBTW_3c_L5, path = file.path(out_dir_low, "sim500_30_10V_lowBTW_3c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_lowINTER_3c_L1, path = file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L2, path = file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L3, path = file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L4, path = file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowINTER_3c_L5, path = file.path(out_dir_low, "sim500_70_10V_lowINTER_3c_L5.xlsx"))

write_xlsx(sim500_70_10V_lowBTW_3c_L1, path = file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L2, path = file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L3, path = file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L4, path = file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_lowBTW_3c_L5, path = file.path(out_dir_low, "sim500_70_10V_lowBTW_3c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_lowINTER_3c_L1, path = file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L2, path = file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L3, path = file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L4, path = file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowINTER_3c_L5, path = file.path(out_dir_low, "sim1000_30_10V_lowINTER_3c_L5.xlsx"))

write_xlsx(sim1000_30_10V_lowBTW_3c_L1, path = file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L2, path = file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L3, path = file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L4, path = file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_lowBTW_3c_L5, path = file.path(out_dir_low, "sim1000_30_10V_lowBTW_3c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_lowINTER_3c_L1, path = file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L2, path = file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L3, path = file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L4, path = file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowINTER_3c_L5, path = file.path(out_dir_low, "sim1000_70_10V_lowINTER_3c_L5.xlsx"))

write_xlsx(sim1000_70_10V_lowBTW_3c_L1, path = file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L2, path = file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L3, path = file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L4, path = file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_lowBTW_3c_L5, path = file.path(out_dir_low, "sim1000_70_10V_lowBTW_3c_L5.xlsx"))

################## 中相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_midINTER_3c_L1, path = file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L2, path = file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L3, path = file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L4, path = file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_midINTER_3c_L5, path = file.path(out_dir_mid, "sim500_30_10V_midINTER_3c_L5.xlsx"))

write_xlsx(sim500_30_10V_midBTW_3c_L1, path = file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L2, path = file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L3, path = file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L4, path = file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_midBTW_3c_L5, path = file.path(out_dir_mid, "sim500_30_10V_midBTW_3c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_midINTER_3c_L1, path = file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L2, path = file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L3, path = file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L4, path = file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_midINTER_3c_L5, path = file.path(out_dir_mid, "sim500_70_10V_midINTER_3c_L5.xlsx"))

write_xlsx(sim500_70_10V_midBTW_3c_L1, path = file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L2, path = file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L3, path = file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L4, path = file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_midBTW_3c_L5, path = file.path(out_dir_mid, "sim500_70_10V_midBTW_3c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_midINTER_3c_L1, path = file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L2, path = file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L3, path = file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L4, path = file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midINTER_3c_L5, path = file.path(out_dir_mid, "sim1000_30_10V_midINTER_3c_L5.xlsx"))

write_xlsx(sim1000_30_10V_midBTW_3c_L1, path = file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L2, path = file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L3, path = file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L4, path = file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_midBTW_3c_L5, path = file.path(out_dir_mid, "sim1000_30_10V_midBTW_3c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_midINTER_3c_L1, path = file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L2, path = file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L3, path = file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L4, path = file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midINTER_3c_L5, path = file.path(out_dir_mid, "sim1000_70_10V_midINTER_3c_L5.xlsx"))

write_xlsx(sim1000_70_10V_midBTW_3c_L1, path = file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L2, path = file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L3, path = file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L4, path = file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_midBTW_3c_L5, path = file.path(out_dir_mid, "sim1000_70_10V_midBTW_3c_L5.xlsx"))

################## 高相关性数据导出 ##################
# 样本量500，变量30
write_xlsx(sim500_30_10V_highINTER_3c_L1, path = file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L2, path = file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L3, path = file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L4, path = file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_highINTER_3c_L5, path = file.path(out_dir_high, "sim500_30_10V_highINTER_3c_L5.xlsx"))

write_xlsx(sim500_30_10V_highBTW_3c_L1, path = file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L2, path = file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L3, path = file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L4, path = file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim500_30_10V_highBTW_3c_L5, path = file.path(out_dir_high, "sim500_30_10V_highBTW_3c_L5.xlsx"))

# 样本量500，变量70
write_xlsx(sim500_70_10V_highINTER_3c_L1, path = file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L2, path = file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L3, path = file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L4, path = file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_highINTER_3c_L5, path = file.path(out_dir_high, "sim500_70_10V_highINTER_3c_L5.xlsx"))

write_xlsx(sim500_70_10V_highBTW_3c_L1, path = file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L2, path = file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L3, path = file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L4, path = file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim500_70_10V_highBTW_3c_L5, path = file.path(out_dir_high, "sim500_70_10V_highBTW_3c_L5.xlsx"))

# 样本量1000，变量30
write_xlsx(sim1000_30_10V_highINTER_3c_L1, path = file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L2, path = file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L3, path = file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L4, path = file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highINTER_3c_L5, path = file.path(out_dir_high, "sim1000_30_10V_highINTER_3c_L5.xlsx"))

write_xlsx(sim1000_30_10V_highBTW_3c_L1, path = file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L2, path = file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L3, path = file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L4, path = file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim1000_30_10V_highBTW_3c_L5, path = file.path(out_dir_high, "sim1000_30_10V_highBTW_3c_L5.xlsx"))

# 样本量1000，变量70
write_xlsx(sim1000_70_10V_highINTER_3c_L1, path = file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L2, path = file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L3, path = file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L4, path = file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highINTER_3c_L5, path = file.path(out_dir_high, "sim1000_70_10V_highINTER_3c_L5.xlsx"))

write_xlsx(sim1000_70_10V_highBTW_3c_L1, path = file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L1.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L2, path = file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L2.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L3, path = file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L3.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L4, path = file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L4.xlsx"))
write_xlsx(sim1000_70_10V_highBTW_3c_L5, path = file.path(out_dir_high, "sim1000_70_10V_highBTW_3c_L5.xlsx"))

# 打印完成信息
cat("所有数据已成功导出到指定目录！\n")
cat("低相关性数据：", out_dir_low, "\n")
cat("中相关性数据：", out_dir_mid, "\n")
cat("高相关性数据：", out_dir_high, "\n")








#############尝试+验证###############
simdata2_with_Y <- add_Y(simdata2, k)
simdata3_with_Y <- add_Y(simdata3, k)


head(simdata3_with_Y)

#尝试数据1
write.xlsx(simdata3_with_Y,"F:/文章/大论文/程序/模拟数据/simdata3_withY.xlsx")

#########因子分析


## 1. 行筛选：time==1
tmp <- simdata3[simdata3$time == 1, ]

## 2. 列筛选：ID + V1-V10
v_cols <- grep("^(V\\d+_)", names(tmp), value = TRUE)
efa_dat <- tmp[ , c("ID", v_cols)]

## 3. 把 ID 设为行名后丢掉 ID 列（纯 Base R）
rownames(efa_dat) <- efa_dat$ID
efa_dat <- efa_dat[ , !(names(efa_dat) %in% "ID")]

## 4. 后续 psych 包代码不变
library(psych)
parallel <- fa.parallel(efa_dat, fm = "ml", fa = "fa")
fit2 <- fa(efa_dat, nfactors = 2, fm = "ml", rotate = "oblimin")
print(fit2, digits = 3)
summary(fit2)

target <- matrix(0, 10, 2)
target[1:3, 1] <- 1
target[4:6, 2] <- 1
congruence <- factor.congruence(fit2$loadings, target)
congruence






####
# 1. 准备数据：提取每个ID在time=1时的观测（基线测量）
simdata2_with_Y <- add_Y(simdata2, k)
baseline_data <- simdata3_with_Y %>%
  filter(time == 1) %>%
  select(ID, starts_with("V"))  # 选择ID和所有V开头的变量

# 查看数据结构
head(baseline_data)

# 2. 仅保留10个变量用于因子分析
efa_data <- baseline_data %>% 
  select(V1_0.55:V10_0.60)

# 3. 检查数据是否适合做因子分析
# 计算KMO取样适当性量数
kmo_result <- KMO(efa_data)
print(kmo_result)

# 巴特利特球形检验
bartlett_result <- cortest.bartlett(cor(efa_data), n = nrow(efa_data))
print(bartlett_result)

# 4. 确定因子数量
# 平行分析（建议的方法）
parallel <- fa.parallel(efa_data, fm = "ml", fa = "fa")
print(parallel)

# 5. 进行探索性因子分析
# 使用最大似然法提取因子，并进行斜交旋转（promax）
# 根据平行分析的结果选择因子数，这里假设平行分析建议2个因子
efa_result <- fa(efa_data, 
                 nfactors = 2, 
                 rotate = "promax",
                 fm = "ml")

# 6. 查看结果
print(efa_result)  # 显示因子载荷等详细信息

# 可视化因子载荷
fa.diagram(efa_result)

# 7. 提取因子载荷
loadings <- efa_result$loadings
print(loadings)

# 将载荷矩阵转换为更容易阅读的数据框格式
loadings_df <- as.data.frame(matrix(as.numeric(loadings), 
                                    attributes(loadings)$dim, 
                                    dimnames = attributes(loadings)$dimnames))
print(loadings_df)

# 8. 计算每个变量的共同度（communality）
communality <- efa_result$communality
print(communality)

#########fold#################
make_positive <- function(M, tol = 1e-8) {
  M <- abs(M)                 # 全部变正
  M <- (M + t(M)) / 2         # 强制对称
  eig <- eigen(M, symmetric = TRUE)
  eig$values <- pmax(eig$values, tol)  # 负特征值压到 tol
  M_new <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  return(M_new)
}

# 处理你的三个矩阵
cov_matrix_high1 <- make_positive(cov_matrix_high1)
cov_matrix_high2 <- make_positive(cov_matrix_high2)
cov_matrix_high3 <- make_positive(cov_matrix_high3)
head(cov_matrix_high1)


simdata3<- sim_multi_class1(
  n = 500, 
  n_time = 5, 
  param_list = param_list_custom,
  Sigma1 = cov_matrix_high1,
  Sigma2 = cov_matrix_high2,
  Sigma3 = cov_matrix_high3,
  seed = 123   # 从123开始，每次+1
)
head(simdata3)


Sigma_int  <- cov_matrix_high1[c(1,3,5), c(1,3,5)]   # V1-3 截距
Sigma_slp  <- cov_matrix_high1[c(2,4,6), c(2,4,6)]   # V1-3 斜率
Sigma_cross<- cov_matrix_high1[c(1,3,5), c(2,4,6)]   # 截距-斜率
Sigma_V1_3 <- Sigma_int + Sigma_slp + Sigma_cross + t(Sigma_cross)
head(Sigma_V1_3)









