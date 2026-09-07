library(tidyverse)
library(flexsurv)   # 参数生存模型
library(survival)   # 基础生存分析
library(writexl)    # 写出 xlsx
library(readxl)     # 读入 xlsx（tidyverse 不含）
set.seed(123)
options(scipen = 999)
options(pillar.width = Inf)

in_dir<- "F:/文章/大论文/程序Trae/数据_resultRF/"

####程序#######

standarize_data <- function(df) {
  df %>% 
    dplyr::select(统计量, 数值, dataset) %>% 
    dplyr::group_by(dataset, 统计量) %>% 
    dplyr::summarise(数值 = mean(数值, na.rm = TRUE), .groups = "drop") %>% 
    tidyr::pivot_wider(
      names_from = 统计量,
      values_from = 数值
    ) %>% 
    dplyr::mutate(sim = paste0("sim", dataset)) %>% 
    dplyr::select(sim,
                  AUC = `时间依赖AUC`,
                  BS  = `Brier Score`,
                  Cindex = `C-index`)
}



######读取数据######
###########resultRF_sim500_30_10V_highBTW_3c################
resultRF_sim500_30_10V_highBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highBTW_3c_L1.xlsx"))
resultRF_sim500_30_10V_highBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highBTW_3c_L2.xlsx"))
resultRF_sim500_30_10V_highBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highBTW_3c_L3.xlsx"))
resultRF_sim500_30_10V_highBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highBTW_3c_L4.xlsx"))
resultRF_sim500_30_10V_highBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highBTW_3c_L5.xlsx"))

resultRF_sim500_30_10V_highBTW_3c_L1<-standarize_data(resultRF_sim500_30_10V_highBTW_3c_L1)
resultRF_sim500_30_10V_highBTW_3c_L2<-standarize_data(resultRF_sim500_30_10V_highBTW_3c_L2)
resultRF_sim500_30_10V_highBTW_3c_L3<-standarize_data(resultRF_sim500_30_10V_highBTW_3c_L3)
resultRF_sim500_30_10V_highBTW_3c_L4<-standarize_data(resultRF_sim500_30_10V_highBTW_3c_L4)
resultRF_sim500_30_10V_highBTW_3c_L5<-standarize_data(resultRF_sim500_30_10V_highBTW_3c_L5)

resultRF_sim500_30_10V_highBTW_3c<-rbind(
  resultRF_sim500_30_10V_highBTW_3c_L1,
  resultRF_sim500_30_10V_highBTW_3c_L2,
  resultRF_sim500_30_10V_highBTW_3c_L3,
  resultRF_sim500_30_10V_highBTW_3c_L4,
  resultRF_sim500_30_10V_highBTW_3c_L5
)

RESULTRF_sim500_30_10V_highBTW_3c <- data.frame(
  var = "sim500_30_10V_highBTW_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_highBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_30_10V_highINTER_3c#######
resultRF_sim500_30_10V_highINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highINTER_3c_L1.xlsx"))
resultRF_sim500_30_10V_highINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highINTER_3c_L2.xlsx"))
resultRF_sim500_30_10V_highINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highINTER_3c_L3.xlsx"))
resultRF_sim500_30_10V_highINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highINTER_3c_L4.xlsx"))
resultRF_sim500_30_10V_highINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_highINTER_3c_L5.xlsx"))

resultRF_sim500_30_10V_highINTER_3c_L1<-standarize_data(resultRF_sim500_30_10V_highINTER_3c_L1) 
resultRF_sim500_30_10V_highINTER_3c_L2<-standarize_data(resultRF_sim500_30_10V_highINTER_3c_L2)
resultRF_sim500_30_10V_highINTER_3c_L3<-standarize_data(resultRF_sim500_30_10V_highINTER_3c_L3)
resultRF_sim500_30_10V_highINTER_3c_L4<-standarize_data(resultRF_sim500_30_10V_highINTER_3c_L4)
resultRF_sim500_30_10V_highINTER_3c_L5<-standarize_data(resultRF_sim500_30_10V_highINTER_3c_L5)

resultRF_sim500_30_10V_highINTER_3c<-rbind(
  resultRF_sim500_30_10V_highINTER_3c_L1,
  resultRF_sim500_30_10V_highINTER_3c_L2,
  resultRF_sim500_30_10V_highINTER_3c_L3,
  resultRF_sim500_30_10V_highINTER_3c_L4,
  resultRF_sim500_30_10V_highINTER_3c_L5
)

RESULTRF_sim500_30_10V_highINTER_3c <- data.frame(
  var = "sim500_30_10V_highINTER_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_highINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_30_10V_midBTW_3c#######
resultRF_sim500_30_10V_midBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midBTW_3c_L1.xlsx"))
resultRF_sim500_30_10V_midBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midBTW_3c_L2.xlsx"))
resultRF_sim500_30_10V_midBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midBTW_3c_L3.xlsx"))
resultRF_sim500_30_10V_midBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midBTW_3c_L4.xlsx"))
resultRF_sim500_30_10V_midBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midBTW_3c_L5.xlsx"))

resultRF_sim500_30_10V_midBTW_3c_L1<-standarize_data(resultRF_sim500_30_10V_midBTW_3c_L1)
resultRF_sim500_30_10V_midBTW_3c_L2<-standarize_data(resultRF_sim500_30_10V_midBTW_3c_L2)
resultRF_sim500_30_10V_midBTW_3c_L3<-standarize_data(resultRF_sim500_30_10V_midBTW_3c_L3)
resultRF_sim500_30_10V_midBTW_3c_L4<-standarize_data(resultRF_sim500_30_10V_midBTW_3c_L4)
resultRF_sim500_30_10V_midBTW_3c_L5<-standarize_data(resultRF_sim500_30_10V_midBTW_3c_L5)

resultRF_sim500_30_10V_midBTW_3c<-rbind(
  resultRF_sim500_30_10V_midBTW_3c_L1,
  resultRF_sim500_30_10V_midBTW_3c_L2,
  resultRF_sim500_30_10V_midBTW_3c_L3,
  resultRF_sim500_30_10V_midBTW_3c_L4,
  resultRF_sim500_30_10V_midBTW_3c_L5
)

RESULTRF_sim500_30_10V_midBTW_3c <- data.frame(
  var = "sim500_30_10V_midBTW_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_midBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_30_10V_midINTER_3c#######
resultRF_sim500_30_10V_midINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midINTER_3c_L1.xlsx"))
resultRF_sim500_30_10V_midINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midINTER_3c_L2.xlsx"))
resultRF_sim500_30_10V_midINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midINTER_3c_L3.xlsx"))
resultRF_sim500_30_10V_midINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midINTER_3c_L4.xlsx"))
resultRF_sim500_30_10V_midINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_midINTER_3c_L5.xlsx"))

resultRF_sim500_30_10V_midINTER_3c_L1<-standarize_data(resultRF_sim500_30_10V_midINTER_3c_L1)
resultRF_sim500_30_10V_midINTER_3c_L2<-standarize_data(resultRF_sim500_30_10V_midINTER_3c_L2)
resultRF_sim500_30_10V_midINTER_3c_L3<-standarize_data(resultRF_sim500_30_10V_midINTER_3c_L3)
resultRF_sim500_30_10V_midINTER_3c_L4<-standarize_data(resultRF_sim500_30_10V_midINTER_3c_L4)
resultRF_sim500_30_10V_midINTER_3c_L5<-standarize_data(resultRF_sim500_30_10V_midINTER_3c_L5)

resultRF_sim500_30_10V_midINTER_3c<-rbind(
  resultRF_sim500_30_10V_midINTER_3c_L1,
  resultRF_sim500_30_10V_midINTER_3c_L2,
  resultRF_sim500_30_10V_midINTER_3c_L3,
  resultRF_sim500_30_10V_midINTER_3c_L4,
  resultRF_sim500_30_10V_midINTER_3c_L5
)

RESULTRF_sim500_30_10V_midINTER_3c <- data.frame(
  var = "sim500_30_10V_midINTER_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_midINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_30_10V_lowBTW_3c#######
resultRF_sim500_30_10V_lowBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowBTW_3c_L1.xlsx"))
resultRF_sim500_30_10V_lowBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowBTW_3c_L2.xlsx"))
resultRF_sim500_30_10V_lowBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowBTW_3c_L3.xlsx"))
resultRF_sim500_30_10V_lowBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowBTW_3c_L4.xlsx"))
resultRF_sim500_30_10V_lowBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowBTW_3c_L5.xlsx"))

resultRF_sim500_30_10V_lowBTW_3c_L1<-standarize_data(resultRF_sim500_30_10V_lowBTW_3c_L1)
resultRF_sim500_30_10V_lowBTW_3c_L2<-standarize_data(resultRF_sim500_30_10V_lowBTW_3c_L2)
resultRF_sim500_30_10V_lowBTW_3c_L3<-standarize_data(resultRF_sim500_30_10V_lowBTW_3c_L3)
resultRF_sim500_30_10V_lowBTW_3c_L4<-standarize_data(resultRF_sim500_30_10V_lowBTW_3c_L4)
resultRF_sim500_30_10V_lowBTW_3c_L5<-standarize_data(resultRF_sim500_30_10V_lowBTW_3c_L5)

resultRF_sim500_30_10V_lowBTW_3c<-rbind(
  resultRF_sim500_30_10V_lowBTW_3c_L1,
  resultRF_sim500_30_10V_lowBTW_3c_L2,
  resultRF_sim500_30_10V_lowBTW_3c_L3,
  resultRF_sim500_30_10V_lowBTW_3c_L4,
  resultRF_sim500_30_10V_lowBTW_3c_L5
)

RESULTRF_sim500_30_10V_lowBTW_3c <- data.frame(
  var = "sim500_30_10V_lowBTW_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_30_10V_lowINTER_3c#######
resultRF_sim500_30_10V_lowINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowINTER_3c_L1.xlsx"))
resultRF_sim500_30_10V_lowINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowINTER_3c_L2.xlsx"))
resultRF_sim500_30_10V_lowINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowINTER_3c_L3.xlsx"))
resultRF_sim500_30_10V_lowINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowINTER_3c_L4.xlsx"))
resultRF_sim500_30_10V_lowINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_30_10V_lowINTER_3c_L5.xlsx"))

resultRF_sim500_30_10V_lowINTER_3c_L1<-standarize_data(resultRF_sim500_30_10V_lowINTER_3c_L1)
resultRF_sim500_30_10V_lowINTER_3c_L2<-standarize_data(resultRF_sim500_30_10V_lowINTER_3c_L2)
resultRF_sim500_30_10V_lowINTER_3c_L3<-standarize_data(resultRF_sim500_30_10V_lowINTER_3c_L3)
resultRF_sim500_30_10V_lowINTER_3c_L4<-standarize_data(resultRF_sim500_30_10V_lowINTER_3c_L4)
resultRF_sim500_30_10V_lowINTER_3c_L5<-standarize_data(resultRF_sim500_30_10V_lowINTER_3c_L5)

resultRF_sim500_30_10V_lowINTER_3c<-rbind(
  resultRF_sim500_30_10V_lowINTER_3c_L1,
  resultRF_sim500_30_10V_lowINTER_3c_L2,
  resultRF_sim500_30_10V_lowINTER_3c_L3,
  resultRF_sim500_30_10V_lowINTER_3c_L4,
  resultRF_sim500_30_10V_lowINTER_3c_L5
)

RESULTRF_sim500_30_10V_lowINTER_3c <- data.frame(
  var = "sim500_30_10V_lowINTER_3c",
  AUC_mean = mean(resultRF_sim500_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_30_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_30_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_highBTW_3c#######
resultRF_sim500_70_10V_highBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highBTW_3c_L1.xlsx"))
resultRF_sim500_70_10V_highBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highBTW_3c_L2.xlsx"))
resultRF_sim500_70_10V_highBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highBTW_3c_L3.xlsx"))
resultRF_sim500_70_10V_highBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highBTW_3c_L4.xlsx"))
resultRF_sim500_70_10V_highBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highBTW_3c_L5.xlsx"))

resultRF_sim500_70_10V_highBTW_3c_L1<-standarize_data(resultRF_sim500_70_10V_highBTW_3c_L1)
resultRF_sim500_70_10V_highBTW_3c_L2<-standarize_data(resultRF_sim500_70_10V_highBTW_3c_L2)
resultRF_sim500_70_10V_highBTW_3c_L3<-standarize_data(resultRF_sim500_70_10V_highBTW_3c_L3)
resultRF_sim500_70_10V_highBTW_3c_L4<-standarize_data(resultRF_sim500_70_10V_highBTW_3c_L4)
resultRF_sim500_70_10V_highBTW_3c_L5<-standarize_data(resultRF_sim500_70_10V_highBTW_3c_L5)

resultRF_sim500_70_10V_highBTW_3c<-rbind(
  resultRF_sim500_70_10V_highBTW_3c_L1,
  resultRF_sim500_70_10V_highBTW_3c_L2,
  resultRF_sim500_70_10V_highBTW_3c_L3,
  resultRF_sim500_70_10V_highBTW_3c_L4,
  resultRF_sim500_70_10V_highBTW_3c_L5
)

RESULTRF_sim500_70_10V_highBTW_3c <- data.frame(
  var = "sim500_70_10V_highBTW_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_highBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_highINTER_3c#######
resultRF_sim500_70_10V_highINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highINTER_3c_L1.xlsx"))
resultRF_sim500_70_10V_highINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highINTER_3c_L2.xlsx"))
resultRF_sim500_70_10V_highINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highINTER_3c_L3.xlsx"))
resultRF_sim500_70_10V_highINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highINTER_3c_L4.xlsx"))
resultRF_sim500_70_10V_highINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_highINTER_3c_L5.xlsx"))

resultRF_sim500_70_10V_highINTER_3c_L1<-standarize_data(resultRF_sim500_70_10V_highINTER_3c_L1)
resultRF_sim500_70_10V_highINTER_3c_L2<-standarize_data(resultRF_sim500_70_10V_highINTER_3c_L2)
resultRF_sim500_70_10V_highINTER_3c_L3<-standarize_data(resultRF_sim500_70_10V_highINTER_3c_L3)
resultRF_sim500_70_10V_highINTER_3c_L4<-standarize_data(resultRF_sim500_70_10V_highINTER_3c_L4)
resultRF_sim500_70_10V_highINTER_3c_L5<-standarize_data(resultRF_sim500_70_10V_highINTER_3c_L5)

resultRF_sim500_70_10V_highINTER_3c<-rbind(
  resultRF_sim500_70_10V_highINTER_3c_L1,
  resultRF_sim500_70_10V_highINTER_3c_L2,
  resultRF_sim500_70_10V_highINTER_3c_L3,
  resultRF_sim500_70_10V_highINTER_3c_L4,
  resultRF_sim500_70_10V_highINTER_3c_L5
)

RESULTRF_sim500_70_10V_highINTER_3c <- data.frame(
  var = "sim500_70_10V_highINTER_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_highINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_midBTW_3c#######
resultRF_sim500_70_10V_midBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midBTW_3c_L1.xlsx"))
resultRF_sim500_70_10V_midBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midBTW_3c_L2.xlsx"))
resultRF_sim500_70_10V_midBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midBTW_3c_L3.xlsx"))
resultRF_sim500_70_10V_midBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midBTW_3c_L4.xlsx"))
resultRF_sim500_70_10V_midBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midBTW_3c_L5.xlsx"))

resultRF_sim500_70_10V_midBTW_3c_L1<-standarize_data(resultRF_sim500_70_10V_midBTW_3c_L1)
resultRF_sim500_70_10V_midBTW_3c_L2<-standarize_data(resultRF_sim500_70_10V_midBTW_3c_L2)
resultRF_sim500_70_10V_midBTW_3c_L3<-standarize_data(resultRF_sim500_70_10V_midBTW_3c_L3)
resultRF_sim500_70_10V_midBTW_3c_L4<-standarize_data(resultRF_sim500_70_10V_midBTW_3c_L4)
resultRF_sim500_70_10V_midBTW_3c_L5<-standarize_data(resultRF_sim500_70_10V_midBTW_3c_L5)

resultRF_sim500_70_10V_midBTW_3c<-rbind(
  resultRF_sim500_70_10V_midBTW_3c_L1,
  resultRF_sim500_70_10V_midBTW_3c_L2,
  resultRF_sim500_70_10V_midBTW_3c_L3,
  resultRF_sim500_70_10V_midBTW_3c_L4,
  resultRF_sim500_70_10V_midBTW_3c_L5
)

RESULTRF_sim500_70_10V_midBTW_3c <- data.frame(
  var = "sim500_70_10V_midBTW_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_midBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_midINTER_3c#######
resultRF_sim500_70_10V_midINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midINTER_3c_L1.xlsx"))
resultRF_sim500_70_10V_midINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midINTER_3c_L2.xlsx"))
resultRF_sim500_70_10V_midINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midINTER_3c_L3.xlsx"))
resultRF_sim500_70_10V_midINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midINTER_3c_L4.xlsx"))
resultRF_sim500_70_10V_midINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_midINTER_3c_L5.xlsx"))

resultRF_sim500_70_10V_midINTER_3c_L1<-standarize_data(resultRF_sim500_70_10V_midINTER_3c_L1)
resultRF_sim500_70_10V_midINTER_3c_L2<-standarize_data(resultRF_sim500_70_10V_midINTER_3c_L2)
resultRF_sim500_70_10V_midINTER_3c_L3<-standarize_data(resultRF_sim500_70_10V_midINTER_3c_L3)
resultRF_sim500_70_10V_midINTER_3c_L4<-standarize_data(resultRF_sim500_70_10V_midINTER_3c_L4)
resultRF_sim500_70_10V_midINTER_3c_L5<-standarize_data(resultRF_sim500_70_10V_midINTER_3c_L5)

resultRF_sim500_70_10V_midINTER_3c<-rbind(
  resultRF_sim500_70_10V_midINTER_3c_L1,
  resultRF_sim500_70_10V_midINTER_3c_L2,
  resultRF_sim500_70_10V_midINTER_3c_L3,
  resultRF_sim500_70_10V_midINTER_3c_L4,
  resultRF_sim500_70_10V_midINTER_3c_L5
)

RESULTRF_sim500_70_10V_midINTER_3c <- data.frame(
  var = "sim500_70_10V_midINTER_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_midINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_lowBTW_3c#######
resultRF_sim500_70_10V_lowBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowBTW_3c_L1.xlsx"))
resultRF_sim500_70_10V_lowBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowBTW_3c_L2.xlsx"))
resultRF_sim500_70_10V_lowBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowBTW_3c_L3.xlsx"))
resultRF_sim500_70_10V_lowBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowBTW_3c_L4.xlsx"))
resultRF_sim500_70_10V_lowBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowBTW_3c_L5.xlsx"))

resultRF_sim500_70_10V_lowBTW_3c_L1<-standarize_data(resultRF_sim500_70_10V_lowBTW_3c_L1)
resultRF_sim500_70_10V_lowBTW_3c_L2<-standarize_data(resultRF_sim500_70_10V_lowBTW_3c_L2)
resultRF_sim500_70_10V_lowBTW_3c_L3<-standarize_data(resultRF_sim500_70_10V_lowBTW_3c_L3)
resultRF_sim500_70_10V_lowBTW_3c_L4<-standarize_data(resultRF_sim500_70_10V_lowBTW_3c_L4)
resultRF_sim500_70_10V_lowBTW_3c_L5<-standarize_data(resultRF_sim500_70_10V_lowBTW_3c_L5)

resultRF_sim500_70_10V_lowBTW_3c<-rbind(
  resultRF_sim500_70_10V_lowBTW_3c_L1,
  resultRF_sim500_70_10V_lowBTW_3c_L2,
  resultRF_sim500_70_10V_lowBTW_3c_L3,
  resultRF_sim500_70_10V_lowBTW_3c_L4,
  resultRF_sim500_70_10V_lowBTW_3c_L5
)

RESULTRF_sim500_70_10V_lowBTW_3c <- data.frame(
  var = "sim500_70_10V_lowBTW_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim500_70_10V_lowINTER_3c#######
resultRF_sim500_70_10V_lowINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowINTER_3c_L1.xlsx"))
resultRF_sim500_70_10V_lowINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowINTER_3c_L2.xlsx"))
resultRF_sim500_70_10V_lowINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowINTER_3c_L3.xlsx"))
resultRF_sim500_70_10V_lowINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowINTER_3c_L4.xlsx"))
resultRF_sim500_70_10V_lowINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim500_70_10V_lowINTER_3c_L5.xlsx"))

resultRF_sim500_70_10V_lowINTER_3c_L1<-standarize_data(resultRF_sim500_70_10V_lowINTER_3c_L1)
resultRF_sim500_70_10V_lowINTER_3c_L2<-standarize_data(resultRF_sim500_70_10V_lowINTER_3c_L2)
resultRF_sim500_70_10V_lowINTER_3c_L3<-standarize_data(resultRF_sim500_70_10V_lowINTER_3c_L3)
resultRF_sim500_70_10V_lowINTER_3c_L4<-standarize_data(resultRF_sim500_70_10V_lowINTER_3c_L4)
resultRF_sim500_70_10V_lowINTER_3c_L5<-standarize_data(resultRF_sim500_70_10V_lowINTER_3c_L5)

resultRF_sim500_70_10V_lowINTER_3c<-rbind(
  resultRF_sim500_70_10V_lowINTER_3c_L1,
  resultRF_sim500_70_10V_lowINTER_3c_L2,
  resultRF_sim500_70_10V_lowINTER_3c_L3,
  resultRF_sim500_70_10V_lowINTER_3c_L4,
  resultRF_sim500_70_10V_lowINTER_3c_L5
)

RESULTRF_sim500_70_10V_lowINTER_3c <- data.frame(
  var = "sim500_70_10V_lowINTER_3c",
  AUC_mean = mean(resultRF_sim500_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim500_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim500_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim500_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim500_70_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim500_70_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_highBTW_3c#######
resultRF_sim1000_30_10V_highBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highBTW_3c_L1.xlsx"))
resultRF_sim1000_30_10V_highBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highBTW_3c_L2.xlsx"))
resultRF_sim1000_30_10V_highBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highBTW_3c_L3.xlsx"))
resultRF_sim1000_30_10V_highBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highBTW_3c_L4.xlsx"))
resultRF_sim1000_30_10V_highBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highBTW_3c_L5.xlsx"))

resultRF_sim1000_30_10V_highBTW_3c_L1<-standarize_data(resultRF_sim1000_30_10V_highBTW_3c_L1)
resultRF_sim1000_30_10V_highBTW_3c_L2<-standarize_data(resultRF_sim1000_30_10V_highBTW_3c_L2)
resultRF_sim1000_30_10V_highBTW_3c_L3<-standarize_data(resultRF_sim1000_30_10V_highBTW_3c_L3)
resultRF_sim1000_30_10V_highBTW_3c_L4<-standarize_data(resultRF_sim1000_30_10V_highBTW_3c_L4)
resultRF_sim1000_30_10V_highBTW_3c_L5<-standarize_data(resultRF_sim1000_30_10V_highBTW_3c_L5)

resultRF_sim1000_30_10V_highBTW_3c<-rbind(
  resultRF_sim1000_30_10V_highBTW_3c_L1,
  resultRF_sim1000_30_10V_highBTW_3c_L2,
  resultRF_sim1000_30_10V_highBTW_3c_L3,
  resultRF_sim1000_30_10V_highBTW_3c_L4,
  resultRF_sim1000_30_10V_highBTW_3c_L5
)

RESULTRF_sim1000_30_10V_highBTW_3c <- data.frame(
  var = "sim1000_30_10V_highBTW_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_highBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_highINTER_3c#######
resultRF_sim1000_30_10V_highINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highINTER_3c_L1.xlsx"))
resultRF_sim1000_30_10V_highINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highINTER_3c_L2.xlsx"))
resultRF_sim1000_30_10V_highINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highINTER_3c_L3.xlsx"))
resultRF_sim1000_30_10V_highINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highINTER_3c_L4.xlsx"))
resultRF_sim1000_30_10V_highINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_highINTER_3c_L5.xlsx"))

resultRF_sim1000_30_10V_highINTER_3c_L1<-standarize_data(resultRF_sim1000_30_10V_highINTER_3c_L1)
resultRF_sim1000_30_10V_highINTER_3c_L2<-standarize_data(resultRF_sim1000_30_10V_highINTER_3c_L2)
resultRF_sim1000_30_10V_highINTER_3c_L3<-standarize_data(resultRF_sim1000_30_10V_highINTER_3c_L3)
resultRF_sim1000_30_10V_highINTER_3c_L4<-standarize_data(resultRF_sim1000_30_10V_highINTER_3c_L4)
resultRF_sim1000_30_10V_highINTER_3c_L5<-standarize_data(resultRF_sim1000_30_10V_highINTER_3c_L5)

resultRF_sim1000_30_10V_highINTER_3c<-rbind(
  resultRF_sim1000_30_10V_highINTER_3c_L1,
  resultRF_sim1000_30_10V_highINTER_3c_L2,
  resultRF_sim1000_30_10V_highINTER_3c_L3,
  resultRF_sim1000_30_10V_highINTER_3c_L4,
  resultRF_sim1000_30_10V_highINTER_3c_L5
)

RESULTRF_sim1000_30_10V_highINTER_3c <- data.frame(
  var = "sim1000_30_10V_highINTER_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_highINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_midBTW_3c#######
resultRF_sim1000_30_10V_midBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midBTW_3c_L1.xlsx"))
resultRF_sim1000_30_10V_midBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midBTW_3c_L2.xlsx"))
resultRF_sim1000_30_10V_midBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midBTW_3c_L3.xlsx"))
resultRF_sim1000_30_10V_midBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midBTW_3c_L4.xlsx"))
resultRF_sim1000_30_10V_midBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midBTW_3c_L5.xlsx"))

resultRF_sim1000_30_10V_midBTW_3c_L1<-standarize_data(resultRF_sim1000_30_10V_midBTW_3c_L1)
resultRF_sim1000_30_10V_midBTW_3c_L2<-standarize_data(resultRF_sim1000_30_10V_midBTW_3c_L2)
resultRF_sim1000_30_10V_midBTW_3c_L3<-standarize_data(resultRF_sim1000_30_10V_midBTW_3c_L3)
resultRF_sim1000_30_10V_midBTW_3c_L4<-standarize_data(resultRF_sim1000_30_10V_midBTW_3c_L4)
resultRF_sim1000_30_10V_midBTW_3c_L5<-standarize_data(resultRF_sim1000_30_10V_midBTW_3c_L5)

resultRF_sim1000_30_10V_midBTW_3c<-rbind(
  resultRF_sim1000_30_10V_midBTW_3c_L1,
  resultRF_sim1000_30_10V_midBTW_3c_L2,
  resultRF_sim1000_30_10V_midBTW_3c_L3,
  resultRF_sim1000_30_10V_midBTW_3c_L4,
  resultRF_sim1000_30_10V_midBTW_3c_L5
)

RESULTRF_sim1000_30_10V_midBTW_3c <- data.frame(
  var = "sim1000_30_10V_midBTW_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_midBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_midINTER_3c#######
resultRF_sim1000_30_10V_midINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midINTER_3c_L1.xlsx"))
resultRF_sim1000_30_10V_midINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midINTER_3c_L2.xlsx"))
resultRF_sim1000_30_10V_midINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midINTER_3c_L3.xlsx"))
resultRF_sim1000_30_10V_midINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midINTER_3c_L4.xlsx"))
resultRF_sim1000_30_10V_midINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_midINTER_3c_L5.xlsx"))

resultRF_sim1000_30_10V_midINTER_3c_L1<-standarize_data(resultRF_sim1000_30_10V_midINTER_3c_L1)
resultRF_sim1000_30_10V_midINTER_3c_L2<-standarize_data(resultRF_sim1000_30_10V_midINTER_3c_L2)
resultRF_sim1000_30_10V_midINTER_3c_L3<-standarize_data(resultRF_sim1000_30_10V_midINTER_3c_L3)
resultRF_sim1000_30_10V_midINTER_3c_L4<-standarize_data(resultRF_sim1000_30_10V_midINTER_3c_L4)
resultRF_sim1000_30_10V_midINTER_3c_L5<-standarize_data(resultRF_sim1000_30_10V_midINTER_3c_L5)

resultRF_sim1000_30_10V_midINTER_3c<-rbind(
  resultRF_sim1000_30_10V_midINTER_3c_L1,
  resultRF_sim1000_30_10V_midINTER_3c_L2,
  resultRF_sim1000_30_10V_midINTER_3c_L3,
  resultRF_sim1000_30_10V_midINTER_3c_L4,
  resultRF_sim1000_30_10V_midINTER_3c_L5
)

RESULTRF_sim1000_30_10V_midINTER_3c <- data.frame(
  var = "sim1000_30_10V_midINTER_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_midINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_lowBTW_3c#######
resultRF_sim1000_30_10V_lowBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowBTW_3c_L1.xlsx"))
resultRF_sim1000_30_10V_lowBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowBTW_3c_L2.xlsx"))
resultRF_sim1000_30_10V_lowBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowBTW_3c_L3.xlsx"))
resultRF_sim1000_30_10V_lowBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowBTW_3c_L4.xlsx"))
resultRF_sim1000_30_10V_lowBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowBTW_3c_L5.xlsx"))

resultRF_sim1000_30_10V_lowBTW_3c_L1<-standarize_data(resultRF_sim1000_30_10V_lowBTW_3c_L1)
resultRF_sim1000_30_10V_lowBTW_3c_L2<-standarize_data(resultRF_sim1000_30_10V_lowBTW_3c_L2)
resultRF_sim1000_30_10V_lowBTW_3c_L3<-standarize_data(resultRF_sim1000_30_10V_lowBTW_3c_L3)
resultRF_sim1000_30_10V_lowBTW_3c_L4<-standarize_data(resultRF_sim1000_30_10V_lowBTW_3c_L4)
resultRF_sim1000_30_10V_lowBTW_3c_L5<-standarize_data(resultRF_sim1000_30_10V_lowBTW_3c_L5)

resultRF_sim1000_30_10V_lowBTW_3c<-rbind(
  resultRF_sim1000_30_10V_lowBTW_3c_L1,
  resultRF_sim1000_30_10V_lowBTW_3c_L2,
  resultRF_sim1000_30_10V_lowBTW_3c_L3,
  resultRF_sim1000_30_10V_lowBTW_3c_L4,
  resultRF_sim1000_30_10V_lowBTW_3c_L5
)

RESULTRF_sim1000_30_10V_lowBTW_3c <- data.frame(
  var = "sim1000_30_10V_lowBTW_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_30_10V_lowINTER_3c#######
resultRF_sim1000_30_10V_lowINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowINTER_3c_L1.xlsx"))
resultRF_sim1000_30_10V_lowINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowINTER_3c_L2.xlsx"))
resultRF_sim1000_30_10V_lowINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowINTER_3c_L3.xlsx"))
resultRF_sim1000_30_10V_lowINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowINTER_3c_L4.xlsx"))
resultRF_sim1000_30_10V_lowINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_30_10V_lowINTER_3c_L5.xlsx"))

resultRF_sim1000_30_10V_lowINTER_3c_L1<-standarize_data(resultRF_sim1000_30_10V_lowINTER_3c_L1)
resultRF_sim1000_30_10V_lowINTER_3c_L2<-standarize_data(resultRF_sim1000_30_10V_lowINTER_3c_L2)
resultRF_sim1000_30_10V_lowINTER_3c_L3<-standarize_data(resultRF_sim1000_30_10V_lowINTER_3c_L3)
resultRF_sim1000_30_10V_lowINTER_3c_L4<-standarize_data(resultRF_sim1000_30_10V_lowINTER_3c_L4)
resultRF_sim1000_30_10V_lowINTER_3c_L5<-standarize_data(resultRF_sim1000_30_10V_lowINTER_3c_L5)

resultRF_sim1000_30_10V_lowINTER_3c<-rbind(
  resultRF_sim1000_30_10V_lowINTER_3c_L1,
  resultRF_sim1000_30_10V_lowINTER_3c_L2,
  resultRF_sim1000_30_10V_lowINTER_3c_L3,
  resultRF_sim1000_30_10V_lowINTER_3c_L4,
  resultRF_sim1000_30_10V_lowINTER_3c_L5
)

RESULTRF_sim1000_30_10V_lowINTER_3c <- data.frame(
  var = "sim1000_30_10V_lowINTER_3c",
  AUC_mean = mean(resultRF_sim1000_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_30_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_30_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_30_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_30_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_highBTW_3c#######
resultRF_sim1000_70_10V_highBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highBTW_3c_L1.xlsx"))
resultRF_sim1000_70_10V_highBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highBTW_3c_L2.xlsx"))
resultRF_sim1000_70_10V_highBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highBTW_3c_L3.xlsx"))
resultRF_sim1000_70_10V_highBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highBTW_3c_L4.xlsx"))
resultRF_sim1000_70_10V_highBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highBTW_3c_L5.xlsx"))

resultRF_sim1000_70_10V_highBTW_3c_L1<-standarize_data(resultRF_sim1000_70_10V_highBTW_3c_L1)
resultRF_sim1000_70_10V_highBTW_3c_L2<-standarize_data(resultRF_sim1000_70_10V_highBTW_3c_L2)
resultRF_sim1000_70_10V_highBTW_3c_L3<-standarize_data(resultRF_sim1000_70_10V_highBTW_3c_L3)
resultRF_sim1000_70_10V_highBTW_3c_L4<-standarize_data(resultRF_sim1000_70_10V_highBTW_3c_L4)
resultRF_sim1000_70_10V_highBTW_3c_L5<-standarize_data(resultRF_sim1000_70_10V_highBTW_3c_L5)

resultRF_sim1000_70_10V_highBTW_3c<-rbind(
  resultRF_sim1000_70_10V_highBTW_3c_L1,
  resultRF_sim1000_70_10V_highBTW_3c_L2,
  resultRF_sim1000_70_10V_highBTW_3c_L3,
  resultRF_sim1000_70_10V_highBTW_3c_L4,
  resultRF_sim1000_70_10V_highBTW_3c_L5
)

RESULTRF_sim1000_70_10V_highBTW_3c <- data.frame(
  var = "sim1000_70_10V_highBTW_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_highBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_highBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_highBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_highBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_highBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_highINTER_3c#######
resultRF_sim1000_70_10V_highINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highINTER_3c_L1.xlsx"))
resultRF_sim1000_70_10V_highINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highINTER_3c_L2.xlsx"))
resultRF_sim1000_70_10V_highINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highINTER_3c_L3.xlsx"))
resultRF_sim1000_70_10V_highINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highINTER_3c_L4.xlsx"))
resultRF_sim1000_70_10V_highINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_highINTER_3c_L5.xlsx"))

resultRF_sim1000_70_10V_highINTER_3c_L1<-standarize_data(resultRF_sim1000_70_10V_highINTER_3c_L1)
resultRF_sim1000_70_10V_highINTER_3c_L2<-standarize_data(resultRF_sim1000_70_10V_highINTER_3c_L2)
resultRF_sim1000_70_10V_highINTER_3c_L3<-standarize_data(resultRF_sim1000_70_10V_highINTER_3c_L3)
resultRF_sim1000_70_10V_highINTER_3c_L4<-standarize_data(resultRF_sim1000_70_10V_highINTER_3c_L4)
resultRF_sim1000_70_10V_highINTER_3c_L5<-standarize_data(resultRF_sim1000_70_10V_highINTER_3c_L5)

resultRF_sim1000_70_10V_highINTER_3c<-rbind(
  resultRF_sim1000_70_10V_highINTER_3c_L1,
  resultRF_sim1000_70_10V_highINTER_3c_L2,
  resultRF_sim1000_70_10V_highINTER_3c_L3,
  resultRF_sim1000_70_10V_highINTER_3c_L4,
  resultRF_sim1000_70_10V_highINTER_3c_L5
)

RESULTRF_sim1000_70_10V_highINTER_3c <- data.frame(
  var = "sim1000_70_10V_highINTER_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_highINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_highINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_highINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_highINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_highINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_midBTW_3c#######
resultRF_sim1000_70_10V_midBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midBTW_3c_L1.xlsx"))
resultRF_sim1000_70_10V_midBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midBTW_3c_L2.xlsx"))
resultRF_sim1000_70_10V_midBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midBTW_3c_L3.xlsx"))
resultRF_sim1000_70_10V_midBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midBTW_3c_L4.xlsx"))
resultRF_sim1000_70_10V_midBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midBTW_3c_L5.xlsx"))

resultRF_sim1000_70_10V_midBTW_3c_L1<-standarize_data(resultRF_sim1000_70_10V_midBTW_3c_L1)
resultRF_sim1000_70_10V_midBTW_3c_L2<-standarize_data(resultRF_sim1000_70_10V_midBTW_3c_L2)
resultRF_sim1000_70_10V_midBTW_3c_L3<-standarize_data(resultRF_sim1000_70_10V_midBTW_3c_L3)
resultRF_sim1000_70_10V_midBTW_3c_L4<-standarize_data(resultRF_sim1000_70_10V_midBTW_3c_L4)
resultRF_sim1000_70_10V_midBTW_3c_L5<-standarize_data(resultRF_sim1000_70_10V_midBTW_3c_L5)

resultRF_sim1000_70_10V_midBTW_3c<-rbind(
  resultRF_sim1000_70_10V_midBTW_3c_L1,
  resultRF_sim1000_70_10V_midBTW_3c_L2,
  resultRF_sim1000_70_10V_midBTW_3c_L3,
  resultRF_sim1000_70_10V_midBTW_3c_L4,
  resultRF_sim1000_70_10V_midBTW_3c_L5
)

RESULTRF_sim1000_70_10V_midBTW_3c <- data.frame(
  var = "sim1000_70_10V_midBTW_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_midBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_midBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_midBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_midBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_midBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_midINTER_3c#######
resultRF_sim1000_70_10V_midINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midINTER_3c_L1.xlsx"))
resultRF_sim1000_70_10V_midINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midINTER_3c_L2.xlsx"))
resultRF_sim1000_70_10V_midINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midINTER_3c_L3.xlsx"))
resultRF_sim1000_70_10V_midINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midINTER_3c_L4.xlsx"))
resultRF_sim1000_70_10V_midINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_midINTER_3c_L5.xlsx"))

resultRF_sim1000_70_10V_midINTER_3c_L1<-standarize_data(resultRF_sim1000_70_10V_midINTER_3c_L1)
resultRF_sim1000_70_10V_midINTER_3c_L2<-standarize_data(resultRF_sim1000_70_10V_midINTER_3c_L2)
resultRF_sim1000_70_10V_midINTER_3c_L3<-standarize_data(resultRF_sim1000_70_10V_midINTER_3c_L3)
resultRF_sim1000_70_10V_midINTER_3c_L4<-standarize_data(resultRF_sim1000_70_10V_midINTER_3c_L4)
resultRF_sim1000_70_10V_midINTER_3c_L5<-standarize_data(resultRF_sim1000_70_10V_midINTER_3c_L5)

resultRF_sim1000_70_10V_midINTER_3c<-rbind(
  resultRF_sim1000_70_10V_midINTER_3c_L1,
  resultRF_sim1000_70_10V_midINTER_3c_L2,
  resultRF_sim1000_70_10V_midINTER_3c_L3,
  resultRF_sim1000_70_10V_midINTER_3c_L4,
  resultRF_sim1000_70_10V_midINTER_3c_L5
)

RESULTRF_sim1000_70_10V_midINTER_3c <- data.frame(
  var = "sim1000_70_10V_midINTER_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_midINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_midINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_midINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_midINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_midINTER_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_lowBTW_3c#######
resultRF_sim1000_70_10V_lowBTW_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowBTW_3c_L1.xlsx"))
resultRF_sim1000_70_10V_lowBTW_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowBTW_3c_L2.xlsx"))
resultRF_sim1000_70_10V_lowBTW_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowBTW_3c_L3.xlsx"))
resultRF_sim1000_70_10V_lowBTW_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowBTW_3c_L4.xlsx"))
resultRF_sim1000_70_10V_lowBTW_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowBTW_3c_L5.xlsx"))

resultRF_sim1000_70_10V_lowBTW_3c_L1<-standarize_data(resultRF_sim1000_70_10V_lowBTW_3c_L1)
resultRF_sim1000_70_10V_lowBTW_3c_L2<-standarize_data(resultRF_sim1000_70_10V_lowBTW_3c_L2)
resultRF_sim1000_70_10V_lowBTW_3c_L3<-standarize_data(resultRF_sim1000_70_10V_lowBTW_3c_L3)
resultRF_sim1000_70_10V_lowBTW_3c_L4<-standarize_data(resultRF_sim1000_70_10V_lowBTW_3c_L4)
resultRF_sim1000_70_10V_lowBTW_3c_L5<-standarize_data(resultRF_sim1000_70_10V_lowBTW_3c_L5)

resultRF_sim1000_70_10V_lowBTW_3c<-rbind(
  resultRF_sim1000_70_10V_lowBTW_3c_L1,
  resultRF_sim1000_70_10V_lowBTW_3c_L2,
  resultRF_sim1000_70_10V_lowBTW_3c_L3,
  resultRF_sim1000_70_10V_lowBTW_3c_L4,
  resultRF_sim1000_70_10V_lowBTW_3c_L5
)

RESULTRF_sim1000_70_10V_lowBTW_3c <- data.frame(
  var = "sim1000_70_10V_lowBTW_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_lowBTW_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_lowBTW_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_lowBTW_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_lowBTW_3c$Cindex, na.rm = TRUE)
)


#######resultRF_sim1000_70_10V_lowINTER_3c#######
resultRF_sim1000_70_10V_lowINTER_3c_L1<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowINTER_3c_L1.xlsx"))
resultRF_sim1000_70_10V_lowINTER_3c_L2<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowINTER_3c_L2.xlsx"))
resultRF_sim1000_70_10V_lowINTER_3c_L3<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowINTER_3c_L3.xlsx"))
resultRF_sim1000_70_10V_lowINTER_3c_L4<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowINTER_3c_L4.xlsx"))
resultRF_sim1000_70_10V_lowINTER_3c_L5<-read_xlsx(paste0(in_dir, "resultRF_sim1000_70_10V_lowINTER_3c_L5.xlsx"))

resultRF_sim1000_70_10V_lowINTER_3c_L1<-standarize_data(resultRF_sim1000_70_10V_lowINTER_3c_L1)
resultRF_sim1000_70_10V_lowINTER_3c_L2<-standarize_data(resultRF_sim1000_70_10V_lowINTER_3c_L2)
resultRF_sim1000_70_10V_lowINTER_3c_L3<-standarize_data(resultRF_sim1000_70_10V_lowINTER_3c_L3)
resultRF_sim1000_70_10V_lowINTER_3c_L4<-standarize_data(resultRF_sim1000_70_10V_lowINTER_3c_L4)
resultRF_sim1000_70_10V_lowINTER_3c_L5<-standarize_data(resultRF_sim1000_70_10V_lowINTER_3c_L5)

resultRF_sim1000_70_10V_lowINTER_3c<-rbind(
  resultRF_sim1000_70_10V_lowINTER_3c_L1,
  resultRF_sim1000_70_10V_lowINTER_3c_L2,
  resultRF_sim1000_70_10V_lowINTER_3c_L3,
  resultRF_sim1000_70_10V_lowINTER_3c_L4,
  resultRF_sim1000_70_10V_lowINTER_3c_L5
)

RESULTRF_sim1000_70_10V_lowINTER_3c <- data.frame(
  var = "sim1000_70_10V_lowINTER_3c",
  AUC_mean = mean(resultRF_sim1000_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  AUC_var = var(resultRF_sim1000_70_10V_lowINTER_3c$AUC, na.rm = TRUE),
  BS_mean = mean(resultRF_sim1000_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  BS_var = var(resultRF_sim1000_70_10V_lowINTER_3c$BS, na.rm = TRUE),
  Cindex_mean = mean(resultRF_sim1000_70_10V_lowINTER_3c$Cindex, na.rm = TRUE),
  Cindex_var = var(resultRF_sim1000_70_10V_lowINTER_3c$Cindex, na.rm = TRUE)
)


######汇总所有结果######
all_results <- rbind(
  RESULTRF_sim500_30_10V_highBTW_3c,
  RESULTRF_sim500_30_10V_highINTER_3c,
  RESULTRF_sim500_30_10V_midBTW_3c,
  RESULTRF_sim500_30_10V_midINTER_3c,
  RESULTRF_sim500_30_10V_lowBTW_3c,
  RESULTRF_sim500_30_10V_lowINTER_3c,
  RESULTRF_sim500_70_10V_highBTW_3c,
  RESULTRF_sim500_70_10V_highINTER_3c,
  RESULTRF_sim500_70_10V_midBTW_3c,
  RESULTRF_sim500_70_10V_midINTER_3c,
  RESULTRF_sim500_70_10V_lowBTW_3c,
  RESULTRF_sim500_70_10V_lowINTER_3c,
  RESULTRF_sim1000_30_10V_highBTW_3c,
  RESULTRF_sim1000_30_10V_highINTER_3c,
  RESULTRF_sim1000_30_10V_midBTW_3c,
  RESULTRF_sim1000_30_10V_midINTER_3c,
  RESULTRF_sim1000_30_10V_lowBTW_3c,
  RESULTRF_sim1000_30_10V_lowINTER_3c,
  RESULTRF_sim1000_70_10V_highBTW_3c,
  RESULTRF_sim1000_70_10V_highINTER_3c,
  RESULTRF_sim1000_70_10V_midBTW_3c,
  RESULTRF_sim1000_70_10V_midINTER_3c,
  RESULTRF_sim1000_70_10V_lowBTW_3c,
  RESULTRF_sim1000_70_10V_lowINTER_3c
)

######保存结果到Excel文件######
write_xlsx(all_results, paste0(in_dir, "RESULTRF_combined_results.xlsx"))

######打印汇总结果######
print("所有模拟结果的汇总统计:")
print(all_results)