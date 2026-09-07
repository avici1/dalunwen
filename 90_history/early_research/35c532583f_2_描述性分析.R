library(dplyr)
library(lubridate)
library(tibble)
library("JM")
library(descr)
library(haven)
options(scipen=999)#禁用科学计数法

freq_a <- function(data, a) {
  var_name <- deparse(substitute(a))
  freq_a <- as.data.frame(freq(data[[var_name]], 
                               report.nas = FALSE, headings = FALSE, plain.ascii = TRUE))
  freq_a <- freq_a %>% 
    rownames_to_column(var = var_name) %>%
    arrange(desc(Frequency))
  return(freq_a)
}                                                                       #分类names

long_GLU<-read.csv("F:/文章/大论文/程序/数据/long_GLU.csv")
long_TC<-read.csv("F:/文章/大论文/程序/数据/long_TC.csv")
long_TG<-read.csv("F:/文章/大论文/程序/数据/long_TG.csv")
long_LDLC<-read.csv("F:/文章/大论文/程序/数据/long_LDLC.csv")
long_HDLC<-read.csv("F:/文章/大论文/程序/数据/long_HDLC.csv")

############查看纵向记录数####################
freq1 <- freq_a(long_GLU, times)
freq2 <- freq_a(long_TG, times)
freq3 <- freq_a(long_TC, times)
freq4 <- freq_a(long_LDLC, times)
freq5 <- freq_a(long_HDLC, times)
cbind_fill <- function(...) {
  dfs <- list(...)
  max_rows <- max(sapply(dfs, nrow))
  dfs_filled <- lapply(dfs, function(df) {
    if (nrow(df) < max_rows) {
      missing_rows <- max_rows - nrow(df)
      df[(nrow(df) + 1):max_rows, ] <- NA
    }
    return(df)
  })
  do.call(cbind, dfs_filled)
}
freq0 <- cbind_fill(freq1, freq2, freq3, freq4, freq5)  #仅计数freq






freq_rep <- function(data, var_rep, var_name) {
  var1 <- deparse(substitute(var_rep))  # 捕获变量名
  var2 <- deparse(substitute(var_name))  # 捕获变量名
  
  result <- data %>%
    group_by(!!sym(var1)) %>%
    summarise(freq = n()) %>%
    rename(
      ID = 1,          # 第一列重命名为ID
      !!var2 := 2   # 第二列使用输入的变量名
    ) %>%
  ungroup()
  
  return(as.data.frame(result))  # 确保返回标准数据框
}

freq_GLU<- freq_rep(long_GLU, B_WT4_ID, GLU)
freq_TC<- freq_rep(long_TC, B_WT4_ID, TC)
freq_TG<- freq_rep(long_TG, B_WT4_ID, TG)
freq_LDLC<- freq_rep(long_LDLC, B_WT4_ID, LDLC)
freq_HDLC<- freq_rep(long_HDLC, B_WT4_ID, HDLC)

#查看记录数
freq1 <- freq_a(freq_GLU, GLU)
freq2 <- freq_a(freq_TG, TG)
freq3 <- freq_a(freq_TC, TC)
freq4 <- freq_a(freq_LDLC, LDLC)
freq5 <- freq_a(freq_HDLC, HDLC)

cbind_fill <- function(...) {
  dfs <- list(...)
  max_rows <- max(sapply(dfs, nrow))
  dfs_filled <- lapply(dfs, function(df) {
    if (nrow(df) < max_rows) {
      missing_rows <- max_rows - nrow(df)
      df[(nrow(df) + 1):max_rows, ] <- NA
    }
    return(df)
  })
  do.call(cbind, dfs_filled)
}

freq0 <- cbind_fill(freq1, freq2, freq3, freq4, freq5)

freq<-cbind.fill(freq_GLU, freq_TC, freq_TG, freq_LDLC, freq_HDLC)
######################################################################

FREQ<- full_join(freq_GLU, freq_HDLC,by="ID")
FREQ<- full_join(FREQ, freq_LDLC,by="ID")
FREQ<- full_join(FREQ, freq_TC,by="ID")
FREQ<- full_join(FREQ, freq_TG,by="ID")
FREQ<- full_join(FREQ, freq_BNP,by="ID")
FREQ<- full_join(FREQ, freq_CKMB,by="ID")
FREQ<- full_join(FREQ, freq_TNI,by="ID")
FREQ<- full_join(FREQ, freq_TNT,by="ID")
FREQ<- full_join(FREQ, freq_WBC,by="ID")

write.csv(FREQ,"F:/安贞医院数据/数据管理/FREQ.csv")


#############事件数#########
event<- freq_a(baseline2013_2023, TIME_DEAD)
event_GLU<-freq_a(long_GLU, TIME_DEAD)
event_TC<-freq_a(long_TC, TIME_DEAD)
event_TG<-freq_a(long_TG, TIME_DEAD)
event_LDLC<-freq_a(long_LDLC, TIME_DEAD)
event_HDLC<-freq_a(long_HDLC, TIME_DEAD)
















