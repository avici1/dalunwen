library(readxl)



RESULT_ALL_COX_4V<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_COX_4V.xlsx")
RESULT_ALL_COX_10V1C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_COX_10V1C.xlsx")
RESULT_ALL_COX_10V3C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_COX_10V3C.xlsx")
RESULT_ALL_JM_4V<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_JM_4V.xlsx")
RESULT_ALL_JM_10V1C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_JM_10V1C.xlsx")
RESULT_ALL_JM_10V3C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_JM_10V3C.xlsx")
RESULT_ALL_RSF_4V<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSF_4V.xlsx")
RESULT_ALL_RSF_10V1C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSF_10V1C.xlsx")
RESULT_ALL_RSF_10V3C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSF_10V3C.xlsx")
RESULT_ALL_RSFLC_4V<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSFLC_4V.xlsx")
RESULT_ALL_RSFLC_10V1C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSFLC_10V1C.xlsx")
RESULT_ALL_RSFLC_10V3C<-read_xlsx("F:/文章_大论文/结果/all_summary/RESULT_ALL_RSFLC_10V3C.xlsx") ##3.3未完成






install.packages("echarts4r")
library(echarts4r)

# 准备数据
df <- data.frame(
  x = rep(c("A","B","C","D"), each=3),
  y = rep(c("组1","组2","组3"), times=4),
  z = c(3,5,2, 4,6,3, 7,2,5, 3,8,4)
)

# 绘制交互式3D柱状图
df |>
  e_charts(x) |>
  e_bar_3d(y, z, shading='realistic') |>
  e_x_axis_3d(name="X轴") |>
  e_y_axis_3d(name="Y轴") |>
  e_z_axis_3d(name="数值") |>
  e_title("3D柱状图示例")



