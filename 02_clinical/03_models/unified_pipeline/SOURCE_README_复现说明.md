# 第五章复现说明

## 固定分析设计

- 原始输入仅为 `stroke_baseline_knn_0824_fold.csv` 与 `stroke_longitudinal_knn_0824_group_fold.csv`。
- 排除 337 条结局时间无效的住院记录，并在 `表5-1a_队列删减记录.csv` 中逐项保留删减理由。
- 统一时间尺度为 `u = t / 28`；静态模型预测 28 天死亡，动态模型里程碑为第 5 天、预测至第 28 天。
- 静态模型固定 31 个变量；动态模型固定 3 个纵向轨迹和 28 个基线/第 1 天变量。
- RSFLC 的纵向固定效应均为 `Y ~ time`。因 DynForest 1.3.2 对 `random = ~1` 触发内部错误，按预先允许的简化结构采用 `random = ~time`；其中 `time` 的数值仍为标准化时间 `u`。
- 联合模型纵向子模型固定效应为 `Y ~ time_u`，随机效应为 `~1`。
- 联合模型从 3 链、3,000 次迭代（预热 1,500 次）开始；若任一关联参数 `Rhat>1.05`，自动扩展至 6,000 次，再扩展至 12,000 次。若仍未收敛，结果保留但在正文中明确标为探索性。
- RSF 使用文章既定参数：`ntree=500, mtry=3, nodesize=10, nsplit=10`。
- RSFLC 使用文章既定参数：`ntree=200, mtry=3, nodesize=1, minsplit=2, nsplit_option=quantile`。
- 五折超参数搜索代码保存在 `07a_rsf_fivefold_cv_optional.R` 与 `07b_rsflc_fivefold_cv_optional.R`，默认不运行。需要运行时设置环境变量 `RUN_OPTIONAL_CV=true`。

## 一键运行

所需 R 包：`dplyr`、`tidyr`、`survival`、`randomForestSRC`、`DynForest`、`nlme`、`JMbayes2`、`coda`、`fastshap`、`shapviz`、`ggplot2`、`patchwork`、`officer` 和 `flextable`。

在已安装所需 R 包的环境中执行：

```powershell
& "F:\R\R-4.4.3\bin\Rscript.exe" "F:\文章_大论文\0830\CODEX\脚本\99_run_all.R"
```

脚本会将中间模型缓存到项目根目录的 `cache`，将最终图表写入 `图像表格`，并生成 `codex第五章.docx`。已有缓存会被复用；删除某个缓存文件即可重算相应步骤。

## 脚本顺序

1. `01_stage_inputs.R`：复制输入并校验 MD5。
2. `02_prepare_data.R`：结局时间核查、337 例删减、静态/动态队列构建。
3. `03_descriptive_tables_and_flow.R`：图5-1及描述性表格。
4. `04_static_models.R`：Cox 与 RSF。
5. `05_static_explain.R`：静态性能、校准、DCA、PDP、全局与典型患者局部 SHAP。
6. `06_rsflc_model.R`：RSFLC、性能及解释图。
7. `08_joint_model.R`：联合模型、MCMC 诊断及预测。
8. `09_combine_results.R`：共同队列比较、动态校准/DCA、时间依赖 AUC 与 Brier。
9. `10_build_docx.R`：汇总为第五章 Word 文档。
