# RSF 解释脚本归档

## 优先阅读的统一版本

`05_static_explain.R`、`00_config.R`、`04_static_models.R` 完整复制自 `F:\文章_大论文\0830\CODEX\脚本`，未截断、未改变算法。`04_static_models.R` 是配套上游源代码快照，会拟合模型并执行性能评估；仅查看解释代码不必运行它。

解释流程：

1. 读取统一静态数据与已拟合 RSF；使用 11 个基线变量和 20 个第 1 天轨迹截面变量，共 31 个变量。group=1 为训练集，group=2 为验证集。
2. 解释目标是模型时间网格中最接近 u=1 的 `1-S(u)`，时间按 28 天归一化，即约 28 天死亡风险。
3. 验证集抽取最多 200 人，包括高预测风险死亡者、低预测风险存活者，其余随机抽取；训练集抽取最多 200 人作为背景。原代码假定验证集中两类结局均存在。
4. 使用随机特征排列的 Monte Carlo SHAP，每人 50 次模拟，按每批 20 人计算并缓存。随机种子 2026；不使用 TreeSHAP。
5. 输出平均绝对 SHAP 排序、全局蜂群图和两个典型患者的局部瀑布图。正值表示相对于背景提高预测风险，负值表示降低预测风险。
6. 同一完整脚本还读取 OOB VIMP 和最小深度，绘制前 4 个 VIMP 变量的 PDP，并输出静态模型校准、决策曲线。

主要图像文件名：

- `图5-4_RSF验证集全局SHAP分布.png`
- `图5-5_RSF典型患者局部SHAP解释.png`
- `图5-2_RSF_OOB收敛与置换变量重要性.png`
- `图5-3_RSF主要变量u1死亡风险PDP.png`

## 运行依赖

这是一份有上游依赖的完整原始脚本，并非没有输入即可运行的演示程序。需要先用同一研究版本完成数据准备及静态模型拟合。主流水线归档位于 `../../03_models/unified_pipeline/`；本目录配套 `04_static_models.R` 保留的是原始源版本，后续调参入口另见 `03_models`。

运行 `05_static_explain.R` 前，设置 `CH5_SCRIPT_DIR` 为本 `rsf_shap` 目录，`CH5_CACHE_DIR` 为相应模型缓存目录，`CH5_ARTIFACT_DIR` 为相应结果目录，`CH5_LOG_DIR` 为日志目录；也可设置 `CH5_PROJECT_ROOT` 与 `CH5_DATA_DIR`。配置通过环境变量定位，不能仅双击脚本并假定它会自动找到数据。

必需已有文件：

- 缓存目录：`static_data.rds`、`static_predictions.rds`、`rsf_static_31.rds`。
- 结果目录：`RSF_OOB_VIMP.csv`、`RSF_最小深度.csv`。
- R 包：`dplyr`、`survival`、`randomForestSRC`、`ggplot2`、`shapviz`、`patchwork`、`tidyr`；上游模型脚本还需其自身声明的包。

原脚本会复用 `rsf_SHAP_200x50.rds` 与 `rsf_shap_chunks` 缓存，不核对模型或配置是否改变。更换模型、样本或 SHAP 参数后，请使用独立的新缓存目录并放入匹配的三个必需 RDS 文件，避免误用旧解释结果。生成结果中的患者明细应保留在本地数据环境；本归档只复制代码。

## 历史完整版本

- `source_versions/0826/0826_RSF_结果输出.R`：原始独立建模及报告脚本，SHAP 部分约在第 291–446 行。8 个基线变量加 20 个截面变量；使用 `fastshap::explain`、`nsim=20`、`adjust=TRUE`，训练数据为背景，验证集最多 200 人。输出 `02_SHAP_beeswarm.png`、`03_典型患者_SHAP.png`、`04_SHAP_dependence.png`。此脚本包含建模、评价、图像和报告的完整上下文。
- `source_versions/0831/02b_static_explain.R` 及同目录 `00_config.R`：另一版完整静态解释脚本。除蜂群图、瀑布图与 PDP 外，还输出 `RSF_08_SHAP_dependence.png`。依赖该版本的上游结果，目录及文件名与统一版不同，不可混用缓存。上游源代码位于 `F:\文章_大论文\0830\补充0831\代码`。
- `source_versions/0402/SHAP.R`：早期四模型合并脚本，RSF 段从第 171 行开始，解释 `predict()$predicted` 的集成死亡率指标，输出 SHAP 明细与重要性。这个目标不同于上面两个版本的 28 天死亡概率，不能直接比较 SHAP 数值。保留完整文件以保留数据准备和变量定义。

历史脚本保留原始绝对路径，其中有自动安装包、拟合模型及写出文件的逻辑；运行前需要按自己的数据环境修改路径。本次只归档并做语法解析，没有执行这些逻辑，也没有重新拟合模型或声称新图已生成。
