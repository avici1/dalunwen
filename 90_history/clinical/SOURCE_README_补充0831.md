# 第五章缺失图表补充（0831）

本目录只补充第五章现有DOCX中错位、未嵌入或数值空缺的图表，不改写原始模型口径。

## 口径

- 静态任务：31个变量，锁定RSF参数 `ntree=500, mtry=3, nodesize=10, nsplit=10`。
- 动态任务：第5天landmark，3条轨迹+25个固定变量，锁定RSFLC参数 `ntree=200, mtry=3, nodesize=1, minsplit=2`。
- 五折结果是对锁定参数组合的重新评价，不声称完成了全参数网格重新选优。

## 脚本

1. `00_export_existing_missing.R`：导出表5-1b死亡结局，复制错位/未嵌入图，并导出配套绘图数据。
2. `01_static_fixed_5fold.R`：按当前31变量口径计算RSF固定参数五折指标，生成表5-3A补充表。
3. `02_dynamic_fixed_5fold_worker.R <fold>`：计算指定一折RSFLC固定参数指标，可并行、可断点续跑。
4. `03_combine_dynamic_fixed_5fold.R`：汇总5折结果并生成表5-3B补充表。

## 一键运行

在PowerShell中运行：

`powershell -ExecutionPolicy Bypass -File .\run_all_supplement.ps1`

脚本会创建仅用于解决Windows中文路径编码问题的 `runtime_links/` 联接；不会修改源模型和源结果。

## 输出目录

- `figures/`：13张错位或未嵌入图。
- `tables/`：死亡结局、图像对照、配套绘图数据，以及表5-3A/5-3B五折补充结果。
- `logs/`：长时间运行日志。
- `model_cache/`：默认不保存五折模型；设置 `SAVE_RSFLC_CV_MODELS=true` 时保存。
