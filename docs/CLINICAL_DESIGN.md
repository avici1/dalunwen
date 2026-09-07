# 实例研究的变量配置与版本边界

归档以原 `0830/CODEX/脚本` 作为共享配置的阅读入口，整套脚本保留在 `02_clinical/03_models/unified_pipeline`。这是归档导航选择，不代表本次重跑确认了全部模型结果。

## 共享变量

以 [00_config.R](../02_clinical/03_models/unified_pipeline/00_config.R#L39) 为准：

- `fixed11`：age、charlson_comorbidity_index、apsiii、sapsii、oasis、preiculos、mechvent、electivesurgery、gender、bmi、stroke_type。
- `long20`：total_urine_output、creat、aki_stage、gcs、ph、pco2、lactate、po2、pao2fio2ratio、glucose、sodium、bicarbonate、hemoglobin、temperature、fio2、sofa_24hours、cns_24hours、renal_24hours、cardiovascular_24hours、respiration_24hours。
- `traj3`：gcs、sofa_24hours、cns_24hours。
- `day1_17 = long20 − traj3`。
- **Cox 与 RSF**：共享 `static31 = fixed11 + long20`，以基线/第 1 天信息建模。
- **JM 与 RSFLC**：共享 `dynamic_fixed28 = fixed11 + day1_17` 和 `traj3`，把三项变量的纵向记录作为轨迹输入。

`04_static_models.R` 用同一 `static31` 构造 Cox/RSF 公式；`06_rsflc_model.R` 与 `08_joint_model.R` 均引用 `dynamic_fixed28`、`traj3`。本次未修改固定效应、随机效应、超参数或变量集合。

## 时间与人群

主线配置把天数标准化为 `u=t/28`；静态终点为第 28 天；动态 landmark 为第 5 天，预测到第 28 天。静态与动态的可用信息、条件人群和预测问题不同，不能把全部指标当成相同任务下的无条件排名。共同动态验证队列的比较位于 `09_combine_results.R`。

## 各步骤在哪里

- SQL 提取：`02_clinical/01_sql/`。
- 早期清洗、筛选、插补和分折：`02_clinical/02_preprocessing/`。
- 主线再核查与队列构建：`unified_pipeline/02_prepare_data.R`。
- 队列图与描述表：`03_descriptive_tables_and_flow.R`。
- Cox/RSF 与解释：`04_static_models.R`、`05_static_explain.R`。
- RSFLC 与 JM：`06_rsflc_model.R`、`08_joint_model.R`。
- 五折调参（文章最终口径）：`07a_rsf_fivefold_cv_optional.R`（RSF，31 变量，108 组）、`07b_rsflc_fivefold_cv_optional.R`（RSFLC，3 轨迹 + 28 固定，36 组）。仅 group=1，按五折 C-index 均值选优；原流程默认不执行。
- 更早完整原稿：`02_clinical/03_models/individual_0826/rsf/0830_RSF_表5-3A_五折交叉验证调参.R`（8+20 变量、折内插补、108 组）、`02_clinical/03_models/individual_0826/rsflc/RSFLC_20V_tune_fold5.R`（20 轨迹 + 11 固定）。0826 的 `*_超参数筛选.R` 用 group=2 外验证集选参，不是表5-3 的正式口径。
- 共同队列、置信区间、校准、DCA、时间依赖指标：`09_combine_results.R`。
- Word 报告：`10_build_docx.R`。

上述 `unified_pipeline` 为 `02_clinical/03_models/unified_pipeline` 的简称。

## 不同版本的区别

[原补充0831版本的配置](../90_history/clinical/ccc0c2da03_00_config.R#L24) 定义过 `dynamic_fixed25 = clinical8 + day1_17`，与这里的 28 个固定变量不同。该版本及其他不同内容保存在历史目录，完整原路径可在 `source_manifest.csv` 找回。本次没有把这些不同变量口径拼接为一个实验。

原复现说明中的“两个输入”与 `01_stage_inputs.R` 不完全一致：后者还复制参考论文 DOCX。数据准备脚本也包含结局时间筛选。以上代码行为均原样保留，不把历史说明中的个案数认定为本次核验结果。
