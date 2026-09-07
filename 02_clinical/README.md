# MIMIC 实例研究

研究主线：SQL 提取 → 数据筛选、整理与分折 → 共享变量配置 → 四模型拟合 → 性能、校准与解释 → 图像和报告。

## 按步骤阅读

1. [01_sql/database_setup](01_sql/database_setup/)：数据库、schema、表、导入、约束与索引脚本。
2. [01_sql/cohort_extraction](01_sql/cohort_extraction/)：`build_data_0506.sql`、`stroke_extract_0520.sql`、`stroke_patient_0520.sql`，记录临床队列和分析数据集提取逻辑。
3. [02_preprocessing](02_preprocessing/)：`source_0521/` 保存清洗、变量筛选、分折及模型输入处理；`source_0824/` 保存纵向 KNN 插补和训练集五折标记。这些是不同阶段实现，输入输出名称没有在本次归档中统一。
4. [03_models/unified_pipeline](03_models/unified_pipeline/)：完整保留原 `0830/CODEX/脚本` 的 13 个 R 脚本与复现说明，以共享变量配置为阅读入口。包括数据再核查、Cox/RSF、RSFLC、JM、可选五折搜索、共同队列比较和报告生成。两个机器学习模型的文章口径五折网格在 `07a_rsf_fivefold_cv_optional.R`（RSF，31 个静态变量）和 `07b_rsflc_fivefold_cv_optional.R`（RSFLC，3 条轨迹 + 28 个固定变量）；默认不跑，需设置 `RUN_OPTIONAL_CV=true`。
5. [03_models/individual_0826](03_models/individual_0826/)：分别保存 Cox、RSF、JM、RSFLC 的单模型执行版本。RSF 调参在 `rsf/`：`0830_RSF_表5-3A_五折交叉验证调参.R`（完整 108 组五折）、`0826_RSF_超参数筛选.R`（训练集拟合、外验证集选参）。RSFLC 调参在 `rsflc/`：`RSFLC_20V_tune_fold5.R`（20 轨迹五折）、`0826_RSFLC超参数筛选.R`（外验证集选参）。`support/` 保留 0826 调参原稿副本。
6. [04_evaluation](04_evaluation/)：其他静态/动态评价口径的原始脚本。主线的性能、校准、DCA 和 bootstrap 实现还保留在 `unified_pipeline` 中，避免拆散相互依赖的程序。
7. [05_explanation](05_explanation/)：R/Python 跨语言 RSFLC SHAP 实现；主线中也有静态 SHAP、变量重要性、PDP 和动态解释。
8. [06_reporting](06_reporting/)：第五章图像、Word 更新及核验脚本。完整流水线的 `10_build_docx.R` 位于 `unified_pipeline`，未重复复制到这里。

## 变量与评价入口

先阅读 [变量配置和版本说明](../docs/CLINICAL_DESIGN.md)。主线保留的是共同变量配置版本；其他版本不会在归档时被拼成一套模型。静态与动态的评价人群、时间起点和可用轨迹不同，应分别核对。

原 [SOURCE_README_复现说明.md](03_models/unified_pipeline/SOURCE_README_%E5%A4%8D%E7%8E%B0%E8%AF%B4%E6%98%8E.md) 描述原工作环境，内容未经本次重跑验证。其中说“仅两个输入”，但 `01_stage_inputs.R` 实际还需要参考论文 DOCX；该差异已记入归档说明。

真实 MIMIC 数据、患者级预测与拟合缓存均未复制。SQL 中有创建、删除表等语句，本次没有执行数据库或模型代码。

完整文件导航见 [实例主线索引](../docs/index_02_clinical.md)。
