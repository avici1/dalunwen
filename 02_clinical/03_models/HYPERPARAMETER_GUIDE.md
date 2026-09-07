# RSF / RSFLC 超参数处理清单

本次为现有统一变量配置补充两个完整调参入口，同时集中复制原始调参/最优模型脚本。原始研究目录未修改；已有 `unified_pipeline` 和 `individual_0826` 中的脚本也没有被本次新增入口覆盖。

## 一、直接从哪里开始

- [RSF：rsf/01_tune_and_refit.R](rsf/01_tune_and_refit.R)
- [RSFLC：rsflc/01_tune_and_refit.R](rsflc/01_tune_and_refit.R)
- [共用实现：hyperparameter_common.R](hyperparameter_common.R)

每个入口调用同目录树内的共用实现，读取 `unified_pipeline/00_config.R` 中的变量、参数网格和 IPCW 函数定义。只载入指定定义，不执行该配置文件的建目录等顶层操作。请保留整个 `03_models` 目录，不要只拷贝几行入口代码。

新入口完成：读取准备好的模型数据 → 核对患者分组和折号 → 逐组合五折拟合/预测 → 记录三个指标 → 完整五折组合排序 → 导出最优参数 → 用全部 group=1 重新拟合 → 保存模型与预处理规则。group=2 不参与选参或这里的最终训练。

## 二、RSF 清单

- 模型与输入：`randomForestSRC::rfsrc` 生存模型，使用统一配置 `static31`（11 个基线变量 + 20 个第 1 天截面变量）。
- `ntree`（树数）：300、500、1000。
- `mtry`（每次分裂候选变量数）：3、6、9。
- `nodesize`（终端节点大小参数）：10、20、30、40。
- `nsplit`（随机候选分裂点数）：10、25、50。
- 共 3×3×4×3 = **108 组**，每组 5 折，完整搜索 **540 次拟合**，之后还有 1 次最优组合全训练集拟合。
- 固定设置：`splitrule="logrank"`，调参时不计算变量重要性。
- 验证预测：从生存曲线取 u=1 的风险 `1-S(1)`，按不晚于评价时点的阶梯值取值；不向预测函数传验证结局。

## 三、RSFLC 清单

- 模型与输入：`DynForest::dynforest`，**生存结局** `Y$type="surv"`；28 个固定变量 + 3 个轨迹（gcs、sofa_24hours、cns_24hours）。
- `ntree`：50、100、200。
- `mtry`：3、6、9、12。
- `nodesize`：1、3、5。
- 共 3×4×3 = **36 组**，完整搜索 **180 次拟合**，之后还有 1 次全训练集拟合。
- 固定设置：`minsplit=2`（读取现有配置）、`nsplit_option="quantile"`、`cause=1`；本轮不搜索这些参数。
- 轨迹固定效应为 `Y ~ time`，随机效应为 `~time`，沿用统一版本；没有为本次调参改变轨迹/固定变量集合。
- 时间按 u=t/28，landmark=5/28，终点=1；拒绝超出 landmark 的输入轨迹。预测后必须按 hadm_id 精确对齐，缺失 ID 不进行顺序猜测。

## 四、两个入口共用的选择与保存规则

1. 只在 group=1 内使用既有 fold=1…5；同一 subject_id 不能跨组或跨折。
2. 每轮 4 折训练、1 折验证。剩余数值缺失使用该训练折中位数，分类缺失/未知水平使用训练折众数；轨迹缺失保留给混合模型处理，每条轨迹要求至少两个有限观测。
3. 每折记录 **C-index、u=1 的 IPCW AUC、IPCW Brier**，删失分布由该训练折估计。
4. 五折全部成功且三个指标均有限，组合才可参与选优；不会用 `na.rm=TRUE` 把失败折忽略掉。
5. 先按平均 C-index 降序，再按平均 Brier 升序，完全相同时按 grid_id 升序。AUC 是输出指标，不是这里的第二排序条件。
6. 逐折写出 `cv_folds.csv`，结束后写出 `cv_summary.csv` 和 `best_parameters.csv`。
7. 将选中的参数直接传入全训练集拟合，保存 `best_model_bundle.rds`，其中含模型、参数、特征、预处理规则、时间定义和种子。
8. 另存 `parameter_grid.csv`、`input_manifest.csv`、`run_summary.csv`、`sessionInfo.txt`。输出目录必须为空，避免混用旧结果；目前不提供断点续跑。

CV 均值用于选参，不应当作选参后独立留出集的无偏性能估计。本次不产生真实研究的“最优参数”或性能结论。

## 五、原来散落的代码在哪里、有什么区别

已原样复制 9 份来源脚本，原始路径及 SHA-256 见 [HYPERPARAMETER_SOURCES.json](HYPERPARAMETER_SOURCES.json)。

- `rsf/source_versions/0826_RSF_超参数筛选.R`：8 基线 + 20 截面；mtry 还包括 12，合计 **144 组**。按留出 group=2 的 C-index、AUC、Brier 排序，因此该留出集已经用于选参，不能同时称作未使用过的最终测试集。
- `rsf/source_versions/RSF_tune_fold5.R`：训练组五折比较，以平均 C-index 选优并输出参数/表现。
- `rsf/source_versions/0830_RSF_表5-3A_五折交叉验证调参.R`：108 组的完整五折实现，包含折内插补、逐折落盘、断点续跑、最优组合完整评价和全训练集重拟合。其排序并列规则与新入口不同，保留原样。
- `rsf/source_versions/RSF最优模型输出结果.R`：历史片段，直接指定 ntree=500、mtry=3、nodesize=10、nsplit=10，依赖前序 R 对象，不是独立的网格搜索。
- `rsflc/source_versions/0826_RSFLC超参数筛选.R`：20 轨迹 + 11 基线，**分类结局** `type="factor"`，36 组，按 group=2 C-index 选优；其 AUC/普通 Brier 不能当作新生存版的同一指标口径。
- `rsflc/source_versions/RSFLC_20V_tune_fold5.R`：上述 20 轨迹分类版本的五折实现，并非当前 3 轨迹生存版本。
- `rsflc/source_versions/RSFLC最优模型输出结果.R`：历史最优配置执行版，直接指定 ntree=200、mtry=3、nodesize=1。
- 两个 `source_versions` 中也保留了 `0521` 阶段的五折实现，便于追溯，未把它们与新入口拼成同一实验。

现有 `unified_pipeline/07a`、`07b` 仍作为该流水线自己的可选调参入口。`04_static_models.R`、`06_rsflc_model.R` 仍按其自身配置/缓存运行；**本次新入口不会自动替换既有报告模型或覆盖旧结果**。要使用新搜索结果，请读取新输出的模型 bundle，并在后续报告流程中显式接入。

## 六、运行方法

先使用现有 `unified_pipeline/02_prepare_data.R` 在你设置的 `CH5_DATA_DIR`、`CH5_CACHE_DIR` 下准备数据；它依赖两份研究 CSV。新调参入口不需要论文 DOCX，也不执行旧的一键全流程。

RSF 的缓存目录需含 `static_data.rds`；RSFLC 需含 `dynamic_base.rds` 与 `dynamic_long.rds`。这些文件由前述数据准备脚本生成。历史输入可能已经 KNN 插补；新增脚本只处理剩余缺失，**不能把之前跨折插补变成严格的原始数据折内插补**。

只检查参数网格（不读数据，不训练）：

```powershell
& 'F:\R\R-4.4.3\bin\Rscript.exe' 'F:\应聘\github_大论文\02_clinical\03_models\rsf\01_tune_and_refit.R' --dry-run
& 'F:\R\R-4.4.3\bin\Rscript.exe' 'F:\应聘\github_大论文\02_clinical\03_models\rsflc\01_tune_and_refit.R' --dry-run
```

正式运行示例（先将缓存路径替换为实际路径，输出目录使用一个新的空目录）：

```powershell
& 'F:\R\R-4.4.3\bin\Rscript.exe' 'F:\应聘\github_大论文\02_clinical\03_models\rsf\01_tune_and_refit.R' '--cache-dir=F:/应聘/github_大论文/cache' '--output-dir=F:/应聘/github_大论文/outputs/rsf_cv_run1' --cores=4
& 'F:\R\R-4.4.3\bin\Rscript.exe' 'F:\应聘\github_大论文\02_clinical\03_models\rsflc\01_tune_and_refit.R' '--cache-dir=F:/应聘/github_大论文/cache' '--output-dir=F:/应聘/github_大论文/outputs/rsflc_cv_run1' --cores=4
```

代码要求 `survival` 及对应的 `randomForestSRC` / `DynForest` 包；数据准备另有原脚本依赖。输出可能包含患者信息，应继续放在被 Git 忽略的本地输出目录。

## 七、本次验证

- 两个入口的 dry-run：108 组 / 36 组，符合配置。
- 合成 100 个训练个体，用真实 R 包分别运行一个小参数组合的五折和最终拟合（RSF 10 棵树，RSFLC 5 棵树）；均成功生成模型 bundle。
- 验证失败折不能选优、患者跨折/跨组会停止、预测时间使用阶梯值、插补统计来自训练折、非空输出目录不会被覆盖。
- 9 份原始源码副本核对哈希；真实患者完整网格没有运行。测试脚本在 [tests/test_hyperparameters.R](tests/test_hyperparameters.R)。

API 对照：[randomForestSRC 预测文档](https://www.randomforestsrc.org/reference/predict.rfsrc.html)、[DynForest 作者发表于 R Journal 的包说明](https://journal.r-project.org/articles/RJ-2025-002/)。模型源码的原始依据以来源清单为准。
