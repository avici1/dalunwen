# 模拟研究

研究主线：情景设计 → 纵向协变量 → 生存时间与删失 → 模型输入 → 四模型拟合 → 指标汇总 → 图像。

## 按步骤阅读

1. [01_design](01_design/)：相关矩阵、多元相关性与单个数据生成设计；[六个设计维度](../docs/SIMULATION_DESIGN.md) 解释其含义。
2. [02_generation/training](02_generation/training/)：分别保存 `4v_1c`、`10v_1c`、`10v_3c` 三组生成脚本。每组保留“数据1”“数据2”“生成Y”的原文件名。
3. [02_generation/paired_test](02_generation/paired_test/)：训练/测试配对的补充生成脚本，原实现使用种子偏移。它们属于补充版本，不能不核对设计就与任意训练结果混用。
4. [03_preprocessing](03_preprocessing/)：转换单一时间点输入，用于静态模型。
5. [04_models](04_models/)：`cox/`、`rsf/`、`jm/`、`rsflc/` 分别保存四个模型；`paired_test/` 保存配对测试集实现；`jm/jmbayes2_0722/` 保留另一套 JM 实现。
6. [05_evaluation](05_evaluation/)：汇总模型脚本输出的 C-index、AUC 和 BS，整理各情景结果。部分指标计算本身位于模型脚本内部，未为归档强行拆分。
7. [06_figures](06_figures/)：分层比较、柱状图、三维图及第四章图像脚本。

## 使用边界

上述编号表示研究逻辑，不是可直接连续运行的命令序列。原脚本可能依赖已经存在的相关矩阵工作簿、R 会话对象和绝对路径。不同版本使用的预测窗口、训练/测试划分及指标实现需按原脚本核对。

所有源码及重复来源见 [模拟主线索引](../docs/index_01_simulation.md) 和 [完整来源清单](../docs/source_manifest.csv)。早期实现、变量子集搜索及调试程序在 [历史目录](../90_history/README.md)。直接加减已有指标的历史作图脚本不作为本主线结果入口。
