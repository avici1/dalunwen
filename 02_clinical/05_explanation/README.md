# 模型解释代码

本目录同时收录 RSF 与 RSFLC 的解释代码。2026-09-08 重新扫描了 `F:\文章\大论文` 和 `F:\文章_大论文`，补齐 RSF 的独立归档入口。

- [RSF 说明与版本清单](rsf_shap/README.md)：全局 SHAP、局部瀑布图、历史版本的 SHAP 依赖图，以及 VIMP、PDP。
- [RSF 主解释脚本](rsf_shap/05_static_explain.R)：完整原始脚本，与配置和上游静态模型脚本一起保存。
- `rsflc_shap/`：原有 RSFLC 拟合、预测与 Python SHAP 脚本。
- [复制来源与 SHA-256](RSF_SOURCE_MANIFEST.json)、[原目录关键词扫描结果](RSF_SCAN_RESULTS.json)。扫描命中含多模型及历史脚本，不能把所有命中都视为 RSF。

此前 RSF 解释代码位于 `../03_models/unified_pipeline/05_static_explain.R` 以及独立模型结果脚本中，本解释目录遗漏了分类入口。此次补充保存原脚本，不改变模型、计算方法或已有 RSFLC 文件。这里归档的是生成图像的代码；本次没有运行真实数据分析或重新生成图像。
