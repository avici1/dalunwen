# 归档文档导航

- [SIMULATION_DESIGN.md](SIMULATION_DESIGN.md)：模拟研究的六个设计因素和代码入口。
- [CLINICAL_DESIGN.md](CLINICAL_DESIGN.md)：临床变量配置、静态/动态区别与流程。
- [ARCHIVE_NOTES.md](ARCHIVE_NOTES.md)：归档范围、已发现的实现差异与运行边界。
- [index_01_simulation.md](index_01_simulation.md)：模拟主线逐文件索引。
- [index_02_clinical.md](index_02_clinical.md)：实例主线逐文件索引。
- [index_90_history.md](index_90_history.md)：其他版本逐文件索引。
- [source_manifest.csv](source_manifest.csv)：全部 485 个源脚本的相对路径、去重关系、归档路径和 SHA-256。
- [source_documents.json](source_documents.json)：额外复制的原始说明文件与哈希。
- [organization_summary.json](organization_summary.json)：归档数量统计。
- [validation_summary.json](validation_summary.json)：复制哈希、来源完整性和静态检查结果。
- [r_syntax_check.csv](r_syntax_check.csv)、[python_syntax_check.json](python_syntax_check.json)：未执行源码的解析检查。
- [code_requirements.csv](code_requirements.csv)：绝对路径、输入、source 依赖和 SQL 操作位置。
- [dependency_inventory.csv](dependency_inventory.csv)：静态包引用记录，不代表可直接安装的锁定依赖。
- [secret_scan.json](secret_scan.json)：有限静态模式检查，不记录凭据值。

source_1 = `F:\文章\大论文`；source_2 = `F:\文章_大论文`。CSV 是归档元数据，均不含患者记录。
