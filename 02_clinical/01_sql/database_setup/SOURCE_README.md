# MIMIC-IV Navicat 导入说明

本文件夹包含可在 **Navicat** 中运行的 SQL 脚本，用于将 MIMIC 数据导入 PostgreSQL。

## 执行顺序

在 Navicat 中**按顺序**运行以下脚本：

| 步骤 | 脚本 | 说明 |
|------|------|------|
| 1 | `01_create_database.sql` | 创建 mimiciv 数据库（需连接 postgres） |
| 2 | `02_create_schema_tables.sql` | 创建 schema 和表结构 |
| 3 | `03_load_data_已填路径.sql` | 加载 CSV 数据 |
| 4 | `04_constraint.sql` | 添加主键与外键 |
| 5 | `05_index.sql` | 创建索引 |

## 操作步骤

### 1. 创建数据库
- 在 Navicat 中连接到 `postgres` 数据库
- 打开 `01_create_database.sql`，点击“运行”
- 刷新连接，新建连接至 `mimiciv` 数据库

### 2. 创建表结构
- 连接到 `mimiciv` 数据库
- 运行 `02_create_schema_tables.sql`

### 3. 加载数据
- 运行 `03_load_data_已填路径.sql`
- **注意**：`COPY` 要求文件在 **PostgreSQL 服务器** 可访问的路径
- 若为本地 PostgreSQL，路径 `f:/文章_大论文/MIMIC数据库_代码/MIMIC` 已写入
- 若 COPY 报错（权限/路径），请使用 **方案 B：Navicat 导入向导**

### 4. 添加约束和索引
- 运行 `04_constraint.sql`
- 运行 `05_index.sql`（可能需数小时）

---

## 方案 B：COPY 失败时用 Navicat 导入向导

若 `03_load_data_已填路径.sql` 报错（如远程库、权限不足），可用 Navicat 手动导入 CSV：

1. 运行完 `02_create_schema_tables.sql` 后
2. 在 Navicat 中右键表 → **导入向导** → 选择 **CSV**
3. 按表对应关系导入：

| 表 | CSV 文件路径 |
|----|-------------|
| mimiciv_hosp.patients | MIMIC\hosp\patients.csv |
| mimiciv_hosp.admissions | MIMIC\hosp\admissions.csv |
| mimiciv_hosp.d_hcpcs | MIMIC\hosp\d_hcpcs.csv |
| ... | ... |
| mimiciv_icu.chartevents | MIMIC\icu\chartevents.csv |
| ... | ... |

4. 导入时勾选“第一行是列标题”，分隔符选逗号
5. 全部导入后再运行 `04_constraint.sql` 和 `05_index.sql`

---

## 常见问题

**Q: COPY 报错 "could not open file for reading"**  
A: 说明服务器无法访问该路径。请使用方案 B 手动导入，或将 MIMIC 放到服务器可访问的目录。

**Q: 需使用 postgres 用户吗？**  
A: 使用 COPY 时，建议用 `postgres` 超级用户连接，否则可能权限不足。

**Q: chartevents 导入很久？**  
A: 正常。该表数据量大，可能需要数小时。
