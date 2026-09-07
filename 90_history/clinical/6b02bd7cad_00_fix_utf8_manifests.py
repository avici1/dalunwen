from __future__ import annotations

import csv
import os
from pathlib import Path

import pandas as pd


supp_root = Path(os.environ.get("CH5_SUPPLEMENT_ROOT", "supplement_output"))
data_root = Path(os.environ.get("CH5_SOURCE_DATA", "source_data"))
table_out = supp_root / "tables"
table_out.mkdir(parents=True, exist_ok=True)

base = pd.read_csv(data_root / "baseline.csv", usecols=["death_28d", "death_90d"])
mortality_rows = []
for label, column in [("28天死亡", "death_28d"), ("90天死亡", "death_90d")]:
    n = int(base[column].notna().sum())
    deaths = int((base[column] == 1).sum())
    percent = deaths / n * 100
    mortality_rows.append(
        {
            "结局": label,
            "样本量": n,
            "死亡例数": deaths,
            "百分比": percent,
            "例数(%)": f"{deaths:,} ({percent:.1f})",
        }
    )
pd.DataFrame(mortality_rows).to_csv(
    table_out / "Table_5-1b_mortality_supplement.csv",
    index=False,
    encoding="utf-8-sig",
)

rows = [
    ("已生成但DOCX位置错误", "图5-7", "Landmark生存RSFLC原生OOB变量重要性", "RSFLC_01_OOB_VIMP.png"),
    ("已生成但DOCX位置错误", "图5-8", "RSFLC验证集置换变量重要性", "RSFLC_02_validation_permutation_VIMP.png"),
    ("已生成但DOCX位置错误", "图5-9", "RSFLC固定协变量SHAP分布", "RSFLC_03_SHAP_beeswarm.png"),
    ("已生成但DOCX位置错误", "图5-10", "RSFLC典型患者局部SHAP解释", "RSFLC_04_typical_patient_SHAP.png"),
    ("已生成但DOCX位置错误", "图5-11", "RSFLC首要固定协变量SHAP依赖关系", "RSFLC_05_SHAP_dependence.png"),
    ("已生成但DOCX位置错误", "图5-12", "JM关联参数MCMC轨迹图", "JM_03_MCMC_trace.png"),
    ("已生成但DOCX位置错误", "图5-13", "JM关联参数后验密度图", "JM_04_MCMC_density.png"),
    ("已生成但DOCX未嵌入", "补图A", "RSF OOB收敛", "RSF_01_OOB_convergence.png"),
    ("已生成但DOCX未嵌入", "补图B", "RSF验证集置换变量重要性", "RSF_03_validation_permutation_VIMP.png"),
    ("已生成但DOCX未嵌入", "补图C", "RSF最小深度", "RSF_04_minimal_depth.png"),
    ("已生成但DOCX未嵌入", "补图D", "静态任务时间依赖指标", "STATIC_11_time_metrics.png"),
    ("已生成但DOCX未嵌入", "补图E", "RSFLC时间依赖指标", "RSFLC_08_time_metrics.png"),
    ("已生成但DOCX未嵌入", "补图F", "四模型验证性能汇总", "ALL_02_validation_performance.png"),
]
manifest = []
for index, (status, number, title, source_file) in enumerate(rows, start=1):
    manifest.append(
        {
            "状态": status,
            "建议编号": number,
            "题名": title,
            "源文件": source_file,
            "输出文件": f"Supplement_{index:02d}_{source_file}",
        }
    )
pd.DataFrame(manifest).to_csv(
    table_out / "missing_and_misplaced_figure_manifest.csv",
    index=False,
    encoding="utf-8-sig",
    quoting=csv.QUOTE_MINIMAL,
)


def metric_text(summary: pd.DataFrame, metric: str) -> str:
    row = summary.loc[summary["metric"] == metric].iloc[0]
    return f"{row['mean']:.4f} ± {row['sd']:.4f}"


static_summary_path = table_out / "Table_5-3A_RSF_fixed_5fold_summary.csv"
if static_summary_path.exists():
    summary = pd.read_csv(static_summary_path)
    pd.DataFrame(
        [
            {
                "模型": "RSF",
                "搜索空间": "ntree={300,500,1000}; mtry={3,6,9}; nodesize={10,20,30,40}; nsplit={10,25,50}",
                "固定评价参数": "ntree=500; mtry=3; nodesize=10; nsplit=10",
                "五折验证 C-index": metric_text(summary, "C_index"),
                "五折验证 AUC": metric_text(summary, "AUC_28"),
                "五折验证 Brier": metric_text(summary, "Brier_28"),
                "五折验证 IBS": metric_text(summary, "IBS_0_28"),
                "说明": "当前31变量口径下对锁定参数组合进行5折评价；并非重新遍历完整参数网格",
            }
        ]
    ).to_csv(
        table_out / "Table_5-3A_RSF_fixed_5fold_ready_to_fill.csv",
        index=False,
        encoding="utf-8-sig",
    )

dynamic_summary_path = table_out / "Table_5-3B_RSFLC_fixed_5fold_summary.csv"
if dynamic_summary_path.exists():
    summary = pd.read_csv(dynamic_summary_path)
    pd.DataFrame(
        [
            {
                "模型": "RSFLC",
                "搜索空间": "ntree={50,100,200}; mtry={3,6,9,12}; nodesize={1,3,5}; minsplit=2; t0=5",
                "固定评价参数": "ntree=200; mtry=3; nodesize=1; minsplit=2; t0=5",
                "五折验证 C-index": metric_text(summary, "C_index"),
                "五折验证 AUC": metric_text(summary, "AUC_28"),
                "五折验证 Brier": metric_text(summary, "Brier_28"),
                "五折验证 IBS": metric_text(summary, "IBS_5_28"),
                "说明": "当前3轨迹+25固定变量口径下对锁定参数组合进行5折评价；并非重新遍历完整参数网格",
            }
        ]
    ).to_csv(
        table_out / "Table_5-3B_RSFLC_fixed_5fold_ready_to_fill.csv",
        index=False,
        encoding="utf-8-sig",
    )

summary_rows = [
    {"项目": "表5-1b 28天死亡", "结果": mortality_rows[0]["例数(%)"], "状态": "已补充"},
    {"项目": "表5-1b 90天死亡", "结果": mortality_rows[1]["例数(%)"], "状态": "已补充"},
    {"项目": "错位或未嵌入图", "结果": "13张", "状态": "已汇总到figures"},
]
if static_summary_path.exists():
    s = pd.read_csv(static_summary_path)
    summary_rows.append(
        {
            "项目": "表5-3A RSF固定参数五折",
            "结果": f"C-index {metric_text(s, 'C_index')}; AUC {metric_text(s, 'AUC_28')}; Brier {metric_text(s, 'Brier_28')}; IBS {metric_text(s, 'IBS_0_28')}",
            "状态": "已完成",
        }
    )
if dynamic_summary_path.exists():
    s = pd.read_csv(dynamic_summary_path)
    summary_rows.append(
        {
            "项目": "表5-3B RSFLC固定参数五折",
            "结果": f"C-index {metric_text(s, 'C_index')}; AUC {metric_text(s, 'AUC_28')}; Brier {metric_text(s, 'Brier_28')}; IBS {metric_text(s, 'IBS_5_28')}",
            "状态": "已完成",
        }
    )
pd.DataFrame(summary_rows).to_csv(
    table_out / "supplement_result_summary.csv",
    index=False,
    encoding="utf-8-sig",
)

print("UTF8_MANIFESTS_OK")
