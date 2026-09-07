# -*- coding: utf-8 -*-
"""给训练集（group=1）按 subject_id 划分 5 折，测试集 fold 为空。

同一 subject_id 在基线表与纵向表中的 fold 相同。
"""
from pathlib import Path

import numpy as np
import pandas as pd

SEED = 2026
N_FOLD = 5
BASE_DIR = Path(r"F:/文章_大论文/0722/实例研究代码")
BASELINE_IN = BASE_DIR / "stroke_baseline_knn_0824.csv"
LONG_IN = BASE_DIR / "stroke_longitudinal_knn_0824_group.csv"
BASELINE_OUT = BASE_DIR / "stroke_baseline_knn_0824_fold.csv"
LONG_OUT = BASE_DIR / "stroke_longitudinal_knn_0824_group_fold.csv"


def assign_folds(train_subjects: pd.DataFrame, seed: int = SEED, n_fold: int = N_FOLD) -> pd.DataFrame:
    rng = np.random.default_rng(seed)
    out = train_subjects.copy()
    out["fold"] = pd.NA
    for event_val in (0, 1):
        idx = out.index[out["event"] == event_val].to_numpy()
        if len(idx) == 0:
            continue
        labels = np.resize(np.arange(1, n_fold + 1, dtype=int), len(idx))
        rng.shuffle(labels)
        out.loc[idx, "fold"] = labels
    out["fold"] = out["fold"].astype("Int64")
    return out


def main() -> None:
    baseline = pd.read_csv(BASELINE_IN, encoding="utf-8")
    longitudinal = pd.read_csv(LONG_IN, encoding="utf-8", low_memory=False)

    if "fold" in baseline.columns:
        baseline = baseline.drop(columns=["fold"])
    if "fold" in longitudinal.columns:
        longitudinal = longitudinal.drop(columns=["fold"])

    train = baseline.loc[baseline["group"] == 1, ["subject_id", "death_28d"]].copy()
    train_subjects = (
        train.groupby("subject_id", as_index=False)
        .agg(event=("death_28d", "max"))
    )
    train_subjects = assign_folds(train_subjects)

    fold_map = train_subjects.set_index("subject_id")["fold"]
    baseline["fold"] = baseline["subject_id"].map(fold_map).astype("Int64")
    longitudinal["fold"] = longitudinal["subject_id"].map(fold_map).astype("Int64")

    # 测试集（group=2）保持 NA；训练集每人必须有折号
    train_mask_b = baseline["group"] == 1
    if baseline.loc[train_mask_b, "fold"].isna().any():
        raise RuntimeError("基线训练集存在未分配 fold 的 subject_id")
    if (baseline.loc[baseline["group"] == 2, "fold"].notna()).any():
        raise RuntimeError("基线测试集不应分配 fold")

    train_mask_l = longitudinal["group"] == 1
    if longitudinal.loc[train_mask_l, "fold"].isna().any():
        missing = sorted(
            set(longitudinal.loc[train_mask_l & longitudinal["fold"].isna(), "subject_id"])
        )
        raise RuntimeError(f"纵向训练集存在未分配 fold 的 subject_id: {missing[:10]}")

    # 两表同一患者折号一致
    b_fold = baseline.loc[baseline["fold"].notna(), ["subject_id", "fold"]].drop_duplicates()
    l_fold = longitudinal.loc[longitudinal["fold"].notna(), ["subject_id", "fold"]].drop_duplicates()
    merged = b_fold.merge(l_fold, on="subject_id", how="inner", suffixes=("_b", "_l"))
    if not (merged["fold_b"] == merged["fold_l"]).all():
        raise RuntimeError("两表同一 subject_id 的 fold 不一致")

    baseline.to_csv(BASELINE_OUT, index=False, encoding="utf-8")
    longitudinal.to_csv(LONG_OUT, index=False, encoding="utf-8")

    print("训练集 subject 数 =", len(train_subjects))
    print("基线 fold 分布（group=1 按 subject，event=该患者任一次住院 28 天死亡）：")
    print(
        train_subjects.groupby("fold")
        .agg(n_subject=("subject_id", "nunique"), n_event=("event", "sum"))
        .to_string()
    )
    print("已写入:")
    print(" ", BASELINE_OUT)
    print(" ", LONG_OUT)


if __name__ == "__main__":
    main()
