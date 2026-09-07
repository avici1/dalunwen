"""
卒中数据 RSFLC SHAP 作图
1) 调用 RSFLC_stroke_fit.R：RF 筛 Top10 + DynForest 拟合
2) 通过 RSFLC_stroke_predict.R 包装 P(dead)
3) Kernel SHAP → beeswarm / bar，结果写入本文件夹
"""
from __future__ import annotations

import json
import subprocess
import tempfile
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import shap

OUT_DIR = Path(r"F:\文章_大论文\0722\pythonProject1\stroke_RSFLC_SHAP")
R_FIT = OUT_DIR / "RSFLC_stroke_fit.R"
R_PRED = OUT_DIR / "RSFLC_stroke_predict.R"

NTREE = 40
MAX_N = 1200
T0 = 5.0
N_BACKGROUND = 20
N_EXPLAIN = 20
NSAMPLES = 40
RANDOM_STATE = 42
REFIT = False  # True 会按上面参数重新拟合


def run_cmd(cmd: list, timeout: int | None = None) -> None:
    print(">>", " ".join(str(c) for c in cmd))
    proc = subprocess.run(
        [str(c) for c in cmd],
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        timeout=timeout,
    )
    if proc.stdout.strip():
        print(proc.stdout[-4000:])
    if proc.returncode != 0:
        raise RuntimeError(
            f"失败 code={proc.returncode}\nSTDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}"
        )


def ensure_artifacts() -> dict:
    meta_path = OUT_DIR / "meta.json"
    need = REFIT or not meta_path.exists() or not (OUT_DIR / "rsflc_model.rds").exists()
    if need:
        OUT_DIR.mkdir(parents=True, exist_ok=True)
        run_cmd(
            ["Rscript", str(R_FIT), str(OUT_DIR), str(NTREE), str(MAX_N), str(T0)],
            timeout=None,
        )
    with open(meta_path, encoding="utf-8") as f:
        return json.load(f)


class StrokeRSFLCPredictor:
    def __init__(self, artifact_dir: Path, feature_cols: list[str], t0: float):
        self.artifact_dir = Path(artifact_dir)
        self.feature_cols = list(feature_cols)
        self.t0 = t0
        self.long_id: int | None = None

    def set_long_id(self, long_id: int) -> None:
        self.long_id = int(long_id)

    def __call__(self, X) -> np.ndarray:
        X = np.asarray(X, dtype=float)
        if X.ndim == 1:
            X = X.reshape(1, -1)
        if self.long_id is None:
            raise RuntimeError("请先 set_long_id()")
        df = pd.DataFrame(X, columns=self.feature_cols)
        df["long_id"] = self.long_id
        with tempfile.TemporaryDirectory() as td:
            td = Path(td)
            feat_path = td / "features.csv"
            out_path = td / "pred.csv"
            df.to_csv(feat_path, index=False)
            run_cmd(
                [
                    "Rscript",
                    str(R_PRED),
                    str(self.artifact_dir),
                    str(feat_path),
                    str(out_path),
                    str(self.t0),
                    str(self.long_id),
                ],
                timeout=900,
            )
            pred = pd.read_csv(out_path)
        return pred["risk"].to_numpy(dtype=float)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    meta = ensure_artifacts()
    feature_cols = list(meta["feature_cols"])
    t0 = float(meta.get("params", {}).get("t0", T0))

    feat = pd.read_csv(OUT_DIR / "subject_features.csv")
    ids = feat["id"].to_numpy()
    X_all = feat[feature_cols]

    print("Top10 特征:", feature_cols)
    if (OUT_DIR / "variable_importance_RF.csv").exists():
        imp = pd.read_csv(OUT_DIR / "variable_importance_RF.csv")
        print("\nRF 重要性（前15）:")
        print(imp.head(15).to_string(index=False))

    rng = np.random.default_rng(RANDOM_STATE)
    bg_idx = rng.choice(len(feat), size=min(N_BACKGROUND, len(feat)), replace=False)
    ex_idx = rng.choice(len(feat), size=min(N_EXPLAIN, len(feat)), replace=False)
    X_bg = X_all.iloc[bg_idx].reset_index(drop=True)
    X_ex = X_all.iloc[ex_idx].reset_index(drop=True)
    id_ex = ids[ex_idx]

    predictor = StrokeRSFLCPredictor(OUT_DIR, feature_cols, t0=t0)
    predictor.set_long_id(int(ids[bg_idx[0]]))
    explainer = shap.KernelExplainer(predictor, X_bg)

    shap_rows = []
    for i, sid in enumerate(id_ex):
        print(f"\n=== 解释 {i + 1}/{len(id_ex)}  id={sid} ===")
        predictor.set_long_id(int(sid))
        sv = explainer.shap_values(X_ex.iloc[[i]], nsamples=NSAMPLES)
        if isinstance(sv, list):
            sv = sv[0]
        shap_rows.append(np.asarray(sv).reshape(-1))

    shap_values = np.vstack(shap_rows)
    mean_abs = np.abs(shap_values).mean(axis=0)
    rank = pd.Series(mean_abs, index=feature_cols).sort_values(ascending=False)
    print("\n平均 |SHAP|:")
    print(rank.round(6).to_string())
    rank.to_csv(OUT_DIR / "rsflc_shap_mean_abs.csv", encoding="utf-8-sig")

    plt.figure()
    shap.summary_plot(shap_values, X_ex, feature_names=feature_cols, show=False)
    plt.title(f"Stroke RSFLC SHAP (beeswarm) — P(dead|t0={t0:g})")
    plt.tight_layout()
    plt.savefig(OUT_DIR / "rsflc_shap_beeswarm.png", dpi=160, bbox_inches="tight")
    plt.close()

    plt.figure()
    shap.summary_plot(
        shap_values, X_ex, feature_names=feature_cols, plot_type="bar", show=False
    )
    plt.title(f"Stroke RSFLC mean |SHAP| — P(dead|t0={t0:g})")
    plt.tight_layout()
    plt.savefig(OUT_DIR / "rsflc_shap_bar.png", dpi=160, bbox_inches="tight")
    plt.close()

    detail = X_ex.copy()
    detail.insert(0, "id", id_ex)
    for j, col in enumerate(feature_cols):
        detail[f"SHAP_{col}"] = shap_values[:, j]
    detail.to_csv(OUT_DIR / "rsflc_shap_values.csv", index=False, encoding="utf-8-sig")
    print(f"\n全部结果已保存到: {OUT_DIR}")


if __name__ == "__main__":
    main()
