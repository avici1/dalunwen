"""
RSFLC (DynForest) → Python Kernel SHAP 作图

流程:
  1) Rscript RSFLC_fit_model.R  拟合并导出 rsflc_shap_artifacts/
  2) 本脚本用 Rscript 调用 RSFLC_predict_bridge.R 作为 f(x)
  3) shap.KernelExplainer 对 V1–V10 计算 SHAP，输出图与表

重要说明:
  - 原版 cal_3_RSF_LC 未纳入 V1–V10；本接口已将其作为固定协变量加入
  - 风险 = DynForest 预测的 CIF（默认 t_horizon=2；t=1 处常全为 0）
  - 解释某个体时，纵向 Y 轨迹固定为该个体（long_id），只扰动 V1–V10
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

PROJECT_DIR = Path(r"F:\文章_大论文\0722\pythonProject1")
DATA_PATH = Path(r"F:\文章\大论文\程序Trae\模拟数据_添加Y\sim500_30_10V_highBTW_1c_L5.xlsx")
ARTIFACT_DIR = PROJECT_DIR / "rsflc_shap_artifacts"
OUT_DIR = PROJECT_DIR / "rsflc_shap_plots"
R_FIT = PROJECT_DIR / "RSFLC_fit_model.R"
R_PRED = PROJECT_DIR / "RSFLC_predict_bridge.R"

NTREE = 100       # 与论文接近可改 500（更慢）
T0 = 1.0          # DynForest landmark
T_HORIZON = 2.0   # 取该时点 CIF 作为风险（t=1 处常无区分度）
N_BACKGROUND = 25
N_EXPLAIN = 30    # 解释个体数；逐个调用 R，约 15–20 秒/人
NSAMPLES = 50
RANDOM_STATE = 42
REFIT = False     # 首次或改 ntree 时改为 True


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
        print(proc.stdout)
    if proc.returncode != 0:
        raise RuntimeError(
            f"命令失败 code={proc.returncode}\nSTDOUT:\n{proc.stdout}\nSTDERR:\n{proc.stderr}"
        )


def ensure_artifacts() -> dict:
    meta_path = ARTIFACT_DIR / "meta.json"
    need = REFIT or not meta_path.exists() or not (ARTIFACT_DIR / "rsflc_model.rds").exists()
    if need:
        ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
        run_cmd(
            [
                "Rscript",
                str(R_FIT),
                str(DATA_PATH),
                str(ARTIFACT_DIR),
                str(NTREE),
                str(T_HORIZON),
            ]
        )
    with open(meta_path, encoding="utf-8") as f:
        return json.load(f)


class RSFLCPredictor:
    """X(n,p) -> risk(n,); 需先 set_long_id 锁定纵向轨迹。"""

    def __init__(self, artifact_dir: Path, feature_cols: list[str], t0: float, t_horizon: float):
        self.artifact_dir = Path(artifact_dir)
        self.feature_cols = list(feature_cols)
        self.t0 = t0
        self.t_horizon = t_horizon
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
                    str(self.t_horizon),
                ],
                timeout=900,
            )
            pred = pd.read_csv(out_path)
        return pred["risk"].to_numpy(dtype=float)


def main() -> None:
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    meta = ensure_artifacts()
    feature_cols = list(meta["feature_cols"])
    t_horizon = float(meta.get("params", {}).get("t_horizon", T_HORIZON))
    t0 = float(meta.get("params", {}).get("t0", T0))

    feat = pd.read_csv(ARTIFACT_DIR / "subject_features.csv")
    ids = feat["id"].to_numpy()
    X_all = feat[feature_cols]

    rng = np.random.default_rng(RANDOM_STATE)
    bg_idx = rng.choice(len(feat), size=min(N_BACKGROUND, len(feat)), replace=False)
    ex_idx = rng.choice(len(feat), size=min(N_EXPLAIN, len(feat)), replace=False)

    X_bg = X_all.iloc[bg_idx].reset_index(drop=True)
    X_ex = X_all.iloc[ex_idx].reset_index(drop=True)
    id_ex = ids[ex_idx]

    predictor = RSFLCPredictor(ARTIFACT_DIR, feature_cols, t0=t0, t_horizon=t_horizon)
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
    print("shap_values:", shap_values.shape)

    mean_abs = np.abs(shap_values).mean(axis=0)
    rank = pd.Series(mean_abs, index=feature_cols).sort_values(ascending=False)
    print("\n平均 |SHAP|:")
    print(rank.round(6).to_string())
    rank.to_csv(OUT_DIR / "rsflc_shap_mean_abs.csv", encoding="utf-8-sig")

    plt.figure()
    shap.summary_plot(shap_values, X_ex, feature_names=feature_cols, show=False)
    plt.title(f"RSFLC SHAP beeswarm — CIF@{t_horizon:g}")
    plt.tight_layout()
    plt.savefig(OUT_DIR / "rsflc_shap_beeswarm.png", dpi=160, bbox_inches="tight")
    plt.close()

    plt.figure()
    shap.summary_plot(
        shap_values, X_ex, feature_names=feature_cols, plot_type="bar", show=False
    )
    plt.title(f"RSFLC mean |SHAP| — CIF@{t_horizon:g}")
    plt.tight_layout()
    plt.savefig(OUT_DIR / "rsflc_shap_bar.png", dpi=160, bbox_inches="tight")
    plt.close()

    detail = X_ex.copy()
    detail.insert(0, "id", id_ex)
    for j, col in enumerate(feature_cols):
        detail[f"SHAP_{col}"] = shap_values[:, j]
    detail.to_csv(OUT_DIR / "rsflc_shap_values.csv", index=False, encoding="utf-8-sig")
    print(f"\n已保存: {OUT_DIR}")


if __name__ == "__main__":
    main()
