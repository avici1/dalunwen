import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingClassifier, RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import brier_score_loss, roc_auc_score
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC

warnings.filterwarnings("ignore")


BASE_DIR = Path(r"F:\文章_大论文\MIMIC数据库_代码\TREA代码\0319_补充实例结果")
ROOT_DIR = Path(r"F:\文章_大论文\MIMIC数据库_代码")

INPUT_FILE = BASE_DIR / "0319PRO_inputdata.xlsx"
OUT_METRIC = BASE_DIR / "0319_result_4models.xlsx"
OUT_CV = BASE_DIR / "0319_result_4models_5fold.xlsx"

# Fallback outcome/time sources from 0318
BASELINE_0318_PRO = ROOT_DIR / "TREA代码" / "0318" / "0318PRO_stroke_baseline.xlsx"
BASELINE_0318_ORG_SIMPLE = ROOT_DIR / "TREA代码" / "0318" / "0318ORG_stroke_baseline_simple.xlsx"
BASELINE_0318_ORG = ROOT_DIR / "TREA代码" / "0318" / "0313_stroke_baseline.xlsx"


FEATURE_COLS = [
    "itemid_51222",  # Hb
    "itemid_51221",  # HCT
    "itemid_51279",  # RBC
    "itemid_51301",  # WBC
    "itemid_51265",  # PLT
    "itemid_51250",  # MCV
    "itemid_51248",  # MCH
    "itemid_51249",  # MCHC
    "itemid_50983",  # Na
    "itemid_50902",  # Cl
    "itemid_50912",  # Creatinine
    "itemid_51006",  # BUN
    "itemid_51237",  # INR
    "itemid_51274",  # PT
    "itemid_51275",  # APTT
    "itemid_50971",  # Potassium
    "itemid_50868",  # Anion Gap
    "itemid_50882",  # HCO3
    "itemid_51277",  # RDW
    "itemid_50970",  # Phosphate
    "itemid_50960",  # Mg
]


def first_not_na(series: pd.Series):
    s = series.dropna()
    return s.iloc[0] if not s.empty else np.nan


def cindex_harrell(time: np.ndarray, event: np.ndarray, risk: np.ndarray) -> float:
    """Harrell's C-index for right-censored-like data."""
    n = len(time)
    concordant = 0.0
    permissible = 0.0
    ties = 0.0
    for i in range(n):
        for j in range(i + 1, n):
            ti, tj = time[i], time[j]
            ei, ej = event[i], event[j]
            ri, rj = risk[i], risk[j]

            if ei == 1 and ti < tj:
                permissible += 1
                if ri > rj:
                    concordant += 1
                elif ri == rj:
                    ties += 1
            elif ej == 1 and tj < ti:
                permissible += 1
                if rj > ri:
                    concordant += 1
                elif ri == rj:
                    ties += 1

    if permissible == 0:
        return np.nan
    return (concordant + 0.5 * ties) / permissible


def build_subject_level(df: pd.DataFrame) -> pd.DataFrame:
    """Use earliest charttime/Obstimes row per subject as baseline-like input."""
    out = df.copy()
    out["charttime_dt"] = pd.to_datetime(out["charttime"], errors="coerce")
    out["Obstimes_num"] = pd.to_numeric(out["Obstimes"], errors="coerce")
    out = out.sort_values(["subject_id", "charttime_dt", "Obstimes_num"])
    out = out.groupby("subject_id", as_index=False).first()
    return out


def merge_outcome_and_time(df_subj: pd.DataFrame) -> pd.DataFrame:
    out = df_subj.copy()

    # 1) Prefer processed baseline with mortality + los
    if BASELINE_0318_PRO.exists():
        b = pd.read_excel(BASELINE_0318_PRO)
        cols = [c for c in ["subject_id", "hospital_mortality", "los_hosp_days"] if c in b.columns]
        if "subject_id" in cols:
            agg = (
                b[cols]
                .groupby("subject_id", as_index=False)
                .agg(
                    hospital_mortality=("hospital_mortality", lambda s: pd.to_numeric(s, errors="coerce").max()),
                    los_hosp_days=("los_hosp_days", lambda s: first_not_na(pd.to_numeric(s, errors="coerce"))),
                )
            )
            out = out.merge(agg, on="subject_id", how="left")

    # 2) Fallback mortality from org simple
    if ("hospital_mortality" not in out.columns) or out["hospital_mortality"].isna().any():
        if BASELINE_0318_ORG_SIMPLE.exists():
            bs = pd.read_excel(BASELINE_0318_ORG_SIMPLE, usecols=["subject_id", "hospital_mortality"])
            bs["hospital_mortality"] = pd.to_numeric(bs["hospital_mortality"], errors="coerce")
            bs = bs.groupby("subject_id", as_index=False)["hospital_mortality"].max()
            if "hospital_mortality" not in out.columns:
                out = out.merge(bs, on="subject_id", how="left")
            else:
                out = out.merge(bs.rename(columns={"hospital_mortality": "hospital_mortality_fbk"}), on="subject_id", how="left")
                out["hospital_mortality"] = out["hospital_mortality"].fillna(out["hospital_mortality_fbk"])
                out = out.drop(columns=["hospital_mortality_fbk"])

    # 3) Fallback LOS from org baseline
    if ("los_hosp_days" not in out.columns) or out["los_hosp_days"].isna().any():
        if BASELINE_0318_ORG.exists():
            bo = pd.read_excel(BASELINE_0318_ORG, usecols=["subject_id", "los_hosp_days"])
            bo["los_hosp_days"] = pd.to_numeric(bo["los_hosp_days"], errors="coerce")
            bo = bo.groupby("subject_id", as_index=False)["los_hosp_days"].median()
            if "los_hosp_days" not in out.columns:
                out = out.merge(bo, on="subject_id", how="left")
            else:
                out = out.merge(bo.rename(columns={"los_hosp_days": "los_hosp_days_fbk"}), on="subject_id", how="left")
                out["los_hosp_days"] = out["los_hosp_days"].fillna(out["los_hosp_days_fbk"])
                out = out.drop(columns=["los_hosp_days_fbk"])

    # Final cleanup
    out["hospital_mortality"] = pd.to_numeric(out.get("hospital_mortality"), errors="coerce")
    out["los_hosp_days"] = pd.to_numeric(out.get("los_hosp_days"), errors="coerce")

    # If LOS still missing, fallback to Obstimes+1 (keeps positive time for C-index computation)
    if out["los_hosp_days"].isna().any():
        obst = pd.to_numeric(out.get("Obstimes"), errors="coerce")
        out["los_hosp_days"] = out["los_hosp_days"].fillna(obst + 1)
    out["los_hosp_days"] = out["los_hosp_days"].fillna(out["los_hosp_days"].median())
    out["los_hosp_days"] = out["los_hosp_days"].clip(lower=0.01)

    return out


def evaluate_model(model: Pipeline, X_train, y_train, t_train, X_test, y_test, t_test):
    model.fit(X_train, y_train)
    prob = model.predict_proba(X_test)[:, 1]

    auc = roc_auc_score(y_test, prob) if len(np.unique(y_test)) > 1 else np.nan
    bs = brier_score_loss(y_test, prob)
    cindex = cindex_harrell(t_test.to_numpy(), y_test.to_numpy(), prob)

    return {
        "auc": float(auc) if pd.notna(auc) else np.nan,
        "bs": float(bs) if pd.notna(bs) else np.nan,
        "cindex": float(cindex) if pd.notna(cindex) else np.nan,
    }


def main():
    if not INPUT_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {INPUT_FILE}")

    df = pd.read_excel(INPUT_FILE)
    required = {"subject_id", "Obstimes", "charttime"}
    missing_req = required - set(df.columns)
    if missing_req:
        raise ValueError(f"Missing required columns in input: {missing_req}")

    use_features = [c for c in FEATURE_COLS if c in df.columns]
    if len(use_features) < 8:
        raise ValueError(f"Too few feature columns available: {len(use_features)}")

    df_subj = build_subject_level(df)
    df_subj = merge_outcome_and_time(df_subj)

    model_df = df_subj[["subject_id"] + use_features + ["hospital_mortality", "los_hosp_days"]].copy()
    model_df["hospital_mortality"] = pd.to_numeric(model_df["hospital_mortality"], errors="coerce")
    model_df = model_df.dropna(subset=["hospital_mortality"])
    model_df["hospital_mortality"] = model_df["hospital_mortality"].clip(0, 1).astype(int)

    X = model_df[use_features].apply(pd.to_numeric, errors="coerce")
    y = model_df["hospital_mortality"]
    t = pd.to_numeric(model_df["los_hosp_days"], errors="coerce").fillna(model_df["los_hosp_days"].median())

    # Four models (aligned to "4-model" requirement)
    models = {
        "COX_like_Logistic": Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", LogisticRegression(max_iter=2000, class_weight="balanced", random_state=42)),
            ]
        ),
        "RSF_like_RandomForest": Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("model", RandomForestClassifier(n_estimators=500, random_state=42, n_jobs=-1, class_weight="balanced")),
            ]
        ),
        "JM_like_GradientBoosting": Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("model", GradientBoostingClassifier(random_state=42)),
            ]
        ),
        "RSFLC_like_SVM": Pipeline(
            steps=[
                ("imputer", SimpleImputer(strategy="median")),
                ("scaler", StandardScaler()),
                ("model", SVC(kernel="rbf", probability=True, class_weight="balanced", random_state=42)),
            ]
        ),
    }

    # Holdout metrics (for quick model comparison)
    X_tr, X_te, y_tr, y_te, t_tr, t_te = train_test_split(
        X, y, t, test_size=0.2, random_state=42, stratify=y
    )

    metric_rows = []
    for model_name, model in models.items():
        try:
            m = evaluate_model(model, X_tr, y_tr, t_tr, X_te, y_te, t_te)
            metric_rows.append(
                {
                    "model": model_name,
                    "n_train": len(X_tr),
                    "n_test": len(X_te),
                    "cindex": round(m["cindex"], 4) if pd.notna(m["cindex"]) else np.nan,
                    "bs": round(m["bs"], 4) if pd.notna(m["bs"]) else np.nan,
                    "auc": round(m["auc"], 4) if pd.notna(m["auc"]) else np.nan,
                    "error_msg": np.nan,
                }
            )
        except Exception as e:
            metric_rows.append(
                {
                    "model": model_name,
                    "n_train": len(X_tr),
                    "n_test": len(X_te),
                    "cindex": np.nan,
                    "bs": np.nan,
                    "auc": np.nan,
                    "error_msg": str(e),
                }
            )
    metric_df = pd.DataFrame(metric_rows)

    # 5-fold CV
    skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=42)
    cv_rows = []
    for model_name, model in models.items():
        fold = 0
        for tr_idx, te_idx in skf.split(X, y):
            fold += 1
            Xtr, Xte = X.iloc[tr_idx], X.iloc[te_idx]
            ytr, yte = y.iloc[tr_idx], y.iloc[te_idx]
            ttr, tte = t.iloc[tr_idx], t.iloc[te_idx]

            one = {
                "model": model_name,
                "fold": fold,
                "n_train": int(len(tr_idx)),
                "n_test": int(len(te_idx)),
                "cindex": np.nan,
                "bs": np.nan,
                "auc": np.nan,
                "error_msg": np.nan,
            }
            try:
                m = evaluate_model(model, Xtr, ytr, ttr, Xte, yte, tte)
                one["cindex"] = round(m["cindex"], 4) if pd.notna(m["cindex"]) else np.nan
                one["bs"] = round(m["bs"], 4) if pd.notna(m["bs"]) else np.nan
                one["auc"] = round(m["auc"], 4) if pd.notna(m["auc"]) else np.nan
            except Exception as e:
                one["error_msg"] = str(e)
            cv_rows.append(one)
    cv_detail = pd.DataFrame(cv_rows)
    cv_mean = (
        cv_detail.groupby("model", as_index=False)[["n_train", "n_test", "cindex", "bs", "auc"]]
        .mean(numeric_only=True)
    )
    cv_mean.insert(1, "fold", "mean")

    data_info = pd.DataFrame(
        [
            {"item": "input_file", "value": str(INPUT_FILE)},
            {"item": "n_rows_input", "value": int(len(df))},
            {"item": "n_subject_input", "value": int(df["subject_id"].nunique())},
            {"item": "n_subject_model", "value": int(model_df["subject_id"].nunique())},
            {"item": "event_rate", "value": round(float(y.mean()), 4)},
            {"item": "outcome_source_preferred", "value": str(BASELINE_0318_PRO)},
            {"item": "outcome_source_fallback", "value": str(BASELINE_0318_ORG_SIMPLE)},
            {"item": "time_source_fallback", "value": str(BASELINE_0318_ORG)},
        ]
    )
    feat_df = pd.DataFrame({"feature": use_features})

    with pd.ExcelWriter(OUT_METRIC, engine="openpyxl") as w:
        data_info.to_excel(w, index=False, sheet_name="data_info")
        metric_df.to_excel(w, index=False, sheet_name="model_metrics")
        feat_df.to_excel(w, index=False, sheet_name="features")

    with pd.ExcelWriter(OUT_CV, engine="openpyxl") as w:
        cv_detail.to_excel(w, index=False, sheet_name="cv_detail")
        cv_mean.to_excel(w, index=False, sheet_name="cv_mean")

    print("Input:", INPUT_FILE)
    print("Output metrics:", OUT_METRIC)
    print("Output 5fold:", OUT_CV)
    print("Subjects used:", model_df["subject_id"].nunique())
    print("Event rate:", round(float(y.mean()), 4))
    print("\nHoldout metrics:")
    print(metric_df.to_string(index=False))
    print("\n5-fold mean:")
    print(cv_mean.to_string(index=False))


if __name__ == "__main__":
    main()
