import numpy as np
import pandas as pd
from pathlib import Path
from sklearn.ensemble import RandomForestRegressor


BASE_DIR = Path(r"F:\文章_大论文\MIMIC数据库_代码\TREA代码\0319_补充实例结果")
INPUT_FILE = BASE_DIR / "0318ORG_widedata_filter2.xlsx"
OUTPUT_FILE = BASE_DIR / "0319PRO_inputdata.xlsx"  # overwrite target


# 10 variables for charttime-based linear interpolation
LINEAR_COLS = [
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
]

# 12 variables for random forest imputation (include total calcium)
RF_COLS = [
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
    "itemid_50893",  # Total Calcium
]


def _time_linear_fill_group(group: pd.DataFrame, col: str) -> pd.Series:
    s = pd.to_numeric(group[col], errors="coerce").copy()
    if s.isna().sum() == 0:
        return s

    t = group["charttime_num"]
    valid = s.notna() & t.notna()

    # Time-based linear interpolation for rows with known charttime.
    if valid.sum() >= 2:
        x = t[valid].to_numpy(dtype=float)
        y = s[valid].to_numpy(dtype=float)
        target = s.isna() & t.notna()
        if target.any():
            s.loc[target] = np.interp(t[target].to_numpy(dtype=float), x, y)

    # Fallback for remaining NaN (e.g. missing charttime).
    s = s.interpolate(method="linear", limit_direction="both")
    return s


def linear_impute(df: pd.DataFrame, linear_cols: list[str]) -> pd.DataFrame:
    out = df.copy()
    for c in linear_cols:
        out[c] = pd.to_numeric(out[c], errors="coerce")

    out = out.sort_values(["subject_id", "charttime_dt", "Obstimes"]).copy()
    parts = []
    for _, g in out.groupby("subject_id", sort=False):
        g2 = g.copy()
        for c in linear_cols:
            g2[c] = _time_linear_fill_group(g2, c)
        parts.append(g2)
    return pd.concat(parts, axis=0).sort_index()


def rf_impute(df: pd.DataFrame, rf_cols: list[str], linear_cols: list[str]) -> pd.DataFrame:
    out = df.copy()
    for c in rf_cols + linear_cols + ["Obstimes", "charttime_num"]:
        if c in out.columns:
            out[c] = pd.to_numeric(out[c], errors="coerce")

    predictors_all = [c for c in (linear_cols + rf_cols + ["Obstimes", "charttime_num"]) if c in out.columns]

    for target in rf_cols:
        y = pd.to_numeric(out[target], errors="coerce")
        if y.isna().sum() == 0:
            continue

        feature_cols = [c for c in predictors_all if c != target]
        X = out[feature_cols].apply(pd.to_numeric, errors="coerce")
        X = X.fillna(X.median(numeric_only=True))

        train_mask = y.notna()
        pred_mask = y.isna()
        if train_mask.sum() < 30:
            out.loc[pred_mask, target] = y.median()
            continue

        model = RandomForestRegressor(
            n_estimators=300,
            random_state=42,
            n_jobs=-1,
            min_samples_leaf=2,
        )
        model.fit(X.loc[train_mask], y.loc[train_mask])
        out.loc[pred_mask, target] = model.predict(X.loc[pred_mask])

    return out


def main() -> None:
    if not INPUT_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {INPUT_FILE}")

    df = pd.read_excel(INPUT_FILE)
    if "subject_id" not in df.columns or "charttime" not in df.columns:
        raise ValueError("Missing required columns: subject_id/charttime")

    linear_cols = [c for c in LINEAR_COLS if c in df.columns]
    rf_cols = [c for c in RF_COLS if c in df.columns]
    target_cols = linear_cols + rf_cols

    before = df[target_cols].isna().sum().sort_values(ascending=False)

    df["charttime_dt"] = pd.to_datetime(df["charttime"], errors="coerce")
    t0 = df["charttime_dt"].min()
    df["charttime_num"] = (df["charttime_dt"] - t0).dt.total_seconds()

    df_lin = linear_impute(df, linear_cols)
    df_imp = rf_impute(df_lin, rf_cols, linear_cols)

    after = df_imp[target_cols].isna().sum().sort_values(ascending=False)

    df_out = df_imp.drop(columns=["charttime_dt", "charttime_num"], errors="ignore")
    df_out.to_excel(OUTPUT_FILE, index=False)

    print("Input:", INPUT_FILE)
    print("Output:", OUTPUT_FILE)
    print("Rows:", len(df_out))
    print("Linear cols used:", linear_cols)
    print("RF cols used:", rf_cols)
    print("\nMissing BEFORE:")
    print(before.to_string())
    print("\nMissing AFTER:")
    print(after.to_string())


if __name__ == "__main__":
    main()
