import numpy as np
import pandas as pd
from pathlib import Path
from sklearn.ensemble import RandomForestRegressor


BASE_DIR = Path(r"F:\文章_大论文\MIMIC数据库_代码\TREA代码\0319_补充实例结果")
INPUT_FILE = BASE_DIR / "0319ORG_widedata_7.xlsx"
OUTPUT_FILE = BASE_DIR / "0319PRO_inputdata.xlsx"


# 10 variables for time-based linear interpolation
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

# 11 variables for random-forest imputation
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
]


def _time_linear_fill_group(group: pd.DataFrame, col: str) -> pd.Series:
    """Linear interpolation using charttime as time axis; fallback to order-based linear fill."""
    s = pd.to_numeric(group[col], errors="coerce").copy()
    if s.isna().sum() == 0:
        return s

    t = group["charttime_num"].copy()
    valid_mask = s.notna() & t.notna()

    # Time-aware interpolation on rows with valid charttime
    if valid_mask.sum() >= 2:
        x = t[valid_mask].to_numpy(dtype=float)
        y = s[valid_mask].to_numpy(dtype=float)
        fill_mask = s.isna() & t.notna()
        if fill_mask.any():
            s.loc[fill_mask] = np.interp(
                t[fill_mask].to_numpy(dtype=float),
                x,
                y,
            )

    # Fallback for any remaining NaN (e.g., missing charttime)
    s = s.interpolate(method="linear", limit_direction="both")
    return s


def linear_impute(df: pd.DataFrame, linear_cols: list[str]) -> pd.DataFrame:
    df = df.copy()
    for c in linear_cols:
        df[c] = pd.to_numeric(df[c], errors="coerce")

    # Sort within patient by time before interpolation
    sort_keys = ["subject_id", "charttime_dt", "Obstimes"]
    present_sort_keys = [k for k in sort_keys if k in df.columns]
    df = df.sort_values(present_sort_keys).copy()

    out_parts = []
    for _, g in df.groupby("subject_id", sort=False):
        g2 = g.copy()
        for c in linear_cols:
            g2[c] = _time_linear_fill_group(g2, c)
        out_parts.append(g2)

    out = pd.concat(out_parts, axis=0)
    out = out.sort_index()
    return out


def rf_impute(df: pd.DataFrame, rf_cols: list[str], linear_cols: list[str]) -> pd.DataFrame:
    df = df.copy()
    for c in rf_cols + linear_cols + ["Obstimes", "charttime_num"]:
        if c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")

    # Use lab variables + time/obstime as predictors
    predictor_pool = [c for c in (linear_cols + rf_cols + ["Obstimes", "charttime_num"]) if c in df.columns]

    for target in rf_cols:
        y = pd.to_numeric(df[target], errors="coerce")
        miss_mask = y.isna()
        if miss_mask.sum() == 0:
            continue

        feature_cols = [c for c in predictor_pool if c != target]
        if not feature_cols:
            continue

        X = df[feature_cols].apply(pd.to_numeric, errors="coerce")
        X_filled = X.fillna(X.median(numeric_only=True))

        train_mask = y.notna()
        pred_mask = y.isna()

        # If too few labels, fallback to median
        if train_mask.sum() < 30:
            med = y.median()
            df.loc[pred_mask, target] = med
            continue

        model = RandomForestRegressor(
            n_estimators=300,
            random_state=42,
            n_jobs=-1,
            min_samples_leaf=2,
        )
        model.fit(X_filled.loc[train_mask], y.loc[train_mask])
        preds = model.predict(X_filled.loc[pred_mask])
        df.loc[pred_mask, target] = preds

    return df


def main() -> None:
    if not INPUT_FILE.exists():
        raise FileNotFoundError(f"Input file not found: {INPUT_FILE}")

    df = pd.read_excel(INPUT_FILE)
    if "charttime" not in df.columns:
        raise ValueError("Missing required column: charttime")
    if "subject_id" not in df.columns:
        raise ValueError("Missing required column: subject_id")

    linear_cols = [c for c in LINEAR_COLS if c in df.columns]
    rf_cols = [c for c in RF_COLS if c in df.columns]

    missing_before = df[linear_cols + rf_cols].isna().sum().sort_values(ascending=False)

    df["charttime_dt"] = pd.to_datetime(df["charttime"], errors="coerce")
    t0 = df["charttime_dt"].min()
    df["charttime_num"] = (df["charttime_dt"] - t0).dt.total_seconds()

    df_lin = linear_impute(df, linear_cols)
    df_imp = rf_impute(df_lin, rf_cols, linear_cols)

    missing_after = df_imp[linear_cols + rf_cols].isna().sum().sort_values(ascending=False)

    # Drop helper columns before export
    df_out = df_imp.drop(columns=["charttime_dt", "charttime_num"], errors="ignore")
    df_out.to_excel(OUTPUT_FILE, index=False)

    print("Input:", INPUT_FILE)
    print("Output:", OUTPUT_FILE)
    print("Rows:", len(df_out))
    print("Linear cols used:", linear_cols)
    print("RF cols used:", rf_cols)
    print("\nMissing count BEFORE:")
    print(missing_before.to_string())
    print("\nMissing count AFTER:")
    print(missing_after.to_string())


if __name__ == "__main__":
    main()
