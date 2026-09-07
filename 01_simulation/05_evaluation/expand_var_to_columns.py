import re
from pathlib import Path

import pandas as pd


def main() -> None:
    base = Path(r"f:\文章_大论文\结果\all_summary")
    in_path = base / "RESULT_ALL.xlsx"
    out_path = base / "RESULT_ALL1.xlsx"

    df = pd.read_excel(in_path)

    # find `var` column (case-insensitive; fallback to any column containing "var")
    var_col = None
    for c in df.columns:
        if str(c).strip().lower() == "var":
            var_col = c
            break
    if var_col is None:
        for c in df.columns:
            if "var" in str(c).strip().lower():
                var_col = c
                break
    if var_col is None:
        raise ValueError(f"未找到 var 列。现有列：{list(df.columns)}")

    s = df[var_col].astype(str).fillna("")

    # Example: sim500_30_4V_lowINTER_1c
    pat = re.compile(
        r"^sim(?P<n>\d+)_+(?P<censor>\d+)_+(?P<vnum>\d+V)_+(?P<corr>[A-Za-z]+)(?P<feature>[A-Za-z]+)_+(?P<class>\d+c)$",
        re.IGNORECASE,
    )
    parsed = s.str.extract(pat)

    parsed["n"] = pd.to_numeric(parsed["n"], errors="coerce")
    parsed["censor"] = pd.to_numeric(parsed["censor"], errors="coerce")

    parsed = parsed.rename(
        columns={
            "n": "样本数",
            "censor": "删失率",
            "vnum": "变量数",
            "corr": "相关性",
            "feature": "相关特性",
            "class": "潜类别数",
        }
    )

    # Show a warning if some rows fail to parse (still writes output)
    bad = int(parsed["样本数"].isna().sum())
    if bad:
        bad_examples = s[parsed["样本数"].isna()].head(10).tolist()
        print(f"警告：有 {bad} 行 var 未按预期解析。示例：{bad_examples}")

    cols_to_add = ["样本数", "删失率", "变量数", "相关性", "相关特性", "潜类别数"]
    for c in cols_to_add:
        if c in df.columns:
            df = df.drop(columns=[c])

    if "model" in df.columns:
        insert_at = list(df.columns).index("model") + 1
    else:
        insert_at = 0

    for i, c in enumerate(cols_to_add):
        df.insert(insert_at + i, c, parsed[c])

    with pd.ExcelWriter(out_path, engine="openpyxl") as w:
        df.to_excel(w, index=False, sheet_name="RESULT_ALL1")

    print("written:", out_path)
    print("shape:", df.shape)
    print("var_col:", var_col)


if __name__ == "__main__":
    main()

