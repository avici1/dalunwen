# -*- coding: utf-8 -*-
import pandas as pd
from pathlib import Path

dir_ = Path(r"F:/文章/大论文/程序Trae/模拟数据_添加Y")
files = {
    "sim500_30_10V_lowINTER_1c_L1": dir_ / "sim500_30_10V_lowINTER_1c_L1.xlsx",
    "sim500_70_10V_lowINTER_1c_L1": dir_ / "sim500_70_10V_lowINTER_1c_L1.xlsx",
}

rows = []
for label, path in files.items():
    d = pd.read_excel(path, sheet_name=0)
    s = d.drop_duplicates("ID")[["ID", "obs_time", "event"]].copy()
    s["event"] = pd.to_numeric(s["event"])
    s["obs_time"] = pd.to_numeric(s["obs_time"])
    cens = s.loc[s["event"] == 0, "obs_time"]
    evt = s.loc[s["event"] == 1, "obs_time"]
    rows.append(
        {
            "dataset": label,
            "n": len(s),
            "n_censored": int(len(cens)),
            "n_event": int(len(evt)),
            "censor_rate": len(cens) / len(s),
            "mean_time_censored": float(cens.mean()),
            "mean_time_event": float(evt.mean()),
            "median_time_censored": float(cens.median()),
            "median_time_event": float(evt.median()),
            "max_obs_time": float(s["obs_time"].max()),
        }
    )
    print("===", label, "===")
    print(
        f"n={len(s)}, censored={len(cens)} ({len(cens)/len(s):.1%}), "
        f"event={len(evt)} ({len(evt)/len(s):.1%})"
    )
    print(
        f"censored mean={cens.mean():.4f} median={cens.median():.4f} "
        f"min={cens.min():.4f} max={cens.max():.4f}"
    )
    print(
        f"event     mean={evt.mean():.4f} median={evt.median():.4f} "
        f"min={evt.min():.4f} max={evt.max():.4f}"
    )
    print(f"overall max(obs_time)={s['obs_time'].max():.4f}")
    print()

out = pd.DataFrame(rows)
print(out.to_string(index=False))
