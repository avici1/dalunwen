"""
对模拟数据计算 SHAP：V1–V10 为自变量，Y 为应变量
"""
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.model_selection import train_test_split
import shap

# ---------- 路径 ----------
DATA_PATH = Path(r"F:\文章\大论文\程序Trae\模拟数据_添加Y\sim500_30_10V_highBTW_1c_L5.xlsx")
OUT_DIR = Path(r"F:\文章_大论文\0722\pythonProject1")
OUT_DIR.mkdir(parents=True, exist_ok=True)

# ---------- 1) 读取数据：V1–V10 → Y ----------
df = pd.read_excel(DATA_PATH)
feature_cols = [f"V{i}" for i in range(1, 11)]
X = df[feature_cols]
y = df["Y"]

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.33, random_state=42
)
print(f"样本量: {len(df)}, 特征数: {len(feature_cols)}")
print(f"训练集: {len(X_train)}, 测试集: {len(X_test)}")

# ---------- 2) 训练树模型 ----------
model = GradientBoostingRegressor(
    n_estimators=120, max_depth=3, learning_rate=0.08, random_state=42
)
model.fit(X_train, y_train)
print("R^2 =", round(model.score(X_test, y_test), 3))

# ---------- 3) 计算 SHAP ----------
explainer = shap.TreeExplainer(model)
shap_values = explainer.shap_values(X_test)  # (n_samples, n_features)
base_value = float(np.asarray(explainer.expected_value).reshape(-1)[0])

# 核对 Local Accuracy：φ0 + Σφ = f(x)
i = 0
pred = model.predict(X_test.iloc[[i]])[0]
print("f(x)     =", pred)
print("φ0+Σφ    =", base_value + shap_values[i].sum())  # 应几乎相等

# 平均 |SHAP|（特征重要性排序）
mean_abs = np.abs(shap_values).mean(axis=0)
rank = pd.Series(mean_abs, index=feature_cols).sort_values(ascending=False)
print("\n平均 |SHAP| 排序:")
print(rank.round(4).to_string())
rank.to_csv(OUT_DIR / "shap_mean_abs.csv", encoding="utf-8-sig")

# ---------- 4) Summary Plot（beeswarm）----------
shap.summary_plot(shap_values, X_test, show=False)
plt.title("SHAP Summary Plot (beeswarm) — V1–V10")
plt.tight_layout()
plt.savefig(OUT_DIR / "shap_summary_beeswarm.png", dpi=160, bbox_inches="tight")
plt.close()

# 平均 |SHAP| 条形图
shap.summary_plot(shap_values, X_test, plot_type="bar", show=False)
plt.title("SHAP Feature Importance (mean |SHAP|)")
plt.tight_layout()
plt.savefig(OUT_DIR / "shap_summary_bar.png", dpi=160, bbox_inches="tight")
plt.close()

print(f"\n图已保存到: {OUT_DIR}")
