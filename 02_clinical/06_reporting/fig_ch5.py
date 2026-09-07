import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from math import pi
import os

outdir = os.path.dirname(os.path.abspath(__file__))

# 第5章用的CV结果
cv_path = r'C:\Users\EDY\Desktop\work\7月\7.8\XXA823\实例研究\处理后文件\结果\RSFLC交叉验证结果'

raw = {}
for f, m in [('metrics_cox_cv.csv','COX'),('metrics_rsf_cv.csv','RSF'),
             ('metrics_jm_cv.csv','JM'),('metrics_gcs_cv.csv','RSF_LC')]:
    raw[m] = pd.read_csv(os.path.join(cv_path, f))

models = ['COX','RSF','JM','RSF_LC']
c = ['#E74C3C','#3498DB','#2ECC71','#9B59B6']

# 图5-1
fig, ax = plt.subplots(1,3,figsize=(14,5))
for i,(col,lab) in enumerate(zip(['auc','cindex','bs'],['AUC','C-index','Brier Score'])):
    means = [raw[m][col].mean() for m in models]
    sds = [raw[m][col].std() for m in models]
    bars = ax[i].bar(models, means, color=c, yerr=sds, capsize=5, width=0.6)
    for b, v in zip(bars, means):
        ax[i].text(b.get_x()+b.get_width()/2, b.get_height()+0.01, f'{v:.3f}',
                   ha='center', va='bottom', fontsize=9, fontweight='bold')
    ax[i].set_title(lab, fontsize=13, fontweight='bold')
    ax[i].set_ylabel(lab, fontsize=11)
    ax[i].set_ylim(0, max(means)+max(sds)+0.08)
    ax[i].spines['top'].set_visible(False)
    ax[i].spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig(os.path.join(outdir,'fig5-1_model_comparison.png'),dpi=200)
plt.close()
print('fig5-1 ok')

# 图5-2
fig, ax = plt.subplots(figsize=(9,5))
for m,cl in zip(models,c):
    ax.plot([1,2,3,4,5], raw[m]['auc'].values, 'o-', color=cl, linewidth=2, markersize=7, label=m)
ax.set_xlabel('Fold',fontsize=12)
ax.set_ylabel('AUC',fontsize=12)
ax.set_title('Per-Fold AUC',fontsize=14,fontweight='bold')
ax.set_xticks([1,2,3,4,5])
ax.legend(fontsize=11)
ax.grid(axis='y',linestyle='--',alpha=0.4)
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
plt.tight_layout()
plt.savefig(os.path.join(outdir,'fig5-2_perfold_auc.png'),dpi=200)
plt.close()
print('fig5-2 ok')

# 图5-3
fig, ax = plt.subplots(figsize=(7,7), subplot_kw=dict(polar=True))
cats = ['AUC','C-index','1-BS']
angles = [n/3*2*pi for n in range(3)] + [0]
for m,cl in zip(models,c):
    vals = [raw[m]['auc'].mean(), raw[m]['cindex'].mean(), 1-raw[m]['bs'].mean()]
    ax.plot(angles, vals+vals[:1], 'o-', linewidth=2, label=m, color=cl)
    ax.fill(angles, vals+vals[:1], alpha=0.1, color=cl)
ax.set_xticks(angles[:-1])
ax.set_xticklabels(cats, fontsize=12)
ax.set_ylim(0,1)
ax.set_title('Radar Chart', fontsize=14, fontweight='bold', pad=20)
ax.legend(loc='upper right', bbox_to_anchor=(1.3,1.1))
plt.tight_layout()
plt.savefig(os.path.join(outdir,'fig5-3_radar.png'),dpi=200)
plt.close()
print('fig5-3 ok')
