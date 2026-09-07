import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

outdir = os.path.dirname(os.path.abspath(__file__))

df = pd.read_excel(r'C:\Users\EDY\Desktop\work\7月\7.8\XXA823\结果整理\RESULT_ALL2.XLSX')
df['模型名'] = df['模型'].map({'cox':'COX','jm':'JM','rsf':'RSF','rsflc':'RSF_LC'})
models = ['COX','RSF','JM','RSF_LC']
c = ['#E74C3C','#3498DB','#2ECC71','#9B59B6']

def group_bar(data, gcol, gvals, title, fname):
    fig, ax = plt.subplots(1,3,figsize=(15,5))
    for i,(mcol,mlab) in enumerate(zip(['AUC_mean','Cindex_mean','BS_mean'],['AUC','C-index','Brier Score'])):
        x = np.arange(len(gvals))
        w = 0.2
        for j,mod in enumerate(models):
            s = data[data['模型名']==mod]
            means = [s[s[gcol]==v][mcol].mean() for v in gvals]
            stds = [s[s[gcol]==v][mcol].std() for v in gvals]
            ax[i].bar(x+j*w, means, w, label=mod, color=c[j], yerr=stds, capsize=3)
        ax[i].set_xlabel(gcol, fontsize=11)
        ax[i].set_title(mlab, fontsize=12, fontweight='bold')
        ax[i].set_xticks(x+w*1.5)
        ax[i].set_xticklabels(gvals)
        ax[i].spines['top'].set_visible(False)
        ax[i].spines['right'].set_visible(False)
        if i==2: ax[i].legend(fontsize=9)
    fig.suptitle(title, fontsize=14, fontweight='bold', y=1.02)
    plt.tight_layout()
    plt.savefig(os.path.join(outdir,fname), dpi=200, bbox_inches='tight')
    plt.close()
    print(f'{fname} ok')

# 图4-1
fig, ax = plt.subplots(1,3,figsize=(14,5))
for i,(mcol,mlab) in enumerate(zip(['AUC_mean','Cindex_mean','BS_mean'],['AUC','C-index','Brier Score'])):
    means = [df[df['模型名']==m][mcol].mean() for m in models]
    stds = [df[df['模型名']==m][mcol].std() for m in models]
    bars = ax[i].bar(models, means, color=c, yerr=stds, capsize=5, width=0.6)
    for b,v in zip(bars, means):
        ax[i].text(b.get_x()+b.get_width()/2, b.get_height()+0.01, f'{v:.3f}',
                   ha='center', va='bottom', fontsize=9, fontweight='bold')
    ax[i].set_title(mlab, fontsize=12, fontweight='bold')
    ax[i].spines['top'].set_visible(False)
    ax[i].spines['right'].set_visible(False)
plt.suptitle('Fig4-1 Overall Comparison', fontsize=14, fontweight='bold', y=1.02)
plt.tight_layout()
plt.savefig(os.path.join(outdir,'fig4-1_overall.png'), dpi=200, bbox_inches='tight')
plt.close()
print('fig4-1 ok')

group_bar(df, '样本量', [500,1000], 'Fig4-2 By Sample Size', 'fig4-2_samplesize.png')
group_bar(df, '删失率', [30,70], 'Fig4-3 By Censoring Rate', 'fig4-3_censoring.png')
group_bar(df, '相关特性', ['INTER','BTW'], 'Fig4-4 By Collinearity', 'fig4-4_collinearity.png')
group_bar(df, '潜类别', ['1C','3C'], 'Fig4-5 By Latent Classes', 'fig4-5_latentclass.png')
group_bar(df, '相关性', ['low','mid','high'], 'Fig4-6 By Correlation', 'fig4-6_correlation.png')
