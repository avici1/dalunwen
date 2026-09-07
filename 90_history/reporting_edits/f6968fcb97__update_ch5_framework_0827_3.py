# -*- coding: utf-8 -*-
"""改写 5.3.1 分析框架，并同步 5.3.2/5.5/5.6/小结中冲突表述 → 0827_3_*.docx"""
from __future__ import annotations

import shutil
from pathlib import Path

from docx import Document

SRC = Path(r"F:\文章_大论文\0722\大论文版本") / (
    "0827_2_基于机器学习的生存分析方法探讨_第五章表5-2方案A精简.docx"
)
OUT = Path(r"F:\文章_大论文\0722\大论文版本") / (
    "0827_3_基于机器学习的生存分析方法探讨_第五章分析框架修订.docx"
)


def replace_paragraph_text(paragraph, new_text: str) -> None:
    if paragraph.runs:
        paragraph.runs[0].text = new_text
        for r in paragraph.runs[1:]:
            r.text = ""
    else:
        paragraph.add_run(new_text)


# (old_startswith_or_exact_match_key, new_text)
# Use unique prefixes from current paragraphs for matching.
REPLACEMENTS: list[tuple[str, str]] = [
    (
        "5.3.1 交叉验证框架",
        "5.3.1 分析框架",
    ),
    (
        "采用嵌套五折交叉验证的方式对四种生存模型",
        (
            "为在真实临床数据上比较COX、JM、RSF与RSFLC的预测能力，并兼顾超参数选择与结果解释，"
            "本研究采用“训练集内交叉验证调参—全训练集重拟合—独立验证集评价”的两阶段框架。"
        ),
    ),
    (
        "交叉验证的实施过程如下。首先将所有的6，299次住院记录",
        (
            "首先，按住院标识将6,299次卒中住院预先划分为训练集与验证集（约70%/30%）。"
            "该划分在数据预处理阶段固定，后续调参、拟合与评价均在同一划分下进行，"
            "以避免临时重抽样造成评价口径漂移。"
            "第一阶段仅在训练集内部实施五折交叉验证，服务于两项目的："
            "一是在候选超参数网格上，以验证折的平均C-index（必要时辅以AUC、Brier Score）筛选最优超参数组合；"
            "二是观察各折性能波动，初步评估模型在不同训练子样本下的稳定性。"
            "交叉验证过程不使用独立验证集，以保证终评样本独立。"
            "第二阶段用第一阶段选定的最优超参数，在全部训练集上重新拟合模型，"
            "并在独立验证集上预测，计算C-index、28天AUC与Brier Score等指标；"
            "四种模型的预测优劣主要依据该验证集结果进行比较。"
        ),
    ),
    (
        "对COX模型而言，在每一个训练折中用基于AIC的逐步变量选择",
        (
            "就具体模型设定而言：COX模型通常无需网格调参，可直接在训练集拟合后于验证集评价；"
            "JM模型将GCS轨迹作为主要纵向结局，纵向子模型采用线性混合效应模型，"
            "生存子模型采用比例风险形式，并通过共享随机效应连接纵向与生存部分；"
            "RSF在训练集内五折交叉验证中对树数、每次分裂候选变量数、终端节点最小样本等超参数进行网格搜索，"
            "再以最优组合在全训练集上重拟合；"
            "RSFLC为landmark动态随机森林，在时刻t0=5天纳入纵向轨迹与固定协变量，"
            "同样经训练集内五折交叉验证选定超参数后，在全训练集重拟合并于验证集评价。"
            "未死亡病例在第28天予以行政删失，以与短期预后终点对齐。"
        ),
    ),
    (
        "需要说明的是，嵌套五折交叉验证用于四种模型的总体性能比较",
        (
            "在完成验证集预测后，进一步对机器学习模型（RSF、RSFLC）开展置换变量重要性（VIMP）与SHAP分析，"
            "以刻画在既定模型与评价协议下各协变量对28天死亡风险的相对贡献。"
            "该类解释性分析建立在上述最优模型及其验证集预测之上，与性能终评共用同一划分，而不另设并行评价协议。"
            "需要说明的是：表5-3汇报的是训练集内五折交叉验证的均值±标准差，主要用于展示调参阶段的折间稳定性"
            "及模型间的初步排序；独立验证集上的终评性能、解释性分析与临床效用评价见第5.5节和第5.6节（如表5-6）。"
            "二者分工不同——前者侧重稳定性与调参过程，后者侧重外样本预测与解释，后文引用时按相应表注区分，"
            "不以交叉验证均值替代验证集终评数字。"
        ),
    ),
    (
        "5.3.2 交叉验证结果",
        "5.3.2 训练集内五折交叉验证结果",
    ),
    (
        "表3总结了四种模型的五折交叉验证结果。",
        (
            "表5-3总结了四种模型在训练集内五折交叉验证中的性能（均值±标准差），"
            "用于反映调参阶段的折间稳定性，并为模型间提供初步排序参考；"
            "独立验证集上的终评见后文表5-6及相关分析。"
            "RSF模型在区分度上表现最高，平均AUC为0.7977（SD=0.0154），平均C-index为0.7753（SD=0.0135）。"
            "RSFLC模型排名第二，平均AUC为0.7223（SD=0.0093），C-index为0.7003（SD=0.0110）。"
            "COX模型表现居中，AUC为0.6761（SD=0.0131），C-index为0.6513（SD=0.0082）。"
            "JM模型区分度相对最弱，平均AUC为0.6563（SD=0.0217），C-index为0.6457（SD=0.0194），整体略低于COX模型。"
        ),
    ),
    (
        "表5-3 五折交叉验证结果（均值±标准差）",
        "表5-3 训练集内五折交叉验证结果（均值±标准差；用于稳定性与初步比较）",
    ),
    (
        "图5-2为交叉验证结果。",
        "图5-2为训练集内五折交叉验证结果。",
    ),
    (
        "图5-3为各个折的AUC值。",
        (
            "图5-3为训练集内各折的AUC值。"
        ),
    ),
    (
        "为提高机器学习模型的可解释性，并识别卒中28天死亡预测中的主要因素，本文在留出集设定下同时报告",
        (
            "为提高机器学习模型的可解释性，并识别卒中28天死亡预测中的主要因素，"
            "本文在独立验证集（约30%）上，基于第二阶段重拟合的最优RSF与RSFLC模型，"
            "同时报告置换变量重要性（permutation VIMP）与SHAP（SHapley Additive Explanations）。"
            "VIMP通过随机打乱某一变量后观察预测性能下降来衡量该变量的全局贡献；"
            "SHAP则把单次预测分解为各特征的加性贡献，正值表示推动28天死亡风险升高，负值表示推动风险降低。"
            "RSF的SHAP基于其纳入的协变量计算；RSFLC的SHAP仅扰动固定协变量，纵向轨迹保持原值，"
            "因此SHAP图不直接给出纵向轨迹变量的局部贡献。"
        ),
    ),
    (
        "表5-6给出两模型在30%测试集上的28天预测性能，供解释性分析对照。需要再次强调",
        (
            "表5-6给出两模型在独立验证集上的28天预测性能，作为本框架下的终评结果，"
            "并作为解释性分析、校准与决策曲线等后续分析的共同对照。"
            "表5-3为训练集内五折交叉验证的稳定性结果，与表5-6分工不同；"
            "引用时应注意区分，不以交叉验证均值替代验证集终评数字。"
        ),
    ),
    (
        "表5-6 留出集28天预测性能（70%/30%划分；不替代表5-3）",
        "表5-6 独立验证集28天预测性能（70%/30%划分；终评结果）",
    ),
    (
        "以下分析均在30%测试集、28天结局上进行，同样不替代表5-3的交叉验证结论。",
        (
            "以下分析均在独立验证集、28天结局上进行，与表5-6终评及第5.5节解释性分析共用同一划分；"
            "表5-3仅反映训练集内交叉验证的稳定性，不作为本节绝对概率与净获益的引用依据。"
        ),
    ),
    (
        "本章以MIMIC-IV数据库中6299次脑卒中住院记录（对应6092名患者；含缺血性与出血性等亚型）的真实临床数据为基础，用系统五折交叉验证、统计检验，以及独立留出集上的VIMP、SHAP、校准、决策曲线和风险分层，对四种生存分析模型（COX、JM、RSF、RSFLC）的预测性能与可解释性做评价。",
        (
            "本章以MIMIC-IV数据库中6299次脑卒中住院记录（对应6092名患者；含缺血性与出血性等亚型）的真实临床数据为基础，"
            "采用“训练集内五折交叉验证调参并评估稳定性—全训练集重拟合—独立验证集评价”的框架，"
            "结合统计检验以及验证集上的VIMP、SHAP、校准、决策曲线和风险分层，"
            "对四种生存分析模型（COX、JM、RSF、RSFLC）的预测性能与可解释性做评价。"
        ),
    ),
    (
        "在独立的70%/30%留出集与28天结局下，RSF的测试集C-index为0.7885、AUC为0.8193，RSFLC的测试集C-index为0.7736、AUC为0.8021。VIMP与SHAP表明，机械通气和SAPSII是最稳定的风险贡献因素，其次为APSIII、OASIS、年龄和CCI；RSFLC中GCS轨迹的置换重要性为0。校准图提示高危端存在高估，但DCA与Kaplan–Meier分层支持其作为风险分层工具。上述解释性结论与表5-3的五折交叉验证排序相互补充，二者评价协议不同，不宜混用数字。结合前面章节的模拟结果，本文为临床生存预测任务中的模型选择提供了可对照的证据。",
        (
            "在独立验证集与28天结局下，RSF的验证集C-index为0.7885、AUC为0.8193，"
            "RSFLC的验证集C-index为0.7736、AUC为0.8021，可作为本框架下的终评结果。"
            "VIMP与SHAP表明，机械通气和SAPSII是最稳定的风险贡献因素，其次为APSIII、OASIS、年龄和CCI；"
            "RSFLC中GCS轨迹的置换重要性为0。"
            "校准图提示高危端存在高估，但DCA与Kaplan–Meier分层支持其作为风险分层工具。"
            "表5-3的训练集内五折结果与上述验证集终评排序总体一致，前者主要用于稳定性与调参过程说明，"
            "后者用于外样本预测与解释；二者分工不同，引用时按表注区分。"
            "结合前面章节的模拟结果，本文为临床生存预测任务中的模型选择提供了可对照的证据。"
        ),
    ),
]


def main() -> None:
    shutil.copy2(SRC, OUT)
    doc = Document(str(OUT))

    # Also update figure captions that hard-code nested/CV framing if exact match
    extra = [
        (
            "图5-2 四种模型在三项评价指标上的性能比较。误差线为±1个标准差。",
            "图5-2 训练集内五折交叉验证：四种模型在三项评价指标上的性能比较。误差线为±1个标准差。",
        ),
        (
            "图5-3 各折AUC对比。在所有的五折中，RSF的AUC值都是最高的。",
            "图5-3 训练集内五折各折AUC对比。在所有折中，RSF的AUC值都是最高的。",
        ),
        (
            "表5-7 留出集置换变量重要性对照（RSFLC含GCS）",
            "表5-7 验证集置换变量重要性对照（RSFLC含GCS）",
        ),
        (
            "图5-4 留出集置换变量重要性（A. RSF，前10项；B. RSFLC，GCS+固定协变量）",
            "图5-4 验证集置换变量重要性（A. RSF，前10项；B. RSFLC，GCS+固定协变量）",
        ),
        (
            "第四，留出集上的VIMP与SHAP分析表明",
            (
                "第四，独立验证集上的VIMP与SHAP分析表明，机械通气、SAPSII、APSIII、OASIS、年龄和Charlson合并症指数"
                "是28天死亡风险的主要贡献因素；其中机械通气和SAPSII在RSF与RSFLC中均位居最前。"
                "与仅使用基线变量的RSF相比，RSFLC额外纳入GCS纵向轨迹后，GCS的置换重要性为0，"
                "提示在已有入院严重度评分的前提下，昏迷评分轨迹的增量有限。"
                "需要指出，本节解释性分析未纳入SOFA等其他纵向实验室指标，因此不能推广为“动态监测整体不重要”。"
            ),
        ),
    ]
    all_reps = REPLACEMENTS + extra

    matched = []
    unmatched = []
    for old_prefix, new_text in all_reps:
        found = False
        for p in doc.paragraphs:
            t = p.text
            if t.startswith(old_prefix) or t == old_prefix:
                # For partial first-sentence replacements that continue with more text
                # we already put full new paragraphs in REPLACEMENTS for key ones.
                # Special case: 图5-3为各个折 — old text continues; we used full replace via startswith
                if old_prefix == "图5-3为各个折的AUC值。":
                    # keep the rest after first sentence
                    rest = t[len("图5-3为各个折的AUC值。") :]
                    replace_paragraph_text(p, new_text + rest)
                elif old_prefix == "图5-2为交叉验证结果。":
                    rest = t[len("图5-2为交叉验证结果。") :]
                    replace_paragraph_text(p, new_text + rest)
                elif old_prefix == "表3总结了四种模型的五折交叉验证结果。":
                    # full rewrite already contains the numbers; ignore old remainder
                    replace_paragraph_text(p, new_text)
                elif old_prefix.startswith("为提高机器学习模型的可解释性"):
                    replace_paragraph_text(p, new_text)
                elif old_prefix.startswith("表5-6给出两模型"):
                    replace_paragraph_text(p, new_text)
                else:
                    replace_paragraph_text(p, new_text)
                matched.append(old_prefix[:40])
                found = True
                break
        if not found:
            unmatched.append(old_prefix[:60])

    # Soften leftover “嵌套” / “不替代表5-3” if any remain in ch5 range
    leftovers = []
    for p in doc.paragraphs:
        t = p.text
        if "嵌套五折" in t or "嵌套交叉" in t:
            leftovers.append(("nested", t[:80]))
        if "不替代表5-3" in t:
            leftovers.append(("not_replace", t[:80]))
        if "评价协议不同" in t and "表5-3" in t:
            leftovers.append(("protocol", t[:80]))

    doc.save(str(OUT))
    print("saved:", OUT)
    print("matched:", len(matched))
    for m in matched:
        print("  OK:", m)
    print("unmatched:", len(unmatched))
    for u in unmatched:
        print("  MISS:", u)
    print("leftover flags:", leftovers)

    # verify key paragraphs
    doc2 = Document(str(OUT))
    keys = ["5.3.1", "两阶段框架", "表5-3", "表5-6", "不替代表", "嵌套", "分析框架"]
    for i, p in enumerate(doc2.paragraphs):
        t = p.text.strip()
        if any(k in t for k in ("5.3.1", "5.3.2", "两阶段", "嵌套", "不替代", "表5-6 独立", "表5-3 训练", "本章小结"[:2])):
            if i >= 259 and i <= 335:
                if t.startswith("5.") or "两阶段" in t or "嵌套" in t or t.startswith("表5-3") or t.startswith("表5-6") or "终评" in t[:40] or t.startswith("本章以"):
                    print(i, t[:140])


if __name__ == "__main__":
    main()
