# -*- coding: utf-8 -*-
"""方案A：新增 5.3.2 超参数选择；原 5.3.2→5.3.3；表5-3起顺延。输出 0827_4_*.docx"""
from __future__ import annotations

import copy
import re
import shutil
from pathlib import Path

import pandas as pd
from docx import Document
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt
from docx.table import Table

ROOT = Path(r"F:\文章_大论文")
SRC = ROOT / r"0722\大论文版本\0827_3_基于机器学习的生存分析方法探讨_第五章分析框架修订.docx"
OUT = ROOT / r"0722\大论文版本\0827_4_基于机器学习的生存分析方法探讨_第五章超参数选择.docx"
RSF_DIR = ROOT / r"0722\实例研究代码\5折RSF"
RSFLC_DIR = ROOT / r"0722\实例研究代码\5折RSFLC"

HEADER_FILL = "D9E2F3"
FONT_ASCII = "Times New Roman"
FONT_EA = "宋体"
FONT_SIZE_PT = 10.5


def set_run_font(run, bold: bool | None = None, size_pt: float = FONT_SIZE_PT):
    run.font.name = FONT_ASCII
    run.font.size = Pt(size_pt)
    if bold is not None:
        run.bold = bold
    rPr = run._r.get_or_add_rPr()
    rFonts = rPr.find(qn("w:rFonts"))
    if rFonts is None:
        rFonts = OxmlElement("w:rFonts")
        rPr.insert(0, rFonts)
    rFonts.set(qn("w:ascii"), FONT_ASCII)
    rFonts.set(qn("w:hAnsi"), FONT_ASCII)
    rFonts.set(qn("w:eastAsia"), FONT_EA)
    rFonts.set(qn("w:cs"), FONT_ASCII)


def set_cell_text(cell, text: str, *, bold: bool = False, align: str = "center"):
    for p in cell.paragraphs:
        for r in p.runs:
            r.text = ""
    p0 = cell.paragraphs[0]
    if p0.runs:
        p0.runs[0].text = text
        set_run_font(p0.runs[0], bold=bold)
        for r in p0.runs[1:]:
            r.text = ""
    else:
        run = p0.add_run(text)
        set_run_font(run, bold=bold)
    pPr = p0._p.get_or_add_pPr()
    jc = pPr.find(qn("w:jc"))
    if jc is None:
        jc = OxmlElement("w:jc")
        pPr.append(jc)
    jc.set(qn("w:val"), align)
    tcPr = cell._tc.get_or_add_tcPr()
    vAlign = tcPr.find(qn("w:vAlign"))
    if vAlign is None:
        vAlign = OxmlElement("w:vAlign")
        tcPr.append(vAlign)
    vAlign.set(qn("w:val"), "center")


def set_cell_shading(cell, fill: str | None):
    tcPr = cell._tc.get_or_add_tcPr()
    shd = tcPr.find(qn("w:shd"))
    if fill is None:
        if shd is not None:
            tcPr.remove(shd)
        return
    if shd is None:
        shd = OxmlElement("w:shd")
        tcPr.append(shd)
    shd.set(qn("w:val"), "clear")
    shd.set(qn("w:color"), "auto")
    shd.set(qn("w:fill"), fill)


def set_tbl_border_el(borders, tag: str):
    el = borders.find(qn(f"w:{tag}"))
    if el is None:
        el = OxmlElement(f"w:{tag}")
        borders.append(el)
    el.set(qn("w:val"), "single")
    el.set(qn("w:color"), "000000")
    el.set(qn("w:sz"), "4")
    el.set(qn("w:space"), "0")


def apply_ref_table_format(table):
    try:
        table.style = "Normal Table"
    except Exception:
        pass
    tbl = table._tbl
    tblPr = tbl.tblPr
    if tblPr is None:
        tblPr = OxmlElement("w:tblPr")
        tbl.insert(0, tblPr)
    borders = tblPr.find(qn("w:tblBorders"))
    if borders is None:
        borders = OxmlElement("w:tblBorders")
        tblPr.append(borders)
    for edge in ["top", "left", "bottom", "right", "insideH", "insideV"]:
        set_tbl_border_el(borders, edge)

    def upsert(tag, attrs):
        el = tblPr.find(qn(f"w:{tag}"))
        if el is None:
            el = OxmlElement(f"w:{tag}")
            tblPr.append(el)
        for k, v in attrs.items():
            el.set(qn(f"w:{k}"), v)

    upsert("tblW", {"w": "5000", "type": "pct"})
    upsert("jc", {"val": "center"})
    upsert("tblLayout", {"type": "autofit"})

    n_cols = len(table.columns)
    for ri, row in enumerate(table.rows):
        is_header = ri == 0
        for ci in range(n_cols):
            cell = row.cells[ci]
            text = cell.text.strip()
            if is_header:
                set_cell_shading(cell, HEADER_FILL)
                set_cell_text(cell, text, bold=True, align="center")
            else:
                set_cell_shading(cell, None)
                set_cell_text(cell, text, bold=False, align="left" if ci == 0 else "center")


def replace_paragraph_text(paragraph, new_text: str):
    if paragraph.runs:
        paragraph.runs[0].text = new_text
        for r in paragraph.runs[1:]:
            r.text = ""
    else:
        paragraph.add_run(new_text)


def clone_p_after(anchor_el, style_src_p, text: str):
    new_p = copy.deepcopy(style_src_p._p)
    texts = list(new_p.iter(qn("w:t")))
    if texts:
        texts[0].text = text
        for t in texts[1:]:
            t.text = ""
    else:
        r = OxmlElement("w:r")
        t = OxmlElement("w:t")
        t.set(qn("xml:space"), "preserve")
        t.text = text
        r.append(t)
        new_p.append(r)
    anchor_el.addnext(new_p)
    return new_p


def read_fold_csvs(folder: Path) -> pd.DataFrame:
    dfs = []
    for i in range(1, 6):
        p = folder / f"{i}.csv"
        try:
            df = pd.read_csv(p, encoding="utf-8")
        except UnicodeDecodeError:
            df = pd.read_csv(p, encoding="gbk")
        df["fold"] = i
        dfs.append(df)
    return pd.concat(dfs, ignore_index=True)


def fmt_pm(mean: float, sd: float, digits: int = 3) -> str:
    return f"{mean:.{digits}f} ± {sd:.{digits}f}"


def summarize_tuning():
    rsf = read_fold_csvs(RSF_DIR)
    rsflc = read_fold_csvs(RSFLC_DIR)

    rsf_g = (
        rsf.groupby(["combo", "ntree", "mtry", "nodesize", "nsplit"], as_index=False)
        .agg(
            mean_c=("fold5_cindex", "mean"),
            sd_c=("fold5_cindex", "std"),
            mean_auc=("fold5_auc", "mean"),
            sd_auc=("fold5_auc", "std"),
            mean_b=("fold5_brier_ipcw", "mean"),
            sd_b=("fold5_brier_ipcw", "std"),
        )
        .sort_values(["mean_c", "mean_auc"], ascending=[False, False])
    )
    lc_g = (
        rsflc.groupby(["combo", "ntree", "mtry", "nodesize", "t0"], as_index=False)
        .agg(
            mean_c=("fold5_cindex", "mean"),
            sd_c=("fold5_cindex", "std"),
            mean_auc=("fold5_auc", "mean"),
            sd_auc=("fold5_auc", "std"),
            mean_b=("fold5_brier_ipcw", "mean"),
            sd_b=("fold5_brier_ipcw", "std"),
        )
        .sort_values(["mean_c", "mean_auc"], ascending=[False, False])
    )
    return rsf_g.iloc[0], lc_g.iloc[0], len(rsf_g), len(lc_g)


def renumber_table_refs(doc: Document):
    """旧表5-3..5-7 → 5-4..5-8；须从大到小替换。"""
    mapping = [
        ("表5-7", "表5-8"),
        ("表5-6", "表5-7"),
        ("表5-5", "表5-6"),
        ("表5-4", "表5-5"),
        ("表5-3", "表5-4"),
    ]
    n = 0
    for p in doc.paragraphs:
        t = p.text
        if not any(a in t for a, _ in mapping):
            continue
        # 跳过即将写入的新表5-3题注（此时尚未插入）；当前文档里所有表5-3都是旧的
        nt = t
        for a, b in mapping:
            nt = nt.replace(a, b)
        if nt != t:
            replace_paragraph_text(p, nt)
            n += 1
    # also tables' first-row? captions are paragraphs. Check table cells for 表5-x unlikely.
    print(f"renumbered paragraphs: {n}")


def insert_tuning_section(doc: Document, rsf_best, lc_best, n_rsf, n_lc):
    # 先锚定正文中的原性能结果标题（勿用 TOC 插入后会漂移的 index）
    head = None
    for i, p in enumerate(doc.paragraphs):
        if i <= 200:
            continue
        t = p.text.strip()
        if t.startswith("5.3.2 训练集内五折") or t.startswith("5.3.3 训练集内五折"):
            head = p
            break
    if head is None:
        raise RuntimeError("未找到原 5.3.2/性能结果标题")

    # TOC
    for i, p in enumerate(doc.paragraphs):
        if i < 100 and p.text.strip() == "5.3.2 训练集内五折交叉验证结果":
            replace_paragraph_text(p, "5.3.2 超参数选择结果")
            clone_p_after(p._p, p, "5.3.3 训练集内五折交叉验证结果")
            print("updated TOC")
            break

    style_body = None
    for p in doc.paragraphs:
        if p.text.startswith("为在真实临床数据上比较"):
            style_body = p
            break
    if style_body is None:
        style_body = head

    replace_paragraph_text(head, "5.3.3 训练集内五折交叉验证结果")

    # 插入点：正文 5.3.3 标题的前一段
    # 重新定位 head（TOC 插入后 python-docx 段落列表已变，但 XML 节点仍在）
    head2 = None
    for p in doc.paragraphs:
        if p.text.strip() == "5.3.3 训练集内五折交叉验证结果" and p._p is head._p:
            head2 = p
            break
    if head2 is None:
        for p in doc.paragraphs:
            if p._p is head._p:
                head2 = p
                break
    head = head2 or head

    # previous sibling paragraph in body
    prev_el = head._p.getprevious()
    while prev_el is not None and prev_el.tag != qn("w:p"):
        prev_el = prev_el.getprevious()
    if prev_el is None:
        raise RuntimeError("无法定位 5.3.2 插入点")

    # wrap prev as needs addnext on element
    class _El:
        pass

    rsf_param = (
        f"ntree={int(rsf_best['ntree'])}, mtry={int(rsf_best['mtry'])}, "
        f"nodesize={int(rsf_best['nodesize'])}, nsplit={int(rsf_best['nsplit'])}"
    )
    lc_param = (
        f"ntree={int(lc_best['ntree'])}, mtry={int(lc_best['mtry'])}, "
        f"nodesize={int(lc_best['nodesize'])}, t0={int(lc_best['t0'])}"
    )

    intro1 = (
        "按照第5.3.1节的两阶段框架，机器学习模型（RSF、RSFLC）的超参数在训练集内部通过五折交叉验证选定，"
        "验证集（约30%）不参与选参。对每一组候选超参数，依次以第k折为验证折、其余四折为训练折拟合模型，"
        "计算验证折的C-index、28天AUC与Brier Score；再以五折验证C-index的平均值作为选参准则，"
        "取平均值最高的组合为最优超参数。COX与JM通常无需网格调参，故本节仅报告RSF与RSFLC的选择结果。"
    )
    intro2 = (
        f"RSF在11个基线协变量上搜索，网格为ntree∈{{300,500,1000}}、mtry∈{{3,6,9}}、"
        f"nodesize∈{{10,20,30,40}}、nsplit∈{{10,25,50}}，共{n_rsf}组。"
        f"RSFLC在20条纵向轨迹与11个固定协变量上搜索，网格为ntree∈{{50,100,200}}、mtry∈{{3,6,9,12}}、"
        f"nodesize∈{{1,3,5}}，预测时纵向信息截断时点固定为t0=5天，共{n_lc}组。"
        f"最优组合及其五折验证性能见表5-3。后续全训练集重拟合与独立验证集评价均采用该最优超参数。"
    )
    note = (
        "注：选参准则为五折验证集平均C-index最大；并列时依次参考平均AUC更高、平均Brier更低。"
        "表中性能为训练集内验证折的均值±标准差，用于超参数选择，不替代独立验证集终评。"
    )

    el = prev_el
    el = clone_p_after(el, head, "5.3.2 超参数选择结果")
    el = clone_p_after(el, style_body, intro1)
    el = clone_p_after(el, style_body, intro2)
    el = clone_p_after(el, head, "表5-3 训练集内五折交叉验证超参数选择结果（RSF与RSFLC）")

    rows_data = [
        [
            "RSF",
            "ntree∈{300,500,1000}; mtry∈{3,6,9}; nodesize∈{10,20,30,40}; nsplit∈{10,25,50}",
            rsf_param,
            fmt_pm(rsf_best["mean_c"], rsf_best["sd_c"]),
            fmt_pm(rsf_best["mean_auc"], rsf_best["sd_auc"]),
            fmt_pm(rsf_best["mean_b"], rsf_best["sd_b"]),
        ],
        [
            "RSFLC",
            "ntree∈{50,100,200}; mtry∈{3,6,9,12}; nodesize∈{1,3,5}; t0=5",
            lc_param,
            fmt_pm(lc_best["mean_c"], lc_best["sd_c"]),
            fmt_pm(lc_best["mean_auc"], lc_best["sd_auc"]),
            fmt_pm(lc_best["mean_b"], lc_best["sd_b"]),
        ],
    ]
    header = [
        "模型",
        "搜索空间",
        "最优超参数",
        "五折验证 C-index",
        "五折验证 AUC",
        "五折验证 Brier",
    ]
    table = doc.add_table(rows=1 + len(rows_data), cols=len(header))
    for j, h in enumerate(header):
        set_cell_text(table.rows[0].cells[j], h, bold=True, align="center")
    for i, row in enumerate(rows_data):
        for j, val in enumerate(row):
            set_cell_text(
                table.rows[i + 1].cells[j],
                val,
                bold=False,
                align="left" if j <= 2 else "center",
            )
    apply_ref_table_format(table)
    el.addnext(table._tbl)
    el = table._tbl
    note_el = clone_p_after(el, style_body, note)
    for r in note_el.iter(qn("w:r")):
        rPr = r.find(qn("w:rPr"))
        if rPr is None:
            rPr = OxmlElement("w:rPr")
            r.insert(0, rPr)
        sz = rPr.find(qn("w:sz"))
        if sz is None:
            sz = OxmlElement("w:sz")
            rPr.append(sz)
        sz.set(qn("w:val"), "18")
        szCs = rPr.find(qn("w:szCs"))
        if szCs is None:
            szCs = OxmlElement("w:szCs")
            rPr.append(szCs)
        szCs.set(qn("w:val"), "18")

    print("inserted 5.3.2 tuning section")
    print("RSF best:", rsf_param, fmt_pm(rsf_best["mean_c"], rsf_best["sd_c"]))
    print("RSFLC best:", lc_param, fmt_pm(lc_best["mean_c"], lc_best["sd_c"]))


def fix_framework_table_mentions(doc: Document):
    """框架段里若仍把表5-4说成调参表等，做定向修正。"""
    for p in doc.paragraphs:
        t = p.text
        if "表5-4汇报的是训练集内五折交叉验证的均值" in t or (
            "主要用于展示调参阶段的折间稳定性" in t and "表5-4" in t
        ):
            nt = t.replace(
                "需要说明的是：表5-4汇报的是训练集内五折交叉验证的均值±标准差，主要用于展示调参阶段的折间稳定性及模型间的初步排序；独立验证集上的终评性能、解释性分析与临床效用评价见第5.5节和第5.6节（如表5-7）。二者分工不同——前者侧重稳定性与调参过程，后者侧重外样本预测与解释，后文引用时按相应表注区分，不以交叉验证均值替代验证集终评数字。",
                "需要说明的是：表5-3给出RSF与RSFLC的超参数选择结果；表5-4汇报四种模型在训练集内五折交叉验证中的性能均值±标准差，用于展示折间稳定性及模型间的初步排序；"
                "独立验证集上的终评性能、解释性分析与临床效用评价见第5.5节和第5.6节（如表5-7）。"
                "表5-3/表5-4侧重训练阶段的选参与稳定性，表5-7侧重外样本预测与解释，后文引用时按相应表注区分，不以交叉验证均值替代验证集终评数字。",
            )
            # if exact long replace failed, do smaller patches
            if nt == t:
                nt = t.replace(
                    "表5-4汇报的是训练集内五折交叉验证的均值±标准差，主要用于展示调参阶段的折间稳定性及模型间的初步排序",
                    "表5-3给出超参数选择结果；表5-4汇报四种模型训练集内五折交叉验证的均值±标准差，用于展示折间稳定性及模型间的初步排序",
                )
            if nt != t:
                replace_paragraph_text(p, nt)
                print("updated framework table mention")
        if "表5-4为训练集内五折交叉验证的稳定性结果，与表5-7分工不同" in t:
            nt = t.replace(
                "表5-4为训练集内五折交叉验证的稳定性结果，与表5-7分工不同",
                "表5-3为超参数选择结果，表5-4为训练集内五折稳定性结果，与表5-7终评分工不同",
            )
            replace_paragraph_text(p, nt)
            print("updated 5.5 table mention")
        if "表5-4仅反映训练集内交叉验证的稳定性" in t:
            nt = t.replace(
                "表5-4仅反映训练集内交叉验证的稳定性，不作为本节绝对概率与净获益的引用依据。",
                "表5-3/表5-4仅反映训练阶段选参与交叉验证稳定性，不作为本节绝对概率与净获益的引用依据。",
            )
            replace_paragraph_text(p, nt)
            print("updated 5.6 lead-in")


def main():
    if not SRC.exists():
        raise FileNotFoundError(SRC)
    rsf_best, lc_best, n_rsf, n_lc = summarize_tuning()
    shutil.copy2(SRC, OUT)
    doc = Document(str(OUT))

    # 1) renumber old tables first (5-3..5-7 → 5-4..5-8)
    renumber_table_refs(doc)

    # 2) insert new 5.3.2 + new 表5-3
    insert_tuning_section(doc, rsf_best, lc_best, n_rsf, n_lc)

    # 3) fix narrative mentions
    fix_framework_table_mentions(doc)

    # 4) ensure body 5.3.3 title exists (insert_tuning_section already set)
    # ensure no duplicate old 5.3.2 performance title left
    doc.save(str(OUT))
    print("saved:", OUT)

    doc2 = Document(str(OUT))
    print("==== verify headings / tables ====")
    for i, p in enumerate(doc2.paragraphs):
        t = p.text.strip()
        if i < 80 and t.startswith("5.3"):
            print("TOC/early", i, t)
        if i >= 255 and (
            t.startswith("5.3")
            or t.startswith("表5-3")
            or t.startswith("表5-4")
            or t.startswith("表5-7")
            or t.startswith("表5-8")
            or t.startswith("按照第5.3.1")
        ):
            print(i, t[:140])


if __name__ == "__main__":
    main()
