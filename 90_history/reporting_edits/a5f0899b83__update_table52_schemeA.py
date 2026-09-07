# -*- coding: utf-8 -*-
"""方案A：精简表5-2——去掉重复观测列，改以表注说明。"""
from __future__ import annotations

import copy
import shutil
from pathlib import Path

from docx import Document
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Pt

ROOT = Path(r"F:\文章_大论文")
SRC_DIR = ROOT / r"0722\大论文版本"
DOCX_IN = SRC_DIR / "0827_2_基于机器学习的生存分析方法探讨_第五章表5-1拆分连续分类.docx"
DOCX_OUT = SRC_DIR / "0827_2_基于机器学习的生存分析方法探讨_第五章表5-2方案A精简.docx"

HEADER_FILL = "D9E2F3"
FONT_ASCII = "Times New Roman"
FONT_EA = "宋体"
FONT_SIZE_PT = 10.5
NOTE_SIZE_PT = 9.0


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


def apply_ref_table_format(table, section_names=None):
    section_names = set(section_names or [])
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

    mar = tblPr.find(qn("w:tblCellMar"))
    if mar is None:
        mar = OxmlElement("w:tblCellMar")
        tblPr.append(mar)
    for side, w in [("top", "0"), ("left", "108"), ("bottom", "0"), ("right", "108")]:
        el = mar.find(qn(f"w:{side}"))
        if el is None:
            el = OxmlElement(f"w:{side}")
            mar.append(el)
        el.set(qn("w:w"), w)
        el.set(qn("w:type"), "dxa")

    n_cols = len(table.columns)
    for ri, row in enumerate(table.rows):
        is_header = ri == 0
        for ci in range(n_cols):
            cell = row.cells[ci]
            text = cell.text.strip()
            if is_header:
                align, bold = "center", True
                set_cell_shading(cell, HEADER_FILL)
            else:
                others = [row.cells[j].text.strip() for j in range(1, n_cols)]
                is_section = text in section_names or (
                    text != "" and all(o in ("", "—", "-") for o in others)
                )
                align = "left" if ci == 0 else "center"
                bold = bool(is_section and ci == 0)
                set_cell_shading(cell, None)
            set_cell_text(cell, text, bold=bold, align=align)


def replace_paragraph_text(paragraph, new_text: str, size_pt: float | None = None):
    if paragraph.runs:
        paragraph.runs[0].text = new_text
        if size_pt is not None:
            set_run_font(paragraph.runs[0], size_pt=size_pt)
        for r in paragraph.runs[1:]:
            r.text = ""
    else:
        run = paragraph.add_run(new_text)
        if size_pt is not None:
            set_run_font(run, size_pt=size_pt)


def rebuild_table_as_4col(old_table, header, body_rows):
    """用新的4列表替换旧表（删旧插新，保持位置）。"""
    parent = old_table._tbl.getparent()
    idx = list(parent).index(old_table._tbl)

    # 新建表写到文档末尾，再移动到原位置
    # 先收集数据，删旧表，再插入
    old_el = old_table._tbl
    doc = old_table._tbl.getparent()  # body

    # Create via temporary Document helper: use deep copy of structure is hard;
    # instead clear and shrink columns by rewriting XML rows.

    # Simpler approach: delete all rows, change grid, rebuild.
    tbl = old_table._tbl
    # remove all tr
    for tr in list(tbl.findall(qn("w:tr"))):
        tbl.remove(tr)

    # update tblGrid if present
    grid = tbl.find(qn("w:tblGrid"))
    if grid is not None:
        for g in list(grid.findall(qn("w:gridCol"))):
            grid.remove(g)
        for _ in range(4):
            gc = OxmlElement("w:gridCol")
            gc.set(qn("w:w"), "2000")
            grid.append(gc)

    # add rows via python-docx Table API after rebinding
    # Since we wiped rows, use low-level add
    def add_row_cells(n):
        tr = OxmlElement("w:tr")
        for _ in range(n):
            tc = OxmlElement("w:tc")
            tcPr = OxmlElement("w:tcPr")
            tcW = OxmlElement("w:tcW")
            tcW.set(qn("w:w"), "2000")
            tcW.set(qn("w:type"), "dxa")
            tcPr.append(tcW)
            tc.append(tcPr)
            p = OxmlElement("w:p")
            tc.append(p)
            tr.append(tc)
        tbl.append(tr)
        return tr

    add_row_cells(4)  # header
    for _ in body_rows:
        add_row_cells(4)

    # re-wrap as Table for writing text
    from docx.table import Table

    table = Table(tbl, old_table._parent)
    for j, h in enumerate(header):
        set_cell_text(table.rows[0].cells[j], h, bold=True, align="center")
    for i, vals in enumerate(body_rows):
        for j, val in enumerate(vals):
            set_cell_text(
                table.rows[i + 1].cells[j],
                val,
                bold=False,
                align="left" if j == 0 else "center",
            )
    return table


def insert_note_after_table(style_src_p, table_el, text: str):
    new_p = copy.deepcopy(style_src_p._p)
    # clear text nodes
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
    table_el.addnext(new_p)

    # shrink font on runs of the inserted paragraph
    for r in new_p.iter(qn("w:r")):
        rPr = r.find(qn("w:rPr"))
        if rPr is None:
            rPr = OxmlElement("w:rPr")
            r.insert(0, rPr)
        sz = rPr.find(qn("w:sz"))
        if sz is None:
            sz = OxmlElement("w:sz")
            rPr.append(sz)
        sz.set(qn("w:val"), str(int(NOTE_SIZE_PT * 2)))
        szCs = rPr.find(qn("w:szCs"))
        if szCs is None:
            szCs = OxmlElement("w:szCs")
            rPr.append(szCs)
        szCs.set(qn("w:val"), str(int(NOTE_SIZE_PT * 2)))
        rFonts = rPr.find(qn("w:rFonts"))
        if rFonts is None:
            rFonts = OxmlElement("w:rFonts")
            rPr.insert(0, rFonts)
        rFonts.set(qn("w:ascii"), FONT_ASCII)
        rFonts.set(qn("w:hAnsi"), FONT_ASCII)
        rFonts.set(qn("w:eastAsia"), FONT_EA)
    return new_p


def find_table_52(doc: Document):
    for ti, tbl in enumerate(doc.tables):
        head = "|".join(c.text.strip() for c in tbl.rows[0].cells)
        if "第1天" in head and ("住院均值" in head or "最差" in head):
            return ti, tbl
    raise RuntimeError("未找到表5-2")


def extract_scheme_a_rows(old_table):
    """从旧6列表提取变量行（跳过概览行）。"""
    rows_out = []
    skip_names = {
        "纵向随访概览（每住院观测天数）",
        "患者-日记录总数",
        "变量",
    }
    for r in old_table.rows[1:]:
        vals = [c.text.strip() for c in r.cells]
        name = vals[0]
        if name in skip_names or name == "":
            continue
        # old: 变量|有观测|人均天数|第1天|均值|最差
        if len(vals) >= 6:
            rows_out.append([name, vals[3], vals[4], vals[5]])
        else:
            raise RuntimeError(f"表5-2列数异常: {vals}")
    return rows_out


def main():
    if not DOCX_IN.exists():
        raise FileNotFoundError(DOCX_IN)
    shutil.copy2(DOCX_IN, DOCX_OUT)
    doc = Document(str(DOCX_OUT))

    _, old_tbl = find_table_52(doc)
    body_rows = extract_scheme_a_rows(old_tbl)
    print(f"变量行数: {len(body_rows)}")

    header = ["变量", "第1天（均值±SD）", "住院均值（均值±SD）", "最差值（均值±SD）"]
    table = rebuild_table_as_4col(old_tbl, header, body_rows)
    apply_ref_table_format(table)

    # 缩短题注
    cap = None
    for p in doc.paragraphs:
        if p.text.strip().startswith("表5-2"):
            cap = p
            break
    if cap is None:
        raise RuntimeError("未找到表5-2题注")
    replace_paragraph_text(cap, "表5-2 研究队列纵向数据基本特征")

    # 表注插在表后（若已有旧注则替换）
    note_text = (
        "注：先按住院汇总第1天值、住院期均值与最差值，再在队列水平报告均值±标准差。"
        "除血氧饱和度外，各纵向变量有观测住院数均为6,299，人均观测天数约为12.5±11.7天；"
        "患者-日记录共78,649条。最差值对GCS、pH、PaO₂/FiO₂、血红蛋白、尿量、血压、血氧等取最小，"
        "对其余指标取最大。"
    )
    # 若表后已是以“注：”开头的段，则改写；否则新建
    next_el = table._tbl.getnext()
    inserted = False
    if next_el is not None and next_el.tag == qn("w:p"):
        texts = "".join((t.text or "") for t in next_el.iter(qn("w:t"))).strip()
        if texts.startswith("注：") or texts.startswith("注:"):
            # rewrite
            t_nodes = list(next_el.iter(qn("w:t")))
            if t_nodes:
                t_nodes[0].text = note_text
                for t in t_nodes[1:]:
                    t.text = ""
            inserted = True
            print("已改写既有表注")
    if not inserted:
        insert_note_after_table(cap, table._tbl, note_text)
        print("已插入表注")

    # 正文说明可略作收紧：表5-2句保持，但去掉题注里已迁移的冗长说明即可（248已较完整）
    for p in doc.paragraphs:
        t = p.text
        if "表5-2按方案先在住院水平" in t:
            # 保持原意，略强调表内三列、观测信息见表注
            new = t.replace(
                "表5-2按方案先在住院水平计算第1天值、住院期均值与最差值，再在队列水平报告均值±标准差，"
                "以避免重复测量导致的样本量膨胀。",
                "表5-2按方案先在住院水平计算第1天值、住院期均值与最差值，再在队列水平报告均值±标准差"
                "（有观测住院数与人均观测天数见表注），以避免重复测量导致的样本量膨胀。",
            )
            if new != t:
                replace_paragraph_text(p, new)
                print("已更新正文表5-2引用")
            break

    doc.save(str(DOCX_OUT))
    print("saved:", DOCX_OUT)

    # verify
    doc2 = Document(str(DOCX_OUT))
    for p in doc2.paragraphs:
        if p.text.strip().startswith("表5-2") or p.text.strip().startswith("注："):
            print("P:", p.text[:180])
    for tbl in doc2.tables:
        head = "|".join(c.text.strip() for c in tbl.rows[0].cells)
        if "第1天" in head:
            print("HEADER:", head, "rows=", len(tbl.rows))
            for r in tbl.rows[:5]:
                print(" | ".join(c.text.strip() for c in r.cells))
            print("...")
            for r in tbl.rows[-2:]:
                print(" | ".join(c.text.strip() for c in r.cells))
            break


if __name__ == "__main__":
    main()
