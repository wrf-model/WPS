# Parse WPS Vtable files (GRIB2-aware lines).

from __future__ import annotations

from pathlib import Path
from typing import NamedTuple

BLANK = -99
SPLAT = -88

class VtableEntry(NamedTuple):

    # One non-comment data row after the header dashes.

    g1_param: int
    g1_level_type: int
    level1: float
    level2: float
    name: str
    units: str
    desc: str
    g2_discipline: int
    g2_category: int
    g2_parameter: int
    g2_level_type: int
    g2_pdt: int

def _split_pipe_line(line: str) -> list[str]:

    # This function splits a Vtable text line on pipes and strips each cell.

    parts = line.split("|")
    return [p.strip() for p in parts]

def _parse_int(tok: str, *, allow_blank: bool = False) -> int:

    # This function parses an integer Vtable token, optionally treating blanks as BLANK.

    t = tok.strip()
    if not t or t == " ":
        if allow_blank:
            return BLANK
        raise ValueError(f"expected integer, got empty in {tok!r}")
    if "*" in t:
        raise ValueError(f"unexpected wildcard in integer field: {tok!r}")
    return int(t)

def _parse_float_level(tok: str) -> float:

    # This function parses a Vtable level field as a float, blank, or SPLAT wildcard.

    t = tok.strip()
    if not t:
        return float(BLANK)
    if "*" in t:
        return float(SPLAT)
    return float(t)

def parse_vtable(path: str | Path) -> tuple[list[VtableEntry], list[VtableEntry]]:

    # This function parses a WPS Vtable file into all rows and output-eligible rows.

    text = Path(path).read_text(encoding="utf-8", errors="replace").splitlines()
    i = 0
    while i < len(text) and not text[i].lstrip().startswith("-"):
        i += 1
    if i >= len(text):
        raise ValueError("Vtable: no header line starting with '-'")
    i += 1
    rows: list[VtableEntry] = []
    while i < len(text):
        raw = text[i].rstrip("\n")
        i += 1
        if not raw.strip() or raw.lstrip().startswith("#"):
            continue
        if raw.lstrip().startswith("-"):
            while i < len(text):
                nxt = text[i].rstrip("\n")
                i += 1
                if not nxt.strip() or nxt.lstrip().startswith("#"):
                    continue
                if nxt.lstrip().startswith("-"):
                    break
            continue
        parts = _split_pipe_line(raw)
        ncols = len(parts)
        if ncols < 11:
            raise ValueError(f"Vtable line needs >= 11 |-columns, got {ncols}: {raw!r}")
        g1_param = _parse_int(parts[0], allow_blank=True)
        g1_level_type = _parse_int(parts[1], allow_blank=True)
        level1 = _parse_float_level(parts[2])
        level2 = _parse_float_level(parts[3])
        name = parts[4].strip()
        if not name:
            raise ValueError(f"missing field name: {raw!r}")
        units = parts[5].strip() if len(parts) > 5 else ""
        desc = parts[6].strip() if len(parts) > 6 else ""
        g2_discipline = _parse_int(parts[7], allow_blank=True)
        g2_category = _parse_int(parts[8], allow_blank=True)
        g2_parameter = _parse_int(parts[9], allow_blank=True)
        g2_level_type = _parse_int(parts[10], allow_blank=True)
        g2_pdt = 0
        if ncols >= 12 and parts[11].strip():
            g2_pdt = _parse_int(parts[11], allow_blank=True)
            if g2_pdt == BLANK:
                g2_pdt = 0
        row = VtableEntry(
            g1_param=g1_param,
            g1_level_type=g1_level_type,
            level1=level1,
            level2=level2,
            name=name[:9].ljust(9)[:9],
            units=units[:25].ljust(25)[:25],
            desc=desc[:46].ljust(46)[:46],
            g2_discipline=g2_discipline,
            g2_category=g2_category,
            g2_parameter=g2_parameter,
            g2_level_type=g2_level_type,
            g2_pdt=g2_pdt,
        )
        rows.append(row)
    output_rows = [r for r in rows if r.desc.strip()]
    return rows, output_rows

def match_entry(
    rows: list[VtableEntry],
    discipline: int,
    cat: int,
    param: int,
    first_surface: int,
    pdt: int,
) -> VtableEntry | None:

    # This function returns the first Vtable row matching GRIB2 keys or None if none match.

    for r in rows:
        if r.g2_discipline == BLANK or r.g2_category == BLANK or r.g2_parameter == BLANK:
            continue
        if (
            discipline == r.g2_discipline
            and cat == r.g2_category
            and param == r.g2_parameter
            and first_surface == r.g2_level_type
            and pdt == r.g2_pdt
        ):
            return r
    return None





