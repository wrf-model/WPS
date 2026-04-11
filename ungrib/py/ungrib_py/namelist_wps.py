# Minimal namelist.wps reader for ungrib.

from __future__ import annotations

import re
from pathlib import Path
from typing import NamedTuple

class UngribNamelist(NamedTuple):
    hstart: str
    hend: str
    interval_seconds: int
    prefix: str
    out_format: str
    ordered_by_date: bool
    debug_level: int
    pmin: float

def _first_int(text: str, name: str, default: int) -> int:

    # This function reads the first integer assignment for name from a namelist section string.

    m = re.search(rf"{name}\s*=\s*(-?\d+)", text, re.I)
    return int(m.group(1)) if m else default

def _first_float(text: str, name: str, default: float) -> float:

    # This function reads the first float assignment for name from a namelist section string.

    m = re.search(rf"{name}\s*=\s*(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)", text, re.I)
    return float(m.group(1)) if m else default

def _first_bool(text: str, name: str, default: bool) -> bool:

    # This function reads the first Fortran logical assignment for name from a section string.

    m = re.search(rf"{name}\s*=\s*\.(true|false)\.", text, re.I)
    if not m:
        return default
    return m.group(1).lower() == "true"

def _quoted_str(text: str, name: str, default: str) -> str:

    # This function reads a single- or double-quoted string assignment for name.

    m = re.search(rf"{name}\s*=\s*'([^']*)'", text, re.I)
    if m:
        return m.group(1)
    m2 = re.search(rf'{name}\s*=\s*"([^"]*)"', text, re.I)
    return m2.group(1) if m2 else default

def _extract_section(full: str, section: str) -> str:

    # This function returns the body text of a Fortran namelist section without delimiters.

    m = re.search(rf"&{section}\s(.*?)^\s*/\s*$", full, re.I | re.S | re.M)
    return m.group(1) if m else ""

def _build_hdate_from_components(
    year: int, month: int, day: int, hour: int, minute: int, second: int
) -> str:

    # This function formats WPS-style hdate components into a 19-character timestamp string.

    return f"{year:04d}-{month:02d}-{day:02d}_{hour:02d}:{minute:02d}:{second:02d}"

def parse_namelist_wps(path: str | Path) -> UngribNamelist:

    # This function parses namelist.wps share and ungrib sections needed by ungrib.

    full = Path(path).read_text(encoding="utf-8", errors="replace")
    share = _extract_section(full, "share")
    ungrib = _extract_section(full, "ungrib")

    start_date = _quoted_str(share, "start_date", "")
    end_date = _quoted_str(share, "end_date", "")

    if not start_date.strip() or start_date.strip().startswith("0000"):
        sy = _first_int(share, "start_year", 0)
        sm = _first_int(share, "start_month", 0)
        sd = _first_int(share, "start_day", 0)
        sh = _first_int(share, "start_hour", 0)
        smin = _first_int(share, "start_minute", 0)
        ss = _first_int(share, "start_second", 0)
        hstart = _build_hdate_from_components(sy, sm, sd, sh, smin, ss)
    else:
        hstart = start_date.strip()[:19].ljust(19, "0")[:19]

    if not end_date.strip() or end_date.strip().startswith("0000"):
        ey = _first_int(share, "end_year", 0)
        em = _first_int(share, "end_month", 0)
        ed = _first_int(share, "end_day", 0)
        eh = _first_int(share, "end_hour", 0)
        emin = _first_int(share, "end_minute", 0)
        es = _first_int(share, "end_second", 0)
        hend = _build_hdate_from_components(ey, em, ed, eh, emin, es)
    else:
        hend = end_date.strip()[:19].ljust(19, "0")[:19]

    interval_seconds = _first_int(share, "interval_seconds", 0)
    debug_level = _first_int(share, "debug_level", 0)
    prefix = _quoted_str(ungrib, "prefix", "FILE")
    out_format = _quoted_str(ungrib, "out_format", "WPS").strip().upper()[:3]
    ordered_by_date = _first_bool(ungrib, "ordered_by_date", True)
    pmin = _first_float(ungrib, "pmin", 100.0)

    if interval_seconds <= 0:
        raise ValueError("namelist.wps: interval_seconds must be > 0")

    return UngribNamelist(
        hstart=hstart,
        hend=hend,
        interval_seconds=interval_seconds,
        prefix=prefix,
        out_format=out_format,
        ordered_by_date=ordered_by_date,
        debug_level=debug_level,
        pmin=pmin,
    )










