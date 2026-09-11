from pathlib import Path

import pytest

from ungrib_py.vtable import VtableEntry, parse_vtable, match_entry

VTABLE_GFS = Path(__file__).resolve().parents[2] / "Variable_Tables" / "Vtable.GFS"

@pytest.mark.skipif(not VTABLE_GFS.is_file(), reason="Vtable.GFS not in tree")
def test_parse_vtable_gfs():

    # This function verifies parse_vtable reads the repository Vtable.GFS when present.

    rows, out_rows = parse_vtable(VTABLE_GFS)
    assert len(rows) >= 10
    assert len(out_rows) >= 10
    first = rows[0]
    assert isinstance(first, VtableEntry)
    assert first.name.strip() == "TT"

def test_match_gfs_temperature_100hPa():

    # This function verifies Vtable match_entry for GFS-like temperature on isobaric surfaces.

    row = VtableEntry(
        g1_param=11,
        g1_level_type=100,
        level1=-88.0,
        level2=-99.0,
        name="TT       ",
        units="K       ",
        desc="Temperature",
        g2_discipline=0,
        g2_category=0,
        g2_parameter=0,
        g2_level_type=100,
        g2_pdt=0,
    )
    m = match_entry([row], 0, 0, 0, 100, 0)
    assert m is row
    assert match_entry([row], 0, 0, 1, 100, 0) is None










