from pathlib import Path

from ungrib_py.namelist_wps import parse_namelist_wps

def test_parse_minimal_namelist(tmp_path: Path):

    # This function verifies parse_namelist_wps on a minimal share and ungrib namelist.

    p = tmp_path / "namelist.wps"
    p.write_text(
        """
&share
 start_date = '2020-01-01_00:00:00',
 end_date   = '2020-01-01_06:00:00',
 interval_seconds = 21600,
 debug_level = 0,
/
&ungrib
 out_format = 'WPS',
 prefix = 'FILE',
 ordered_by_date = .true.,
 pmin = 100.,
/
""",
        encoding="ascii",
    )
    n = parse_namelist_wps(p)
    assert n.hstart.startswith("2020-01-01")
    assert n.interval_seconds == 21600
    assert n.out_format == "WPS"
    assert n.pmin == 100.0










