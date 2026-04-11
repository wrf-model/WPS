from pathlib import Path

import eccodes
import pytest

from ungrib_py.grib2 import compute_wps_level, extract_file
from ungrib_py.vtable import parse_vtable

_REPO = Path(__file__).resolve().parents[3]
_IFS_GRIB = _REPO / "ungrib_out" / "data" / "ifs.2026041106.f000.grib2"
_IFS_VT = _REPO / "ungrib_out" / "Vtable"


def test_compute_wps_level_soil_type151_200100():

    # This function checks GRIB2 fixed-surface 151 (ECMWF IFS soil layer) maps to WPS surface code 200100.

    if not _IFS_GRIB.is_file():
        pytest.skip("ungrib_out sample IFS GRIB not present")
    with _IFS_GRIB.open("rb") as fh:
        while True:
            gid = eccodes.codes_grib_new_from_file(fh)
            if gid is None:
                break
            try:
                if eccodes.codes_get(gid, "shortName") != "sot":
                    continue
                assert compute_wps_level(gid, 50000.0) == 200100.0
                return
            finally:
                eccodes.codes_release(gid)
    pytest.fail("expected at least one sot field in sample GRIB")


@pytest.mark.skipif(not _IFS_GRIB.is_file() or not _IFS_VT.is_file(), reason="IFS sample or Vtable missing")
def test_extract_ifs_soil_st_sm_four_layers():

    # This function checks ECMWF IFS layer-151 soil temperature and moisture decode to four ST/SM fields each.

    _, vt_out = parse_vtable(_IFS_VT)
    out, _ = extract_file(str(_IFS_GRIB), vt_out, pmin=50000.0)
    assert out
    bucket = next(iter(out.values()))
    st = sorted({k[1] for k in bucket if k[1].startswith("ST")})
    sm = sorted({k[1] for k in bucket if k[1].startswith("SM")})
    assert st == ["ST000007", "ST007028", "ST028100", "ST100289"]
    assert sm == ["SM000007", "SM007028", "SM028100", "SM100289"]

