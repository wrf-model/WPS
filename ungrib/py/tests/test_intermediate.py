import struct

import numpy as np

from ungrib_py.intermediate import FieldSlab, MapInfo, sort_fields_fortran_order, write_wps_intermediate_v5
from ungrib_py.vtable import VtableEntry

def _read_fortran_record_be_int32(fh) -> int:

    # This function reads one sequential record (big-endian length tags) payload as big-endian int32.

    n = int.from_bytes(fh.read(4), byteorder="big")
    payload = fh.read(n)
    fh.read(4)
    return struct.unpack(">i", payload)[0]

def test_write_then_read_version_and_dims(tmp_path):

    # This function checks WPS v5 intermediate writes a leading version record of 5 (big-endian payload).

    path = tmp_path / "out.bin"
    m = MapInfo(
        source="test".ljust(32),
        igrid=0,
        nx=2,
        ny=3,
        startloc="SWCORNER",
        lat1=20.0,
        lon1=-120.0,
        dx=1.0,
        dy=-1.0,
        lov=0.0,
        truelat1=0.0,
        truelat2=0.0,
        r_earth_km=6371.0,
        grid_wind=True,
        centerlat=0.0,
        centerlon=0.0,
    )
    slab = np.arange(6, dtype=np.float32).reshape(2, 3, order="F")
    fld = FieldSlab(
        name="TT",
        units="K",
        desc="Temperature",
        level=50000.0,
        data=slab,
    )
    write_wps_intermediate_v5(str(path), "2020-01-01_00:00:00", m, [fld])
    with open(path, "rb") as f:
        assert _read_fortran_record_be_int32(f) == 5


def test_sort_matches_fortran_level_then_vtable_order():

    # This function checks output order: descending level, then first Vtable row index per name.

    rows = [
        VtableEntry(0, 0, 0.0, 0.0, "AA       ", "u".ljust(25), "d".ljust(46), 0, 0, 0, 100, 0),
        VtableEntry(0, 0, 0.0, 0.0, "BB       ", "u".ljust(25), "d".ljust(46), 0, 0, 1, 100, 0),
    ]
    sa = np.float32(0)
    sb = np.float32(0)
    items = [
        (100.0, "BB", FieldSlab("BB", "u", "d", 100.0, np.array([[sb]]))),
        (500.0, "AA", FieldSlab("AA", "u", "d", 500.0, np.array([[sa]]))),
        (500.0, "BB", FieldSlab("BB", "u", "d", 500.0, np.array([[sb]]))),
        (100.0, "AA", FieldSlab("AA", "u", "d", 100.0, np.array([[sa]]))),
    ]
    got = [f.name.strip() for f in sort_fields_fortran_order(items, rows)]
    assert got == ["AA", "BB", "AA", "BB"]
