# WPS intermediate format version 5 writer (Fortran unformatted sequential).
#
# On-disk layout matches WPS `ungrib/src/output.F` (WPS format) and `metgrid/src/read_met_module.F`
# for `fg_data % version == 5`: metgrid reads each field as five sequential unformatted records.
#
# Endianness: payloads use big-endian float32 / int32 (Fortran `-fconvert=big-endian`). Record
# delimiters are big-endian uint32 byte counts (`-frecord-marker=4`), consistent with typical
# WPS `configure` / `arch/configure.defaults` gfortran builds.
#
# Per field, records are:
#   1) int32 version code 5 (4-byte payload).
#   2) Mixed header, 156 bytes: character*24 hdate, real xfcst, character*32 map_source,
#      character*9 field, character*25 units, character*46 desc, real xlvl, int32 nx, ny, iproj.
#      Here `iproj` is the ungrib `map%igrid` code (0 lat-lon, 1 Mercator, 3 Lambert, 4 Gaussian,
#      5 polar stereographic, 6 Cassini).
#   3) Map extension: character*8 startloc then a projection-dependent list of real32 values
#      (see `_map_extension_bytes_be` and `_MAP_EXT_PAYLOAD_LEN`).
#   4) Logical `is_wind_grid_rel`: written as a 4-byte big-endian integer 0 or 1 (gfortran
#      commonly uses 4-byte LOGICAL in unformatted sequential I/O; metgrid accepts this pattern).
#   5) real32 array slab(nx, ny) in Fortran array element order, big-endian IEEE float32.

from __future__ import annotations

import struct
from typing import BinaryIO, NamedTuple, Sequence

import numpy as np

from ungrib_py.vtable import VtableEntry

WPS_V5_VERSION: int = 5

WPS_V5_MIXED_HEADER_BYTES: int = 156

_MAP_EXT_PAYLOAD_LEN: dict[int, int] = {
    0: 28,
    1: 32,
    3: 40,
    4: 28,
    5: 36,
    6: 36,
}

def _pad_str(s: str, n: int) -> bytes:

    # This function encodes a string to ASCII bytes padded or truncated to length n.

    b = s.encode("ascii", errors="replace")[:n]
    return b + b" " * (n - len(b))

def _fortran_unformatted_record(fh: BinaryIO, payload: bytes) -> None:

    # This function writes one gfortran sequential unformatted record. With -fconvert=big-endian,
    # gfortran also uses big-endian 32-bit record length tags (see WPS arch/configure.defaults).

    n = len(payload)
    len_be = struct.pack(">I", n)
    fh.write(len_be)
    fh.write(payload)
    fh.write(len_be)

def _be_i32(v: int) -> bytes:

    # This function encodes a 32-bit signed integer in big-endian form for WPS I/O (-fconvert=big-endian).

    return struct.pack(">i", v)

def _be_f32(v: float) -> bytes:

    # This function encodes a 32-bit IEEE float in big-endian form for WPS I/O (-fconvert=big-endian).

    return struct.pack(">f", v)

class MapInfo(NamedTuple):

    # Projection metadata for WPS intermediate version 5.

    source: str
    igrid: int
    nx: int
    ny: int
    startloc: str
    lat1: float
    lon1: float
    dx: float
    dy: float
    lov: float
    truelat1: float
    truelat2: float
    r_earth_km: float
    grid_wind: bool
    centerlat: float
    centerlon: float

class FieldSlab(NamedTuple):
    name: str
    units: str
    desc: str
    level: float
    data: np.ndarray

def write_wps_intermediate_v5(
    path: str,
    hdate19: str,
    map_info: MapInfo,
    fields: Sequence[FieldSlab],
    *,
    xfcst: float = 0.0,
) -> None:

    # This function writes WPS intermediate format version 5 for one timestep to path.

    h24 = _pad_str(hdate19[:19], 24).decode("ascii")
    src32 = _pad_str(map_info.source[:32], 32).decode("ascii")

    with open(path, "wb") as fh:
        for fld in fields:
            name9 = _pad_str(fld.name.strip()[:9], 9).decode("ascii")
            units25 = _pad_str(fld.units.strip()[:25], 25).decode("ascii")
            desc46 = _pad_str(fld.desc.strip()[:46], 46).decode("ascii")
            slab = np.asfortranarray(
                np.asarray(fld.data, dtype=np.float32).reshape(map_info.nx, map_info.ny)
            )

            ver_b = _be_i32(WPS_V5_VERSION)
            if len(ver_b) != 4:
                raise AssertionError("WPS v5 version record must be 4 bytes")

            hdr_b = _mixed_header_bytes_be(
                h24,
                float(xfcst),
                src32,
                name9,
                units25,
                desc46,
                float(fld.level),
                int(map_info.nx),
                int(map_info.ny),
                int(map_info.igrid),
            )
            if len(hdr_b) != WPS_V5_MIXED_HEADER_BYTES:
                raise AssertionError(
                    f"WPS v5 mixed header must be {WPS_V5_MIXED_HEADER_BYTES} bytes, got {len(hdr_b)}"
                )

            ext_b = _map_extension_bytes_be(map_info)
            exp_len = _MAP_EXT_PAYLOAD_LEN.get(map_info.igrid)
            if exp_len is None:
                raise ValueError(f"unsupported igrid={map_info.igrid} for WPS v5 writer")
            if len(ext_b) != exp_len:
                raise AssertionError(
                    f"map extension for igrid={map_info.igrid} must be {exp_len} bytes, got {len(ext_b)}"
                )

            wind_b = _be_i32(1 if map_info.grid_wind else 0)
            if len(wind_b) != 4:
                raise AssertionError("grid_wind record must be 4 bytes")

            slab_b = _slab_bytes_be(slab)
            if len(slab_b) != map_info.nx * map_info.ny * 4:
                raise AssertionError("slab payload size must be nx*ny*4 bytes")

            _fortran_unformatted_record(fh, ver_b)
            _fortran_unformatted_record(fh, hdr_b)
            _fortran_unformatted_record(fh, ext_b)
            _fortran_unformatted_record(fh, wind_b)
            _fortran_unformatted_record(fh, slab_b)

def _mixed_header_bytes_be(
    h24: str,
    xfcst: float,
    src32: str,
    name9: str,
    units25: str,
    desc46: str,
    level: float,
    nx: int,
    ny: int,
    iproj: int,
) -> bytes:

    # This function builds the mixed-type header record with big-endian reals and integers.

    buf = bytearray()
    buf.extend(_pad_str(h24, 24))
    buf.extend(_be_f32(xfcst))
    buf.extend(_pad_str(src32, 32))
    buf.extend(_pad_str(name9, 9))
    buf.extend(_pad_str(units25, 25))
    buf.extend(_pad_str(desc46, 46))
    buf.extend(_be_f32(level))
    buf.extend(_be_i32(nx))
    buf.extend(_be_i32(ny))
    buf.extend(_be_i32(iproj))
    return bytes(buf)

def _map_extension_bytes_be(m: MapInfo) -> bytes:

    # This function builds the map projection extension record with big-endian floats.

    start = _pad_str(m.startloc[:8], 8).decode("ascii")
    if m.igrid == 0:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dy, m.dx, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    if m.igrid == 1:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dx, m.dy, m.truelat1, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    if m.igrid == 3:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dx, m.dy, m.lov, m.truelat1, m.truelat2, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    if m.igrid == 5:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dx, m.dy, m.lov, m.truelat1, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    if m.igrid == 4:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dx, m.dy, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    if m.igrid == 6:
        buf = bytearray()
        buf.extend(_pad_str(start, 8))
        for v in (m.lat1, m.lon1, m.dx, m.dy, m.centerlat, m.centerlon, m.r_earth_km):
            buf.extend(_be_f32(v))
        return bytes(buf)
    raise ValueError(f"unsupported igrid={m.igrid} for WPS v5 writer")

def _slab_bytes_be(slab: np.ndarray) -> bytes:

    # This function serializes a Fortran-order (nx, ny) float32 slab as big-endian floats.

    be = np.asfortranarray(slab).astype(np.dtype(">f4"), order="K", copy=True)
    return be.tobytes("F")

def intermediate_filename_datelen(interval_seconds: int) -> int:

    # This function returns output filename date substring length from interval_seconds.

    if interval_seconds % 3600 == 0:
        return 13
    if interval_seconds % 60 == 0:
        return 16
    return 19

def hdate_slice(hdate: str, datelen: int) -> str:

    # This function returns the first datelen characters of an hdate string.

    return hdate[:datelen]

def sort_fields_fortran_order(
    items: list[tuple[float, str, FieldSlab]],
    vtable_rows: list[VtableEntry],
) -> list[FieldSlab]:

    # This function orders fields like WPS ungrib output.F: descending level (get_plvls), then Vtable row order.

    name_first_index: dict[str, int] = {}
    for i, row in enumerate(vtable_rows):
        nm = row.name.strip()
        if nm not in name_first_index:
            name_first_index[nm] = i
    levels = sorted({t[0] for t in items}, reverse=True)
    out: list[FieldSlab] = []
    for lev in levels:
        at_level = [(t[1], t[2]) for t in items if t[0] == lev]
        at_level.sort(
            key=lambda pair: (name_first_index.get(pair[0].strip(), 10**9), pair[0].strip()),
        )
        out.extend(slab for _, slab in at_level)
    return out

def sort_fields_for_output(
    items: list[tuple[float, str, FieldSlab]],
    vtable_rows: list[VtableEntry],
) -> list[FieldSlab]:

    # This function sorts field items for WPS intermediate output (alias for sort_fields_fortran_order).

    return sort_fields_fortran_order(items, vtable_rows)










