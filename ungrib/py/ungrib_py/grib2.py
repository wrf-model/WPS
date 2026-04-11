# GRIB2 extraction using ecCodes (Vtable-driven).

from __future__ import annotations

from collections.abc import Iterator
from pathlib import Path

import numpy as np

import eccodes

from ungrib_py.intermediate import FieldSlab, MapInfo
from ungrib_py.vtable import BLANK, SPLAT, VtableEntry, match_entry

_SOILT_SHALLOW_FIRST: tuple[str, ...] = (
    "SOILT001",
    "SOILT002",
    "SOILT006",
    "SOILT018",
    "SOILT054",
    "SOILT162",
    "SOILT486",
    "SOILT999",
)

_GRAVITY_M_S2: float = 9.81


def _is_pressure_level_pa(lev: float) -> bool:

    # Isobaric levels in WPS intermediate use Pa; match rrpr "plvl < 200000" and count_distinct_pressure_levels_pa.

    lf = float(lev)
    return 50.0 <= lf <= 120000.0


def _delete_surface_geopotential_sources(bucket: dict[tuple[float, str], FieldSlab]) -> None:

    # SOILGEO / surface GEOPT are not written by Fortran WPS output when desc is blank; drop for metgrid.

    for k in list(bucket.keys()):
        lev, nm = k
        if nm.strip() not in ("SOILGEO", "GEOPT"):
            continue
        lf = float(lev)
        if abs(lf - 200100.0) < 0.5 or abs(lf - 1.0) < 0.5:
            del bucket[k]


def _hgt_field_meta_from_vtable(rows: list[VtableEntry] | None) -> tuple[str, str, str]:

    # Prefer the HGT row from the same Vtable as Fortran/Python ungrib (e.g. ungrib_out/Vtable).

    if rows:
        for r in rows:
            if r.name.strip() == "HGT" and r.desc.strip():
                return (r.name[:9].ljust(9)[:9], r.units[:25], r.desc[:46])
    return ("HGT".ljust(9)[:9], "m".ljust(25)[:25], "Height".ljust(46)[:46])


def ensure_hgt_from_geopt(
    bucket: dict[tuple[float, str], FieldSlab],
    *,
    vtable_rows: list[VtableEntry] | None = None,
) -> None:

    # Fortran rrpr.F: HGT (m) = GEOPT (m^2 s^-2) / g on isobaric levels; metgrid reads HGT as GHT (METGRID.TBL).

    name9, units25, desc46 = _hgt_field_meta_from_vtable(vtable_rows)
    for key in list(bucket.keys()):
        lev, name = key
        if name.strip() != "GEOPT":
            continue
        if not _is_pressure_level_pa(lev):
            continue
        hgt_key = (float(lev), "HGT")
        if hgt_key in bucket:
            del bucket[key]
            continue
        geopt = bucket[key]
        arr = np.asarray(geopt.data, dtype=np.float64)
        hgt = np.asfortranarray((arr / _GRAVITY_M_S2).astype(np.float32))
        bucket[hgt_key] = FieldSlab(
            name=name9,
            units=units25,
            desc=desc46,
            level=float(lev),
            data=hgt,
        )
        del bucket[key]


def drop_fields_blank_vtable_desc(
    bucket: dict[tuple[float, str], FieldSlab],
    all_rows: list[VtableEntry],
) -> None:

    # Fortran output.F (iflag=2): fields whose Vtable description is all blanks are not written to intermediate.

    by_name: dict[str, list[VtableEntry]] = {}
    for r in all_rows:
        by_name.setdefault(r.name.strip(), []).append(r)
    drop_names = {
        nm
        for nm, rs in by_name.items()
        if rs and all(not x.desc.strip() for x in rs)
    }
    for key in list(bucket.keys()):
        if key[1].strip() in drop_names:
            del bucket[key]


def ensure_soilt000_for_metgrid(bucket: dict[tuple[float, str], FieldSlab]) -> None:

    # METGRID.TBL ICON fills derived SOILT level 0 from SOILT000(200100). Vtable.ICONp uses SOILT001 for the top layer.
    # If SOILT000 is absent, that output level is skipped and NUM_METGRID_SOIL_LEVELS can be 7 vs namelist 8.
    # Duplicate the shallowest present SOILT* slab as SOILT000 when needed (also covers SOILT001 match failures).

    levels = {k[0] for k in bucket}
    for lev in levels:
        if (lev, "SOILT000") in bucket:
            continue
        for name in _SOILT_SHALLOW_FIRST:
            key = (lev, name)
            if key not in bucket:
                continue
            src = bucket[key]
            bucket[(lev, "SOILT000")] = FieldSlab(
                name="SOILT000".ljust(9)[:9],
                units=src.units,
                desc=src.desc,
                level=lev,
                data=np.asfortranarray(src.data.copy()),
            )
            break

def ensure_soilhgt_from_soilgeo(bucket: dict[tuple[float, str], FieldSlab]) -> None:

    # Fortran ungrib runs rrpr.F: SOILHGT (m) from SOILGEO (m^2 s^-2) when SOILHGT is missing.
    # Also accept surface GEOPT (same units) or surface HGT already in meters (some Vtables).

    for (lev, name) in bucket:
        if name.strip() != "SOILHGT":
            continue
        if abs(float(lev) - 200100.0) < 0.5:
            _delete_surface_geopotential_sources(bucket)
            return

    def find_at_surface(names: tuple[str, ...]) -> FieldSlab | None:
        for (lev, name), fld in bucket.items():
            if name.strip() not in names:
                continue
            lf = float(lev)
            if abs(lf - 200100.0) < 0.5 or abs(lf - 1.0) < 0.5:
                return fld
        return None

    geo_like = find_at_surface(("SOILGEO", "GEOPT"))
    if geo_like is not None:
        arr = np.asarray(geo_like.data, dtype=np.float64)
        hgt = np.asfortranarray((arr / _GRAVITY_M_S2).astype(np.float32))
        bucket[(200100.0, "SOILHGT")] = FieldSlab(
            name="SOILHGT".ljust(9)[:9],
            units="m".ljust(25)[:25],
            desc="Terrain field of source analysis".ljust(46)[:46],
            level=200100.0,
            data=hgt,
        )
        _delete_surface_geopotential_sources(bucket)
        return

    hgt_sfc = find_at_surface(("HGT",))
    if hgt_sfc is None:
        return
    arr = np.asarray(hgt_sfc.data, dtype=np.float64)
    if float(np.nanmax(np.abs(arr))) > 12000.0:
        return
    bucket[(200100.0, "SOILHGT")] = FieldSlab(
        name="SOILHGT".ljust(9)[:9],
        units="m".ljust(25)[:25],
        desc="Terrain field of source analysis".ljust(46)[:46],
        level=200100.0,
        data=np.asfortranarray(arr.astype(np.float32)),
    )

def ecmwf_surface_geopotential_vtable_row(gid: int, rows: list[VtableEntry]) -> VtableEntry | None:

    # ERA5 / ECMWF surface geopotential is often paramId 129 (or shortName=z); Vtable.ERA-interim.pl has no GRIB2 columns so match_entry fails.
    # Prefer SOILGEO, then GEOPT, when discipline 0 / surface type 1 matches typical orography GRIB2.

    pid = _try_get(gid, "paramId")
    pid_ok = False
    if pid is not None:
        try:
            pid_ok = int(pid) == 129
        except (TypeError, ValueError):
            pid_ok = False
    sn = str(_try_get(gid, "shortName", "") or "").strip().lower()
    if not pid_ok and sn != "z":
        return None
    if _iget(gid, "discipline") != 0:
        return None
    if type_of_first_fixed_surface_int(gid) != 1:
        return None
    soilgeo = [r for r in rows if r.name.strip() == "SOILGEO"]
    if soilgeo:
        return soilgeo[0]
    geopt = [r for r in rows if r.name.strip() == "GEOPT"]
    if geopt:
        return geopt[0]
    return None

def _iget(gid: int, key: str) -> int:

    # This function returns an integer ecCodes key value for a GRIB handle.

    return int(eccodes.codes_get(gid, key))

def _fget(gid: int, key: str) -> float:

    # This function returns a floating-point ecCodes key value for a GRIB handle.

    return float(eccodes.codes_get(gid, key))

def _sget(gid: int, key: str) -> str:

    # This function returns a string ecCodes key value for a GRIB handle.

    return str(eccodes.codes_get(gid, key))

def _try_get(gid: int, key: str, default: object | None = None) -> object | None:

    # This function returns a GRIB key value or default if the key is missing.

    try:
        return eccodes.codes_get(gid, key)
    except eccodes.KeyValueNotFoundError:
        return default

_TYPE_OF_FIRST_FIXED_SURFACE_STR: dict[str, int] = {

    # This table maps ecCodes string codetable labels to WMO GRIB2 Table 4.5 codes.

    "sfc": 1,
    "surface": 1,
    "msl": 101,
    "pl": 100,
    "pt": 100,
    "hl": 103,
    "hhl": 118,
    "sol": 106,
    "soil": 106,
    "sot": 106,
    "pv": 109,
    "lcl": 212,
    "lcd": 213,
}

def type_of_first_fixed_surface_int(gid: int) -> int:

    # This function returns typeOfFirstFixedSurface as an integer (some GRIBs expose it as a string).

    try:
        return int(eccodes.codes_get(gid, "typeOfFirstFixedSurface", int))
    except (TypeError, ValueError, eccodes.CodesInternalError, eccodes.GribInternalError):
        pass
    raw = _try_get(gid, "typeOfFirstFixedSurface")
    if raw is None:
        raise ValueError("typeOfFirstFixedSurface is missing from GRIB message")
    if isinstance(raw, bool):
        return int(raw)
    if isinstance(raw, int):
        return raw
    if isinstance(raw, float):
        return int(raw)
    s = str(raw).strip().lower()
    if s.isdigit():
        return int(s)
    if s in _TYPE_OF_FIRST_FIXED_SURFACE_STR:
        return _TYPE_OF_FIRST_FIXED_SURFACE_STR[s]
    raise ValueError(f"unsupported typeOfFirstFixedSurface string {raw!r}")

def earth_radius_km(gid: int) -> float:

    # This function returns spherical Earth radius in km from GRIB shape metadata.

    shape = _try_get(gid, "shapeOfTheEarth")
    if shape is None:
        return 6371.229
    shape = int(shape)
    if shape == 0:
        return 6367.47
    if shape == 1:
        try:
            sc = int(eccodes.codes_get(gid, "scaleFactorOfRadiusOfSphericalEarth"))
            val = int(eccodes.codes_get(gid, "scaledValueOfRadiusOfSphericalEarth"))
            return (val / (10**sc)) * 0.001
        except eccodes.KeyValueNotFoundError:
            return 6371.229
    if shape == 6:
        return 6371.229
    if shape == 8:
        return 6371.2
    return 6371.229

def _wps_source32(s: str) -> str:

    # This function pads a source label to 32 characters like Fortran map%source.

    return s.ljust(32)[:32]


def _year_from_gid(gid: int) -> int:

    # This function returns the validity year from GRIB date keys (for NCEP RAP vs RUC).

    vdate = _try_get(gid, "validityDate", _try_get(gid, "dataDate"))
    if vdate is None:
        return 2000
    return int(vdate) // 10000


def guess_source(gid: int) -> str:

    # This function matches rd_grib2.F map%source assignment from centre and generating process.

    gp = _try_get(gid, "generatingProcessIdentifier")
    if gp is None:
        return _wps_source32("Unknown GRIB2")
    try:
        gp_i = int(gp)
    except (TypeError, ValueError):
        return _wps_source32("Unknown GRIB2")

    c = _try_get(gid, "centre")
    if isinstance(c, str):
        cs = c.strip().lower()
        if cs in ("ecmf", "ecmwf"):
            return _wps_source32("ECMWF")
        if cs == "edzw":
            return _wps_source32("DWD")
        try:
            c_i = int(cs)
        except ValueError:
            return _wps_source32("unknown model and orig center")
    elif isinstance(c, (int, float)):
        c_i = int(c)
    else:
        return _wps_source32("unknown model and orig center")

    if c_i == 7:
        yr = _year_from_gid(gid)
        if gp_i == 81:
            return _wps_source32("NCEP GFS Analysis")
        if gp_i == 82:
            return _wps_source32("NCEP GFS GDAS/FNL")
        if gp_i == 83:
            return _wps_source32("NCEP HRRR Model")
        if gp_i == 84:
            return _wps_source32("NCEP MESO NAM Model")
        if gp_i == 89:
            return _wps_source32("NCEP NMM ")
        if gp_i == 96:
            return _wps_source32("NCEP GFS Model")
        if gp_i in (86, 100):
            return _wps_source32("NCEP RUC Model")
        if gp_i == 101:
            return _wps_source32("NCEP RUC Model")
        if gp_i == 105:
            if yr > 2011:
                return _wps_source32("NCEP RAP Model")
            return _wps_source32("NCEP RUC Model")
        if gp_i == 107:
            return _wps_source32("NCEP GEFS")
        if gp_i == 109:
            return _wps_source32("NCEP RTMA")
        if gp_i == 140:
            return _wps_source32("NCEP NARR")
        if gp_i == 44:
            return _wps_source32("NCEP SST Analysis")
        if gp_i == 70:
            return _wps_source32("GFDL Hurricane Model")
        if gp_i == 80:
            return _wps_source32("NCEP GFS Ensemble")
        if gp_i == 111:
            return _wps_source32("NCEP NMMB Model")
        if gp_i == 112:
            return _wps_source32("NCEP WRF-NMM Model")
        if gp_i == 116:
            return _wps_source32("NCEP WRF-ARW Model")
        if gp_i == 129:
            return _wps_source32("NCEP GODAS")
        if gp_i == 197:
            return _wps_source32("NCEP CDAS CFSV2")
        if gp_i == 25:
            return _wps_source32("NCEP SNOW COVER ANALYSIS")
        return _wps_source32("unknown model from NCEP")
    if c_i == 57:
        if gp_i == 87:
            return _wps_source32("AFWA AGRMET")
        return _wps_source32("AFWA")
    if c_i == 58:
        return _wps_source32("US Navy FNOC")
    if c_i == 59:
        if gp_i == 125:
            return _wps_source32("NOAA GSD Rapid Refresh Model")
        if gp_i == 83:
            return _wps_source32("NOAA GSD HRRR Model")
        if gp_i == 105:
            return _wps_source32("NOAA GSD")
        return _wps_source32("NOAA GSD")
    if c_i == 60:
        return _wps_source32("NCAR")
    if c_i == 98:
        return _wps_source32("ECMWF")
    if c_i == 34:
        return _wps_source32("JMA")
    if c_i in (74, 75):
        return _wps_source32("UKMO")
    if c_i in (78, 79):
        return _wps_source32("DWD")
    return _wps_source32("unknown model and orig center")

def validity_hdate(gid: int) -> str:

    # This function formats GRIB validity date and time as a 19-character hdate string.

    vdate = _try_get(gid, "validityDate", _try_get(gid, "date"))
    vtime = _try_get(gid, "validityTime", _try_get(gid, "time"))
    if vdate is None:
        vdate = _try_get(gid, "dataDate")
    if vtime is None:
        vtime = _try_get(gid, "dataTime", 0)
    vdate = int(vdate)
    vtime = int(vtime)
    y, m, d = vdate // 10000, (vdate % 10000) // 100, vdate % 100
    hh = vtime // 100
    mm = vtime % 100
    ss = 0
    if vtime > 2400:
        hh = vtime // 10000
        mm = (vtime % 10000) // 100
        ss = vtime % 100
    return f"{y:04d}-{m:02d}-{d:02d}_{hh:02d}:{mm:02d}:{ss:02d}"

def grid_relative_wind(gid: int) -> bool:

    # This function reports whether winds are grid-relative from GRIB metadata.

    v = _try_get(gid, "uvRelativeToGrid")
    if v is not None:
        return bool(int(v))
    flags = _try_get(gid, "resolutionAndComponentFlags")
    if flags is None:
        return True
    flags = int(flags)
    bit4 = (flags >> 3) & 1
    return bit4 != 0

def map_info_from_grib(gid: int) -> MapInfo:

    # This function builds WPS MapInfo from ecCodes grid description for supported projections.

    gt = _sget(gid, "gridType")
    src = guess_source(gid)
    r_earth = earth_radius_km(gid)
    gw = grid_relative_wind(gid)
    startloc = "SWCORNER"

    if gt == "regular_ll":
        ni = _iget(gid, "Ni")
        nj = _iget(gid, "Nj")
        lat1 = _fget(gid, "latitudeOfFirstGridPointInDegrees")
        lon1 = _fget(gid, "longitudeOfFirstGridPointInDegrees")
        dlon = _fget(gid, "iDirectionIncrementInDegrees")
        dlat = _fget(gid, "jDirectionIncrementInDegrees")
        j_scan_pos = _try_get(gid, "jScansPositively")
        if j_scan_pos is not None and int(j_scan_pos) == 0 and dlat > 0:
            dlat = -dlat
        return MapInfo(
            source=src,
            igrid=0,
            nx=ni,
            ny=nj,
            startloc=startloc,
            lat1=lat1,
            lon1=lon1,
            dx=dlon,
            dy=dlat,
            lov=0.0,
            truelat1=0.0,
            truelat2=0.0,
            r_earth_km=r_earth,
            grid_wind=gw,
            centerlat=0.0,
            centerlon=0.0,
        )

    if gt == "lambert":
        ni = _iget(gid, "Ni")
        nj = _iget(gid, "Nj")
        lat1 = _fget(gid, "latitudeOfFirstGridPointInDegrees")
        lon1 = _fget(gid, "longitudeOfFirstGridPointInDegrees")
        lov = _fget(gid, "LoVInDegrees")
        t1 = _fget(gid, "Latin1InDegrees")
        t2 = _fget(gid, "Latin2InDegrees")
        dx_m = _fget(gid, "DxInMetres")
        dy_m = _fget(gid, "DyInMetres")
        return MapInfo(
            source=src,
            igrid=3,
            nx=ni,
            ny=nj,
            startloc=startloc,
            lat1=lat1,
            lon1=lon1,
            dx=dx_m * 0.001,
            dy=dy_m * 0.001,
            lov=lov,
            truelat1=t1,
            truelat2=t2,
            r_earth_km=r_earth,
            grid_wind=gw,
            centerlat=0.0,
            centerlon=0.0,
        )

    if gt == "polar_stereographic":
        ni = _iget(gid, "Ni")
        nj = _iget(gid, "Nj")
        lat1 = _fget(gid, "latitudeOfFirstGridPointInDegrees")
        lon1 = _fget(gid, "longitudeOfFirstGridPointInDegrees")
        lov = _fget(gid, "orientationOfTheGridInDegrees")
        dx_m = _fget(gid, "DxInMetres")
        dy_m = _fget(gid, "DyInMetres")
        return MapInfo(
            source=src,
            igrid=5,
            nx=ni,
            ny=nj,
            startloc=startloc,
            lat1=lat1,
            lon1=lon1,
            dx=dx_m * 0.001,
            dy=dy_m * 0.001,
            lov=lov,
            truelat1=60.0,
            truelat2=91.0,
            r_earth_km=r_earth,
            grid_wind=gw,
            centerlat=0.0,
            centerlon=0.0,
        )

    if gt == "mercator":
        ni = _iget(gid, "Ni")
        nj = _iget(gid, "Nj")
        lat1 = _fget(gid, "latitudeOfFirstGridPointInDegrees")
        lon1 = _fget(gid, "longitudeOfFirstGridPointInDegrees")
        dlon = _fget(gid, "iDirectionIncrementInDegrees")
        dlat = _fget(gid, "jDirectionIncrementInDegrees")
        latin = _try_get(gid, "LaDInDegrees", _try_get(gid, "LatinInDegrees", 0.0))
        latin = float(latin or 0.0)
        return MapInfo(
            source=src,
            igrid=1,
            nx=ni,
            ny=nj,
            startloc=startloc,
            lat1=lat1,
            lon1=lon1,
            dx=dlon,
            dy=dlat,
            lov=0.0,
            truelat1=latin,
            truelat2=0.0,
            r_earth_km=r_earth,
            grid_wind=gw,
            centerlat=0.0,
            centerlon=0.0,
        )

    raise ValueError(f"unsupported gridType={gt!r} for Python ungrib (extend map_info_from_grib)")

def scaled_surface_value(gid: int, which: str) -> float:

    # This function decodes scaled first or second fixed surface values from GRIB2.

    if which == "first":
        sf = int(_try_get(gid, "scaleFactorOfFirstFixedSurface", 0) or 0)
        sv = int(_try_get(gid, "scaledValueOfFirstFixedSurface", 0) or 0)
    else:
        sf = int(_try_get(gid, "scaleFactorOfSecondFixedSurface", 0) or 0)
        sv = int(_try_get(gid, "scaledValueOfSecondFixedSurface", 0) or 0)
    return float(sv) * (10.0 ** (-sf))

def pressure_level_pa(gid: int) -> float:

    # This function returns isobaric pressure in Pa for WPS intermediate (Fortran rd_grib2 uses Pa; pmin in namelist.wps is Pa).
    # ECMWF ERA5 GRIB2 often has typeOfLevel=isobaricInhPa with ecCodes key "level" in hPa.

    tol = str(_try_get(gid, "typeOfLevel", "") or "")
    if "InhPa" in tol:
        lev = _try_get(gid, "level")
        if lev is not None:
            return float(lev) * 100.0
    if "InPa" in tol and "InhPa" not in tol:
        lev = _try_get(gid, "level")
        if lev is not None:
            return float(lev)
    return scaled_surface_value(gid, "first")

def compute_wps_level(gid: int, pmin: float) -> float | None:

    # This function maps GRIB2 vertical type to WPS intermediate level codes, honoring pmin.

    t = type_of_first_fixed_surface_int(gid)
    if t == 100:
        pa = pressure_level_pa(gid)
        if pa < pmin:
            return None
        return pa
    if t in (105, 118, 150):
        return scaled_surface_value(gid, "first")
    if t == 104:
        return float(_iget(gid, "scaledValueOfFirstFixedSurface"))
    if t == 101:
        return 201300.0
    if t == 103:
        depth = scaled_surface_value(gid, "first")
        if depth in (2.0, 10.0, 1000.0):
            return 200100.0
        return None
    if 206 <= t <= 234 or 242 <= t <= 254 or t in (200, 10):
        return 200100.0
    if t in (106, 1, 151):
        return 200100.0
    if t == 6:
        return 200100.0
    if t == 7:
        return 200100.0
    return None

def soil_depths_cm(gid: int) -> tuple[float, float]:

    # This function returns soil layer depths in centimeters for GRIB2 type-106 levels.

    g1 = scaled_surface_value(gid, "first")
    g2 = scaled_surface_value(gid, "second")
    if g1 > 1e6 or g2 > 1e6:
        sv1 = float(_iget(gid, "scaledValueOfFirstFixedSurface"))
        sv2 = float(_iget(gid, "scaledValueOfSecondFixedSurface"))
        return sv1 * 100.0, sv2 * 100.0
    return 100.0 * g1, 100.0 * g2

def g1_level_type_from_ecmwf_type_of_level(type_of_level: str) -> int | None:

    # This function maps ECMWF-style GRIB1 typeOfLevel strings to WPS Vtable g1_level_type codes.

    match type_of_level:
        case "isobaricInhPa" | "isobaricInPa":
            return 100
        case "surface":
            return 1
        case "heightAboveGround":
            return 103
        case "depthBelowLandLayer":
            return 112
        case _:
            return None


def _grib1_row_matches_level_value(row: VtableEntry, level_val: float) -> bool:

    # This function checks Vtable level1/level2 against a GRIB1 scalar level (pressure hPa or meters).

    if abs(row.level1 - SPLAT) < 1e-6:
        return True
    return abs(row.level1 - level_val) < 0.01


def pick_vtable_row_grib1(gid: int, rows: list[VtableEntry]) -> VtableEntry | None:

    # This function selects the Vtable row for a GRIB edition-1 message using g1_param and g1_level_type.

    pid = _try_get(gid, "paramId")
    param = int(pid) if pid is not None else _iget(gid, "indicatorOfParameter")
    tol = str(_sget(gid, "typeOfLevel"))
    if tol == "depthBelowLandLayer":
        g1 = float(_try_get(gid, "topLevel", 0) or 0)
        g2 = float(_try_get(gid, "bottomLevel", 0) or 0)
        for r in rows:
            if r.g1_param == BLANK or r.g1_param != param:
                continue
            if r.g1_level_type != 112:
                continue
            l1_ok = abs(r.level1 - SPLAT) < 1e-6 or abs(r.level1 - g1) < 0.01
            if not l1_ok:
                continue
            if g2 > 1e6:
                l2_ok = r.level2 > SPLAT and abs(r.level1 - g1) < 0.01
            else:
                l2_ok = r.level2 <= SPLAT or abs(r.level2 - g2) < 0.01
            if l2_ok:
                return r
        return None
    g1_lt = g1_level_type_from_ecmwf_type_of_level(tol)
    if g1_lt is None:
        return None
    level_val = float(_try_get(gid, "level", 0) or 0)
    for r in rows:
        if r.g1_param == BLANK or r.g1_param != param:
            continue
        if r.g1_level_type == BLANK:
            continue
        if r.g1_level_type != g1_lt:
            continue
        if not _grib1_row_matches_level_value(r, level_val):
            continue
        return r
    return None


def compute_wps_level_grib1(gid: int, pmin: float) -> float | None:

    # This function maps GRIB1 vertical coordinates to WPS intermediate level codes, honoring pmin.

    tol = str(_sget(gid, "typeOfLevel"))
    lev = float(_try_get(gid, "level", 0) or 0)
    if tol == "isobaricInhPa":
        lev_pa = lev * 100.0
        if lev_pa < pmin:
            return None
        return lev_pa
    if tol == "isobaricInPa":
        if lev < pmin:
            return None
        return lev
    if tol == "surface":
        return 200100.0
    if tol == "heightAboveGround":
        if lev in (2.0, 10.0, 1000.0):
            return 200100.0
        return None
    if tol == "depthBelowLandLayer":
        return 200100.0
    return None


def pick_vtable_row_ecmwf_soil_layer151(
    gid: int,
    rows: list[VtableEntry],
    discipline: int,
    cat: int,
    param: int,
    pdt: int,
) -> VtableEntry | None:

    # This function picks the ECMWF IFS soil Vtable row for GRIB2 type-of-first-fixed-surface 151 using layer index.

    layer_start = scaled_surface_value(gid, "first")
    for strict_pdt in (True, False):
        candidates: list[VtableEntry] = []
        for r in rows:
            if r.g2_discipline != discipline or r.g2_category != cat or r.g2_parameter != param:
                continue
            if r.g2_level_type != 151:
                continue
            if strict_pdt and r.g2_pdt != pdt:
                continue
            candidates.append(r)
        if not candidates:
            continue
        candidates.sort(key=lambda x: (x.level1, x.level2))
        idx = int(round(layer_start))
        if 0 <= idx < len(candidates):
            return candidates[idx]
    return None

def pick_vtable_row(gid: int, rows: list[VtableEntry]) -> VtableEntry | None:

    # This function selects the Vtable row for a GRIB message, including soil depth matching.

    discipline = _iget(gid, "discipline")
    cat = _iget(gid, "parameterCategory")
    param = _iget(gid, "parameterNumber")
    first_surface = type_of_first_fixed_surface_int(gid)
    pdt = _iget(gid, "productDefinitionTemplateNumber")
    if first_surface == 106:
        g1, g2 = soil_depths_cm(gid)
        for strict_pdt in (True, False):
            for r in rows:
                if r.g2_discipline != discipline or r.g2_category != cat or r.g2_parameter != param:
                    continue
                if r.g2_level_type != 106:
                    continue
                if strict_pdt and r.g2_pdt != pdt:
                    continue
                l1_ok = abs(r.level1 - SPLAT) < 1e-6 or abs(r.level1 - g1) < 0.01
                l2_ok = r.level2 <= SPLAT or abs(r.level2 - g2) < 0.01
                if l1_ok and l2_ok:
                    return r
        return None
    if first_surface == 151:
        m151 = pick_vtable_row_ecmwf_soil_layer151(gid, rows, discipline, cat, param, pdt)
        if m151 is not None:
            return m151
    m = match_entry(rows, discipline, cat, param, first_surface, pdt)
    if m is not None:
        return m
    return ecmwf_surface_geopotential_vtable_row(gid, rows)

def values_to_slab(values: np.ndarray, ni: int, nj: int) -> np.ndarray:

    # This function reshapes ecCodes 1-D values to a Fortran-order (ni, nj) slab.

    arr = values.reshape((nj, ni), order="C")
    return np.asfortranarray(arr.T.astype(np.float32))

def extract_file(
    path: str | Path,
    vtable_rows: list[VtableEntry],
    *,
    pmin: float,
) -> tuple[dict[str, dict[tuple[float, str], FieldSlab]], MapInfo | None]:

    # This function scans one GRIB1/GRIB2 file and collects Vtable-matched fields grouped by hdate.

    out: dict[str, dict[tuple[float, str], FieldSlab]] = {}
    map_info: MapInfo | None = None
    p = Path(path)
    if not p.is_file():
        return out, None
    with p.open("rb") as fh:
        while True:
            gid = eccodes.codes_grib_new_from_file(fh)
            if gid is None:
                break
            try:
                edition = _iget(gid, "editionNumber")
                if edition == 2:
                    row = pick_vtable_row(gid, vtable_rows)
                    if row is None:
                        continue
                    level = compute_wps_level(gid, pmin)
                elif edition == 1:
                    row = pick_vtable_row_grib1(gid, vtable_rows)
                    if row is None:
                        continue
                    level = compute_wps_level_grib1(gid, pmin)
                else:
                    continue
                if level is None:
                    continue
                hdate = validity_hdate(gid)
                mi = map_info_from_grib(gid)
                if map_info is None:
                    map_info = mi
                ni = _iget(gid, "Ni")
                nj = _iget(gid, "Nj")
                vals = np.asarray(eccodes.codes_get_values(gid), dtype=np.float64)
                slab = values_to_slab(vals, ni, nj)
                if (ni, nj) != (mi.nx, mi.ny):
                    continue
                fld = FieldSlab(
                    name=row.name[:9].strip().ljust(9)[:9],
                    units=row.units[:25],
                    desc=row.desc[:46],
                    level=float(level),
                    data=slab,
                )
                bucket = out.setdefault(hdate, {})
                bucket[(float(level), fld.name.strip())] = fld
            finally:
                eccodes.codes_release(gid)
    for bucket in out.values():
        ensure_soilt000_for_metgrid(bucket)
        ensure_soilhgt_from_soilgeo(bucket)
        ensure_hgt_from_geopt(bucket, vtable_rows=vtable_rows)
    return out, map_info

def iter_grib_filenames() -> Iterator[str]:

    # This function yields GRIBFILE.AAA through GRIBFILE.ZZZ names in lexicographic order.

    letters = [chr(c) for c in range(ord("A"), ord("Z") + 1)]
    for a in letters:
        for b in letters:
            for c in letters:
                yield f"GRIBFILE.{a}{b}{c}"










