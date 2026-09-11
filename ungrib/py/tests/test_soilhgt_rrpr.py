import numpy as np

from ungrib_py.grib2 import (
    drop_fields_blank_vtable_desc,
    ensure_hgt_from_geopt,
    ensure_soilhgt_from_soilgeo,
)
from ungrib_py.intermediate import FieldSlab
from ungrib_py.vtable import BLANK, VtableEntry


def test_soilhgt_derived_from_soilgeo():

    # This function checks rrpr-style SOILHGT = SOILGEO / 9.81.

    slab = np.asfortranarray(np.full((2, 3), 98.1, dtype=np.float32))
    geo = FieldSlab(
        name="SOILGEO",
        units="m2 s-2".ljust(25),
        desc="x",
        level=200100.0,
        data=slab,
    )
    bucket: dict[tuple[float, str], FieldSlab] = {(200100.0, "SOILGEO"): geo}
    ensure_soilhgt_from_soilgeo(bucket)
    assert (200100.0, "SOILHGT") in bucket
    assert (200100.0, "SOILGEO") not in bucket
    h = bucket[(200100.0, "SOILHGT")]
    assert abs(float(h.data.flat[0]) - 10.0) < 1e-5


def test_soilhgt_skips_when_present():

    # This function checks existing SOILHGT is not overwritten.

    a = np.asfortranarray(np.ones((2, 3), dtype=np.float32))
    existing = FieldSlab(
        name="SOILHGT",
        units="m".ljust(25),
        desc="y",
        level=200100.0,
        data=a,
    )
    geo = FieldSlab(
        name="SOILGEO",
        units="m2 s-2".ljust(25),
        desc="z",
        level=200100.0,
        data=np.asfortranarray(np.full((2, 3), 98.1, dtype=np.float32)),
    )
    bucket = {(200100.0, "SOILHGT"): existing, (200100.0, "SOILGEO"): geo}
    ensure_soilhgt_from_soilgeo(bucket)
    assert bucket[(200100.0, "SOILHGT")] is existing
    assert (200100.0, "SOILGEO") not in bucket


def test_soilhgt_from_soilgeo_level_one():

    # This function matches rrpr hybrid-level storage at level 1.

    slab = np.asfortranarray(np.full((2, 3), 98.1, dtype=np.float32))
    geo = FieldSlab(
        name="SOILGEO",
        units="m2 s-2".ljust(25),
        desc="x",
        level=1.0,
        data=slab,
    )
    bucket = {(1.0, "SOILGEO"): geo}
    ensure_soilhgt_from_soilgeo(bucket)
    assert (200100.0, "SOILHGT") in bucket
    assert (1.0, "SOILGEO") not in bucket


def test_soilhgt_from_geopt_surface():

    # This function checks GEOPT at surface is accepted like SOILGEO (rrpr divides by g).

    slab = np.asfortranarray(np.full((2, 3), 98.1, dtype=np.float32))
    geo = FieldSlab(
        name="GEOPT",
        units="m2 s-2".ljust(25),
        desc="gp",
        level=200100.0,
        data=slab,
    )
    bucket = {(200100.0, "GEOPT"): geo}
    ensure_soilhgt_from_soilgeo(bucket)
    assert (200100.0, "GEOPT") not in bucket
    assert abs(float(bucket[(200100.0, "SOILHGT")].data.flat[0]) - 10.0) < 1e-5


def test_soilhgt_from_hgt_meters():

    # This function checks surface HGT in meters can be copied when geopotential is absent.

    slab = np.asfortranarray(np.full((2, 3), 500.0, dtype=np.float32))
    h = FieldSlab(
        name="HGT",
        units="m".ljust(25),
        desc="h",
        level=200100.0,
        data=slab,
    )
    bucket = {(200100.0, "HGT"): h}
    ensure_soilhgt_from_soilgeo(bucket)
    assert abs(float(bucket[(200100.0, "SOILHGT")].data.flat[0]) - 500.0) < 1e-5


def test_hgt_from_geopt_pressure_levels():

    # rrpr: HGT = GEOPT / g on isobaric levels (metgrid reads HGT as GHT).

    slab = np.asfortranarray(np.full((2, 2), 9810.0, dtype=np.float32))
    geopt = FieldSlab(
        name="GEOPT",
        units="m2 s-2".ljust(25),
        desc="gp",
        level=50000.0,
        data=slab,
    )
    bucket: dict[tuple[float, str], FieldSlab] = {(50000.0, "GEOPT"): geopt}
    ensure_hgt_from_geopt(bucket)
    assert (50000.0, "GEOPT") not in bucket
    assert (50000.0, "HGT") in bucket
    assert abs(float(bucket[(50000.0, "HGT")].data.flat[0]) - 1000.0) < 1e-3


def test_hgt_from_geopt_uses_vtable_hgt_row():

    # Same Vtable as ungrib (e.g. ungrib_out/Vtable): HGT row supplies units/desc for derived slab.

    hgt_row = VtableEntry(
        g1_param=156,
        g1_level_type=100,
        level1=float(BLANK),
        level2=float(BLANK),
        name="HGT".ljust(9)[:9],
        units="m".ljust(25)[:25],
        desc="Custom height desc".ljust(46)[:46],
        g2_discipline=0,
        g2_category=3,
        g2_parameter=5,
        g2_level_type=100,
        g2_pdt=0,
    )
    slab = np.asfortranarray(np.full((2, 2), 9810.0, dtype=np.float32))
    geopt = FieldSlab(
        name="GEOPT",
        units="m2 s-2".ljust(25),
        desc="gp",
        level=50000.0,
        data=slab,
    )
    bucket: dict[tuple[float, str], FieldSlab] = {(50000.0, "GEOPT"): geopt}
    ensure_hgt_from_geopt(bucket, vtable_rows=[hgt_row])
    h = bucket[(50000.0, "HGT")]
    assert "Custom height desc" in h.desc
    assert h.units.strip() == "m"


def test_geopt_dropped_when_hgt_present():

    g = FieldSlab(
        name="GEOPT",
        units="m2 s-2".ljust(25),
        desc="gp",
        level=85000.0,
        data=np.asfortranarray(np.ones((2, 2), dtype=np.float32)),
    )
    h = FieldSlab(
        name="HGT",
        units="m".ljust(25),
        desc="Height".ljust(46),
        level=85000.0,
        data=np.asfortranarray(np.full((2, 2), 99.0, dtype=np.float32)),
    )
    bucket = {(85000.0, "GEOPT"): g, (85000.0, "HGT"): h}
    ensure_hgt_from_geopt(bucket)
    assert (85000.0, "GEOPT") not in bucket
    assert bucket[(85000.0, "HGT")] is h


def test_drop_fields_only_when_all_vtable_rows_blank_desc():

    dewpt = VtableEntry(
        g1_param=168,
        g1_level_type=1,
        level1=0.0,
        level2=0.0,
        name="DEWPT".ljust(9)[:9],
        units="K".ljust(25)[:25],
        desc=" " * 46,
        g2_discipline=0,
        g2_category=0,
        g2_parameter=6,
        g2_level_type=103,
        g2_pdt=0,
    )
    tt = VtableEntry(
        g1_param=168,
        g1_level_type=1,
        level1=0.0,
        level2=0.0,
        name="TT".ljust(9)[:9],
        units="K".ljust(25)[:25],
        desc="Temperature".ljust(46)[:46],
        g2_discipline=0,
        g2_category=0,
        g2_parameter=0,
        g2_level_type=103,
        g2_pdt=0,
    )
    uu_sfc = VtableEntry(
        g1_param=165,
        g1_level_type=1,
        level1=0.0,
        level2=0.0,
        name="UU".ljust(9)[:9],
        units="m s-1".ljust(25)[:25],
        desc=" " * 46,
        g2_discipline=0,
        g2_category=0,
        g2_parameter=2,
        g2_level_type=103,
        g2_pdt=0,
    )
    uu_pl = VtableEntry(
        g1_param=131,
        g1_level_type=100,
        level1=float(BLANK),
        level2=float(BLANK),
        name="UU".ljust(9)[:9],
        units="m s-1".ljust(25)[:25],
        desc="U".ljust(46)[:46],
        g2_discipline=0,
        g2_category=0,
        g2_parameter=2,
        g2_level_type=100,
        g2_pdt=0,
    )
    slab = np.asfortranarray(np.ones((2, 2), dtype=np.float32))
    bucket: dict[tuple[float, str], FieldSlab] = {
        (200100.0, "DEWPT"): FieldSlab(
            name="DEWPT",
            units="K".ljust(25),
            desc="x",
            level=200100.0,
            data=slab,
        ),
        (200100.0, "TT"): FieldSlab(
            name="TT",
            units="K".ljust(25),
            desc="y",
            level=200100.0,
            data=slab,
        ),
        (200100.0, "UU"): FieldSlab(
            name="UU",
            units="m s-1".ljust(25),
            desc="z",
            level=200100.0,
            data=slab,
        ),
    }
    drop_fields_blank_vtable_desc(bucket, [dewpt, tt, uu_sfc, uu_pl])
    assert (200100.0, "DEWPT") not in bucket
    assert (200100.0, "TT") in bucket
    assert (200100.0, "UU") in bucket
