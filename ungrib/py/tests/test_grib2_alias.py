import numpy as np

from ungrib_py.grib2 import ensure_soilt000_for_metgrid
from ungrib_py.intermediate import FieldSlab


def test_ensure_soilt000_from_soilt001():

    # This function checks SOILT000 is created from SOILT001 when missing.

    lev = 200100.0
    slab = np.asfortranarray(np.ones((2, 3), dtype=np.float32))
    src = FieldSlab(
        name="SOILT001",
        units="K".ljust(25),
        desc="soil t",
        level=lev,
        data=slab,
    )
    bucket: dict[tuple[float, str], FieldSlab] = {(lev, "SOILT001"): src}
    ensure_soilt000_for_metgrid(bucket)
    assert (lev, "SOILT000") in bucket
    dup = bucket[(lev, "SOILT000")]
    assert dup.name.strip() == "SOILT000"
    assert np.array_equal(dup.data, src.data)


def test_ensure_soilt000_prefers_shallowest():

    # This function checks SOILT000 is taken from SOILT002 when SOILT001 is absent.

    lev = 200100.0
    a = np.asfortranarray(np.full((2, 3), 2.0, dtype=np.float32))
    b = np.asfortranarray(np.full((2, 3), 3.0, dtype=np.float32))
    bucket = {
        (lev, "SOILT002"): FieldSlab(
            name="SOILT002",
            units="K".ljust(25),
            desc="x",
            level=lev,
            data=a,
        ),
        (lev, "SOILT006"): FieldSlab(
            name="SOILT006",
            units="K".ljust(25),
            desc="y",
            level=lev,
            data=b,
        ),
    }
    ensure_soilt000_for_metgrid(bucket)
    assert (lev, "SOILT000") in bucket
    assert np.array_equal(bucket[(lev, "SOILT000")].data, a)


def test_ensure_soilt000_skips_if_present():

    # This function checks an existing SOILT000 is not overwritten.

    lev = 200100.0
    a = np.asfortranarray(np.zeros((2, 3), dtype=np.float32))
    b = np.asfortranarray(np.ones((2, 3), dtype=np.float32))
    existing = FieldSlab(
        name="SOILT000",
        units="K".ljust(25),
        desc="x",
        level=lev,
        data=a,
    )
    fld = FieldSlab(
        name="SOILT001",
        units="K".ljust(25),
        desc="y",
        level=lev,
        data=b,
    )
    bucket = {(lev, "SOILT000"): existing, (lev, "SOILT001"): fld}
    ensure_soilt000_for_metgrid(bucket)
    assert bucket[(lev, "SOILT000")] is existing


def test_ensure_soilt000_noop_without_soilt():

    # This function checks that buckets without SOILT* fields are unchanged.

    bucket: dict[tuple[float, str], FieldSlab] = {
        (200100.0, "TT"): FieldSlab(
            name="TT",
            units="K".ljust(25),
            desc="t",
            level=100000.0,
            data=np.asfortranarray(np.ones((2, 3), dtype=np.float32)),
        )
    }
    ensure_soilt000_for_metgrid(bucket)
    assert len(bucket) == 1
