import numpy as np

from ungrib_py.intermediate import FieldSlab
from ungrib_py.parallel_ungrib import count_distinct_pressure_levels_pa


def test_count_distinct_pressure_levels_excludes_surface():

    # This function checks that 200100-style codes are not counted as isobaric levels.

    slab = FieldSlab(
        name="TT",
        units="K".ljust(25),
        desc="t",
        level=85000.0,
        data=np.asfortranarray(np.ones((2, 3), dtype=np.float32)),
    )
    sfc = FieldSlab(
        name="TT",
        units="K".ljust(25),
        desc="2m",
        level=200100.0,
        data=np.asfortranarray(np.ones((2, 3), dtype=np.float32)),
    )
    merged = {
        "2025-07-13_00:00:00": {
            (85000.0, "TT"): slab,
            (70000.0, "UU"): slab,
            (200100.0, "TT"): sfc,
        }
    }
    assert count_distinct_pressure_levels_pa(merged) == 2
