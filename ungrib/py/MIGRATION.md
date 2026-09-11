# Fortran ungrib → `ungrib_py`

## Done here

- Vtable (GRIB2 columns), ecCodes GRIB2 scan, common level types vs `rd_grib2.F`.
- Grids: `regular_ll`, Lambert, polar stereographic, Mercator, **NCEP GDT 3.32769** (`ncep_32769`, WPS `igrid=6`).
- WPS intermediate **v5** (`out_format` WPS) aligned with `metgrid/src/read_met_module.F`.
- Parallel **GRIBFILE.*** workers, merge by time, CLI **`--workers`**.
- Minimal **`namelist.wps`** (`start`/`end`, `interval_seconds`, `prefix`, `pmin`, …); many `&ungrib` options still ignored.

## Not done / gaps

- **`rrpr` / `datint` / `PFILE:`** second pass — Python writes final `FILE:` from merged messages.
- **GRIB1** — partial; may differ from `rd_grib1.F` on edge cases.
- **Other gaps**: JMA per-field grid refresh, Gaussian `igrid=4`, UM soil specials, `add_lvls`, EC record quirks, regression harness vs `ungrib.exe`.

## Test

```bash
cd ungrib/py && pip install -r requirements.txt && pytest tests/ -q
```

Integration: same `namelist.wps` / `Vtable` / `GRIBFILE.*` as Fortran; compare with `rd_intermediate.exe` or slab diffs.

## Practical success

Same field names/levels and **metgrid-acceptable** v5 files as Fortran for the same GRIB2 + Vtable on supported grids; speedup when I/O allows multiple workers and split inputs.
