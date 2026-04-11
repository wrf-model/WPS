# Migration plan: Fortran ungrib → Python (`ungrib_py`)

This document lists tasks to reach feature parity with `ungrib.exe`, notes what is already done in `ungrib_py`, and explains how to test the Python code.

## Task outline

### Phase A — Core decode and output (done in this port)

1. **Vtable parsing** — Port `parse_table.F` logic for GRIB2 columns (discipline, category, parameter, first fixed surface / level type, product definition template). *Status: implemented for standard 11–12 column Vtables.*
2. **GRIB2 read path** — Replace `rd_grib2.F` + NCEP g2lib with **ecCodes** message iteration. *Status: implemented for common level types (pressure, mean sea level, height above ground, surface, soil 106, etc.) aligned with `rd_grib2.F`.*
3. **Grid metadata** — Port `gridinfo` population from GRIB2 Section 3 (lat/lon, Lambert, polar stereographic, Mercator). *Status: lat/lon (`regular_ll`) and Lambert/polar/Mercator via ecCodes `gridType`.*
4. **WPS intermediate writer** — Port `output.F` records for `out_format='WPS'`, version **5** (matches `metgrid/src/read_met_module.F`). *Status: implemented.*

### WPS v5 file structure (metgrid)

Metgrid reads each time-varying field as **five** Fortran sequential unformatted records: version `5`; **156-byte** mixed header (`hdate`, `xfcst`, `map_source`, `field`, `units`, `desc`, `xlvl`, `nx`, `ny`, `iproj`); projection extension (length depends on `iproj` / `map%igrid`); **4-byte** grid-relative wind flag; **`nx*ny*4`** bytes of big-endian `real32` slab data in Fortran order. The Python writer documents this in `ungrib_py/intermediate.py` and asserts payload sizes so the **layout stays aligned** with Fortran WPS builds that use gfortran **big-endian** unformatted I/O. **Floating-point values** may differ slightly from Fortran (ecCodes vs g2lib); **structure** (record order, lengths, endianness) is what metgrid relies on.
5. **namelist.wps** — Read `start_date` / `end_date`, `interval_seconds`, `prefix`, `out_format`, `ordered_by_date`, `pmin`. *Status: minimal reader; many `&ungrib` options still ignored.*

### Phase B — Parallelism and workflow (done in this port)

6. **Parallel file processing** — One worker per `GRIBFILE.*`; merge fields by validity time; write one output file per timestep in range. *Status: implemented (`parallel_ungrib.py`).*
7. **Configurable worker count** — CLI `--workers` (recommended 8–32 depending on filesystem). *Status: implemented.*

### Phase C — Parity gaps (future work)

8. **GRIB1** — Vtable GRIB1 columns and ECMWF-style `typeOfLevel` matching in `extract_file`. *Implemented for common cases; edge cases may differ from `rd_grib1.F`.*
9. **`rrpr` / `datint` / `PFILE:` pipeline** — Second pass, temporal interpolation, temp file handling from `ungrib.F`. *Not started; Python tool writes final `FILE:` directly from merged messages.*
10. **Edge cases** — JMA per-field grid refresh, Gaussian grid as `igrid=4`, Cassini `igrid=6`, UM soil specials, `ec_rec_len` EC paths, `add_lvls` / vertical interpolation. *Partial or not started.*
11. **Regression harness** — Automated byte-compare or field-compare against `ungrib.exe` on a reference dataset. *Not started; see testing below.*

## How to test the Python code

### 1. Unit tests (no GRIB files)

From `ungrib/py` with a virtualenv and dependencies installed:

```bash
cd ungrib/py
pip install -r requirements.txt
pytest tests/ -q
```

The `tests/conftest.py` file prepends the `ungrib/py` directory to `sys.path`, so `pytest` finds the `ungrib_py` package without setting `PYTHONPATH`.

These cover Vtable parsing, namelist parsing, intermediate-file record layout, and pure-Python helpers.

### 2. Integration test (with GRIB2 + ecCodes)

1. Build or obtain WPS `namelist.wps` and link `Vtable` (e.g. `Vtable.GFS`) in a run directory.
2. Link or copy `GRIBFILE.AAA`, … as for the Fortran workflow.
3. Run (no `PYTHONPATH` if you use the launcher):

   ```bash
   cd your_run_directory
   ln -sf /path/to/WPS/ungrib/py/ungrib.py ./ungrib.py
   chmod +x ./ungrib.py
   ./ungrib.py --workers 8
   ```

4. Compare outputs:
   - **Inventory**: Run WPS `rd_intermediate.exe` (from a full WPS build) on both Fortran and Python `FILE:` outputs and compare field lists and headers.
   - **Numerical**: For a few fields, use Python or NCL to read slabs (same shape) and compare max abs error; expect small differences if bitmap/missing-value handling differs.

### 3. Optional: ecCodes sanity check

```bash
python -c "import eccodes; print('eccodes OK')"
```

If import fails, install the system ecCodes library (e.g. `libeccodes-dev` on Debian/Ubuntu) before `pip install eccodes`.

## Success criteria (practical)

- For a **GRIB2 lat-lon** (e.g. GFS on a regular grid) run with a standard Vtable, `ungrib_py` produces intermediate files that **metgrid accepts** and that list the same WPS field names and levels as Fortran ungrib for the same inputs.
- Wall time improves when using **multiple workers** and **multiple `GRIBFILE.*` chunks**, subject to storage bandwidth.
