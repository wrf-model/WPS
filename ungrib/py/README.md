# Python ungrib (`ungrib/py`)

Parallel Python port of WPS **ungrib**: GRIB2 → WPS intermediate v5 for **metgrid**, with clearer structure than `ungrib/src/` Fortran.

## Fortran ungrib (reference)

| Piece | Role |
|-------|------|
| `ungrib.F` | Main: `GRIBFILE.*` loop, storage, `output`, `rrpr` / `datint` |
| `read_namelist.F` | `namelist.wps` (`&share`, `&ungrib`) |
| `parse_table.F`, `table.F` | **Vtable** → WPS names / levels |
| `rd_grib1.F`, `rd_grib2.F` | Decode GRIB, match Vtable, grid info |
| `output.F` | WPS intermediate (v5 when `out_format='WPS'`) |

## This package (`ungrib_py`)

- One worker per **`GRIBFILE.*`**; parent merges by validity time; one **`prefix:YYYY-MM-DD_HH`** file per step (hourly pattern matches Fortran).
- **GRIB2 only** via **ecCodes** (`pip install eccodes`; OS `libeccodes` often required).
- Scope: Vtable-driven extraction and v5 I/O for common grids—not full Fortran parity (`rrpr`/`datint`/GRIB1 edge cases, etc.). Details: **`MIGRATION.md`**.

## Quick start

```bash
pip install --user -r /path/to/WPS/ungrib/py/requirements.txt
ln -sf /path/to/WPS/ungrib/py/ungrib.py ./ungrib.py && chmod +x ./ungrib.py
./ungrib.py --workers 16
```

Run from the directory containing `namelist.wps`, `Vtable`, and `GRIBFILE.AAA`, … Optional: `pip install /path/to/WPS/ungrib/py` for the `ungrib-py` console script.

## Layout

| Path | Role |
|------|------|
| `ungrib.py` | Launcher (`sys.path` + CLI); symlink like `ungrib.exe` |
| `ungrib_py/vtable.py` | Vtable parse / GRIB2 match |
| `ungrib_py/grib2.py` | ecCodes scan, levels, `extract_file` |
| `ungrib_py/intermediate.py` | WPS v5 writer |
| `ungrib_py/namelist_wps.py` | `&share`, `&ungrib` |
| `ungrib_py/parallel_ungrib.py` | Pool + merge + write |
| `ungrib_py/cli.py` | CLI |
| `tests/` | `pytest` |
