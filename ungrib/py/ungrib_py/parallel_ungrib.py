# Parallel GRIB-file processing and WPS intermediate output.

from __future__ import annotations

import os
from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path

from ungrib_py.grib2 import (
    drop_fields_blank_vtable_desc,
    ensure_hgt_from_geopt,
    ensure_soilhgt_from_soilgeo,
    ensure_soilt000_for_metgrid,
    extract_file,
    iter_grib_filenames,
)
from ungrib_py.vtable import VtableEntry
from ungrib_py.intermediate import (
    FieldSlab,
    MapInfo,
    hdate_slice,
    intermediate_filename_datelen,
    sort_fields_for_output,
    write_wps_intermediate_v5,
)
from ungrib_py.namelist_wps import UngribNamelist
from ungrib_py.vtable import parse_vtable

def discover_grib_files(cwd: Path) -> list[str]:

    # This function lists GRIBFILE.AAA-style names that exist under cwd.

    return [name for name in iter_grib_filenames() if (cwd / name).is_file()]

def _worker_extract(
    args: tuple[str, str, str, float],
) -> tuple[str, dict, MapInfo | None]:

    # This function runs extract_file for one GRIB path inside a worker process.

    cwd_s, fname, vtable_path, pmin = args
    cwd = Path(cwd_s)
    rows, _ = parse_vtable(vtable_path)
    data, mi = extract_file(cwd / fname, rows, pmin=pmin)
    return fname, data, mi

def count_distinct_pressure_levels_pa(
    merged: dict[str, dict[tuple[float, str], FieldSlab]],
) -> int:

    # This function counts distinct isobaric levels in Pa (WPS convention), excluding surface/special codes.

    levels: set[float] = set()
    for bucket in merged.values():
        for lev, _name in bucket:
            lf = float(lev)
            if 50.0 <= lf <= 120000.0:
                levels.add(round(lf, 1))
    return len(levels)

def merge_by_time(
    parts: list[tuple[dict, MapInfo | None]],
    hstart: str,
    hend: str,
    all_vtable_rows: list[VtableEntry],
) -> tuple[dict[str, dict[tuple[float, str], FieldSlab]], MapInfo]:

    # This function merges worker dicts by hdate, filters the time window, and checks the grid.

    merged: dict[str, dict[tuple[float, str], FieldSlab]] = {}
    map_ref: MapInfo | None = None
    for blob, mi in parts:
        if mi is not None:
            if map_ref is None:
                map_ref = mi
            elif (mi.nx, mi.ny, mi.igrid) != (map_ref.nx, map_ref.ny, map_ref.igrid):
                raise ValueError(
                    "Inconsistent grid dimensions or projection between GRIB inputs."
                )
        for hdate, fields in blob.items():
            if hdate < hstart or hdate > hend:
                continue
            bucket = merged.setdefault(hdate, {})
            bucket.update(fields)
    if map_ref is None:
        raise ValueError("No GRIB2 fields matched the Vtable in the given date range.")
    for bucket in merged.values():
        ensure_soilt000_for_metgrid(bucket)
        ensure_soilhgt_from_soilgeo(bucket)
        ensure_hgt_from_geopt(bucket, vtable_rows=all_vtable_rows)
        drop_fields_blank_vtable_desc(bucket, all_vtable_rows)
    return merged, map_ref

def run_ungrib_parallel(
    cwd: Path,
    nml: UngribNamelist,
    vtable_path: Path,
    *,
    grib_files: list[str] | None = None,
    max_workers: int | None = None,
) -> list[str]:

    # This function processes GRIB files in parallel and writes WPS intermediate v5 outputs.

    if nml.out_format[:2] != "WP":
        raise NotImplementedError(
            f"out_format={nml.out_format!r}: only WPS intermediate (version 5) is implemented."
        )
    files = grib_files if grib_files is not None else discover_grib_files(cwd)
    if not files:
        raise FileNotFoundError(
            "No GRIBFILE.AAA-style inputs found in working directory."
        )
    vtable_abs = str(vtable_path.resolve())
    vtable_rows, _ = parse_vtable(vtable_path)
    workers = max_workers or min(32, max(1, (os.cpu_count() or 4)))
    tasks = [(str(cwd.resolve()), fn, vtable_abs, float(nml.pmin)) for fn in files]
    results: list[tuple[dict, MapInfo | None]] = []
    with ProcessPoolExecutor(max_workers=workers) as ex:
        futs = [ex.submit(_worker_extract, t) for t in tasks]
        for fut in as_completed(futs):
            _fname, data, mi = fut.result()
            results.append((data, mi))
    merged, map_info = merge_by_time(results, nml.hstart, nml.hend, vtable_rows)
    n_pressure = count_distinct_pressure_levels_pa(merged)
    print(
        "ungrib_py: distinct isobaric levels in intermediate output: "
        f"{n_pressure} (set num_metgrid_levels = {n_pressure} in namelist.input &domains "
        "to match met_em / BOTTOM-TOP_GRID_DIMENSION)"
    )
    datelen = intermediate_filename_datelen(nml.interval_seconds)
    written: list[str] = []
    for hdate in sorted(merged.keys()):
        items = [(k[0], k[1], v) for k, v in merged[hdate].items()]
        fields = sort_fields_for_output(items, vtable_rows)
        out_name = f"{nml.prefix}:{hdate_slice(hdate, datelen)}"
        out_path = cwd / out_name
        write_wps_intermediate_v5(str(out_path), hdate, map_info, fields)
        written.append(str(out_path))
    return written










