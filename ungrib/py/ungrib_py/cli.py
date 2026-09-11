# Command-line interface.

from __future__ import annotations

import argparse
import os
from pathlib import Path

from ungrib_py.namelist_wps import parse_namelist_wps
from ungrib_py.parallel_ungrib import run_ungrib_parallel


def main(argv: list[str] | None = None) -> int:

    # This function parses CLI options, reads namelist.wps, and runs parallel ungrib.

    p = argparse.ArgumentParser(
        description="Parallel Python ungrib: GRIB2 -> WPS intermediate (v5)."
    )
    p.add_argument(
        "--namelist",
        default="namelist.wps",
        help="Path to namelist.wps (default: ./namelist.wps)",
    )
    p.add_argument(
        "--vtable",
        default="Vtable",
        help="Path to Vtable (default: ./Vtable)",
    )
    p.add_argument(
        "--workers",
        type=int,
        default=None,
        help="Process pool size (default: min(32, CPU count))",
    )
    p.add_argument(
        "--cwd",
        default=".",
        help="Working directory for GRIBFILE.* and outputs (default: .)",
    )
    args = p.parse_args(argv)
    cwd = Path(args.cwd).resolve()
    namelist_path = Path(args.namelist)
    if not namelist_path.is_absolute():
        namelist_path = cwd / namelist_path
    vtable_path = Path(args.vtable)
    if not vtable_path.is_absolute():
        vtable_path = cwd / vtable_path
    os.chdir(cwd)
    nml = parse_namelist_wps(namelist_path)
    written = run_ungrib_parallel(
        cwd,
        nml,
        vtable_path,
        max_workers=args.workers,
    )
    print(f"Wrote {len(written)} intermediate file(s).")
    for w in written:
        print(" ", w)
    return 0





