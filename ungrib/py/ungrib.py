#!/usr/bin/env python3

# Add sibling package ungrib_py to sys.path when this file is run or symlinked (Fortran-style workflow).

import sys
from pathlib import Path

_root = Path(__file__).resolve().parent
if str(_root) not in sys.path:
    sys.path.insert(0, str(_root))

from ungrib_py.cli import main

if __name__ == "__main__":
    raise SystemExit(main())








