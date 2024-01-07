#! /usr/bin/env python3

# Prepend a + to the file named page.svelte contained in the parent directory
# specified as the argument to this script

import sys
import re

from pathlib import Path

assert len(sys.argv) == 2, "Must have exactly one argument, the parent dir name"

folder = Path(sys.argv[1])
f = "page.svelte"
p = folder / f

p.rename(str(folder) + "/" + "+" + f)
