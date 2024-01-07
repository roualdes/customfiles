#! /usr/bin/env python3

# Remove the + from each of the sub-directories files' named +page.svelte, where
# the argument contains the sub-directories

import sys
import re

from pathlib import Path

assert len(sys.argv) == 2, "Must have exactly one argument, the parent^2 dir name"

folder = Path(sys.argv[1])

for subdir in folder.iterdir():
    if subdir.is_dir():
        for child in subdir.iterdir():
            p = child.resolve()
            p.rename(str(p.parent) + "/" + p.name.replace("+", ""))
