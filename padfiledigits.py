#! /usr/bin/env python3

import sys
import re

assert len(sys.argv) == 2, "Must have exactly two arguments."

filename = sys.argv[1]

m = re.search(r'\d+', filename)
newfilename = re.sub(r'\d+', m.group(0).rjust(2, "0"), filename)
print(newfilename)
