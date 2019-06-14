#!/usr/bin/python3

# Simple script for plotting a histogram given a line-separated list of values
# on standard input.

import matplotlib.pyplot as plt
import sys

x = []

for line in sys.stdin:
    x.append(line)

# If everything is numerical, make it a numerical plot
try:
    x = [float(e) for e in x]
except ValueError:
    pass

plt.hist(x,bins='auto',density=True)
plt.show()
