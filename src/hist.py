#!/usr/bin/python3

import matplotlib.pyplot as plt
import sys

def readline():
    return sys.stdin.readline().rstrip()

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
