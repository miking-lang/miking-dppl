# Running and Plotting Examples

To compile and build examples run (assuming a `cppl` executable in `<project-root>/build`)
```
make
```

This will create plot-data for each example in the form of a `json` file. To
plot these run (assumes `python` in path together with the numpy and pandas
packages.)
```
make plot
```

Finally,
```
make clean
```

Clean generated data files and executables from this directory.
