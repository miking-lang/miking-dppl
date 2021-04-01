Verification of RootPPL on the Bird Trees
========================================================
author: Viktor Senderov
date: 01 Apri 2021
width: 1920
height: 1080




What I tested?
========================================================


```r
trees
```

```
[1] "Accipitridae" "BC7"          "P20b"         "TitTyranRest"
```

```r
num_nodes
```

```
[1] 349 179 249 631
```

```r
depth
```

```
[1] 30 14 15 22
```
- 10,000 and 100,000 particles
- Comparison to Birch results

Models and priors
========================================================

Models: 


```
[1] "clads2-d-<U+03BB>"    "clads2-d-<U+03BB>µ"   "clads2-d-<U+03BB>µas"
[4] "clads2"               "crbd-d-<U+03BB>"      "crbd-d-<U+03BB>µ"    
[7] "crbd"                 "birch-clads2"         "birch-crbda"         
```

- Survivorship bias correction for CRBD not implemented in RootPPL

Priors:

```
floating_t kLambda  = 1.0;
floating_t thetaLambda = 1.0;
floating_t epsMin = 0.0;
floating_t epsMax = 1.0;
floating_t kMu  = 1.0;
floating_t thetaMu = 0.5;
floating_t a = 1.0;
floating_t b = 0.2;
```

- <U+03C1> as needed.

Accipitridae (10,000)
========================================================

































```
Error in file(con, "r") : cannot open the connection
```
