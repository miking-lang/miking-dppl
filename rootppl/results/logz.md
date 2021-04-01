Some results on the stability of log Z
========================================================
author: Viktor
date: 3/25/2021
width: 1440
height: 900


Experiment 1 Setup
========================================================

- CUDA on moth tree (NUM_NODES = 2411; MAX_DEPTH = 41;)
- 100,000 particles x 20 runs
- 8.2 GB of GPU memory with stack size 1024 (8.4 GB total)
- Total running time real	68m47.187s ~ 3.5 min per run
- Priors based on delayed sampling with 200,000 particles

```
  floating_t lambda_0 = 0.2;
  floating_t alpha    = 0.8;
  floating_t sigma    = 0.5;
  floating_t epsilon  = 0.9;
  floating_t rho      = 1.0;
```
***


```r
mean(logz)
```

```
[1] -11170.75
```

```r
var(logz)
```

```
[1] 4.512079
```

```r
sd(logz)
```

```
[1] 2.124165
```

Evaluation Experiment 1
========================================================

- Variance still high
- TODO Try lowering sigma (Experiment 2)
- TODO Try increasing the number of particles (Experiment 3)

Experiment 2 Setup
========================================================

- CUDA on moth tree (NUM_NODES = 2411; MAX_DEPTH = 41;)
- 100,000 particles x 20 runs
- 8.2 GB of GPU memory with stack size 1024 (8.4 GB total)
- Total running time real 12m4.850s ~ 0.6 min per run
- Lowered sigma a lot to simulate fixed initial lambdas

```
  floating_t lambda_0 = 0.2;
  floating_t alpha    = 0.8;
  floating_t sigma    = 0.0005;
  floating_t epsilon  = 0.9;
  floating_t rho      = 1.0;
```
***


```r
mean(logz)
```

```
[1] -13526.69
```

```r
var(logz)
```

```
[1] 0.007868421
```

```r
sd(logz)
```

```
[1] 0.08870412
```

Evaluation Experiment 2
========================================================

- variance low enough
- running time much lower

Experiment 3 Setup
========================================================

- CUDA on moth tree (NUM_NODES = 2411; MAX_DEPTH = 41;)
- 400,000 particles x 5 runs
- 8.2 GB of GPU memory with stack size 256 (8.4 GB total)
- Total running time real	21m44.526s ~ 4.4 min per run

```
  floating_t lambda_0 = 0.2;
  floating_t alpha    = 0.8;
  floating_t sigma    = 0.5;
  floating_t epsilon  = 0.9;
  floating_t rho      = 1.0;
```
***


```r
mean(logz)
```

```
[1] -11168.82
```

```r
var(logz)
```

```
[1] 1.937
```

```r
sd(logz)
```

```
[1] 1.391761
```

Evaluation Experiment 3
========================================================

- Variance lower but still high
- Maybe possible to lower under one by running around 1,000,000 particles;
  however, only possible on cluster
- TODO Try for a smaller tree


Experiment 4
========================================================

- CUDA on primate tree (NUM_NODES = 465; MAX_DEPTH = 18;)
- 100,000 x 20
- real	3m43.239s ~ 18 s per run
- Delayed sampling did not work due to negative branch lengths. Used the same estimates from previous experiments.

```
    floating_t lambda_0 = 0.2;
    floating_t alpha    = 0.8;
    floating_t sigma    = 0.5;
    floating_t epsilon  = 0.9;
    floating_t rho      = 1.0;
```

***


```r
mean(logz)
```

```
[1] -1609.056
```

```r
var(logz)
```

```
[1] 0.1281924
```

```r
sd(logz)
```

```
[1] 0.3580396
```

Evaluation Experiment 4
========================================================

- var log z low enough
- TODO try even less particles


Experiment 5
========================================================

- CUDA on primate tree (NUM_NODES = 465; MAX_DEPTH = 18;)
- 10,000 x 20
- 0m12.586s ~ 0.65 s per run
- variance still OK!

```
    floating_t lambda_0 = 0.2;
    floating_t alpha    = 0.8;
    floating_t sigma    = 0.5;
    floating_t epsilon  = 0.9;
    floating_t rho      = 1.0;
```

***


```r
mean(logz)
```

```
[1] -1609.776
```

```r
var(logz)
```

```
[1] 0.8648253
```

```r
sd(logz)
```

```
[1] 0.9299598
```
