#ifndef DISTRIBUTIONS_INCLUDED
#define DISTRIBUTIONS_INCLUDED

#include <random>
#include <time.h>
const double PI = 3.1415926535897932384626433832795028841971693993751;

#ifdef GPU
#include <curand_kernel.h>
#endif

#include "../../Inference/Smc/smc.cuh"
#include "../misc.cuh"


default_random_engine generatorDists;

uniform_real_distribution<floating_t> uniformDist(0.0, 1.0);
normal_distribution<floating_t> normalDist(0, 1);
// gamma_distribution<floating_t> gammaDist(1.0, 1.0);
// exponential_distribution<floating_t> exponentialDist(2);

void initGen() {
    generatorDists.seed(time(NULL));
}

#ifdef GPU

#include "distributionsGPU.cuh"

#else

#include "distributionsCPU.cuh"

#endif


// Could be optimized
// Log of normal pdf
HOST DEV floating_t logPDFNormal(floating_t x, floating_t mean, floating_t std) {
    return log(exp(-pow(x - mean, 2) / (std * std)) / (std * sqrt(2 * PI)));
}

DEV int bernoulli(RAND_STATE_DECLARE floating_t p) {
    return uniform(RAND_STATE_ACCESS 0, 1) < p ? 1 : 0;
}

DEV floating_t exponential(RAND_STATE_DECLARE floating_t lambda) {
    
    return -log(1 - uniform(RAND_STATE_ACCESS 0, 1)) / lambda;
}


// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
DEV floating_t gamma(RAND_STATE_DECLARE floating_t k, floating_t theta) {

    if (k < 1) {
        double u = uniform(RAND_STATE_ACCESS 0, 1);
        return gamma(RAND_STATE_ACCESS 1.0 + k, theta) * pow(u, 1.0 / k);
    }
    
    {
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = normal(RAND_STATE_ACCESS 0, 1);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = uniform(RAND_STATE_ACCESS 0, 1);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        return theta * d * v;
    }
    
}

DEV void dirichlet(RAND_STATE_DECLARE const floating_t* alpha, floating_t* ret, const int n) {
    
    floating_t sum = 0;
    for (int k = 0; k < n; k++) {
        ret[k] = gamma(RAND_STATE_ACCESS alpha[k], 1);
        sum += ret[k];
    }

    for (int k = 0; k < n; k++) 
        ret[k] /= sum;
}

// Could be optimized, if many samples should be done
DEV int discrete(RAND_STATE_DECLARE const floating_t* dist, const int n) {
    floating_t u = uniform(RAND_STATE_ACCESS 0, 1);
    floating_t sum = 0;
    int idx = 0;
    for(idx = 0; idx < n-1; idx++) {
        sum += dist[idx];
        if(u <= sum)
            break;
    }
    return idx;
}

template <typename T2>
DEV T2 categorical(RAND_STATE_DECLARE const floating_t* dist, const int n, T2* arr) {
    int idx = discrete(RAND_STATE_ACCESS dist, n);
    return arr[idx];
}

// returns integer [0, n)
DEV int randomInteger(RAND_STATE_DECLARE const int n) {
    floating_t u = uniform(RAND_STATE_ACCESS 0, n) - 0.000000000000001;
    return static_cast<int>(u);
}

DEV floating_t beta(RAND_STATE_DECLARE floating_t a, floating_t b) {
    floating_t x = gamma(RAND_STATE_ACCESS a, 1);
    floating_t y = gamma(RAND_STATE_ACCESS b, 1);
    return x / (x + y);
}

// Could be done more efficiently, see alias methods or webppl source code
DEV int binomial(RAND_STATE_DECLARE floating_t p, int n) {
    int numSuccesses = 0;
    for(int t = 0; t < n; t++)
        numSuccesses += bernoulli(RAND_STATE_ACCESS p);

    return numSuccesses;
}

DEV floating_t cauchy(RAND_STATE_DECLARE floating_t loc, floating_t scale) {
    return loc + scale * tan(PI * (uniform(RAND_STATE_ACCESS 0, 1) - 0.5));
}

DEV floating_t laplace(RAND_STATE_DECLARE floating_t loc, floating_t scale) {
    floating_t u = uniform(RAND_STATE_ACCESS -0.5, 0.5);
    return loc - scale * sgn(u) * log(1 - 2 * abs(u));
}

// Can be much more efficient for large n, using host API on GPU (alias methods)
// Just sorting probabilities in descending order gives performance increase for many samples
DEV void multinomial(RAND_STATE_DECLARE floating_t* ps, int n, int* returnArr, int numCategories) {
    
    for(int k = 0; k < numCategories; k++)
        returnArr[k] = 0;

    for(int t = 0; t < n; t++) {
        int idx = discrete(RAND_STATE_ACCESS ps, numCategories);
        returnArr[idx]++;
    }
}

// Could be optimized with curand2 or curand4 that samples more than one value at a time.
DEV void diagCovNormal(RAND_STATE_DECLARE floating_t* mu, floating_t* sigma, int n, floating_t* ret) {
    for(int k = 0; k < n; k++) {
        ret[k] = normal(RAND_STATE_ACCESS mu[k], sigma[k]);
    }
}

DEV void multivariateStandardNormal(RAND_STATE_DECLARE floating_t* ret, int n) {
    for(int k = 0; k < n; k++) {
        ret[k] = normal(RAND_STATE_ACCESS 0, 1);
    }
}

template <size_t n>
DEV void multivariateNormal(RAND_STATE_DECLARE floating_t mu[n], floating_t (&cov)[n][n], floating_t ret[n]) {
    floating_t A[n][n];
    choleskyDecomposition<n>(cov, A);
    floating_t z[n];
    multivariateStandardNormal(RAND_STATE_ACCESS z, n);
    transformColumn<n, n>(A, z, mu, ret);
}

#endif