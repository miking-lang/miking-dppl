#ifndef DISTRIBUTIONS_INCLUDED
#define DISTRIBUTIONS_INCLUDED

// #define _USE_MATH_DEFINES

#include <random>
#include <time.h>
// #include <cmath>
// Optimize!
// constexpr double pi() { return std::atan(1)*4; }
const double PI = 3.1415926535897932384626433832795028841971693993751;

#ifdef GPU
#include <curand_kernel.h>
#endif

#include "../Inference/Smc/smc.cuh"
#include "misc.cuh"
// #include "../macros.cuh"

default_random_engine generatorDists;

uniform_real_distribution<floating_t> uniformDist(0.0, 1.0);
normal_distribution<floating_t> normalDist(0, 1);
// gamma_distribution<floating_t> gammaDist(1.0, 1.0);
// exponential_distribution<floating_t> exponentialDist(2);

void initGen() {
    generatorDists.seed(time(NULL));
}

#ifdef GPU

template <typename T>
__device__ int sampleBernoulli(particles_t<T>* particles, int i, floating_t p = 0.5) {
    return curand_uniform(&particles->randStates[i]) < p ? 1 : 0;
}

template <typename T> // (min, max]
__device__ floating_t sampleUniform(particles_t<T>* particles, int i, floating_t min, floating_t max) {
    return (curand_uniform(&particles->randStates[i]) * (max - min)) + min;
}

template <typename T> 
__device__ floating_t sampleNormal(particles_t<T>* particles, int i, floating_t mean, floating_t std) {
    return (curand_normal(&particles->randStates[i]) * std) + mean;
}

template <typename T>
__device__ floating_t sampleExponential(particles_t<T>* particles, int i, floating_t lambda) {
    
    // particles->randStates[i];
    //curandState localState = particles->randStates[i];
    //curand_uniform(&localState);
    //floating_t u = curand_uniform(&localState);
    // particles->randStates[i] = localState;
    return -log(1 - curand_uniform(&particles->randStates[i])) / lambda;
}

// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
template <typename T>
__device__ floating_t sampleGamma(particles_t<T>* particles, int i, floating_t k, floating_t theta) {

    /* assume a > 0 */
    
    if (k < 1) {
        double u = curand_uniform(&particles->randStates[i]);
        return sampleGamma(particles, i, 1.0 + k, theta) * pow (u, 1.0 / k);
    }
    
    {
        curandState localState = particles->randStates[i];
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = curand_normal(&localState);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = curand_uniform(&localState);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        particles->randStates[i] = localState;
        return theta * d * v;
    }
    

    // return exponential(particles, i, 1.0); // exponential(1) is special case of gamma: gamma(1, 1)
}

template <typename T>
__device__ unsigned int samplePoisson(particles_t<T>* particles, int i, double lambda) {
    return curand_poisson(&particles->randStates[i], lambda);
}

#else

template <typename T> // [min, max)
floating_t sampleUniform(particles_t<T>* particles, int i, floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

template <typename T>
floating_t sampleNormal(particles_t<T>* particles, int i, floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}

template <typename T>
int flipK(particles_t<T>* particles, int i, floating_t p = 0.5) {
    
    return uniformDist(generatorDists) < p ? 1 : 0;
}

template <typename T>
floating_t sampleExponential(particles_t<T>* particles, int i, floating_t lambda) {
    
    // return exponentialDist(generatorDists);
    return -log(1 - uniformDist(generatorDists)) / lambda;
}

// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
template <typename T>
floating_t sampleGamma(particles_t<T>* particles, int i, floating_t k, floating_t theta) {

    /* assume a > 0 */
    
    if (k < 1) {
        double u = uniformDist(generatorDists);
        return sampleGamma(particles, i, 1.0 + k, theta) * pow(u, 1.0 / k);
    }
    
    {
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = normalDist(generatorDists);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = uniformDist(generatorDists);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        return theta * d * v;
    }
    
}

template <typename T>
int sampleBernoulli(particles_t<T>* particles, int i, double p = 0.5) {
    return uniformDist(generatorDists) < p ? 1 : 0;
}

// Reuse distribution object when possible? However, does not seem to be that expensive to create dist object
template <typename T>
unsigned int samplePoisson(particles_t<T>* particles, int i, double lambda) {
    std::poisson_distribution<unsigned int> poissonDist(lambda);
    return poissonDist(generatorDists);
}


#endif



// Could be optimized
// Log of normal pdf
HOST DEV
floating_t logPDFNormal(floating_t x, floating_t mean, floating_t std) {
    return log(exp(-pow(x - mean, 2) / (std * std)) / (std * sqrt(2 * PI)));
}

template <typename T>
HOST DEV void sampleDirichlet(particles_t<T>* particles, int i, const floating_t* alpha, floating_t* ret, const int n) {
    
    floating_t sum = 0;
    for (int k = 0; k < n; k++) {
        ret[k] = sampleGamma(particles, i, alpha[k], 1);
        sum += ret[k];
    }

    for (int k = 0; k < n; k++) 
        ret[k] /= sum;
}

// Could be optimized, if many samples should be done
template <typename T>
HOST DEV int sampleDiscrete(particles_t<T>* particles, int i, const floating_t* dist, const int n) {
    floating_t u = sampleUniform(particles, i, 0, 1);
    floating_t sum = 0;
    int idx = 0;
    for(idx = 0; idx < n-1; idx++) {
        sum += dist[idx];
        if(u <= sum)
            break;
    }
    return idx;
}

// returns integer [0, n)
template <typename T>
HOST DEV int sampleRandomInteger(particles_t<T>* particles, int i, const int n) {
    floating_t u = sampleUniform(particles, i, 0, n) - 0.000000000000001;
    return static_cast<int>(u);
}

template <typename T>
HOST DEV floating_t sampleBeta(particles_t<T>* particles, int i, floating_t a, floating_t b) {
    floating_t x = sampleGamma(particles, i, a, 1);
    floating_t y = sampleGamma(particles, i, b, 1);
    return x / (x + y);
}

// Could be done more efficiently, see alias methods or webppl source code
template <typename T>
HOST DEV floating_t sampleBinomial(particles_t<T>* particles, int i, floating_t p, int n) {
    int numSuccesses = 0;
    for(int t = 0; t < n; t++)
        numSuccesses += sampleBernoulli(particles, i, p);

    return numSuccesses;
}

template <typename T>
HOST DEV floating_t sampleCauchy(particles_t<T>* particles, int i, floating_t loc, floating_t scale) {
    return loc + scale * tan(PI * (sampleUniform(particles, i, 0, 1) - 0.5));
}

template <typename T>
HOST DEV floating_t sampleLaplace(particles_t<T>* particles, int i, floating_t loc, floating_t scale) {
    floating_t u = sampleUniform(particles, i, -0.5, 0.5);
    return loc - scale * sgn(u) * log(1 - 2 * abs(u));
}

// Can be much more efficient for large n, using host API on GPU (alias methods)
// Just sorting probabilities in descending order gives performance increase for many samples
template <typename T>
HOST DEV void sampleMultinomial(particles_t<T>* particles, int i, floating_t* ps, int n, int* returnArr, int numCategories) {
    
    for(int k = 0; k < numCategories; k++)
        returnArr[k] = 0;

    for(int t = 0; t < n; t++) {
        int idx = sampleDiscrete(particles, i, ps, numCategories);
        returnArr[idx]++;
    }
}

#endif