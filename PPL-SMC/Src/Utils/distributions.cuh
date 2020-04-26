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

default_random_engine generatorDists;

uniform_real_distribution<floating_t> uniformDist(0.0, 1.0);
normal_distribution<floating_t> normalDist(0, 1);
gamma_distribution<floating_t> gammaDist(1.0, 1.0);
// exponential_distribution<floating_t> exponentialDist(2);

void initGen() {
    generatorDists.seed(time(NULL));
}

#ifdef GPU

template <typename T>
__device__ int flipK(particles_t<T>* particles, int i, floating_t p = 0.5) {
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
    

    // return gammaDist(generatorDists);
}

int flip(double p = 0.5) {
    return uniformDist(generatorDists) < p ? 1 : 0;
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

// Could prolly be optimized
template <typename T>
HOST DEV int sampleCategorical(particles_t<T>* particles, int i, const floating_t* dist, const int n) {
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
HOST DEV int sampleCategoricalStandard(particles_t<T>* particles, int i, const int n) {
    floating_t u = sampleUniform(particles, i, 0, n) - 0.000000000000001;
    return static_cast<int>(u);
}

#endif