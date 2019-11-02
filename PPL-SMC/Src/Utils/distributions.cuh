#ifndef DISTRIBUTIONS_INCLUDED
#define DISTRIBUTIONS_INCLUDED

#include <random>
#include <time.h>

#ifdef GPU
#include <curand_kernel.h>
#endif

#include "../Smc/smc.cuh"

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
__device__ floating_t uniform(particles_t<T>* particles, int i, floating_t min, floating_t max) {
    return (curand_uniform(&particles->randStates[i]) * (max - min)) + min;
}

template <typename T> 
__device__ floating_t normal(particles_t<T>* particles, int i, floating_t mean, floating_t std) {
    return (curand_normal(&particles->randStates[i]) * std) + mean;
}

template <typename T>
__device__ floating_t exponential(particles_t<T>* particles, int i, floating_t lambda) {
    
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
__device__ floating_t gamma(particles_t<T>* particles, int i, floating_t k, floating_t theta) {

    /* assume a > 0 */
    
    if (k < 1) {
        double u = curand_uniform(&particles->randStates[i]);
        return gamma(particles, i, 1.0 + k, theta) * pow (u, 1.0 / k);
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
floating_t uniform(particles_t<T>* particles, int i, floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

template <typename T>
floating_t normal(particles_t<T>* particles, int i, floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}

template <typename T>
int flipK(particles_t<T>* particles, int i, floating_t p = 0.5) {
    
    return uniformDist(generatorDists) < p ? 1 : 0;
}

template <typename T>
floating_t exponential(particles_t<T>* particles, int i, floating_t lambda) {
    
    // return exponentialDist(generatorDists);
    return -log(1 - uniformDist(generatorDists)) / lambda;
}

// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
template <typename T>
floating_t gamma(particles_t<T>* particles, int i, floating_t k, floating_t theta) {

    /* assume a > 0 */
    
    if (k < 1) {
        double u = uniformDist(generatorDists);
        return gamma(particles, i, 1.0 + k, theta) * pow(u, 1.0 / k);
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

#endif