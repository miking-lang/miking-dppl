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
    
    return -log(1 - curand_uniform(&particles->randStates[i])) / lambda;
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

int flip(double p = 0.5) {
    return uniformDist(generatorDists) < p ? 1 : 0;
}


#endif

#endif