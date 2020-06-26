
#ifndef GENERAL_KERNELS_INCLUDED
#define GENERAL_KERNELS_INCLUDED

#include <curand_kernel.h>
#include "smc.cuh"


template <typename T>
__global__ void initParticlesNoCurand(particles_t<T>* particles, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    particles->weights[idx] = 0;
    particles->pcs[idx] = 0;
}

template <typename T>
__global__ void initParticles(curandState* randStates, particles_t<T>* particles, int numParticles, int seed=0) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if(i >= numParticles || i < 0) return;

    particles->weights[i] = 0;
    particles->pcs[i] = 0;

    // Double check this seed, need only to be unique over one inference, as time will vary between inferences. 
    // curand_init(1234 + clock64(), seed * numParticles + i, 0, &particles->randStates[i]);
    curandState randStateLocal = randStates[i];
    curand_init(1234 + clock64(), seed * numParticles + i, 0, &randStateLocal);
    randStates[i] = randStateLocal;
    // printf("seed: %d\n", seed);
}

// Better to sort particles after the func to exec, and do separate kernels for each func?
template <typename T>
__global__ void execFuncs(curandState* randStates, particles_t<T>* particles, pplFunc_t<T>* funcs, int numParticles, void* arg) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if(i >= numParticles || i < 0) return;

    RAND_STATE_LOCAL;

    //int pc = particles->pcs[i];
    //if(funcs[pc] != NULL)
    funcs[particles->pcs[i]](RAND_STATE particles, i, arg);

    RAND_STATE_RESTORE;
}

#endif