
#ifndef GENERAL_KERNELS_INCLUDED
#define GENERAL_KERNELS_INCLUDED

#include <curand_kernel.h>
#include "smc.cuh"


template <typename T>
__global__ void initParticles(particles_t<T>* particles, int timeSeed, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    particles->weights[idx] = 0;
    particles->pcs[idx] = 0;
    curand_init(1234 + timeSeed, idx, 0, &particles->randStates[idx]);
}

/*__global__ void initRandStatesKernel(curandState* states, int timeSeed, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    curand_init(1234 + timeSeed, idx, 0, &states[idx]);
}*/

// Better to sort particles after the func to exec, and do separate kernels for each func?
template <typename T>
__global__ void execFuncs(particles_t<T>* particles, int t, pplFunc_t<T>* funcs, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    int pc = particles->pcs[idx];
    if(funcs[pc] != NULL)
        funcs[pc](particles, idx, t);
}

#endif