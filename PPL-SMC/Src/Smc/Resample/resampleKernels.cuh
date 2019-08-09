#ifndef RESAMPLE_KERNELS_INCLUDED
#define RESAMPLE_KERNELS_INCLUDED

#include <curand_kernel.h>
#include "../smc.cuh"

__global__ void systematicCumulativeOffspringKernel(floating_t* prefixSum, int* cumulativeOffspring, floating_t u) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    floating_t expectedCumulativeOffspring = NUM_PARTICLES * prefixSum[idx] / prefixSum[NUM_PARTICLES - 1]; // opimize by saving W[n-1] to constant?
    cumulativeOffspring[idx] = min(NUM_PARTICLES, static_cast<int>(floor(expectedCumulativeOffspring + u)));
}

__global__ void cumulativeOffspringToAncestorKernel(int* cumulativeOffspring, int* ancestor) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    int start = idx == 0 ? 0 : cumulativeOffspring[idx - 1];
    int numCurrentOffspring = cumulativeOffspring[idx] - start;
    for(int j = 0; j < numCurrentOffspring; j++)
        ancestor[start+j] = idx;
}

template <typename T>
__global__ void copyStatesToTemp(particles_t<T>* particlesSrc, particles_t<T>* particlesDst) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    copyParticle(particlesSrc, particlesDst, idx, idx);
}

template <typename T>
__global__ void copyStatesFromTemp(particles_t<T>* particlesSrc, particles_t<T>* particlesDst, int* ancestor) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    copyParticle(particlesSrc, particlesDst, ancestor[idx], idx);
}


#endif