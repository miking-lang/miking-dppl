#ifndef RESAMPLE_KERNELS_INCLUDED
#define RESAMPLE_KERNELS_INCLUDED

#include <curand_kernel.h>
#include "inference/smc/smc.cuh"

__global__ void expWeightsKernel(floating_t* w, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    w[idx] = exp(w[idx] - maxLogWeight);
}

__global__ void renormaliseSumsKernel(floating_t* prefixSum, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    // prefixSum[idx] = exp(log(prefixSum[idx]) + maxLogWeight);
    prefixSum[idx] = log(prefixSum[idx]) + maxLogWeight;
}

__global__ void systematicCumulativeOffspringKernel(const floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    // floating_t expectedCumulativeOffspring = numParticles * prefixSum[idx] / prefixSum[numParticles - 1];
    floating_t expectedCumulativeOffspring = numParticles * exp(prefixSum[idx] - prefixSum[numParticles - 1]);
    cumulativeOffspring[idx] = min(numParticles, static_cast<int>(floor(expectedCumulativeOffspring + u)));
}

__global__ void cumulativeOffspringToAncestorKernel(const int* cumulativeOffspring, int* ancestor, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    int start = idx == 0 ? 0 : cumulativeOffspring[idx - 1];
    int numCurrentOffspring = cumulativeOffspring[idx] - start;
    for(int j = 0; j < numCurrentOffspring; j++)
        ancestor[start+j] = idx;
}

/*
template <typename T>
__global__ void copyStatesToTemp(particles_t<T>* particlesSrc, particles_t<T>* particlesDst, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    copyParticle(particlesDst, particlesSrc, idx, idx);
}
*/

template <typename T>
__global__ void copyStatesKernel(particles_t<T>* particlesDst, const particles_t<T>* particlesSrc, int* ancestor, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    copyParticle(particlesDst, particlesSrc, idx, ancestor[idx]);
}



#endif