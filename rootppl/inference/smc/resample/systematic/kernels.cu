
/*
 * File kernels.cu contains definitions of kernels used by systematic resampling. 
 */

#ifdef __NVCC__

#include <curand_kernel.h>
#include "inference/smc/smc.cuh"
#include "inference/smc/resample/common.cuh"
#include "kernels.cuh"

__global__ void expWeightsKernel(floating_t* w, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;
    
    if (isnan(w[idx]))
        w[idx] = -INFINITY;

    w[idx] = exp(w[idx] - maxLogWeight);
}

__global__ void renormaliseKernel(floating_t* w, floating_t* prefixSum, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    w[idx] = log(w[idx]) + maxLogWeight;
    prefixSum[idx] = log(prefixSum[idx]) + maxLogWeight;
}

__global__ void expSquareWeightsKernel(floating_t* w, floating_t* wSquared, floating_t maxLogWeight, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    floating_t scaledW = w[idx] - maxLogWeight;
    floating_t expWSquared = exp(scaledW);
    if (isnan(expWSquared))
        wSquared[idx] = 0;
    else
        wSquared[idx] = expWSquared * expWSquared;
}

__global__ void systematicCumulativeOffspringKernel(const floating_t* logPrefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    floating_t expectedCumulativeOffspring = numParticles * exp(logPrefixSum[idx] - logPrefixSum[numParticles - 1]);
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

__global__ void copyStatesKernel(particles_t particlesDst, const particles_t particlesSrc, int* ancestor, int numParticles, size_t progStateSize) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    copyParticle(particlesDst, particlesSrc, idx, ancestor[idx], progStateSize);
}

__global__ void normaliseWeightsKernel(floating_t* w, floating_t logWeightSum, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    w[idx] -= logWeightSum;
}



#endif