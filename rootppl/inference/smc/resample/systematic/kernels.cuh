#ifndef RESAMPLE_KERNELS_INCLUDED
#define RESAMPLE_KERNELS_INCLUDED

/*
 * File kernels.cuh contains kernels used by systematic resampling. 
 */

#include <curand_kernel.h>
#include "inference/smc/smc.cuh"

/**
 * Scales the weights (shift in log-space) and takes the exponent of them.
 * This is used when calculating the weight sums safely. 
 * 
 * @param w the weight array.
 * @param numParticles the number of particles used in SMC.
 * @param maxLogWeight the log of the maximum weight. 
 */
__global__ void expWeightsKernel(floating_t* w, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    w[idx] = exp(w[idx] - maxLogWeight);
}

/**
 * Takes the logarithm of the prefixSum belonging to the current particle then scales back. 
 * This is used when calculating the weight sums safely. 
 * 
 * @param prefixSum the calculated inclusive prefix sums which should be logged and scaled back. 
 * @param numParticles the number of particles used in SMC.
 * @param maxLogWeight the log of the maximum weight. 
 */
__global__ void renormaliseSumsKernel(floating_t* prefixSum, int numParticles, floating_t maxLogWeight) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    // prefixSum[idx] = exp(log(prefixSum[idx]) + maxLogWeight);
    prefixSum[idx] = log(prefixSum[idx]) + maxLogWeight;
}

/**
 * Calculates the cumulative offspring of each particle. 
 * 
 * @param logPrefixSum the logarithm of the calculated inclusive prefix sums.
 * @param cumulativeOffspring the array to store the result in. 
 * @param u a sample from the standard uniform distribution. 
 * @param numParticles the number of particles used in SMC.
 */
__global__ void systematicCumulativeOffspringKernel(const floating_t* logPrefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    floating_t expectedCumulativeOffspring = numParticles * exp(logPrefixSum[idx] - logPrefixSum[numParticles - 1]);
    cumulativeOffspring[idx] = min(numParticles, static_cast<int>(floor(expectedCumulativeOffspring + u)));
}

/**
 * Uses the cumulative offspring to assign the ancestor indices used for resample propagation. 
 * 
 * @param cumulativeOffspring the array to read the cumulative offspring from. 
 * @param ancestor the array to store the result in. 
 * @param numParticles the number of particles used in SMC.
 */
__global__ void cumulativeOffspringToAncestorKernel(const int* cumulativeOffspring, int* ancestor, int numParticles) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    int start = idx == 0 ? 0 : cumulativeOffspring[idx - 1];
    int numCurrentOffspring = cumulativeOffspring[idx] - start;
    for(int j = 0; j < numCurrentOffspring; j++)
        ancestor[start+j] = idx;
}

/**
 * Propagates the particles. Copies them from the ancestor to the new particles. 
 * 
 * @param particlesDst the destination array to copy to.
 * @param particlesSrc the source array to copy from.
 * @param ancestor the array containing the ancestor indices. 
 * @param numParticles the number of particles used in SMC.
 */
__global__ void copyStatesKernel(particles_t particlesDst, const particles_t particlesSrc, int* ancestor, int numParticles, size_t progStateSize) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= numParticles || idx < 0) return;

    copyParticle(particlesDst, particlesSrc, idx, ancestor[idx], progStateSize);
}



#endif