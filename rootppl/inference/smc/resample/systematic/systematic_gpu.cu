
/*
 * File systematic_gpu.cuh contains the GPU implementation of the systematic resampling. 
 * This implementation is inspired by the paper by L. M. Murray et. al.: 
 * Parallel resampling in the particle filter https://arxiv.org/abs/1301.4019
 */

#ifdef __NVCC__

#include "inference/smc/resample/common.cuh"
#include "kernels.cuh"
#include "utils/cuda_error_utils.cuh"
#include "systematic_gpu.cuh"

#include <curand_kernel.h>
#include <thrust/scan.h>

HOST DEV void prefixSumNaive(floating_t* w, resampler_t resampler, int numParticles) {
    resampler.prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        resampler.prefixSum[i] = resampler.prefixSum[i-1] + w[i];
}

HOST DEV floating_t calcLogWeightSumGpu(floating_t* w, resampler_t& resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {

    floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    resampler.maxLogWeight = maxLogWeight;
    // floating_t maxLogWeight = maxNaive(w, numParticles);
    
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, maxLogWeight);
    cudaDeviceSynchronize();
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    // prefixSumNaive(w, resampler, numParticles);

    renormaliseKernel<<<numBlocks, numThreadsPerBlock>>>(w, resampler.prefixSum, numParticles, maxLogWeight);
    
    cudaDeviceSynchronize();
    return resampler.prefixSum[numParticles - 1];
}

HOST DEV floating_t calcESSGpu(floating_t* w, floating_t logWeightSum, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {

    // Kernel saving new square exp log weights
    expSquareWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, resampler.wSquared, resampler.maxLogWeight, numParticles);

    // Thrust for summing squared weights
    cudaDeviceSynchronize();
    floating_t wSumOfSquares = (thrust::reduce(thrust::device, resampler.wSquared, resampler.wSquared + numParticles));

    floating_t scaledLogSum = logWeightSum - resampler.maxLogWeight;
    floating_t wSumSquared = exp(scaledLogSum + scaledLogSum);

    return wSumSquared / wSumOfSquares;
}

HOST DEV void decideAncestors(resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    systematicCumulativeOffspringKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);

    cumulativeOffspringToAncestorKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.cumulativeOffspring, resampler.ancestor, numParticles);

}

HOST DEV void postUniform(particles_t& particles, resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    decideAncestors(resampler, u, numParticles, numBlocks, numThreadsPerBlock);

    // Copy states
    copyStatesKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.auxParticles, particles, resampler.ancestor, numParticles, resampler.progStateSize);
    cudaDeviceSynchronize();

    // Swap pointers
    particles_t tempAux = resampler.auxParticles;
    resampler.auxParticles = particles;
    particles = tempAux;
}

DEV void resampleSystematicGpuNested(curandState* randState, particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks) {
    
    floating_t u = uniform(randState, 0.0f, 1.0f);
    
    postUniform(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK_NESTED);
}

void resampleSystematicGpu(particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks) {

    floating_t u = uniformCPU(generatorRes);

    postUniform(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK);
}

void normaliseWeightsGpu(floating_t* w, floating_t logWeightSum, int numParticles, int numBlocks, int numThreadsPerBlock) {
    normaliseWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, logWeightSum, numParticles);
    cudaDeviceSynchronize();
}


#endif