
/*
 * File parallel.cuh contains the parallel implementation of the systematic resampling. 
 */

#ifdef __NVCC__

#include "common.cuh"
#include "kernels.cuh"
#include "utils/cuda_error_utils.cuh"
#include "parallel.cuh"

#include <curand_kernel.h>
// #include <thrust/reduce.h>
#include <thrust/scan.h>
// #include <thrust/execution_policy.h>
// #include <thrust/extrema.h>

HOST DEV void prefixSumNaive(floating_t* w, resampler_t resampler, int numParticles) {
    resampler.prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        resampler.prefixSum[i] = resampler.prefixSum[i-1] + w[i];
}

HOST DEV floating_t calcLogWeightSumPar(floating_t* w, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {

    floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    // floating_t maxLogWeight = maxNaive(w, numParticles);
    
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, maxLogWeight);
    cudaDeviceSynchronize();
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    // prefixSumNaive(w, resampler, numParticles);

    renormaliseSumsKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, numParticles, maxLogWeight);
    
    cudaDeviceSynchronize();
    return resampler.prefixSum[numParticles - 1];
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

DEV void resampleSystematicParNested(curandState* randState, particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks) {
    
    floating_t u = uniform(randState, 0.0f, 1.0f);
    
    postUniform(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK_NESTED);
}

void resampleSystematicPar(particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks) {

    floating_t u = uniformCPU(generatorRes);

    postUniform(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK);
}


#endif