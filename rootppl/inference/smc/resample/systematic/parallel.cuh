#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

#ifdef GPU

#include "common.cuh"
#include "kernels.cuh"
#include "utils/cuda_error_utils.cu"

#include <curand_kernel.h>
#include <thrust/reduce.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>


template <typename T>
HOST DEV floating_t calcWeightSumPar(floating_t* w, resampler_t<T> resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {

    floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, maxLogWeight);
    // Calculate inclusive prefix sum
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    renormaliseSumsKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, numParticles, maxLogWeight);
    
    cudaDeviceSynchronize();
    return resampler.prefixSum[numParticles - 1];
}


template <typename T>
HOST DEV void decideAncestors(resampler_t<T>& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    systematicCumulativeOffspringKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);

    cumulativeOffspringToAncestorKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.cumulativeOffspring, resampler.ancestor, numParticles);

}

template <typename T>
HOST DEV void postUniform(particles_t<T>& particles, resampler_t<T>& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    decideAncestors<T>(resampler, u, numParticles, numBlocks, numThreadsPerBlock);

    // Copy states
    copyStatesKernel<T><<<numBlocks, numThreadsPerBlock>>>(resampler.tempArr, particles, resampler.ancestor, numParticles);
    cudaDeviceSynchronize();

    // Swap pointers
    particles_t<T> tempArrP;
    tempArrP = resampler.tempArr;
    resampler.tempArr = particles;
    particles = tempArrP;
}

#ifdef GPU
template <typename T>
DEV void resampleSystematicParNested(curandState* randState, particles_t<T>& particles, resampler_t<T>& resampler, int numParticles, int numBlocks) {
    
    floating_t u = uniform(randState, 0.0f, 1.0f);
    
    postUniform<T>(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK_NESTED);
}
#endif

template <typename T>
void resampleSystematicPar(particles_t<T>& particles, resampler_t<T>& resampler, int numParticles, int numBlocks) {

    floating_t u = uDistRes(generatorRes);

    postUniform<T>(particles, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK);
}

#endif

#endif