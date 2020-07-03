#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

#ifdef GPU

#include "resample_common.cuh"
#include "resample_kernels.cuh"
#include "utils/cuda_error_utils.cu"

#include <curand_kernel.h>
#include <thrust/reduce.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>


HOST DEV floating_t calcWeightSumPar(floating_t* w, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {

    floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, maxLogWeight);
    // Calculate inclusive prefix sum
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    renormaliseSumsKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, numParticles, maxLogWeight);
    
    cudaDeviceSynchronize();
    return resampler.prefixSum[numParticles - 1];
}


template <typename T>
HOST DEV void decideAncestors(resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    systematicCumulativeOffspringKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);

    cumulativeOffspringToAncestorKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.cumulativeOffspring, resampler.ancestor, numParticles);

}

template <typename T>
HOST DEV void postUniform(particles_t<T>** particlesPtrToPtr, resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    particles_t<T>* particles = *particlesPtrToPtr;

    decideAncestors<T>(resampler, u, numParticles, numBlocks, numThreadsPerBlock);

    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    copyStatesKernel<T><<<numBlocks, numThreadsPerBlock>>>(tempArrP, particles, resampler.ancestor, numParticles);
    cudaDeviceSynchronize();

    // Swap pointers
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;
}

#ifdef GPU
// If nested and doing parallel (which only works on GPU top-level inference), 
// let other functions know, so they can adjust kernel settings and num_particles
template <typename T>
DEV void resampleSystematicParNested(curandState* randState, particles_t<T>** particlesPtrToPtr, resampler_t& resampler, int numParticles, int numBlocks) {
    
    // particles_t<T>* particles = *particlesPtrToPtr;
    floating_t u = uniform(randState, 0.0f, 1.0f);
    
    postUniform<T>(particlesPtrToPtr, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK_NESTED);

    /*
    decideAncestors<T>(resampler, u, NUM_PARTICLES_NESTED, NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED);

    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    copyStatesKernel<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(tempArrP, particles, resampler.ancestor, NUM_PARTICLES_NESTED);
    cudaDeviceSynchronize();

    // Swap pointers
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;
    */
}
#endif

template <typename T>
void resampleSystematicPar(particles_t<T>** particlesPtrToPtr, resampler_t& resampler, int numParticles, int numBlocks) {

    // particles_t<T>* particles = *particlesPtrToPtr;
    
    floating_t u = uDistRes(generatorRes);

    postUniform<T>(particlesPtrToPtr, resampler, u, numParticles, numBlocks, NUM_THREADS_PER_BLOCK);
    
    /*
    decideAncestors<T>(resampler, u, numParticles, numBlocks);

    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    
    copyStatesKernel<T><<<numBlocks, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, resampler.ancestor, numParticles);
    cudaDeviceSynchronize();
    cudaCheckError();
    // Swap pointers
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;
    */
}

#endif

#endif