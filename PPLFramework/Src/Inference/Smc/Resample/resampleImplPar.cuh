#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

#ifdef GPU

#include "resampleCommon.cuh"
#include "resampleKernels.cuh"
#include "../../../Utils/cudaErrorUtils.cu"

#include <curand_kernel.h>
#include <thrust/reduce.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>


/*
template <typename T>
HOST DEV void calcInclusivePrefixSumPar(particles_t<T>* particles, floating_t* prefixSum) {
    floating_t* w = particles->weights;
    thrust::inclusive_scan(thrust::device, w, w + NUM_PARTICLES, prefixSum); // prefix sum
    // cudaCheckError();
}

HOST DEV void systematicCumulativeOffspringPar(floating_t* prefixSum, int* cumulativeOffspring, floating_t u) {
    systematicCumulativeOffspringKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(prefixSum, cumulativeOffspring, u);
    // cudaCheckError();
}

HOST DEV void cumulativeOffspringToAncestorPar(int* cumulativeOffspring, int* ancestor) {

    cumulativeOffspringToAncestorKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(cumulativeOffspring, ancestor);
    // cudaCheckError();
}

template <typename T>
HOST DEV void copyStatesPar(particles_t<T>* particles, int* ancestor, void* tempArr) {
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(tempArr);
    
    copyStatesToTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, tempArrP);
    // cudaCheckError();
    copyStatesFromTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, ancestor);
    // cudaCheckError();
}
*/

template <typename T>
HOST DEV floating_t calcWeightSumPar(particles_t<T>* particles, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(particles->weights, numParticles);
    // Calculate inclusive prefic sum
    floating_t* w = particles->weights;
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    // cudaCheckError();
    // if(resampler.prefixSum[numParticles-1] == 0) // Bad performance since it is a transfer
    //     printf("Error: prefixSum = 0!\n");
    return resampler.prefixSum[numParticles - 1];
}


template <typename T>
HOST DEV void postUniform(particles_t<T>* particles, resampler_t resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock) {

    systematicCumulativeOffspringKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);

    cumulativeOffspringToAncestorKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.cumulativeOffspring, resampler.ancestor, numParticles);


    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);

    copyStatesToTemp<T><<<numBlocks, numThreadsPerBlock>>>(particles, tempArrP, numParticles);
    // cudaCheckError();
    copyStatesFromTemp<T><<<numBlocks, numThreadsPerBlock>>>(tempArrP, particles, resampler.ancestor, numParticles);
    // cudaCheckError();

    cudaDeviceSynchronize();
}

// If nested and doing parallel (which only works on GPU top-level inference), 
// let other functions know, so they can adjust kernel settings and num_particles
template <typename T>
DEV void resampleSystematicParNested(particles_t<T>* particles, resampler_t resampler) {
    
    floating_t u = sampleUniform(particles, 0, 0.0f, 1.0f);
    
    postUniform(particles, resampler, u, NUM_PARTICLES_NESTED, NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED);
}

template <typename T>
void resampleSystematicPar(particles_t<T>* particles, resampler_t resampler) {
    
    floating_t u = uDistRes(generatorRes);
    
    postUniform(particles, resampler, u, NUM_PARTICLES, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
}

#endif

#endif