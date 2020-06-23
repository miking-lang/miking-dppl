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
    floating_t* w = particles->weights;
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles);
    // Calculate inclusive prefic sum
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    // if(resampler.prefixSum[numParticles-1] == 0) // Bad performance since it is a transfer
    //     printf("Error: prefixSum = 0!\n");
    return resampler.prefixSum[numParticles - 1];
}


template <typename T>
HOST DEV void decideAncestors(resampler_t& resampler, floating_t u, const int numParticles, const int numBlocks, const int numThreadsPerBlock) {

    systematicCumulativeOffspringKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);

    cumulativeOffspringToAncestorKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.cumulativeOffspring, resampler.ancestor, numParticles);

}

// If nested and doing parallel (which only works on GPU top-level inference), 
// let other functions know, so they can adjust kernel settings and num_particles
template <typename T>
DEV void resampleSystematicParNested(curandState* randState, particles_t<T>** particlesPtrToPtr, resampler_t& resampler) {
    
    particles_t<T>* particles = *particlesPtrToPtr;
    // int i = 0; // Necessary for SAMPLE macro
    // floating_t u = SAMPLE(uniform, 0.0f, 1.0f);
    floating_t u = uniform(randState, 0.0f, 1.0f);
    
    decideAncestors<T>(resampler, u, NUM_PARTICLES_NESTED, NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED);

    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    //copyStatesToTemp<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, tempArrP, NUM_PARTICLES_NESTED);
    copyStatesKernel<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, resampler.ancestor, NUM_PARTICLES_NESTED);
    cudaDeviceSynchronize();

    // Swap pointers
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;
}

template <typename T>
void resampleSystematicPar(particles_t<T>** particlesPtrToPtr, resampler_t& resampler) {

    particles_t<T>* particles = *particlesPtrToPtr;
    
    floating_t u = uDistRes(generatorRes);
    
    decideAncestors<T>(resampler, u, NUM_PARTICLES, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);

    // Copy states
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    
    copyStatesKernel<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, resampler.ancestor, NUM_PARTICLES);
    cudaDeviceSynchronize();
    cudaCheckError();
    // Swap pointers
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;
    
    /*
    copyStatesToTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, tempArrP, NUM_PARTICLES);
    // cudaCheckError();
    copyStatesKernel<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, tempArrP, resampler.ancestor, NUM_PARTICLES);
    // copyStatesFromTemp<T><<<numBlocks, numThreadsPerBlock>>>(tempArrP, particles, resampler.ancestor, numParticles);
    // cudaCheckError();
    cudaDeviceSynchronize();
    cudaCheckError();
    */
    
}

#endif

#endif