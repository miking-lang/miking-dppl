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


//#pragma hd_warning_disable
// #pragma nv_exec_check_disable
//constexpr floating_t getMax(const floating_t* w, const int numParticles) {
    //floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    //return maxLogWeight;
//}


HOST DEV floating_t calcWeightSumPar(floating_t* w, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock) {
    // floating_t* w = particles->weights;
    // printf("logWeights: %f, %f, %f, %f\n", w[0], w[1], w[2], w[numParticles-1]);
    // floating_t maxLogWeight = maxNaive(w, numParticles);
    // printf("MaxW: %f\n", maxLogWeight2);
    floating_t maxLogWeight = *(thrust::max_element(thrust::device, w, w + numParticles));
    // floating_t maxLogWeight = getMax(w, numParticles);
    // maxLogWeight = 2;
    // printf("MaxW: %f\n", maxLogWeight);
    
    
    expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, maxLogWeight);
    // Calculate inclusive prefix sum
    thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum); // prefix sum
    renormaliseSumsKernel<<<numBlocks, numThreadsPerBlock>>>(resampler.prefixSum, numParticles, maxLogWeight);
    

    // if(resampler.prefixSum[numParticles-1] == 0) // Bad performance since it is a transfer
    //     printf("Error: prefixSum = 0!\n");
    // cudaDeviceSynchronize();
    // floating_t* ps = resampler.prefixSum;
    // printf("prefixSum ye-accurate: %f, %f, %f, %f\n", ps[0], ps[1], ps[2], ps[numParticles - 1]);
    // expWeightsKernel<<<numBlocks, numThreadsPerBlock>>>(w, numParticles, 0);
    // thrust::inclusive_scan(thrust::device, w, w + numParticles, resampler.prefixSum);
    

    cudaDeviceSynchronize();
    // printf("prefixSum no-accurate: %f, %f, %f, %f\n\n", ps[0], ps[1], ps[2], ps[numParticles - 1]);
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