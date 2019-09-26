#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

#include "resampleCommon.cuh"
#include "resampleKernels.cuh"
#include "../../cudaErrorUtils.cu"

#include <curand_kernel.h>
#include <thrust/reduce.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>


template <typename T>
void calcInclusivePrefixSumPar(particles_t<T>* particles, floating_t* prefixSum) {
    floating_t* w = particles->weights;
    thrust::inclusive_scan(thrust::device, w, w + NUM_PARTICLES, prefixSum); // prefix sum
    cudaCheckError();
}

void systematicCumulativeOffspringPar(floating_t* prefixSum, int* cumulativeOffspring, floating_t u) {
    systematicCumulativeOffspringKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(prefixSum, cumulativeOffspring, u);
    cudaCheckError();
}

void cumulativeOffspringToAncestorPar(int* cumulativeOffspring, int* ancestor) {

    cumulativeOffspringToAncestorKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(cumulativeOffspring, ancestor);
    cudaCheckError();
}

template <typename T>
void copyStatesPar(particles_t<T>* particles, int* ancestor, void* tempArr) {
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(tempArr);

    copyStatesToTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, tempArrP);
    cudaCheckError();
    copyStatesFromTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, ancestor);
    cudaCheckError();
}

template <typename T>
floating_t resampleSystematicPar(particles_t<T>* particles, resampler_t resampler, bool nested = false) {
    expWeightsKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles->weights);
    calcInclusivePrefixSumPar<T>(particles, resampler.prefixSum);
    //if(prefixSum[NUM_PARTICLES-1] == 0) // Bad performance since it is a transfer
        //printf("Error: prefixSum = 0!\n");

    floating_t u;
    if(nested)
        u = uniform(particles, 0, 0.0f, 1.0f); // i is not used if CPU
    else 
        u = uDistRes(generatorRes);

    systematicCumulativeOffspringPar(resampler.prefixSum, resampler.cumulativeOffspring, u);
    cumulativeOffspringToAncestorPar(resampler.cumulativeOffspring, resampler.ancestor);
    copyStatesPar<T>(particles, resampler.ancestor, resampler.tempArr);
    cudaDeviceSynchronize();
    return resampler.prefixSum[NUM_PARTICLES-1];
}


#endif