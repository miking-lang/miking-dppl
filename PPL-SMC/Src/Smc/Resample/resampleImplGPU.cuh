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
void calcInclusivePrefixSum(particles_t<T>* particles, floating_t* prefixSum) {
    floating_t* w = particles->weights;
    thrust::inclusive_scan(thrust::device, w, w + NUM_PARTICLES, prefixSum); // prefix sum
    cudaCheckError();
}

void systematicCumulativeOffspring(floating_t* prefixSum, int* cumulativeOffspring) {
    floating_t u = uDistRes(generatorRes);
    systematicCumulativeOffspringKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(prefixSum, cumulativeOffspring, u);
    cudaCheckError();
}

void cumulativeOffspringToAncestor(int* cumulativeOffspring, int* ancestor) {

    cumulativeOffspringToAncestorKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(cumulativeOffspring, ancestor);
    cudaCheckError();
}

template <typename T>
void copyStates(particles_t<T>* particles, int* ancestor) {
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(tempArr);

    copyStatesToTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, tempArrP);
    cudaCheckError();
    copyStatesFromTemp<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(tempArrP, particles, ancestor);
    cudaCheckError();
}

template <typename T>
void resampleSystematic(particles_t<T>* particles) {
    calcInclusivePrefixSum<T>(particles, prefixSum);
    systematicCumulativeOffspring(prefixSum, cumulativeOffspring);
    cumulativeOffspringToAncestor(cumulativeOffspring, ancestor);
    copyStates<T>(particles, ancestor);
    cudaDeviceSynchronize();
}


#endif