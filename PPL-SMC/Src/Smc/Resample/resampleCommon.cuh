#ifndef RESAMPLE_COMMON_INCLUDED
#define RESAMPLE_COMMON_INCLUDED

#include <cstddef>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <random>
#include "../smc.cuh"


default_random_engine generatorRes;
uniform_real_distribution<floating_t> uDistRes(0.0, 1.0);

int *ancestor, *cumulativeOffspring;
floating_t* prefixSum;
void* tempArr;


template <typename T>
void initResampler() {
    generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    /*
    allocateMemory<int>(&ancestor, (size_t)NUM_PARTICLES);
    allocateMemory<int>(&cumulativeOffspring, NUM_PARTICLES);
    allocateMemory<floating_t>(&prefixSum, NUM_PARTICLES);
    allocateMemory<particle_t<T>>(&tempArr, 1);
    */

    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(&ancestor, NUM_PARTICLES * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&cumulativeOffspring, NUM_PARTICLES * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&prefixSum, NUM_PARTICLES * sizeof(floating_t)));
    cudaSafeCall(cudaMallocManaged(&tempArr, sizeof(particles_t<T>)));
    #else
    ancestor = new int[NUM_PARTICLES];
    cumulativeOffspring = new int[NUM_PARTICLES];
    prefixSum = new floating_t[NUM_PARTICLES];
    tempArr = new particles_t<T>;
    #endif
}

template <typename T>
void destResampler() {

    #ifdef GPU
    cudaSafeCall(cudaFree(ancestor));
    cudaSafeCall(cudaFree(cumulativeOffspring));
    cudaSafeCall(cudaFree(prefixSum));
    cudaSafeCall(cudaFree(tempArr));
    #else
    delete[] ancestor;
    delete[] cumulativeOffspring;
    delete[] prefixSum;
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(tempArr);
    delete tempArrP;
    #endif
}

template <typename T>
#ifdef GPU
__device__
#endif
void copyParticle(particles_t<T>* particlesSrc, particles_t<T>* particlesDst, int srcIdx, int dstIdx) {
    particlesDst->progStates[dstIdx] = particlesSrc->progStates[srcIdx];
    #ifdef GPU
    particlesDst->randStates[dstIdx] = particlesSrc->randStates[srcIdx];
    #endif
    particlesDst->pcs[dstIdx] = particlesSrc->pcs[srcIdx];
    // particlesDst->weights[dstIdx] = particlesSrc->weights[srcIdx];
    particlesDst->weights[dstIdx] = 0;
}

#endif