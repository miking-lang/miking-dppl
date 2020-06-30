#ifndef RESAMPLE_COMMON_INCLUDED
#define RESAMPLE_COMMON_INCLUDED

#include <cstddef>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <random>
#include <time.h>
#include "inference/smc/smc.cuh"
#include "inference/smc/particles_memory_handler.cuh"

default_random_engine generatorRes;
uniform_real_distribution<floating_t> uDistRes(0.0, 1.0);

struct resampler_t {

    int *ancestor, *cumulativeOffspring;
    floating_t* prefixSum;
    void* tempArr;
};

template <typename T>
HOST DEV resampler_t initResamplerNested() {

    // generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    resampler_t resampler;

    resampler.ancestor = new int[NUM_PARTICLES_NESTED];
    resampler.cumulativeOffspring = new int[NUM_PARTICLES_NESTED];
    resampler.prefixSum = new floating_t[NUM_PARTICLES_NESTED];
    resampler.tempArr = allocateParticlesNested<T>();

    return resampler;
}


template <typename T>
resampler_t initResampler() {

    generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    resampler_t resampler;

    /*
    allocateMemory<int>(&ancestor, (size_t)NUM_PARTICLES);
    allocateMemory<int>(&cumulativeOffspring, NUM_PARTICLES);
    allocateMemory<floating_t>(&prefixSum, NUM_PARTICLES);
    allocateMemory<particle_t<T>>(&tempArr, 1);
    */

    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(&resampler.ancestor, NUM_PARTICLES * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&resampler.cumulativeOffspring, NUM_PARTICLES * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&resampler.prefixSum, NUM_PARTICLES * sizeof(floating_t)));
    //cudaSafeCall(cudaMallocManaged(&resampler.tempArr, sizeof(particles_t<T>)));
    #else
    resampler.ancestor = new int[NUM_PARTICLES];
    resampler.cumulativeOffspring = new int[NUM_PARTICLES];
    resampler.prefixSum = new floating_t[NUM_PARTICLES];
    #endif
    resampler.tempArr = allocateParticles<T>();

    return resampler;
}

template <typename T>
HOST DEV void destResamplerNested(resampler_t resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    // delete tempArrP;
    freeParticlesNested<T>(tempArrP);
}

template <typename T>
void destResampler(resampler_t resampler) {

    #ifdef GPU
    cudaSafeCall(cudaFree(resampler.ancestor));
    cudaSafeCall(cudaFree(resampler.cumulativeOffspring));
    cudaSafeCall(cudaFree(resampler.prefixSum));
    // cudaSafeCall(cudaFree(resampler.tempArr));
    #else
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    // particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    // delete tempArrP;
    #endif
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    freeParticles<T>(tempArrP);
}

template <typename T>
DEV void copyParticle(particles_t<T>* particlesDst, const particles_t<T>* particlesSrc, int dstIdx, int srcIdx) {
    particlesDst->progStates[dstIdx] = particlesSrc->progStates[srcIdx];
    particlesDst->pcs[dstIdx] = particlesSrc->pcs[srcIdx];
    particlesDst->weights[dstIdx] = 0;
}

#endif