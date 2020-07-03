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
HOST DEV resampler_t initResamplerNested(int numParticles) {

    // generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    resampler_t resampler;

    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    resampler.tempArr = allocateParticlesNested<T>();

    return resampler;
}


template <typename T>
resampler_t initResampler(int numParticles) {

    generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    resampler_t resampler;

    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(&resampler.ancestor, numParticles * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&resampler.cumulativeOffspring, numParticles * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&resampler.prefixSum, numParticles * sizeof(floating_t)));
    #else
    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    #endif
    resampler.tempArr = allocateParticles<T>(numParticles);

    return resampler;
}

template <typename T>
HOST DEV void destResamplerNested(resampler_t resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
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