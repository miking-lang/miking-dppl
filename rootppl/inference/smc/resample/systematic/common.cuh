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
#include "utils/misc.cuh"

default_random_engine generatorRes;
uniform_real_distribution<floating_t> uDistRes(0.0, 1.0);

template <typename T>
struct resampler_t {

    int *ancestor, *cumulativeOffspring;
    floating_t* prefixSum;
    particles_t<T> tempArr;
};

template <typename T>
HOST DEV resampler_t<T> initResamplerNested(int numParticles) {

    resampler_t<T> resampler;

    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    resampler.tempArr = allocateParticlesNested<T>(numParticles);

    return resampler;
}


template <typename T>
resampler_t<T> initResampler(int numParticles) {

    generatorRes.seed(time(NULL) * 3); // avoid same seed as main?
    resampler_t<T> resampler;

    allocateMemory<int>(&resampler.ancestor, numParticles);
    allocateMemory<int>(&resampler.cumulativeOffspring, numParticles);
    allocateMemory<floating_t>(&resampler.prefixSum, numParticles);
    
    resampler.tempArr = allocateParticles<T>(numParticles);

    return resampler;
}

template <typename T>
HOST DEV void destResamplerNested(resampler_t<T> resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    // particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);
    freeParticlesNested<T>(resampler.tempArr);
}

template <typename T>
void destResampler(resampler_t<T> resampler) {

    freeMemory<int>(resampler.ancestor);
    freeMemory<int>(resampler.cumulativeOffspring);
    freeMemory<floating_t>(resampler.prefixSum);
    freeParticles<T>(resampler.tempArr);
}

template <typename T>
DEV void copyParticle(particles_t<T> particlesDst, const particles_t<T> particlesSrc, int dstIdx, int srcIdx) {
    particlesDst.progStates[dstIdx] = particlesSrc.progStates[srcIdx];
    particlesDst.pcs[dstIdx] = particlesSrc.pcs[srcIdx];
    particlesDst.weights[dstIdx] = 0;
}

#endif