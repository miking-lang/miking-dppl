
/*
 * File common.cu contains definitions used by both sequential and parallel systematic resampling. 
 */

#include <random>
#include <time.h>
#include "common.cuh"
#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "inference/smc/particles_memory_handler.cuh"

std::default_random_engine generatorRes;
std::uniform_real_distribution<floating_t> uniformCPU(0.0, 1.0);

resampler_t initResampler(int numParticles, size_t progStateSize) {

    generatorRes.seed(time(NULL) * 3); // Multiply by 3 to avoid same seed as distributions. 
    resampler_t resampler;

    allocateMemory<int>(&resampler.ancestor, numParticles);
    allocateMemory<int>(&resampler.cumulativeOffspring, numParticles);
    allocateMemory<floating_t>(&resampler.prefixSum, numParticles);
    
    resampler.auxParticles = allocateParticles(numParticles, progStateSize);
    resampler.progStateSize = progStateSize;

    return resampler;
}

HOST DEV resampler_t initResamplerNested(int numParticles, size_t progStateSize) {

    resampler_t resampler;

    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    resampler.auxParticles = allocateParticlesNested(numParticles, progStateSize);

    return resampler;
}

void destResampler(resampler_t resampler) {

    freeMemory<int>(resampler.ancestor);
    freeMemory<int>(resampler.cumulativeOffspring);
    freeMemory<floating_t>(resampler.prefixSum);
    freeParticles(resampler.auxParticles);
}

HOST DEV void destResamplerNested(resampler_t resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    freeParticlesNested(resampler.auxParticles);
}

HOST DEV void copyParticle(particles_t particlesDst, const particles_t particlesSrc, int dstIdx, int srcIdx, size_t progStateSize) {
    
    /*
    char* psDst = static_cast<char*>(particlesDst.progStates);
    char* psSrc = static_cast<char*>(particlesSrc.progStates);
    memcpy(&(psDst[progStateSize * dstIdx]), &(psSrc[progStateSize * srcIdx]), progStateSize);
    */

    
    long* psDst = static_cast<long*>(particlesDst.progStates);
    long* psSrc = static_cast<long*>(particlesSrc.progStates);
    // printf("progStateSize: %lu\n", progStateSize);
    int numDblWords = progStateSize/8;
    for(int i = 0; i < numDblWords; i++)
        psDst[numDblWords * dstIdx + i] = psSrc[numDblWords * srcIdx + i];
    
    // cudaMemcpyAsync(&(psDst[progStateSize * dstIdx]), &(psSrc[progStateSize * srcIdx]), progStateSize, cudaMemcpyDeviceToDevice);
    particlesDst.pcs[dstIdx] = particlesSrc.pcs[srcIdx];
    particlesDst.weights[dstIdx] = 0;
}
