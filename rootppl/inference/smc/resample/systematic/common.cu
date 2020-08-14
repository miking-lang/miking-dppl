
/*
 * File common.cu contains definitions used by both sequential and parallel systematic resampling. 
 */

#include <random>
#include <time.h>
#include "common.cuh"
#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "inference/smc/particles_memory_handler.cuh"

#ifdef __NVCC__
std::default_random_engine generatorRes;
std::uniform_real_distribution<floating_t> uniformCPU(0.0, 1.0);
#endif

resampler_t initResampler(int numParticles, size_t progStateSize) {

    #ifdef __NVCC__
    generatorRes.seed(time(NULL) * 3); // Multiply by 3 to avoid same seed as distributions. 
    #endif
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

HOST DEV void copyParticle(const particles_t particlesDst, const particles_t particlesSrc, int dstIdx, int srcIdx, size_t progStateSize) {
    
    char* psDstAddress = static_cast<char*>(particlesDst.progStates) + progStateSize * dstIdx;
    char* psSrcAddress = static_cast<char*>(particlesSrc.progStates) + progStateSize * srcIdx;

    // If the struct is aligned in the correct way, the loop long copying can give huge speedups compared to memcpy on GPU
    bool longAligned = progStateSize % sizeof(long) == 0 
                    && ((std::uintptr_t)psDstAddress) % sizeof(long) == 0
                    && ((std::uintptr_t)psSrcAddress) % sizeof(long) == 0;

    if(longAligned) {
        long* psDstLong = (long*)(psDstAddress);
        long* psSrcLong = (long*)(psSrcAddress);

        int numDblWords = progStateSize / sizeof(long);

        for(int i = 0; i < numDblWords; i++) {
            psDstLong[i] = psSrcLong[i];
        }
        
    } else {
        bool intAligned = progStateSize % sizeof(int) == 0 
                    && ((std::uintptr_t)psDstAddress) % sizeof(int) == 0
                    && ((std::uintptr_t)psSrcAddress) % sizeof(int) == 0;
        if(intAligned) {

            int* psDstInt = (int*)(psDstAddress);
            int* psSrcInt = (int*)(psSrcAddress);

            int numWords = progStateSize / sizeof(int);

            for(int i = 0; i < numWords; i++) {
                psDstInt[i] = psSrcInt[i];
            }

        } else {
            memcpy(psDstAddress, psSrcAddress, progStateSize);
        }
    }
    
    particlesDst.pcs[dstIdx] = particlesSrc.pcs[srcIdx];
    particlesDst.weights[dstIdx] = 0;
}
