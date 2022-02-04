
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

    resampler_t resampler;

    #ifdef __NVCC__
    generatorRes.seed(time(NULL) * 3); // Multiply by 3 to avoid same seed as distributions.
    allocateMemory<floating_t>(&resampler.wSquared, numParticles);
    #endif

    allocateMemory<int>(&resampler.ancestor, numParticles);
    allocateMemory<int>(&resampler.cumulativeOffspring, numParticles);
    allocateMemory<floating_t>(&resampler.prefixSum, numParticles);

    resampler.auxParticles = allocateParticles(numParticles, progStateSize);
    resampler.progStateSize = progStateSize;

    return resampler;
}

void destResampler(resampler_t resampler) {

    freeMemory<int>(resampler.ancestor);
    freeMemory<int>(resampler.cumulativeOffspring);
    freeMemory<floating_t>(resampler.prefixSum);
    #ifdef __NVCC__
    freeMemory<floating_t>(resampler.wSquared);
    #endif
    freeParticles(resampler.auxParticles);
}

/*
// Obsolete
HOST DEV resampler_t initResamplerNested(int numParticles, size_t progStateSize) {

    resampler_t resampler;

    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    resampler.auxParticles = allocateParticlesNested(numParticles, progStateSize);

    return resampler;
}

HOST DEV void destResamplerNested(resampler_t resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    freeParticlesNested(resampler.auxParticles);
}
*/

HOST DEV void copyParticle(const particles_t particlesDst, const particles_t particlesSrc, int dstIdx, int srcIdx, size_t progStateSize) {

    // Program states
    #ifdef STACK_SIZE_PROGSTATE
    progStateStack_t* dstProgState = particlesDst.progStates + dstIdx;
    progStateStack_t* srcProgState = particlesSrc.progStates + srcIdx;

    copyStack(dstProgState, srcProgState);

    #else
    char* psDstAddress = static_cast<char*>(particlesDst.progStates) + progStateSize * dstIdx;
    char* psSrcAddress = static_cast<char*>(particlesSrc.progStates) + progStateSize * srcIdx;

    copyChunk(psDstAddress, psSrcAddress, progStateSize);

    #endif

    // Generic particle stuff
    particlesDst.next[dstIdx] = particlesSrc.next[srcIdx];
    particlesDst.weights[dstIdx] = 0;
}

#ifdef STACK_SIZE_PROGSTATE
HOST DEV void copyStack(progStateStack_t* dst, progStateStack_t* src) {
    dst->stackPtr = src->stackPtr;
    size_t stackSpaceUsed = src->stackPtr;

    // Try to round up copy size to nearest multiple of sizeof(long), this can speed up GPU copying
    #ifdef __NVCC__
    int remainder = stackSpaceUsed % sizeof(long);
    if (remainder > 0)
        stackSpaceUsed = MIN(stackSpaceUsed + sizeof(long) - remainder, STACK_SIZE_PROGSTATE);
    #endif

    copyChunk(dst->stack, src->stack, stackSpaceUsed);
}
#endif

HOST DEV void copyChunk(void* dst, void* src, size_t bytes) {
    #ifdef __NVCC__
    // Manual loop copying can be much faster on GPU than device memcpy
    // If the struct is aligned in the correct way, the loop long copying can give huge speedups compared to memcpy on GPU
    bool longAligned = bytes % sizeof(long) == 0
                    && ((std::uintptr_t)dst) % sizeof(long) == 0
                    && ((std::uintptr_t)src) % sizeof(long) == 0;

    if(longAligned) {
        long* dstLong = (long*)(dst);
        long* srcLong = (long*)(src);

        int numDblWords = bytes / sizeof(long);

        for(int i = 0; i < numDblWords; i++) {
            dstLong[i] = srcLong[i];
        }

    } else {
        bool intAligned = bytes % sizeof(int) == 0
                    && ((std::uintptr_t)dst) % sizeof(int) == 0
                    && ((std::uintptr_t)src) % sizeof(int) == 0;
        if(intAligned) {

            int* dstInt = (int*)(dst);
            int* srcInt = (int*)(src);

            int numWords = bytes / sizeof(int);

            for(int i = 0; i < numWords; i++) {
                dstInt[i] = srcInt[i];
            }

        } else {
            // Not aligned, fall back to memcpy
            memcpy(dst, src, bytes);
        }
    }
    #else
    // On CPU, memcpy seems to perform much better. Seems to be true with OpenMP as well
    memcpy(dst, src, bytes);
    #endif
}

// This could probably be optimized by sorting the particles by descending weights first
DEV int sampleAncestor(RAND_STATE_DECLARE const floating_t* w, const floating_t logWeightSum, const int numParticles) {
    floating_t u = SAMPLE(uniform, 0.0f, logWeightSum);
    floating_t accLogWeightSum = 0;
    for (int i = 0; i < numParticles; i++) {
        accLogWeightSum += w[i];
        if (accLogWeightSum >= u) {
            return i;
        }
    }
    return numParticles - 1;
}
