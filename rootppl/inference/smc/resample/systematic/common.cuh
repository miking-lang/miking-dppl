#ifndef RESAMPLE_COMMON_INCLUDED
#define RESAMPLE_COMMON_INCLUDED

/*
 * File common.cuh contains definitions used by both sequential and parallel systematic resampling. 
 */

#include <random>
#include <time.h>

// Generator for CPU RNG
std::default_random_engine generatorRes;
std::uniform_real_distribution<floating_t> uniformCPU(0.0, 1.0);

/*
 * Resampler structure for Systematic resampling. Contains pointers to structures necessary for the resampling. 
 *
 * ancestor: Array used to store indices of ancestors in resampling.
 * cumulativeOffspring: Array used to store the cumulative number of offspring for each particle. 
 * prefixSum: Array used to store the inclusive prefix sum. 
 * auxParticles: Particles structure used to copy particles in resample propagation. Required as the propagation is not in-place. 
 */
struct resampler_t {

    int* ancestor; 
    int* cumulativeOffspring;
    floating_t* prefixSum;
    particles_t auxParticles;
    size_t progStateSize;
};

/**
 * Allocates resampler and its arrays and set the seed for the CPU RNG.
 * This should be used for top-level inference.  
 *
 * @param numParticles the number of particles used in SMC.
 * @return the allocated resampler struct. 
 */
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

/**
 * Allocates resampler and its arrays. 
 * This should be used for nseted inference. 
 *
 * @param numParticles the number of particles used in nested SMC.
 * @return the allocated resampler struct. 
 */
/*template <typename T>
HOST DEV resampler_t<T> initResamplerNested(int numParticles) {

    resampler_t<T> resampler;

    resampler.ancestor = new int[numParticles];
    resampler.cumulativeOffspring = new int[numParticles];
    resampler.prefixSum = new floating_t[numParticles];
    resampler.auxParticles = allocateParticlesNested<T>(numParticles);

    return resampler;
}*/

/**
 * Frees the allocated arrays used by the resampler.
 * This should be used for top-level inference.  
 *
 * @param resampler the resampler which should be freed.
 */
void destResampler(resampler_t resampler) {

    freeMemory<int>(resampler.ancestor);
    freeMemory<int>(resampler.cumulativeOffspring);
    freeMemory<floating_t>(resampler.prefixSum);
    freeParticles(resampler.auxParticles);
}

/**
 * Frees the allocated arrays used by the resampler.
 * This should be used for nested inference.  
 *
 * @param resampler the resampler which should be freed.
 */
/*template <typename T>
HOST DEV void destResamplerNested(resampler_t<T> resampler) {
    delete[] resampler.ancestor;
    delete[] resampler.cumulativeOffspring;
    delete[] resampler.prefixSum;
    freeParticlesNested<T>(resampler.auxParticles);
}*/

/**
 * Copies data from one particle in the source array to a particle in the destination array and resets the weight. 
 * Used in resampling. Does NOT handle references that may exist in the progStates.
 * Such references could be handled by overloading the progState struct's assignment operator. 
 *
 * @param particlesDst the destination array to copy to.
 * @param particlesSrc the source array to copy from.
 * @param dstIdx the index in particlesDst to write to.
 * @param srcIdx the index in particlesSrc to read from. 
 */
HOST DEV void copyParticle(particles_t particlesDst, const particles_t particlesSrc, int dstIdx, int srcIdx, int progStateSize) {
    // particlesDst.progStates[dstIdx] = particlesSrc.progStates[srcIdx];
    for(int i = 0; i < progStateSize/4; i++)
        static_cast<int*>(particlesDst.progStates)[dstIdx + i] = static_cast<int*>(particlesSrc.progStates)[srcIdx + i];
    particlesDst.pcs[dstIdx] = particlesSrc.pcs[srcIdx];
    particlesDst.weights[dstIdx] = 0;
}

#endif