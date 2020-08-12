#ifndef RESAMPLE_COMMON_INCLUDED
#define RESAMPLE_COMMON_INCLUDED

/*
 * File common.cuh contains headers used by both sequential and parallel systematic resampling. 
 */

#include <random>
#include "inference/smc/smc.cuh"

// Generator for CPU RNG
extern std::default_random_engine generatorRes;
extern std::uniform_real_distribution<floating_t> uniformCPU;

/*
 * Resampler structure for Systematic resampling. Contains pointers to structures necessary for the resampling. 
 *
 * ancestor: Array used to store indices of ancestors in resampling.
 * cumulativeOffspring: Array used to store the cumulative number of offspring for each particle. 
 * prefixSum: Array used to store the inclusive prefix sum. 
 * auxParticles: Particles structure used to copy particles in resample propagation. Required as the propagation is not in-place. 
 * progStateSize: the size of the program state
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
 * @param progStateSize the size of the particles program states in bytes. 
 * @return the allocated resampler struct. 
 */
resampler_t initResampler(int numParticles, size_t progStateSize);

/**
 * Allocates resampler and its arrays. 
 * This should be used for nseted inference. 
 *
 * @param numParticles the number of particles used in nested SMC.
 * @param progStateSize the size of the particles program states in bytes. 
 * @return the allocated resampler struct. 
 */
HOST DEV resampler_t initResamplerNested(int numParticles, size_t progStateSize);

/**
 * Frees the allocated arrays used by the resampler.
 * This should be used for top-level inference.  
 *
 * @param resampler the resampler which should be freed.
 */
void destResampler(resampler_t resampler);

/**
 * Frees the allocated arrays used by the resampler.
 * This should be used for nested inference.  
 *
 * @param resampler the resampler which should be freed.
 */
HOST DEV void destResamplerNested(resampler_t resampler);

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
HOST DEV void copyParticle(particles_t particlesDst, const particles_t particlesSrc, int dstIdx, int srcIdx, size_t progStateSize);

#endif