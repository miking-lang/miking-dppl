#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

/*
 * File systematic_cpu.cuh contains the CPU implementation of the systematic resampling. 
 */

 #include "inference/smc/resample/common.cuh"

/**
 * Calculates the log weight prefix sums. 
 * 
 * @param w the weight array.
 * @param resampler the resampler struct.
 * @param numParticles the number of particles used in SMC.
 * @return the logarithm of the total weight sum. 
 */
HOST DEV floating_t calcLogWeightSumCpu(floating_t* w, resampler_t& resampler, int numParticles);

/**
 * Calculates the ESS (effective sample size). 
 *
 * @param w the array of particle weights
 * @param logWeightSum the logarithm of the sum of weights. 
 * @param numParticles the number of particles used in SMC.
 * @return the effective sample size.
 */
HOST DEV floating_t calcESSCpu(floating_t* w, floating_t logWeightSum, resampler_t resampler, int numParticles);

/**
 * Calculates the cumulative offspring of each particle. 
 * 
 * @param logPrefixSum the logarithm of the calculated inclusive prefix sums.
 * @param cumulativeOffspring the array to store the result in. 
 * @param u a sample from the standard uniform distribution. 
 * @param numParticles the number of particles used in SMC.
 */
HOST DEV void systematicCumulativeOffspringCpu(floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles);

/**
 * Uses the cumulative offspring to assign the ancestor indices used for resample propagation. 
 * 
 * @param cumulativeOffspring the array to read the cumulative offspring from. 
 * @param ancestor the array to store the result in. 
 * @param numParticles the number of particles used in SMC.
 */
HOST DEV void cumulativeOffspringToAncestorCpu(int* cumulativeOffspring, int* ancestor, int numParticles);

/**
 * Propagates the particles. Copies them from the ancestor to the new particles. 
 * 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 */
HOST DEV void copyStatesCpu(particles_t& particles, resampler_t& resampler, int numParticles);

/**
 * Performs CPU resampling. Used in both top-level and nested SMC. 
 * 
 * @param takes a curandState as argument if it runs on the GPU (nested SMC).
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 */
DEV void resampleSystematicCpu(RAND_STATE_DECLARE particles_t& particles, resampler_t& resampler, int numParticles);

/**
 * Takes the log-weights and subtracts the logarithm of the sum of weights. 
 *
 * @param w the array of scaled particle weights
 * @param logWeightSum the logarithm of the sum of weights. 
 * @param numParticles the number of particles used in SMC.
 */
 DEV void normaliseWeightsCpu(floating_t* w, floating_t logWeightSum, int numParticles);


#endif