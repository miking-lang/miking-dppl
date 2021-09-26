#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

/*
 * File systematic_gpu.cuh contains the GPU implementation of the systematic resampling. 
 */

#ifdef __NVCC__

#include <tuple>

#include "inference/smc/resample/common.cuh"

HOST DEV void prefixSumNaive(floating_t* w, resampler_t resampler, int numParticles);
/**
 * Calculates the log weight prefix sums safely according to the identity in the appendix of 
 * the paper "Parallel resampling in the particle filter", L. M. Murray et. al. 
 * 
 * @param w the weight array.
 * @param resampler the resampler struct.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 * @param numThreadsPerBlock kernel launch setting.
 * @return tuple (log weight sum, ess)
 */
HOST std::tuple<floating_t, floating_t> calcLogWeightSumAndESSGpu(floating_t* w, resampler_t& resampler, int numParticles, int numBlocks, int numThreadsPerBlock);

/**
 * Calculates the ESS (effective sample size).  
 * 
 * @param scaledW the max weight scaled weight array.
 * @param scaledWeightSum the max weight scaled weight sum. 
 * @param scaledWSquared the max weight scaled weights squared. 
 * @param numParticles the number of particles used in SMC.
 * @return the effective sample size.
 */
HOST floating_t calcESSHelperGpu(floating_t* scaledW, floating_t scaledWeightSum, floating_t* scaledWSquared, int numParticles);


/**
 * Calculates the cumulative offsprings and then uses it to calculate the ancestor indices. 
 * 
 * @param resampler the resampler struct reference.
 * @param u a sample from the standard uniform distribution.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 * @param numThreadsPerBlock kernel launch setting.
 */
HOST DEV void decideAncestors(resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock);

/**
 * Decides ancestors, propagates particles, and swaps pointers. (To avoid an additional copy kernel.)
 * 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param u a sample from the standard uniform distribution.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 * @param numThreadsPerBlock kernel launch setting.
 */
HOST DEV void postUniform(particles_t& particles, resampler_t& resampler, floating_t u, int numParticles, int numBlocks, int numThreadsPerBlock);

/**
 * Performs GPU resampling. Uses CUDA Dynamic Parallelism (launches kernels within kernels). Used in nested inference.
 * 
 * @param randState the curandState used to draw a random uniform sample on the GPU. 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 */
DEV void resampleSystematicGpuNested(curandState* randState, particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks);

/**
 * Performs GPU resampling. Uses CUDA Dynamic Parallelism (launches kernels within kernels). Used in top-level inference. 
 * 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 */
void resampleSystematicGpu(particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks);


/**
 * Takes the log-weights and subtracts the logarithm of the sum of weights. 
 *
 * @param w the array of scaled particle weights
 * @param logWeightSum the logarithm of the sum of weights. 
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 * @param numThreadsPerBlock kernel launch setting.
 */
void normaliseWeightsGpu(floating_t* w, floating_t logWeightSum, int numParticles, int numBlocks, int numThreadsPerBlock);

#endif

#endif
