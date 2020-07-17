#ifndef RESAMPLE_GPU_INCLUDED
#define RESAMPLE_GPU_INCLUDED

/*
 * File parallel.cuh contains the parallel implementation of the systematic resampling. 
 */

#ifdef __NVCC__

#include "common.cuh"

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
 * @return the logarithm of the total weight sum. 
 */
HOST DEV floating_t calcLogWeightSumPar(floating_t* w, resampler_t resampler, int numParticles, int numBlocks, int numThreadsPerBlock);

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
 * Performs parallel resampling. Uses CUDA Dynamic Parallelism (launches kernels within kernels). Used in nested inference.
 * 
 * @param randState the curandState used to draw a random uniform sample on the GPU. 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 */
DEV void resampleSystematicParNested(curandState* randState, particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks);

/**
 * Performs parallel resampling. Uses CUDA Dynamic Parallelism (launches kernels within kernels). Used in top-level inference. 
 * 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 * @param numBlocks kernel launch setting.
 */
void resampleSystematicPar(particles_t& particles, resampler_t& resampler, int numParticles, int numBlocks);

#endif

#endif