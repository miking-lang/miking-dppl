#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

/*
 * File sequential.cuh contains the sequential implementation of the systematic resampling. 
 */

#include "common.cuh"


/**
 * Calculates the log weight prefix sums safely according to the identity in the appendix of 
 * the paper "Parallel resampling in the particle filter", L. M. Murray et. al. 
 * 
 * @param w the weight array.
 * @param resampler the resampler struct.
 * @param numParticles the number of particles used in SMC.
 * @return the logarithm of the total weight sum. 
 */
template <typename T>
HOST DEV floating_t calcLogWeightSumSeq(floating_t* w, resampler_t<T> resampler, int numParticles) {
    floating_t maxLogWeight = maxNaive(w, numParticles);

    // Corresponds to ExpWeightsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        w[i] = exp(w[i] - maxLogWeight);

    // Calculates in the inclusive prefix sum
    resampler.prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        resampler.prefixSum[i] = resampler.prefixSum[i-1] + w[i];

    // Corresponds to the renormaliseSumsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        resampler.prefixSum[i] = log(resampler.prefixSum[i]) + maxLogWeight;    

    return resampler.prefixSum[numParticles - 1];
}

/**
 * Calculates the cumulative offspring of each particle. 
 * 
 * @param logPrefixSum the logarithm of the calculated inclusive prefix sums.
 * @param cumulativeOffspring the array to store the result in. 
 * @param u a sample from the standard uniform distribution. 
 * @param numParticles the number of particles used in SMC.
 */
HOST DEV void systematicCumulativeOffspringSeq(floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {

    floating_t totalSum = prefixSum[numParticles-1];
    for(int i = 0; i < numParticles; i++) {

        floating_t expectedCumulativeOffspring = numParticles * exp(prefixSum[i] - totalSum);
        cumulativeOffspring[i] = MIN(numParticles, static_cast<int>(floor(expectedCumulativeOffspring + u)));
    }
}

/**
 * Uses the cumulative offspring to assign the ancestor indices used for resample propagation. 
 * 
 * @param cumulativeOffspring the array to read the cumulative offspring from. 
 * @param ancestor the array to store the result in. 
 * @param numParticles the number of particles used in SMC.
 */
HOST DEV void cumulativeOffspringToAncestorSeq(int* cumulativeOffspring, int* ancestor, int numParticles) {
    for(int i = 0; i < numParticles; i++) {
        int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
        int numCurrentOffspring = cumulativeOffspring[i] - start;
        for(int j = 0; j < numCurrentOffspring; j++)
            ancestor[start+j] = i;
    }
}

/**
 * Propagates the particles. Copies them from the ancestor to the new particles. 
 * 
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 */
template <typename T>
HOST DEV void copyStatesSeq(particles_t<T>& particles, resampler_t<T>& resampler, int numParticles) {
        
    for(int i = 0; i < numParticles; i++)
        copyParticle(resampler.auxParticles, particles, i, resampler.ancestor[i]);
    
    particles_t<T> tempAux = resampler.auxParticles;
    resampler.auxParticles = particles;
    particles = tempAux;
}

/**
 * Performs sequential resampling. Used in both top-level and nested SMC. 
 * 
 * @param takes a curandState as argument if it runs on the GPU (nested SMC).
 * @param particles the particles struct reference.
 * @param resampler the resampler struct reference.
 * @param numParticles the number of particles used in SMC.
 */
template <typename T>
DEV void resampleSystematicSeq(RAND_STATE_DECLARE particles_t<T>& particles, resampler_t<T>& resampler, int numParticles) {
    
    floating_t u = SAMPLE(uniform, 0.0f, 1.0f);

    systematicCumulativeOffspringSeq(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);
    cumulativeOffspringToAncestorSeq(resampler.cumulativeOffspring, resampler.ancestor, numParticles);
    copyStatesSeq<T>(particles, resampler, numParticles);
}

#endif