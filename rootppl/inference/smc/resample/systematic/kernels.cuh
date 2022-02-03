#ifndef RESAMPLE_KERNELS_INCLUDED
#define RESAMPLE_KERNELS_INCLUDED

/*
 * File kernels.cuh contains kernels used by systematic resampling.
 */


/**
 * Scales the weights (shift in log-space) and takes the exponent of them.
 * This is used when calculating the weight sums safely.
 *
 * @param w the weight array.
 * @param numParticles the number of particles used in SMC.
 * @param maxLogWeight the log of the maximum weight.
 * @param wSquared the output array that should hold the scaled squared weights
 */
__global__ void scaleExpWeightsAndSquareWeightsKernel(floating_t* w, int numParticles, floating_t maxLogWeight, floating_t* wSquared);

/**
 * Takes the logarithm of the prefixSum and weight belonging to the current particle then scales back.
 * This is used when calculating the weight sums safely.
 *
 * @param w the weight array.
 * @param prefixSum the calculated inclusive prefix sums which should be logged and scaled back.
 * @param numParticles the number of particles used in SMC.
 * @param maxLogWeight the log of the maximum weight.
 */
__global__ void renormaliseKernel(floating_t* w, floating_t* prefixSum, int numParticles, floating_t maxLogWeight);

/**
 * Calculates the cumulative offspring of each particle.
 *
 * @param logPrefixSum the logarithm of the calculated inclusive prefix sums.
 * @param cumulativeOffspring the array to store the result in.
 * @param u a sample from the standard uniform distribution.
 * @param numParticles the number of particles used in SMC.
 */
__global__ void systematicCumulativeOffspringKernel(const floating_t* logPrefixSum, int* cumulativeOffspring, floating_t u, int numParticles);

/**
 * Uses the cumulative offspring to assign the ancestor indices used for resample propagation.
 *
 * @param cumulativeOffspring the array to read the cumulative offspring from.
 * @param ancestor the array to store the result in.
 * @param numParticles the number of particles used in SMC.
 */
__global__ void cumulativeOffspringToAncestorKernel(const int* cumulativeOffspring, int* ancestor, int numParticles);

/**
 * Propagates the particles. Copies them from the ancestor to the new particles.
 *
 * @param particlesDst the destination array to copy to.
 * @param particlesSrc the source array to copy from.
 * @param ancestor the array containing the ancestor indices.
 * @param numParticles the number of particles used in SMC.
 */
__global__ void copyStatesKernel(particles_t particlesDst, const particles_t particlesSrc, int* ancestor, int numParticles, size_t progStateSize);


/**
 * Takes the log of the exponentiated log weight and subtracts with the logarithm of the sum of weights.
 *
 * @param w the scaled particle weights
 * @param logWeightSum the logarithm of the sum of weights
 * @param numParticles the number of particles used in SMC.
 */
__global__ void normaliseWeightsKernel(floating_t* w, floating_t logWeightSum, int numParticles);


#endif