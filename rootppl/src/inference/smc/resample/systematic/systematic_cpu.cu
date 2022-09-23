
/*
 * File systematic_cpu.cu contains the definitions of the CPU implementation of the systematic resampling.
 */

#include "inference/smc/resample/common.cuh"
#include "utils/misc.cuh"
#include "systematic_cpu.cuh"

#include <math.h>


HOST std::tuple<floating_t, floating_t> calcLogWeightSumAndESSCpu(floating_t* w, resampler_t& resampler, int numParticles) {

    floating_t maxLogWeight = -INFINITY;
    #pragma omp parallel for reduction(max:maxLogWeight)
    for(int i = 0; i < numParticles; i++) {
        maxLogWeight = w[i] > maxLogWeight ? w[i] : maxLogWeight;
    }
    resampler.maxLogWeight = maxLogWeight;

    // Corresponds to ExpWeightsKernel used in the parallel implementation
    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++) {
        w[i] = exp(w[i] - maxLogWeight);
    }

    // Calculates the inclusive prefix sum, parallelize?
    resampler.prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        resampler.prefixSum[i] = resampler.prefixSum[i-1] + w[i];

    // At this point: w are scaled weights (not log), prefixSum[numParticles-1] is the scaled sum
    floating_t ess = calcESSHelperCpu(w, resampler.prefixSum[numParticles - 1], numParticles);

    // Corresponds to the renormaliseKernel used in the parallel implementation
    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++) {
        resampler.prefixSum[i] = log(resampler.prefixSum[i]) + maxLogWeight;
        w[i] = log(w[i]) + maxLogWeight;
    }

    // return resampler.prefixSum[numParticles - 1];
    return std::make_tuple(resampler.prefixSum[numParticles - 1], ess);
}

HOST floating_t calcESSHelperCpu(floating_t* scaledW, floating_t scaledWeightSum, int numParticles) {
    floating_t wScaledSumOfSquares = 0;
    #pragma omp parallel for reduction (+:wScaledSumOfSquares)
    for(int i = 0; i < numParticles; i++) {
      wScaledSumOfSquares += scaledW[i] * scaledW[i];
    }

    floating_t wSumSquared = scaledWeightSum * scaledWeightSum;

    return wSumSquared / wScaledSumOfSquares;
}


HOST DEV void systematicCumulativeOffspringCpu(floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {

    floating_t totalSum = prefixSum[numParticles-1];
    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++) {

        floating_t expectedCumulativeOffspring = numParticles * exp(prefixSum[i] - totalSum);
        cumulativeOffspring[i] = MIN(numParticles, static_cast<int>(floor(expectedCumulativeOffspring + u)));
    }
}

HOST DEV void cumulativeOffspringToAncestorCpu(int* cumulativeOffspring, int* ancestor, int numParticles) {
    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++) {
        int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
        int numCurrentOffspring = cumulativeOffspring[i] - start;
        for(int j = 0; j < numCurrentOffspring; j++)
            ancestor[start+j] = i;
    }
}

HOST DEV void copyStatesCpu(particles_t& particles, resampler_t& resampler, int numParticles) {

    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++)
        copyParticle(resampler.auxParticles, particles, i, resampler.ancestor[i], resampler.progStateSize);

    particles_t tempAux = resampler.auxParticles;
    resampler.auxParticles = particles;
    particles = tempAux;
}

DEV void resampleSystematicCpu(RAND_STATE_DECLARE particles_t& particles, resampler_t& resampler, int numParticles) {

    floating_t u = SAMPLE(uniform, 0.0f, 1.0f);

    systematicCumulativeOffspringCpu(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);
    cumulativeOffspringToAncestorCpu(resampler.cumulativeOffspring, resampler.ancestor, numParticles);
    copyStatesCpu(particles, resampler, numParticles);
}

DEV void normaliseWeightsCpu(floating_t* w, floating_t logWeightSum, int numParticles) {

    #pragma omp parallel for
    for(int i = 0; i < numParticles; i++)
        w[i] -= logWeightSum;
        // w[i] = log(w[i]) + resampler.maxLogWeight - logWeightSum;
}
