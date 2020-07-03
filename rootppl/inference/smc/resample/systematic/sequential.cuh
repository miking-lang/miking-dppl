#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

#include "common.cuh"


HOST DEV void calcInclusivePrefixSumSeq(floating_t* w, floating_t* prefixSum, int numParticles) {
    prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        prefixSum[i] = prefixSum[i-1] + w[i];
}

template <typename T>
HOST DEV floating_t calcWeightSumSeq(floating_t* w, resampler_t<T> resampler, int numParticles) {
    floating_t maxLogWeight = maxNaive(w, numParticles);

    // Corresponds to ExpWeightsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        w[i] = exp(w[i] - maxLogWeight);


    calcInclusivePrefixSumSeq(w, resampler.prefixSum, numParticles);

    // Corresponds to the renormaliseSumsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        resampler.prefixSum[i] = log(resampler.prefixSum[i]) + maxLogWeight;    

    return resampler.prefixSum[numParticles - 1];
}

HOST DEV void systematicCumulativeOffspringSeq(floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {

    floating_t totalSum = prefixSum[numParticles-1];
    for(int i = 0; i < numParticles; i++) {

        floating_t expectedCumulativeOffspring = numParticles * exp(prefixSum[i] - totalSum);
        cumulativeOffspring[i] = min(numParticles, static_cast<int>(floor(expectedCumulativeOffspring + u)));
    }
}

HOST DEV void cumulativeOffspringToAncestorSeq(int* cumulativeOffspring, int* ancestor, int numParticles) {
    for(int i = 0; i < numParticles; i++) {
        int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
        int numCurrentOffspring = cumulativeOffspring[i] - start;
        for(int j = 0; j < numCurrentOffspring; j++)
            ancestor[start+j] = i;
    }
}

template <typename T>
HOST DEV void copyStatesSeq(particles_t<T>& particles, resampler_t<T>& resampler, int numParticles) {
        
    for(int i = 0; i < numParticles; i++)
        copyParticle(resampler.tempArr, particles, i, resampler.ancestor[i]); // watch out for references in the struct!
    
    particles_t<T> tempArrP;
    tempArrP = resampler.tempArr;
    resampler.tempArr = particles;
    particles = tempArrP;
}


template <typename T>
DEV void resampleSystematicSeq(RAND_STATE_DECLARE particles_t<T>& particles, resampler_t<T>& resampler, int numParticles) {
    
    floating_t u = uniform(RAND_STATE_ACCESS 0.0f, 1.0f);

    systematicCumulativeOffspringSeq(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);
    cumulativeOffspringToAncestorSeq(resampler.cumulativeOffspring, resampler.ancestor, numParticles);
    copyStatesSeq<T>(particles, resampler, numParticles);
}

#endif