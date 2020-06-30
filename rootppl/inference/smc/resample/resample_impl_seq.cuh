#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

#include "resample_common.cuh"

HOST DEV void expWeightsSeq(floating_t* w, int numParticles, floating_t maxLogWeight) {
    for(int i = 0; i < numParticles; i++) {
        w[i] = exp(w[i]- maxLogWeight);
    }
}

HOST DEV void calcInclusivePrefixSumSeq(floating_t* w, floating_t* prefixSum, int numParticles) {
    prefixSum[0] = w[0];
    for(int i = 1; i < numParticles; i++)
        prefixSum[i] = prefixSum[i-1] + w[i];
}

HOST DEV floating_t calcWeightSumSeq(floating_t* w, resampler_t resampler, int numParticles) {
    floating_t maxLogWeight = maxNaive(w, numParticles);

    // Corresponds to ExpWeightsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        w[i] = exp(w[i]- maxLogWeight);

    // expWeightsSeq(particles->weights, numParticles, maxLogWeight);

    calcInclusivePrefixSumSeq(w, resampler.prefixSum, numParticles);

    // Corresponds to the renormaliseSumsKernel used in the parallel implementation
    for(int i = 0; i < numParticles; i++)
        resampler.prefixSum[i] = exp(log(resampler.prefixSum[i]) + maxLogWeight);

    return resampler.prefixSum[numParticles - 1];
}

HOST DEV
void systematicCumulativeOffspringSeq(floating_t* prefixSum, int* cumulativeOffspring, floating_t u, int numParticles) {

    floating_t totalSum = prefixSum[numParticles-1];
    for(int i = 0; i < numParticles; i++) {
        floating_t expectedCumulativeOffspring = numParticles * prefixSum[i] / totalSum;
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
HOST DEV void copyStatesSeq(particles_t<T>** particlesPtrToPtr, resampler_t& resampler, int numParticles) {
    particles_t<T>* particles = *particlesPtrToPtr;
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(resampler.tempArr);

    //for(int i = 0; i < numParticles; i++)
        //copyParticle(particles, tempArrP, i, i);
        
    for(int i = 0; i < numParticles; i++)
        copyParticle(tempArrP, particles, i, resampler.ancestor[i]); // watch out for references in the struct!
    
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;

}

// i is index of executing CUDA-thread, in case of nested inference
template <typename T>
DEV void resampleSystematicSeq(RAND_STATE_DECLARE particles_t<T>** particlesPtrToPtr, resampler_t& resampler, int numParticles) {
    
    // This should do it?
    // floating_t u = uniform(RAND_STATE_ACCESS, 0.0f, 1.0f);
    #ifdef GPU
    // int i = 0; // needed by SAMPLE, index of particle to use curandState from in case of nested inference
    // curandState* randState = &particles->randStates[i];
    // curandState* randState = randStates[i];
    floating_t u = uniform(randState, 0.0f, 1.0f);
    #else
    floating_t u = uniform(0.0f, 1.0f); // CPU: i is not used, GPU: use randState in first particle
    // floating_t u = uniform(&particles->randStates[i], 0.0f, 1.0f); // CPU: i is not used, GPU: use randState in first particle
    #endif

    systematicCumulativeOffspringSeq(resampler.prefixSum, resampler.cumulativeOffspring, u, numParticles);
    cumulativeOffspringToAncestorSeq(resampler.cumulativeOffspring, resampler.ancestor, numParticles);
    copyStatesSeq<T>(particlesPtrToPtr, resampler, numParticles);
}

#endif