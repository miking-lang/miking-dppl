#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

#include "resampleCommon.cuh"

HOST DEV void expWeightsSeq(floating_t* w, int numParticles) {
    for(int i = 0; i < numParticles; i++) {
        // w[i] = pow(2, w[i]);
        w[i] = exp(w[i]);
    }
}

template <typename T>
HOST DEV void calcInclusivePrefixSumSeq(particles_t<T>* particles, floating_t* prefixSum, int numParticles) {
    prefixSum[0] = particles->weights[0];
    for(int i = 1; i < numParticles; i++) {
        prefixSum[i] = prefixSum[i-1] + particles->weights[i];
    }
}

template <typename T>
HOST DEV floating_t calcWeightSumSeq(particles_t<T>* particles, resampler_t resampler, int numParticles) {
    expWeightsSeq(particles->weights, numParticles);
    calcInclusivePrefixSumSeq<T>(particles, resampler.prefixSum, numParticles);
    //if(resampler.prefixSum[numParticles-1] == 0)
        //printf("Error: prefixSum = 0!\n");
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

    for(int i = 0; i < numParticles; i++)
        copyParticle(particles, tempArrP, i, i);

    //for(int i = 0; i < numParticles; i++)
        //copyParticle(tempArrP, particles, resampler.ancestor[i], i); // watch out for references in the struct!
    
    resampler.tempArr = static_cast<void*>(particles);
    *particlesPtrToPtr = tempArrP;

}

// i is index of executing CUDA-thread, in case of nested inference
template <typename T>
DEV void resampleSystematicSeq(RAND_STATE_DECLARE particles_t<T>** particlesPtrToPtr, resampler_t& resampler, int numParticles) {
    
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

/*
template <typename T>
HOST DEV floating_t resampleSystematicSeqNested(particles_t<T>* particles, resampler_t resampler, bool nested = false) {
    expWeightsSeq(particles->weights);
    calcInclusivePrefixSumSeq<T>(particles, resampler.prefixSum);
    if(resampler.prefixSum[NUM_PARTICLES-1] == 0)
        printf("Error: prefixSum = 0!\n");
    
    floating_t u = uniform(particles, 0, 0.0f, 1.0f); // i is not used if CPU
        
    systematicCumulativeOffspringSeq(resampler.prefixSum, resampler.cumulativeOffspring, u);
    cumulativeOffspringToAncestorSeq(resampler.cumulativeOffspring, resampler.ancestor);
    copyStatesSeq<T>(particles, resampler.ancestor, resampler.tempArr);
    return resampler.prefixSum[NUM_PARTICLES-1];
}
*/

#endif