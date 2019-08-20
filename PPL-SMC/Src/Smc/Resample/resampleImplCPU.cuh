#ifndef RESAMPLE_CPU_INCLUDED
#define RESAMPLE_CPU_INCLUDED

#include "resampleCommon.cuh"

void expWeights(floating_t* w) {
    for(int i = 0; i < NUM_PARTICLES; i++) {
        // w[i] = pow(2, w[i]);
        w[i] = exp(w[i]);
    }
}

template <typename T>
void calcInclusivePrefixSum(particles_t<T>* particles, floating_t* prefixSum) {
    prefixSum[0] = particles->weights[0];
    for(int i = 1; i < NUM_PARTICLES; i++) {
        prefixSum[i] = prefixSum[i-1] + particles->weights[i];
    }
}

void systematicCumulativeOffspring(floating_t* prefixSum, int* cumulativeOffspring) {

    floating_t u = uDistRes(generatorRes);
    floating_t totalSum = prefixSum[NUM_PARTICLES-1];
    for(int i = 0; i < NUM_PARTICLES; i++) {
        floating_t expectedCumulativeOffspring = NUM_PARTICLES * prefixSum[i] / totalSum;
        cumulativeOffspring[i] = min(NUM_PARTICLES, static_cast<int>(floor(expectedCumulativeOffspring + u)));
    }
}

void cumulativeOffspringToAncestor(int* cumulativeOffspring, int* ancestor) {
    for(int i = 0; i < NUM_PARTICLES; i++) {
        int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
        int numCurrentOffspring = cumulativeOffspring[i] - start;
        for(int j = 0; j < numCurrentOffspring; j++)
            ancestor[start+j] = i;
    }
}

template <typename T>
void copyStates(particles_t<T>* particles, int* ancestor) {
    particles_t<T>* tempArrP = static_cast<particles_t<T>*>(tempArr);

    for(int i = 0; i < NUM_PARTICLES; i++)
        copyParticle(particles, tempArrP, i, i);

    for(int i = 0; i < NUM_PARTICLES; i++)
        copyParticle(tempArrP, particles, ancestor[i], i); // watch out for references in the struct!

}

template <typename T>
floating_t resampleSystematic(particles_t<T>* particles) {
    expWeights(particles->weights);
    calcInclusivePrefixSum<T>(particles, prefixSum);
    if(prefixSum[NUM_PARTICLES-1] == 0)
        printf("Error: prefixSum = 0!\n");
    systematicCumulativeOffspring(prefixSum, cumulativeOffspring);
    cumulativeOffspringToAncestor(cumulativeOffspring, ancestor);
    copyStates<T>(particles, ancestor);
    return prefixSum[NUM_PARTICLES-1];
}


#endif