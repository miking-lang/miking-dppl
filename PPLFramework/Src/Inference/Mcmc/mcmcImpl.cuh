
#include "mcmc.cuh"

template <typename T>
double runMCMC(pplFunc_t<T>* bblocks, statusFunc_t<T> statusFunc, int numBblocks, void* arg = NULL) {
    
    // Allocate and initialize
    samplesMcmc_t<T> samples;

    samples.traces = new T*[NUM_ITERATIONS];
    samples.weights = new floating_t[NUM_ITERATIONS];
    for(int i = 0; i < NUM_ITERATIONS; i++) {
        samples.traces[i] = new T[numBblocks]; // How many? Dynamic length of traces?
        samples.weights[i] = 0;
    }
    

    // Do inference in model
    int pc = 0; // Regen point
    for (int it = 0; it < NUM_ITERATIONS; it++) {
        bblocks[pc](&samples, it, pc, arg);
    }

    statusFunc(&samples, NUM_ITERATIONS-1);

    // Clean up
    delete[] samples.weights;
    for(int i = 0; i < NUM_ITERATIONS; i++)
        delete[] samples.traces[i];
    delete[] samples.traces;

}
