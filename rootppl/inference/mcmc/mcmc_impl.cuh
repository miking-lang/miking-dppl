
#include "mcmc.cuh"

/*
 * WORK IN PROGRESS 
 */

default_random_engine generatorDistsTemp;

    uniform_real_distribution<floating_t> uniformDistTemp(0.0, 1.0);

// [min, max)
floating_t sampleUniformTemp(floating_t min, floating_t max) {
    return (uniformDistTemp(generatorDistsTemp) * (max - min)) + min;
}

template <typename T>
void runMCMC(pplFunc_t<T>* bblocks, statusFunc_t<T> statusFunc, int numBblocks, void* arg = NULL) {

    
    int maxTraceLength = 100; // Dynamic arraylist structure?
    // Allocate and initialize
    samplesMcmc_t<T> samples;

    samples.traces = new T*[NUM_ITERATIONS];
    samples.weights = new floating_t[NUM_ITERATIONS];
    samples.traceIdxs = new int[NUM_ITERATIONS];
    for(int i = 0; i < NUM_ITERATIONS; i++) {
        samples.traces[i] = new T[maxTraceLength]; // How many? Dynamic length of traces?
        samples.weights[i] = 0;
    }
    
    // Check how trace naming is done in the C3 paper (or its preceeding paper)
    // Do inference in model
    int pc = 0; // Regen point
    for (int it = 0; it < NUM_ITERATIONS; it++) {
        bblocks[pc](&samples, it, pc, arg);
        int traceIdx = samples.traceIdxs[it];
        pc = static_cast<int>(sampleUniformTemp(0, traceIdx)); // Will at least have one trace step remaining
    }

    statusFunc(&samples, NUM_ITERATIONS-1);

    // Clean up
    delete[] samples.weights;
    delete[] samples.traceIdxs;
    for(int i = 0; i < NUM_ITERATIONS; i++)
        delete[] samples.traces[i];
    delete[] samples.traces;

}
