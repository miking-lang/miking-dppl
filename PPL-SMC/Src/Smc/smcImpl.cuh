#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

#include <iostream>
#include "../cudaErrorUtils.cu"
#include "../Utils/timer.h"

#ifdef GPU
#include "Resample/resampleImplGPU.cuh"
#include "generalKernels.cuh"
#else
#include "Resample/resampleImplCPU.cuh"
#endif

const bool DEBUG = false;

template <typename T>
void allocateMemory(T** pointer, size_t n) {
    size_t memSize = sizeof(T) * n;
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(pointer, memSize));
    #else
    *pointer = static_cast<T*>(malloc(memSize));
    #endif
}

template <typename T>
void freeMemory(T* pointer) {
    #ifdef GPU
    cudaSafeCall(cudaFree(pointer));
    #else
    delete[] pointer;
    #endif
}


#ifdef GPU
void initRandStates(curandState* randStates) {
    initRandStatesKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, time(NULL));
    cudaDeviceSynchronize();
    cudaCheckError();
}
#endif

template <typename T>
void runSMC(pplFunc_t<T>* funcs, statusFunc_t<T> statusFunc) {

    // Init
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1); 

    #ifdef GPU
    initRandStates(particles->randStates);
    #endif

    startTimer();

    initResampler<T>();

    int t = 0;
    
    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, t, funcs);
        cudaDeviceSynchronize();
        cudaCheckError();
        #else
        for(int i = 0; i < NUM_PARTICLES; i++) {
            int pc = particles->pcs[i];
            if(funcs[pc] != NULL)
                funcs[pc](particles, i, t); 
        }
        #endif

        
        statusFunc(particles, t);
        
        if(funcs[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        
        if(particles->resample[0]) // Assumption: All resample at the same time
            resampleSystematic<T>(particles); // Only call "resample" and decide which resampling strategy inside?

        t++;
    }
        
    // Clean up
    destResampler<T>();
    
    #ifdef GPU
    freeMemory(particles);
    #else
    delete particles;
    #endif

    double duration = getTimeElapsed();
    cout << "Duration: " << duration << " seconds" << endl;
}


#endif



/*

* Resample after final weigh? WebPPL verkar alltid resampla efter en "factor"? Vill jag försöka skapa det? Eller bara modellera det med funktioner?

* Verkar bli mycket kod, med mycket #ifdef GPU

* Sträva efter att göra ett snyggare interface? Kanske går att minska mängden problemspecifik kod


*/