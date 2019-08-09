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
    initRandStatesKernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates); // Apparently slow!
    cudaDeviceSynchronize();
    cudaCheckError();
}
#endif

template <typename T>
void runSMC(pplFunc_t<T>* funcs, bool* resampleArr, statusFunc_t<T> statusFunc) {

    
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1); 

    #ifdef GPU
    initRandStates(particles->randStates);
    #endif

    startTimer();

    initResampler<T>();

    bool resample = true;
    int t = 0;
    
    while(true) {
        bool terminate = true;

        resample = resampleArr[particles->pcs[0]]; // Assumption: All resample at the same time

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

        if(funcs[particles->pcs[0]] != NULL) // Assumption: All terminate at the same time
            terminate = false;

        if(DEBUG)
            statusFunc(particles, t);

        if(terminate)
            break;

        if(resample)
            resampleSystematic<T>(particles);

        t++;
    }

    if(!DEBUG)
        statusFunc(particles, t);

    destResampler<T>();
    freeMemory(particles);

    double duration = getTimeElapsed();
    cout << "Duration: " << duration << " seconds" << endl;
}


#endif