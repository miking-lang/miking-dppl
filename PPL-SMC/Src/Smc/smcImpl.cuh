#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

#include <iostream>
#include "../Utils/timer.h"

#ifdef GPU
#include "../cudaErrorUtils.cu"
#include "Resample/resampleImplGPU.cuh"
#include "generalKernels.cuh"
#else
#include "Resample/resampleImplCPU.cuh"
#endif

const bool DEBUG = false;
floating_t weightSum = 0;
floating_t marginalLikelihood;

template <typename T>
void allocateMemory(T** pointer, size_t n) {
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(pointer, sizeof(T) * n));
    #else
    // *pointer = static_cast<T*>(malloc(memSize));
    *pointer = new T[n];
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
    initRandStatesKernel<<<NUM_BLOCKS_INITRAND, NUM_THREADS_PER_BLOCK_INITRAND>>>(randStates, time(NULL));
    cudaDeviceSynchronize();
    cudaCheckError();
}
#endif

template <typename T>
double runSMC(pplFunc_t<T>* bblocks, statusFunc_t<T> statusFunc, int numBblocks) {

    marginalLikelihood = 1;
    pplFunc_t<T> bblocksLocal[numBblocks];
    for(int i = 0; i < numBblocks; i++) {
        bblocksLocal[i] = bblocks[i];
    }
    
    // Init
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1); 
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles);
    //cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    initRandStates(particles->randStates);
    cudaDeviceSynchronize();
    #endif
    
    // startTimer();

    initResampler<T>();

    int t = 0;
    
    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS_FUNCS, NUM_THREADS_PER_BLOCK_FUNCS>>>(particles, t, bblocks);
        cudaDeviceSynchronize();
        cudaCheckError();
        #else
        for(int i = 0; i < NUM_PARTICLES; i++) {
            int pc = particles->pcs[i];
            if(bblocks[pc] != NULL)
                bblocks[pc](particles, i, t); 
        }
        #endif

        
        statusFunc(particles, t);
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            weightSum = resampleSystematic<T>(particles); // Only call "resample" and decide which resampling strategy inside?
            marginalLikelihood *= (weightSum / NUM_PARTICLES);
            //printf("margLH=%f\n", marginalLikelihood);
        }
        
        if(bblocksLocal[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        

        t++;
    }
        
    // Clean up
    destResampler<T>();

    // double duration = getTimeElapsed();

    // cout << "ln(Marginal Likelihood) = " << log(marginalLikelihood) << endl;
    

    #ifdef GPU
    freeMemory(particles);
    #else
    delete[] particles;
    #endif

    // cout << "Duration: " << duration << " seconds" << endl;
    return 0;
}


#endif
