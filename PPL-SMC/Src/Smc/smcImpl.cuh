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


template <typename T>
void allocateMemory(T** pointer, size_t n) {
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(pointer, sizeof(T) * n));
    #else
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

    floating_t logMarginalLikelihood = 0;
    pplFunc_t<T> bblocksLocal[numBblocks];
    for(int i = 0; i < numBblocks; i++)
        bblocksLocal[i] = bblocks[i];
    
    // Init
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1); 
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles);
    //cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    initRandStates(particles->randStates);
    cudaDeviceSynchronize();
    #endif

    resampler_t resampler = initResampler<T>();

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
        
        statusFunc(particles, t); // Is this really necessary? Expensive for GPU-version
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            floating_t weightSum = resampleSystematic<T>(particles, resampler); // Only call "resample" and decide which resampling strategy inside?
            logMarginalLikelihood += log(weightSum / NUM_PARTICLES);
        }
        
        if(bblocksLocal[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        

        t++;
    }
        
    // Clean up
    destResampler<T>(resampler);

    #ifdef GPU
    freeMemory(particles);
    #else
    delete[] particles;
    #endif

    return logMarginalLikelihood;
}


/* Do not use useGPU=true if GPU is not defined! */
template <typename T>
double runSMCNested(pplFunc_t<T>* bblocks, callbackFunc_t<T> callback, void* ret, bool parallel) {

    if(parallel) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    floating_t logMarginalLikelihood = 0;
    
    // Init
    particles_t<T>* particles = new particles_t<T>; // Should work for both host and dev. code
    
    #ifdef GPU
    if(parallel) {
        initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles);
        initRandStates(particles->randStates);
        cudaDeviceSynchronize();
    }
    #endif

    resampler_t resampler = initResampler<T>(); // Needs to be handled!

    int t = 0;
    
    // Run program/inference
    while(true) {

        if(parallel) {
            #ifdef GPU
            execFuncs<T><<<NUM_BLOCKS_FUNCS, NUM_THREADS_PER_BLOCK_FUNCS>>>(particles, t, bblocks);
            cudaDeviceSynchronize();
            cudaCheckError();
            #endif
        
        } else {
            for(int i = 0; i < NUM_PARTICLES; i++) {
                int pc = particles->pcs[i];
                if(bblocks[pc] != NULL)
                    bblocks[pc](particles, i, t); 
            }
        }
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            floating_t weightSum = resampleSystematic<T>(particles, resampler, true); // Only call "resample" and decide which resampling strategy inside?
            logMarginalLikelihood += log(weightSum / NUM_PARTICLES);
        }
        
        if(bblocks[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        
        t++;
    }

    callback(particles, t, ret);
        
    // Clean up
    destResampler<T>(resampler); // Needs to be handled!

    delete particles;

    return logMarginalLikelihood;
}


#endif
