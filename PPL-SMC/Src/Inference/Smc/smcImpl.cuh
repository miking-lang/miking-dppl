#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

#include <iostream>
#include <limits>
#include "../../Utils/timer.h"

#ifdef GPU
#include "../../Utils/cudaErrorUtils.cu"
#include "Resample/resampleImplPar.cuh"
#include "generalKernels.cuh"
#endif
#include "Resample/resampleImplSeq.cuh"
#include "particlesMemoryHandler.cuh"

void configureMemSizeGPU() {
    #ifdef GPU
    // Increase heap size on device for device allocation ( required for nested inference with > ~100 particles )
    cudaDeviceSetLimit(cudaLimitMallocHeapSize, numeric_limits<uint32_t>::max() / 100.0);
    size_t size, stackSize;
    cudaDeviceGetLimit(&size, cudaLimitMallocHeapSize);
    cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);
    
    printf("Allocateable per thread: cudaMalloc heap: %f MB\n", size / 1000000.0);
    printf("Size limit stack default: %f KB\n", stackSize / 1000.0);
    // cudaDeviceSetLimit(cudaLimitStackSize, numeric_limits<uint32_t>::max());
    cudaDeviceSetLimit(cudaLimitStackSize, stackSize * 10); // Might be hardware sensitive as memory size varies
    cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);
    printf("Size limit stack: %f KB\n", stackSize / 1000.0);
    printf("Allocated for particle stacks total top-level inference: %f MB\n\n", stackSize * NUM_PARTICLES / 1000000.0);
    #endif
}

template <typename T>
double runSMC(pplFunc_t<T>* bblocks, statusFunc_t<T> statusFunc, int numBblocks, void* arg = NULL) {

    // floating_t particleSize = sizeof(particles_t<T>) / 1000000.0;
    // printf("Particles size: %f MB\n", particleSize);
    
    floating_t logNormConstant = 0;
    pplFunc_t<T> bblocksLocal[numBblocks]; // Local bblocks means slightly less transfers from GPU to CPU
    for(int i = 0; i < numBblocks; i++)
        bblocksLocal[i] = bblocks[i];
    
    // Init
    particles_t<T>* particles = allocateParticles<T>(false);
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, NUM_PARTICLES);
    // cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    cudaDeviceSynchronize();
    cudaCheckError();
    #endif

    resampler_t resampler = initResampler<T>();

    int t = 0;

    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS_FUNCS, NUM_THREADS_PER_BLOCK_FUNCS>>>(particles, t, bblocks, NUM_PARTICLES, arg);
        cudaDeviceSynchronize();
        cudaCheckError();
        #else
        for(int i = 0; i < NUM_PARTICLES; i++) {
            int pc = particles->pcs[i];
            if(bblocks[pc] != NULL)
                bblocks[pc](particles, i, t, arg); 
        }
        #endif
        
        // statusFunc(particles, t); // Is this really necessary? Expensive for GPU-version
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            #ifdef GPU
            floating_t weightSum = resampleSystematicPar<T>(particles, resampler);
            #else
            floating_t weightSum = resampleSystematicSeq<T>(particles, resampler, NUM_PARTICLES);
            #endif
            logNormConstant += log(weightSum / NUM_PARTICLES);
        }
        
        if(bblocksLocal[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        

        t++;
    }

    statusFunc(particles, t);
        
    // Clean up
    destResampler<T>(resampler);
    freeParticles<T>(particles);

    return logNormConstant;
}


/* Do not use parallel setting if GPU is not defined! */
template <typename T>
DEV double runSMCNested(pplFunc_t<T>* bblocks, callbackFunc_t<T> callback, void* ret, void* arg, bool parallelExec, bool parallelResampling, int seed) {

    if(parallelExec || parallelResampling) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    floating_t logNormConstant = 0;
    
    // Init
    particles_t<T>* particles = allocateParticlesNested<T>();
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, NUM_PARTICLES_NESTED, seed);
    cudaDeviceSynchronize();
    cudaCheckErrorDev();
    #endif

    resampler_t resampler = initResamplerNested<T>();

    int t = 0;
    // Run program/inference
    while(true) {

        if(parallelExec) {
            #ifdef GPU
            execFuncs<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, t, bblocks, NUM_PARTICLES_NESTED, arg);
            cudaDeviceSynchronize();
            cudaCheckErrorDev();
            #endif
        
        } else {
            
            for(int i = 0; i < NUM_PARTICLES_NESTED; i++) {
                int pc = particles->pcs[i];
                if(bblocks[pc] != NULL) 
                    bblocks[pc](particles, i, t, arg); 
            }
            #ifdef GPU
            if(parallelExec)
                cudaCheckErrorDev();
            #endif
        }
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            floating_t weightSum;
            if(parallelResampling) {
                #ifdef GPU
                weightSum = resampleSystematicParNested<T>(particles, resampler);
                #endif
            } else {
                weightSum = resampleSystematicSeq<T>(particles, resampler, NUM_PARTICLES_NESTED);
            }
            logNormConstant += log(weightSum / NUM_PARTICLES_NESTED);
        }
        
        if(bblocks[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        
        t++;
    }

    callback(particles, t, ret);
        
    // Clean up
    destResamplerNested<T>(resampler);
    freeParticlesNested<T>(particles);

    return logNormConstant;
}


#endif
