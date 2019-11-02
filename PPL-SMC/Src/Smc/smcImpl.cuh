#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

#include <iostream>
#include <limits>
#include "../Utils/timer.h"

#ifdef GPU
#include "../cudaErrorUtils.cu"
#include "Resample/resampleImplPar.cuh"
#include "generalKernels.cuh"
//#else
//#include "Resample/resampleImplSeq.cuh"
#endif
#include "Resample/resampleImplSeq.cuh"
#include "particlesMemoryHandler.cuh"


/*
#ifdef GPU
void initRandStates(curandState* randStates) {
    initRandStatesKernel<<<NUM_BLOCKS_INITRAND, NUM_THREADS_PER_BLOCK_INITRAND>>>(randStates, time(NULL), NUM_PARTICLES);
    cudaDeviceSynchronize();
    cudaCheckError();
}

// only used in nested, where all particles must have a initialized curandState in case of GPU
template <typename T>
DEV void initParticlesSeq(particles_t<T>* particles) {
    for(int i = 0; i < NUM_PARTICLES; i++) {
        particles->weights[i] = 0;
        particles->pcs[i] = 0;
    }
}
#endif
*/

template <typename T>
double runSMC(pplFunc_t<T>* bblocks, statusFunc_t<T> statusFunc, int numBblocks) {

    #ifdef GPU
    // Increase heap size on device for device allocation (required for nested inference with > ~100 particles )
    // cudaDeviceSetLimit(cudaLimitMallocHeapSize, numeric_limits<uint32_t>::max());
    size_t size;
    cudaDeviceGetLimit(&size, cudaLimitMallocHeapSize);
    //size_t stackSize;
    //cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);
    
    printf("Size limit malloc heap: %f MB\n", size / 1000000.0);
    //printf("Size limit stack default: %f MB\n", stackSize / 1000000.0);
    //cudaDeviceSetLimit(cudaLimitStackSize, numeric_limits<uint32_t>::max());
    //printf("Size limit stack: %f MB\n", stackSize / 1000000.0);
    #endif

    floating_t particleSize = sizeof(particles_t<T>) / 1000000.0;
    printf("Particles size: %f MB\n", particleSize);
    

    floating_t logMarginalLikelihood = 0;
    pplFunc_t<T> bblocksLocal[numBblocks]; // Local bblocks means slightly less transfers from GPU to CPU
    for(int i = 0; i < numBblocks; i++)
        bblocksLocal[i] = bblocks[i];
    
    // Init
    particles_t<T>* particles = allocateParticles<T>();
    // allocateMemory<particles_t<T>>(&particles, 1); 
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(particles, time(NULL), NUM_PARTICLES);
    //cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    // initRandStates(particles->randStates);
    cudaDeviceSynchronize();
    #endif

    resampler_t resampler = initResampler<T>();

    int t = 0;

    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS_FUNCS, NUM_THREADS_PER_BLOCK_FUNCS>>>(particles, t, bblocks, NUM_PARTICLES);
        cudaDeviceSynchronize();
        cudaCheckError();
        #else
        for(int i = 0; i < NUM_PARTICLES; i++) {
            int pc = particles->pcs[i];
            if(bblocks[pc] != NULL)
                bblocks[pc](particles, i, t); 
        }
        #endif
        
        // statusFunc(particles, t); // Is this really necessary? Expensive for GPU-version
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            #ifdef GPU
            floating_t weightSum = resampleSystematicPar<T>(particles, resampler);
            #else
            floating_t weightSum = resampleSystematicSeq<T>(particles, resampler, NUM_PARTICLES);
            #endif
            logMarginalLikelihood += log(weightSum / NUM_PARTICLES);
        }
        
        if(bblocksLocal[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        

        t++;
    }
        
    // Clean up
    destResampler<T>(resampler);

    // freeMemory(particles);
    freeParticles<T>(particles);

    return logMarginalLikelihood;
}


/* Do not use useGPU=true if GPU is not defined! */
template <typename T>
DEV double runSMCNested(pplFunc_t<T>* bblocks, callbackFunc_t<T> callback, void* ret, bool parallelExec, bool parallelResampling, int seed) {

    if(parallelExec || parallelResampling) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    floating_t logMarginalLikelihood = 0;
    
    // Init
    particles_t<T>* particles = allocateParticlesNested<T>(); // = new particles_t<T>; // Should work for both host and dev. code
    
    #ifdef GPU
    initParticles<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, seed, NUM_PARTICLES_NESTED);
    // initRandStatesKernel<<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles->randStates, 0, NUM_PARTICLES_NESTED); // NO TIME SEED NOW
    // initRandStates(particles->randStates);
    cudaDeviceSynchronize();
    cudaCheckErrorDev();

    // TODO: DOUBLE CHECK THAT I DO NOT NEED TO INIT PARICLES ON SEQUENTIAL VARIANT HERE

    #endif

    resampler_t resampler = initResamplerNested<T>();

    int t = 0;
    
    // Run program/inference
    while(true) {

        if(parallelExec) {
            #ifdef GPU
            execFuncs<T><<<NUM_BLOCKS_NESTED, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, t, bblocks, NUM_PARTICLES_NESTED);
            cudaDeviceSynchronize();
            cudaCheckErrorDev();
            #endif
        
        } else {
            
            for(int i = 0; i < NUM_PARTICLES_NESTED; i++) {

                int pc = particles->pcs[i];
                if(bblocks[pc] != NULL) 
                    bblocks[pc](particles, i, t); 
            }
            #ifdef GPU
            if(parallelExec)
                cudaCheckErrorDev();
            #endif
        }
        
        if(particles->resample[0]) { // Assumption: All resample at the same time
            // CURRENTLY ONLY SEQ NESTED RESAMPLING
            floating_t weightSum;
            if(parallelResampling) {
                #ifdef GPU
                weightSum = resampleSystematicParNested<T>(particles, resampler);
                #endif
            } else {
                weightSum = resampleSystematicSeq<T>(particles, resampler, NUM_PARTICLES_NESTED); // Only call "resample" and decide which resampling strategy inside?
            }
            logMarginalLikelihood += log(weightSum / NUM_PARTICLES_NESTED);
        }
        
        if(bblocks[particles->pcs[0]] == NULL) // Assumption: All terminate at the same time
            break;
        
        t++;
    }

    callback(particles, t, ret);
        
    // Clean up
    destResamplerNested<T>(resampler);

    // delete particles;
    freeParticlesNested<T>(particles);

    return logMarginalLikelihood;
}


#endif
