#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

/*
 * File smc_impl.cuh contains the implementation of the top-level SMC.
 */

#include <iostream>
#include "smc.cuh"
#include "utils/dists/dists.cuh"
#include "resample/systematic/sequential.cuh"
#include "particles_memory_handler.cuh"

#ifdef GPU
#include "cuda_profiler_api.h"
#include "utils/cuda_error_utils.cuh"
#include "resample/systematic/parallel.cuh"
#include "smc_kernels.cuh"
#endif

#include "smc_impl_nested.cuh"

/**
 * This is an attempt to make most of the GPU memory available 
 * from kernels via implicit stacks and device malloc calls
 * When running programs that reaches the memory limit, this could 
 * be tweaked to prioritize the memory type required by the program
 * 
 * @param numParticles the number of particles used in SMC.
 */
void configureMemSizeGPU(int numParticles) {
    #ifdef GPU

    // Read memory properties and define limits
    cudaDeviceProp devProp;
    cudaGetDeviceProperties(&devProp, 0);
    size_t MAX_THREADS_RESIDENT = devProp.maxThreadsPerMultiProcessor * devProp.multiProcessorCount;
    size_t GPU_MEM_TOT = devProp.totalGlobalMem * 0.95; // Leave 5% of memory for global structures or just to be sure
    size_t GPU_MEM_HEAP = GPU_MEM_TOT * 0.20; // Arbitrarily set 20% of GPU memory to device allocated heap memory
    size_t GPU_MEM_STACK = GPU_MEM_TOT - GPU_MEM_HEAP;
    size_t MAX_LOCAL_MEM_PER_THREAD = 512000; // 512 KB on all compute capabilities according to CUDA docs
    size_t MAX_STACK_SIZE = min(MAX_LOCAL_MEM_PER_THREAD, GPU_MEM_STACK / MAX_THREADS_RESIDENT);
    MAX_STACK_SIZE *= 0.5; // For some reason, with nested inference, this limit must be lower. Also, lower can give better performance.
    
    // Set limits and read the resulting set limits
    size_t heapSize, stackSize;
    cudaDeviceSetLimit(cudaLimitMallocHeapSize, GPU_MEM_HEAP);
    cudaDeviceSetLimit(cudaLimitStackSize, MAX_STACK_SIZE);
    cudaDeviceGetLimit(&heapSize, cudaLimitMallocHeapSize);
    cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);

    if(false) {
        cout << "Global Memory size: " << GPU_MEM_TOT / 1000000.0 << " MB" << endl;
        cout << "MaxStackSize Per Thread: " << MAX_STACK_SIZE / 1000000.0 << " MB" << endl;
        cout << "Device allocation heap max size: " << heapSize / 1000000.0 << " MB" << endl;
        cout << "Stack per thread max size: " << stackSize / 1000.0 << " KB" << endl;
        cout << "Allocated for particle stacks total top-level inference: " << stackSize * numParticles / 1000000.0 << " MB\n" << endl;
    }
    // cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    #endif
}

/**
 * Runs Sequential Monte Carlo inference on the given bblock functions, then calls 
 * optional callback that can use resulting particles before memory is cleaned up.
 * 
 * @param bblocks the array of functions that will be executed by SMC.
 * @param numBblocks the size of the bblocks array.
 * @param numParticles number of particles to be used in SMC.
 * @param callback optional function that should be called with the resulting particles after inference.
 * @param arg optional argument to be passed to the bblocks (global data is often used instead for top-level SMC).
 * @return the logged normalization constant.
 */
template <typename T>
double runSMC(pplFunc_t<T>* bblocks, int numBblocks, int numParticles, callbackFunc_t<T> callback = NULL, void* arg = NULL) {

    floating_t logNormConstant = 0;

    particles_t<T> particles = allocateParticles<T>(numParticles, false);
    
    #ifdef GPU
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

    curandState* randStates;
    cudaSafeCall(cudaMallocManaged(&randStates, sizeof(curandState) * numParticles));
    initCurandStates<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, numParticles, 0);
    cudaDeviceSynchronize();
    cudaCheckError();
    #endif

    resampler_t<T> resampler = initResampler<T>(numParticles);

    // cudaProfilerStart();
    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, particles, bblocks, numParticles, arg);
        cudaDeviceSynchronize();
        cudaCheckError();
        floating_t logWeightSum = calcLogWeightSumPar(particles.weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
        #else

        for(int i = 0; i < numParticles; i++) {
            int pc = particles.pcs[i];
            if(pc < numBblocks)
                bblocks[pc](particles, i, arg);
        }
        floating_t logWeightSum = calcLogWeightSumSeq(particles.weights, resampler, numParticles);
        #endif

        logNormConstant += logWeightSum - log(numParticles);
        if(particles.pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;

        
        #ifdef GPU
        resampleSystematicPar<T>(particles, resampler, numParticles, NUM_BLOCKS);
        #else
        resampleSystematicSeq<T>(particles, resampler, numParticles);
        #endif
        
        
    }
    // cudaProfilerStop();

    if(callback != NULL)
        callback(particles, numParticles, NULL);

    // Clean up
    destResampler<T>(resampler);
    freeParticles<T>(particles);
    #ifdef GPU
    cudaSafeCall(cudaFree(randStates));
    #endif

    return logNormConstant;
}


#endif
