#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

/*
 * File smc_impl.cuh contains the implementation of the top-level SMC.
 */

#include <iostream>
#ifdef _OPENMP
#include <omp.h>
#endif

#include "macros/macros.cuh"
#include "smc.cuh"
#include "dists/dists.cuh"
#include "particles_memory_handler.cuh"
#include "resample/systematic/systematic_cpu.cuh"
// #include "smc_include.cuh"

#ifdef __NVCC__
#include <curand_kernel.h>
// #include "cuda_profiler_api.h"
#include "utils/cuda_error_utils.cuh"
#include "resample/systematic/systematic_gpu.cuh"
#include "smc_kernels.cuh"
#endif

 
double runSMC(const pplFunc_t* bblocks, int numBblocks, const int numParticles, const int ompThreads, const int particlesPerThread,
                size_t progStateSize, callbackFunc_t callback, void* arg) {

    #ifdef _OPENMP
    if(ompThreads > 0)
        omp_set_num_threads(ompThreads);
    #endif
    floating_t logNormConstant = 0;

    particles_t particles = allocateParticles(numParticles, progStateSize, false);
    
    #ifdef __NVCC__
    // Rather add an extra thread than add iterations for a few threads
    const int numThreads = (numParticles + particlesPerThread - 1) / particlesPerThread;

    const int NUM_BLOCKS_EXEC = (numThreads + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

    curandState* randStates;
    cudaSafeCall(cudaMalloc(&randStates, sizeof(curandState) * numThreads));
    initCurandStates<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, numThreads, 0);
    cudaDeviceSynchronize();
    cudaCheckError();
    #endif

    resampler_t resampler = initResampler(numParticles, progStateSize);

    // Run program/inference
    while(true) {

        #ifdef __NVCC__
        execFuncs<<<NUM_BLOCKS_EXEC, NUM_THREADS_PER_BLOCK>>>(randStates, particles, bblocks, numParticles, numThreads, arg);
        cudaDeviceSynchronize();
        cudaCheckError();
        floating_t logWeightSum = calcLogWeightSumGpu(particles.weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
        #else

        #pragma omp parallel for
        for(int i = 0; i < numParticles; i++) {
            int pc = particles.pcs[i];
            if(pc < numBblocks && pc >= 0)
                bblocks[pc](particles, i, arg);
        }
        floating_t logWeightSum = calcLogWeightSumCpu(particles.weights, resampler, numParticles);
        #endif

        logNormConstant += logWeightSum - log(numParticles);

        // Resampling will be skipped the last SMC iteration. Instead, weights will be renormalised and logged so the represent log-probabilities.
        if(particles.pcs[0] >= numBblocks) { // Assumption: All terminate at the same time
            #ifdef __NVCC__
            logAndRenormaliseWeightsGpu(particles.weights, resampler, logWeightSum, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
            #else
            logAndRenormaliseWeightsCpu(particles.weights, resampler, logWeightSum, numParticles);
            #endif
            break;
        }
        
        #ifdef __NVCC__
        resampleSystematicGpu(particles, resampler, numParticles, NUM_BLOCKS);
        #else
        resampleSystematicCpu(particles, resampler, numParticles);
        #endif
        
    }

    if(callback != NULL)
        callback(particles, numParticles, NULL);

    // Clean up
    destResampler(resampler);
    freeParticles(particles);
    #ifdef __NVCC__
    cudaSafeCall(cudaFree(randStates));
    #endif

    return logNormConstant;
}


void configureMemSizeGPU() {
    #ifdef __NVCC__

    // Read memory properties and define limits
    cudaDeviceProp devProp;
    cudaGetDeviceProperties(&devProp, 0);
    size_t MAX_THREADS_RESIDENT = devProp.maxThreadsPerMultiProcessor * devProp.multiProcessorCount;
    size_t GPU_MEM_TOT = devProp.totalGlobalMem * 0.95; // Leave 5% of memory for global structures or just to be sure
    size_t GPU_MEM_HEAP = GPU_MEM_TOT * 0.20; // Arbitrarily set 20% of GPU memory to device allocated heap memory
    size_t GPU_MEM_STACK = GPU_MEM_TOT - GPU_MEM_HEAP;
    size_t MAX_LOCAL_MEM_PER_THREAD = 512000; // 512 KB on all compute capabilities according to CUDA docs
    size_t MAX_STACK_SIZE = min(MAX_LOCAL_MEM_PER_THREAD, GPU_MEM_STACK / MAX_THREADS_RESIDENT);
    MAX_STACK_SIZE *= 1.0; // For some reason, with nested inference, this limit must be lower. Also, lower can give better performance.
    
    // Set limits and read the resulting set limits
    size_t heapSize, stackSize;
    cudaDeviceSetLimit(cudaLimitMallocHeapSize, GPU_MEM_HEAP);
    cudaDeviceSetLimit(cudaLimitStackSize, MAX_STACK_SIZE);
    cudaDeviceGetLimit(&heapSize, cudaLimitMallocHeapSize);
    cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);

    if(false) {
        std::cout << "Global Memory size: " << GPU_MEM_TOT / 1000000.0 << " MB" << std::endl;
        std::cout << "Stack per thread max size attempted to set: " << MAX_STACK_SIZE / 1000.0 << " KB" << std::endl;
        std::cout << "Stack per thread max size set: " << stackSize / 1000.0 << " KB" << std::endl;
        std::cout << "Device allocation heap max size: " << heapSize / 1000000.0 << " MB" << std::endl;
    }
    // cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    #endif
}


#endif
