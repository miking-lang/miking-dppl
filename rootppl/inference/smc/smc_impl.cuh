#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

#include <iostream>
#include <limits>
#include "smc.cuh"
#include "utils/distributions/distributions.cuh"
#include "resample/resample_impl_seq.cuh"
#include "particles_memory_handler.cuh"

#ifdef GPU
#include "cuda_profiler_api.h"
#include "utils/cuda_error_utils.cu"
#include "resample/resample_impl_par.cuh"
#include "general_kernels.cuh"
#endif


/* 
This is an attempt to make most of the GPU memory available 
from kernels via implicit stacks and device malloc calls
When running programs that reaches the memory limit, this could 
be tweaked to prioritize the memory type required by the program
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

/*
Runs Sequential Monte Carlo inference on the given bblock functions, then calls 
optional callback that can use resulting particles before memory is cleaned up.
Arg is an optional argument that can be passed to the bblocks. For top-level
inference, this might as well be allocated as global data. 
*/
// const floating_t LOG_NUM_PARTICLES = log(NUM_PARTICLES);
template <typename T>
double runSMC(pplFunc_t<T>* bblocks, int numBblocks, int numParticles, callbackFunc_t<T> callback = NULL, void* arg = NULL) {

    floating_t logNormConstant = 0;

    particles_t<T>* particles = allocateParticles<T>(numParticles, false);
    
    #ifdef GPU
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

    curandState* randStates;
    cudaSafeCall(cudaMallocManaged(&randStates, sizeof(curandState) * numParticles));
    initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, particles, numParticles);
    // initParticles<T>KERNEL_SETTINGS(numParticles, NUM_THREADS_PER_BLOCK)(randStates, particles, numParticles);
    cudaDeviceSynchronize();
    cudaCheckError();
    #endif

    resampler_t resampler = initResampler<T>(numParticles);

    // cudaProfilerStart();
    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, particles, bblocks, numParticles, arg);
        cudaDeviceSynchronize();
        cudaCheckError();
        floating_t logWeightSum = calcWeightSumPar(particles->weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
        // floating_t logWeightSum = calcWeightSumSeq(particles->weights, resampler, numParticles);
        #else

        for(int i = 0; i < numParticles; i++) {
            int pc = particles->pcs[i];
            if(pc < numBblocks)
                bblocks[pc](particles, i, arg);
        }
        floating_t logWeightSum = calcWeightSumSeq(particles->weights, resampler, numParticles);
        #endif

        // logNormConstant += log(weightSum / numParticles);
        // logNormConstant += logWeightSum - LOG_numParticles;
        logNormConstant += logWeightSum - log(numParticles);
        if(particles->pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;

        
        #ifdef GPU
        resampleSystematicPar<T>(&particles, resampler, numParticles, NUM_BLOCKS);
        #else
        resampleSystematicSeq<T>(&particles, resampler, numParticles);
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


/*
Runs Sequential Monte Carlo inference on the given bblock functions with arg as optional argument, 
then calls the given callback with the ret pointer. This allows caller to extract results
from the particles to a structure before particles are cleaned. 
The boolean arguments define whether new kernels should be launched within this nested inference. 
(Requires CUDA dynamic parallelism, which implies compute capability requirements and a couple of compile directives)

Note:
Do not use parallel settings if GPU is not defined! 
New nested curandStates are only necessary with parallel execution (at least with systematic resampling)
*/
template <typename T>
DEV double runSMCNested(
    #ifdef GPU
    curandState* randState, // Parent's randState
    #endif
    pplFunc_t<T>* bblocks, callbackFunc_t<T> callback, int numBblocks, int numParticles, void* ret, void* arg, bool parallelExec, bool parallelResampling, int parentIdx) {

    if(parallelExec || parallelResampling) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    floating_t logNormConstant = 0;
    
    particles_t<T>* particles = allocateParticlesNested<T>();
    
    #ifdef GPU
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK_NESTED - 1) / NUM_THREADS_PER_BLOCK_NESTED;

    curandState* randStates = new curandState[numParticles];
    if(parallelExec)
        initParticles<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(randStates, particles, numParticles, parentIdx);
    else
        initParticlesNoCurand<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(particles, numParticles);
    cudaDeviceSynchronize();
    cudaCheckErrorDev();
    #endif

    resampler_t resampler = initResamplerNested<T>(numParticles);

    // Run program/inference
    while(true) {

        if(parallelExec) {
            #ifdef GPU
            // Use nested randStates
            execFuncs<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(randStates, particles, bblocks, numParticles, arg);
            cudaDeviceSynchronize();
            cudaCheckErrorDev();
            #endif
        
        } else {
            
            for(int i = 0; i < numParticles; i++) {
                int pc = particles->pcs[i];
                if(pc < numBblocks) {
                    bblocks[pc](
                        #ifdef GPU
                        randState, // Use parent's randState
                        #endif
                        particles, i, arg); 
                }
            }
        }
        
        floating_t logWeightSum;
        if(parallelResampling) {
            #ifdef GPU
            logWeightSum = calcWeightSumPar(particles->weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED);
            #endif
        } else {
            logWeightSum = calcWeightSumSeq(particles->weights, resampler, numParticles);
        }

        logNormConstant += logWeightSum - log(static_cast<floating_t>(numParticles));
        // logNormConstant += log(weightSum / numParticles);
        
        if(particles->pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;

        if(parallelResampling) {
            #ifdef GPU
            resampleSystematicParNested<T>(randState, &particles, resampler, numParticles, NUM_BLOCKS); // Use parent's randState
            #endif
        } else {
            resampleSystematicSeq<T>(
                #ifdef GPU
                randState, // Use parent's randState
                #endif 
                &particles, resampler, numParticles);
        }
        
        
    }

    callback(particles, numParticles, ret);
        
    // Clean up
    destResamplerNested<T>(resampler);
    freeParticlesNested<T>(particles);
    #ifdef GPU
    delete[] randStates;
    #endif

    return logNormConstant;
}


#endif
