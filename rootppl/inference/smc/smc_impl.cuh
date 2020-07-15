#ifndef SMC_IMPL_INCLUDED
#define SMC_IMPL_INCLUDED

/*
 * File smc_impl.cuh contains the implementation of the top-level SMC.
 */

#include <iostream>

#include "macros/macros.cuh"
#include "smc.cuh"
#include "dists/dists.cuh"
#include "particles_memory_handler.cuh"
#include "resample/systematic/sequential.cuh"

#ifdef GPU
#include <curand_kernel.h>
// #include "cuda_profiler_api.h"
#include "utils/cuda_error_utils.cuh"
#include "resample/systematic/parallel.cuh"
#include "smc_kernels.cuh"
#endif

#include "smc_impl_nested.cuh"

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
double runSMC(const pplFunc_t* bblocks, int numBblocks, const int numParticles, const int particlesPerThread, 
                const int progStateSize, callbackFunc_t callback = NULL, void* arg = NULL) {

    floating_t logNormConstant = 0;

    particles_t particles = allocateParticles(numParticles, progStateSize, false);
    
    #ifdef GPU
    // Rather add an extra thread than add iterations for a few threads
    const int numThreads = (numParticles + particlesPerThread - 1) / particlesPerThread;
    printf("NumThreads: %d, NumParticles: %d\n", numThreads, numParticles);
    printf("NumThreads*ParticlesPerThread = %d, Should be equal to: %d\n", numThreads*particlesPerThread, numParticles);

    const int NUM_BLOCKS_EXEC = (numThreads + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

    curandState* randStates;
    cudaSafeCall(cudaMalloc(&randStates, sizeof(curandState) * numThreads));
    initCurandStates<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(randStates, numThreads, 0);
    cudaDeviceSynchronize();
    cudaCheckError();
    #endif

    resampler_t<char[progStateSize]> resampler = initResampler<T>(numParticles);

    // cudaProfilerStart();
    // Run program/inference
    while(true) {

        #ifdef GPU
        execFuncs<T><<<NUM_BLOCKS_EXEC, NUM_THREADS_PER_BLOCK>>>(randStates, particles, bblocks, numParticles, numThreads, arg);
        cudaDeviceSynchronize();
        cudaCheckError();
        floating_t logWeightSum = calcLogWeightSumPar(particles.weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK);
        #else

        for(int i = 0; i < numParticles; i++) {
            int pc = particles.pcs[i];
            if(pc < numBblocks && pc >= 0)
                bblocks[pc](particles, i, arg);
        }
        floating_t logWeightSum = calcLogWeightSumSeq(particles.weights, resampler, numParticles);
        #endif

        logNormConstant += logWeightSum - log(numParticles);
        
        #ifdef GPU
        resampleSystematicPar<T>(particles, resampler, numParticles, NUM_BLOCKS);
        #else
        resampleSystematicSeq<T>(particles, resampler, numParticles);
        #endif
        
        // This last resample increases variance perhaps? But convenient to not have to consider weights when extracting distribution. 
        if(particles.pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;
        
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
