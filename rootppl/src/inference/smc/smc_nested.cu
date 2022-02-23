
/*
 * File smc_impl_nested.cuh contains the implementation of the nested SMC.
 * This file is included by smc_impl.cuh and relies on the includes in smc_impl.cuh.
 */

// Nested inference is never used, and this code has become obsolete.
/*
 #include "macros/macros.cuh"
 #include "smc.cuh"
 #include "dists/dists.cuh"
 #include "particles_memory_handler.cuh"
 #include "resample/systematic/systematic_cpu.cuh"

 #ifdef __NVCC__
 #include <curand_kernel.h>
 #include "utils/cuda_error_utils.cuh"
 #include "resample/systematic/systematic_gpu.cuh"
 #include "smc_kernels.cuh"
 #endif


DEV double runSMCNested(
    #ifdef __NVCC__
    curandState* randState,
    #endif
    pplFunc_t* bblocks, int numBblocks, int numParticles, size_t progStateSize, bool parallelExec, bool parallelResampling, int parentIdx,
    callbackFunc_t callback, void* ret, void* arg) {

    if(parallelExec || parallelResampling) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    bool requireRandStates = parallelExec;

    floating_t logNormConstant = 0;

    particles_t particles = allocateParticlesNested(numParticles, progStateSize);

    #ifdef __NVCC__
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK_NESTED - 1) / NUM_THREADS_PER_BLOCK_NESTED;

    curandState* randStates;
    if(requireRandStates) {
        randStates = new curandState[numParticles];
        initCurandStates<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(randStates, numParticles, parentIdx);
        cudaDeviceSynchronize();
        cudaCheckErrorDev();
    }
    #endif

    resampler_t resampler = initResamplerNested(numParticles, progStateSize);

    // Run program/inference
    while(true) {

        if(parallelExec) {
            #ifdef __NVCC__
            // Use nested randStates
            execFuncs<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(randStates, particles, bblocks, numParticles, numParticles, numBblocks, arg);
            cudaDeviceSynchronize();
            cudaCheckErrorDev();
            #endif

        } else {

            for(int i = 0; i < numParticles; i++) {
                int pc = particles.pcs[i];
                if(pc < numBblocks) {
                    bblocks[pc](
                        #ifdef __NVCC__
                        randState,
                        #endif
                        particles, i, arg);
                }
            }
        }

        floating_t logWeightSum;
        if(parallelResampling) {
            #ifdef __NVCC__
            logWeightSum = calcLogWeightSumGpu(particles.weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED);
            #endif
        } else {
            logWeightSum = calcLogWeightSumCpu(particles.weights, resampler, numParticles);
        }

        logNormConstant += logWeightSum - log(static_cast<floating_t>(numParticles));

        if(particles.pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;

        if(parallelResampling) {
            #ifdef __NVCC__
            resampleSystematicGpuNested(randState, particles, resampler, numParticles, NUM_BLOCKS);
            #endif
        } else {
            resampleSystematicCpu(
                #ifdef __NVCC__
                randState,
                #endif
                particles, resampler, numParticles);
        }


    }

    callback(particles, numParticles, ret);

    // Clean up
    destResamplerNested(resampler);
    freeParticlesNested(particles);
    #ifdef __NVCC__
    if(requireRandStates)
        delete[] randStates;
    #endif

    return logNormConstant;
}
*/
