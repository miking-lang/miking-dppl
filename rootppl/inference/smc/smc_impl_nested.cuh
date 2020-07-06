#ifndef SMC_IMPL_NESTED_INCLUDED
#define SMC_IMPL_NESTED_INCLUDED

/*
 * File smc_impl_nested.cuh contains the implementation of the nested SMC.
 * This file is included by smc_impl.cuh and relies on the includes in smc_impl.cuh.
 */


/**
 * Runs Sequential Monte Carlo inference on the given bblock functions with arg as optional argument, 
 * then calls the given callback with the ret pointer. This allows caller to extract results
 * from the particles to a structure before particles are cleaned. 
 * The boolean arguments define whether new kernels should be launched within this nested inference. 
 * (Requires CUDA dynamic parallelism, which implies compute capability requirements and a couple of compile directives) 
 *
 * Note:
 * Do not use parallel settings if GPU is not defined! 
 * New nested curandStates are only necessary with parallel execution (at least with systematic resampling)
 *
 * @param randState GPU only, parent particle's randState required for RNG without the new particles.
 * @param bblocks the array of functions that will be executed by SMC.
 * @param numBblocks the size of the bblocks array.
 * @param numParticles number of particles to be used in SMC.
 * @param parallelExec whether new kernels should be launched for executing the bblocks.
 * @param parallelResampling whether new kernels should be launched within resampling.
 * @param parentIdx the index of the parent SMC particle.
 * @param callback optional function that should be called with the resulting particles after inference.
 * @param ret optional return structure to be passed to the callback function so that data can be stored there and kept after SMC cleanup.
 * @param arg optional argument to be passed to the bblocks (global data is often used instead for top-level SMC).
 * @return the logged normalization constant.
 */
template <typename T>
DEV double runSMCNested(
    #ifdef GPU
    curandState* randState,
    #endif
    pplFunc_t<T>* bblocks, int numBblocks, int numParticles, bool parallelExec, bool parallelResampling, int parentIdx, 
    callbackFunc_t<T> callback = NULL, void* ret = NULL, void* arg = NULL) {

    if(parallelExec || parallelResampling) {
        #ifndef GPU
        printf("Cannot run in parallel when not compiled for GPU");
        return 0.0;
        #endif
    }

    bool requireRandStates = parallelExec;

    floating_t logNormConstant = 0;
    
    particles_t<T> particles = allocateParticlesNested<T>(numParticles);
    
    #ifdef GPU
    const int NUM_BLOCKS = (numParticles + NUM_THREADS_PER_BLOCK_NESTED - 1) / NUM_THREADS_PER_BLOCK_NESTED;

    curandState* randStates;
    if(requireRandStates) {
        randStates = new curandState[numParticles];
        initCurandStates<T><<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED>>>(randStates, numParticles, parentIdx);
        cudaDeviceSynchronize();
        cudaCheckErrorDev();
    }
    #endif

    resampler_t<T> resampler = initResamplerNested<T>(numParticles);

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
                int pc = particles.pcs[i];
                if(pc < numBblocks) {
                    bblocks[pc](
                        #ifdef GPU
                        randState,
                        #endif
                        particles, i, arg); 
                }
            }
        }
        
        floating_t logWeightSum;
        if(parallelResampling) {
            #ifdef GPU
            logWeightSum = calcLogWeightSumPar(particles.weights, resampler, numParticles, NUM_BLOCKS, NUM_THREADS_PER_BLOCK_NESTED);
            #endif
        } else {
            logWeightSum = calcLogWeightSumSeq(particles.weights, resampler, numParticles);
        }

        logNormConstant += logWeightSum - log(static_cast<floating_t>(numParticles));
        
        if(particles.pcs[0] >= numBblocks) // Assumption: All terminate at the same time
            break;

        if(parallelResampling) {
            #ifdef GPU
            resampleSystematicParNested<T>(randState, particles, resampler, numParticles, NUM_BLOCKS);
            #endif
        } else {
            resampleSystematicSeq<T>(
                #ifdef GPU
                randState,
                #endif 
                particles, resampler, numParticles);
        }
        
        
    }

    callback(particles, numParticles, ret);
        
    // Clean up
    destResamplerNested<T>(resampler);
    freeParticlesNested<T>(particles);
    #ifdef GPU
    if(requireRandStates)
        delete[] randStates;
    #endif

    return logNormConstant;
}

#endif