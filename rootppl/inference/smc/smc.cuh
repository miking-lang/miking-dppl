#ifndef SMC_INCLUDED
#define SMC_INCLUDED

/*
 * File smc.cuh contains the fundamental type definitions used by SMC. 
 * This file is included by smc_impl.cuh. 
 */

// #include <math.h>

#include "macros/macros.cuh"
#include "dists/dists.cuh"

#ifdef __NVCC__
#include <curand_kernel.h>
#endif

// Kernel launch settings
#define NUM_THREADS_PER_BLOCK 128
#define NUM_THREADS_PER_BLOCK_NESTED 32


/*
 * Particle structure, allocated at start of inference. This SoA (Struct of Arrays) structure is
 * important for the performance on GPU:s as it results in coalesced memory handling. 
 * Large program states will result in worse performance not only due to copying in resampling, but
 * also since it results in strided memory accesses. There are likely better solutions for these cases. 
 * 
 * progStates: These are the model-specific states. 
 * pcs: These are the "program counters" which are used by the particles to define what BBLOCK to execute in each BBLOCK-iteration.
 * weights: These are the weights used in resampling and approximation of the normalization constant. Equivalent to "factor" in WebPPL.
 */
 
struct particles_t {
    void* progStates;
    int* pcs;
    floating_t* weights;
};

/*
 * This corresponds to the BBLOCK functions, which are passed to SMC and executed during inference. 
 * The arguments described below are mostly hidden from the model definitions via the macros. This 
 * way the model does not only become simpler due to the increased level of abstraction, but also
 * decouples the model definitions from the choice of compilation to CPU vs GPU. 
 * 
 * curandState*: The RNG states required for sampling on the GPU. These are only included when compiled for the GPU. 
 * particles_t: The SMC particles, required for particles to access the weight, PC and progStates during execution of BBLOCKS.
 * int: The particle index of the executing particle. 
 * void*: optional argument, often set to NULL and ignored for top-level inference. 
 */
using pplFunc_t = void (*)(
    #ifdef __NVCC__ 
    curandState*, 
    #endif
    particles_t&,
    int,
    void*);


// Callback function, like bblock function but without index. As all particles are usually used here.

/*
 * Callback function, optional argument to SMC. It will be called after inference is done with the resulting particles.
 * This way the result can be calculated/aggregated and saved. 
 *
 * particles_t: The resulting SMC particles. 
 * int: The number of particles. 
 * void*: Argument to be passed. Mostly ignored in top-level inference. Can be used as a way of keeping data from nested inference after its clean up.
 */
using callbackFunc_t = void (*)(particles_t&, int, void*);


/**
 * Runs Sequential Monte Carlo inference on the given bblock functions, then calls 
 * optional callback that can use resulting particles before memory is cleaned up.
 * 
 * @param bblocks the array of functions that will be executed by SMC.
 * @param numBblocks the size of the bblocks array.
 * @param numParticles number of particles to be used in SMC.
 * @param ompThreads controls the maximum number of threads used by Open MP on the CPU variant. 
 * @param particlesPerThread indirectly determines how many CUDA threads will be necessary. Not used in CPU variant.
 * @param progStateSize The size of the program state used by each particle. 
 * @param callback optional function that should be called with the resulting particles after inference.
 * @param arg optional argument to be passed to the bblocks (global data is often used instead for top-level SMC).
 * @return the logged normalization constant.
 */
double runSMC(const pplFunc_t* bblocks, int numBblocks, const int numParticles, const int ompThreads, const int particlesPerThread,
    size_t progStateSize, callbackFunc_t callback = NULL, void* arg = NULL);

    
/**
 * This is an attempt to make most of the GPU memory available 
 * from kernels via implicit stacks and device malloc calls
 * When running programs that reaches the memory limit, this could 
 * be tweaked to prioritize the memory type required by the program
 */
void configureMemSizeGPU();




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
 * @param progStateSize The size of the program state used by each particle. 
 * @param parallelExec whether new kernels should be launched for executing the bblocks.
 * @param parallelResampling whether new kernels should be launched within resampling.
 * @param parentIdx the index of the parent SMC particle.
 * @param callback optional function that should be called with the resulting particles after inference.
 * @param ret optional return structure to be passed to the callback function so that data can be stored there and kept after SMC cleanup.
 * @param arg optional argument to be passed to the bblocks (global data is often used instead for top-level SMC).
 * @return the logged normalization constant.
 */
 DEV double runSMCNested(
    #ifdef __NVCC__
    curandState* randState,
    #endif
    pplFunc_t* bblocks, int numBblocks, int numParticles, size_t progStateSize, bool parallelExec, bool parallelResampling, int parentIdx, 
    callbackFunc_t callback = NULL, void* ret = NULL, void* arg = NULL);


#endif

