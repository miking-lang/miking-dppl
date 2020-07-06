#ifndef SMC_INCLUDED
#define SMC_INCLUDED

/*
 * File smc.cuh contains the fundamental type definitions used by SMC. 
 * This file is included by smc_impl.cuh. 
 */

#include <cstddef>
#include "macros/macros.cuh"
#ifdef GPU
#include "utils/cuda_error_utils.cuh"
#include <curand_kernel.h>
#endif

using namespace std;

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
template <typename T>
struct particles_t {
    T* progStates;
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
 * int: The index of the executing particle. 
 * void*: optional argument, often set to NULL and ignored for top-level inference. 
 */
template <typename T>
using pplFunc_t = void (*)(
    #ifdef GPU 
    curandState*, 
    #endif
    particles_t<T>&, 
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
template <typename T>
using callbackFunc_t = void (*)(particles_t<T>&, int, void*);


#endif

