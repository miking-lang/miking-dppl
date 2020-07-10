
#ifndef GENERAL_KERNELS_INCLUDED
#define GENERAL_KERNELS_INCLUDED

/*
 * File smc_kernels.cuh contains kernels used by SMC. 
 */


/**
 * This function initializes the curandStates.
 * 
 * @param randStates the curandStates, one for each particle, that should be initialized and used in inference.
 * @param numParticles the number of particles used by SMC.
 * @param seed used in curand_init to achieve unique RNG states in nested SMC (set to zero for top-level SMC).
 */
template <typename T>
__global__ void initCurandStates(curandState* randStates, int numParticles, int seed) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if(i >= numParticles || i < 0) return;

    // Double check this seed, need only to be unique over one inference, as time should vary between inferences. 
    // curand_init(1234 + clock64(), seed * numParticles + i, 0, &particles->randStates[i]);
    curandState randStateLocal = randStates[i];
    curand_init(1234 + clock64(), seed * numParticles + i, 0, &randStateLocal);
    randStates[i] = randStateLocal;
}

/**
 * Each thread executes the bblock pointed to by the corresponding particle's PC. 
 * 
 * @param randStates the curandStates, one for each particle, that should be used in inference.
 * @param particles the particles used by SMC.
 * @param funcs the array of bblocks that can be executed.
 * @param numParticles the number of particles used by SMC.
 * @param arg argument that are passed to the bblocks when invoking them, often not used and set to NULL. 
 */
template <typename T>
__global__ void execFuncs(curandState* randStates, particles_t<T> particles, const pplFunc_t<T>* funcs, int numParticles, void* arg) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if(i >= numParticles || i < 0) return;

    curandState randStateLocal = randStates[i];
    
    funcs[particles.pcs[i]](&randStateLocal, particles, i, arg);

    randStates[i] = randStateLocal;
}

#endif