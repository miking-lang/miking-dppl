
/*
 * File smc_kernels.cu contains definitions of kernels used by SMC. 
 */

 #ifdef __NVCC__

 #include <curand_kernel.h>
 #include "inference/smc/smc.cuh"
 #include "smc_kernels.cuh"

__global__ void initCurandStates(curandState* randStates, int numThreads, int seed) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if(i >= numThreads || i < 0) return;

    // Double check this seed, need only to be unique over one inference, as time should vary between inferences. 
    // curand_init(1234 + clock64(), seed * numThreads + i, 0, &particles->randStates[i]);
    curandState randStateLocal = randStates[i];
    curand_init(1234 + clock64(), seed * numThreads + i, 0, &randStateLocal);
    randStates[i] = randStateLocal;
}

__global__ void execFuncs(curandState* randStates, particles_t particles, const pplFunc_t* funcs, 
                            int numParticles, int numThreads, int numBblocks, void* arg) {

    int i = blockIdx.x * blockDim.x + threadIdx.x;
    // if(i >= numParticles || i < 0) return;
    if(i >= numThreads || i < 0) return;

    curandState randStateLocal = randStates[i];
    
    for(int j = i; j < numParticles; j += numThreads) {
        // funcs[particles.pcs[i]](&randStateLocal, particles, i, arg);
        int pc = particles.pcs[j];
        if(pc < numBblocks && pc >= 0)
            funcs[pc](&randStateLocal, particles, j, arg);
    }

    randStates[i] = randStateLocal;
}

#endif
