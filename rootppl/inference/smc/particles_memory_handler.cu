
/*
 * File particles_memory_handler.cuh contains helper functions used by the smc implementation files. 
 * Functions for allocating and deleting particles belongs here. The memory management differs in 
 * top-level SMC and nested SMC, since the top-level will differ depending whether its compiled for CPU 
 * or GPU. While nested inference is done either on the device or CPU and both have 
 * the same API (as opposed to the host functions for handling memory on the device). 
 */

#include <cstring>
#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "particles_memory_handler.cuh"

particles_t allocateParticles(int numParticles, size_t progStateSize, bool printMemSize) {

    particles_t particles;
    
    allocateMemoryVoid(&particles.progStates, numParticles * progStateSize);
    allocateMemory<int>(&particles.pcs, numParticles);
    allocateMemory<floating_t>(&particles.weights, numParticles);

    #ifdef __NVCC__
    cudaMemset(particles.pcs, 0, sizeof(int) * numParticles);
    cudaMemset(particles.weights, 0, sizeof(floating_t) * numParticles);
    #else
    memset(particles.pcs, 0, sizeof(int) * numParticles);
    memset(particles.weights, 0, sizeof(floating_t) * numParticles);
    #endif

    if (printMemSize) {
        floating_t totalMem = progStateSize * numParticles + sizeof(int) * numParticles + sizeof(floating_t) * numParticles;
        printf("Particles memory size for N=%d: %fMB\n", numParticles, totalMem * 2 / 1000000.0);
    }
    
    return particles;
}

void freeParticles(particles_t particles) {
    freeMemoryVoid(particles.progStates);
    freeMemory<int>(particles.pcs);
    freeMemory<floating_t>(particles.weights);
}

HOST DEV particles_t allocateParticlesNested(int numParticles, size_t progStateSize) {
    particles_t particles;
    particles.progStates = malloc(progStateSize * numParticles);;
    particles.pcs = new int[numParticles];
    memset(particles.pcs, 0, sizeof(int) * numParticles);
    particles.weights = new floating_t[numParticles];
    memset(particles.weights, 0, sizeof(floating_t) * numParticles);
    return particles;
}

HOST DEV void freeParticlesNested(particles_t particles) {
    free(particles.progStates);
    delete[] particles.pcs;
    delete[] particles.weights;
}
