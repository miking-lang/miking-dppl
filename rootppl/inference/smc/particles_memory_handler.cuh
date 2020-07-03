#ifndef PARICLES_MEMORY_HANDLER_INCLUDED
#define PARICLES_MEMORY_HANDLER_INCLUDED

/*
 * File particles_memory_handler.cuh contains helper functions used by the smc implementation files. 
 * Functions for allocating and deleting particles belongs here. The memory management differs in 
 * top-level SMC and nested SMC, since the top-level will differ depending whether its compiled for CPU 
 * or GPU. While nested inference is done either on the device or CPU and both have 
 * the same API (as opposed to the host functions for handling memory on the device). 
 */

#include <cstring>
#include "utils/misc.cuh"

/**
 * Allocates particles for top-level SMC.
 * 
 * @param numParticles the number of particles that should be allocated.
 * @param printMemSize whether the total size of the allocated particles should be printed. 
 * @return particle structure with pointers to the allocated data.
 */
template <typename T>
particles_t<T> allocateParticles(int numParticles, bool printMemSize=false) {

    particles_t<T> particles;
    
    allocateMemory<T>(&particles.progStates, numParticles);
    allocateMemory<int>(&particles.pcs, numParticles);
    allocateMemory<floating_t>(&particles.weights, numParticles);

    #ifdef GPU
    cudaMemset(particles.pcs, 0, sizeof(int) * numParticles);
    cudaMemset(particles.weights, 0, sizeof(floating_t) * numParticles);
    #else
    memset(particles.pcs, 0, sizeof(int) * numParticles);
    memset(particles.weights, 0, sizeof(floating_t) * numParticles);
    #endif

    if (printMemSize) {
        floating_t totalMem = sizeof(T) * numParticles + sizeof(int) * numParticles + sizeof(floating_t) * numParticles;
        printf("Particles memory size for N=%d: %fMB\n", numParticles, totalMem * 2 / 1000000.0);
    }
    
    return particles;
}

/**
 * Frees particles for top-level SMC.
 *  
 * @param particles structure with pointers to the allocated data.
 */
template <typename T>
void freeParticles(particles_t<T> particles) {
    freeMemory<T>(particles.progStates);
    freeMemory<int>(particles.pcs);
    freeMemory<floating_t>(particles.weights);
}

/**
 * Allocates particles for nested SMC.
 *  
 * @param numParticles the number of particles that should be allocated.
 */
template <typename T>
HOST DEV particles_t<T> allocateParticlesNested(int numParticles) {
    particles_t<T> particles;
    particles.progStates = new T[numParticles];
    particles.pcs = new int[numParticles];
    memset(particles.pcs, 0, sizeof(int) * numParticles);
    particles.weights = new floating_t[numParticles];
    memset(particles.weights, 0, sizeof(floating_t) * numParticles);
    return particles;
}

/**
 * Frees particles for nested SMC.
 *  
 * @param particles structure with pointers to the allocated data.
 */
template <typename T>
HOST DEV void freeParticlesNested(particles_t<T> particles) {
    delete[] particles.progStates;
    delete[] particles.pcs;
    delete[] particles.weights;
}

#endif