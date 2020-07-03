#ifndef PARICLES_MEMORY_HANDLER_INCLUDED
#define PARICLES_MEMORY_HANDLER_INCLUDED

#include <cstring>
#include "utils/misc.cuh"

template <typename T>
particles_t<T>* allocateParticles(int numParticles, bool printMemSize=false) {
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1);
    
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(&particles->progStates, sizeof(T) * numParticles));
    cudaSafeCall(cudaMallocManaged(&particles->pcs, sizeof(int) * numParticles));
    cudaSafeCall(cudaMallocManaged(&particles->weights, sizeof(floating_t) * numParticles));
    #else
    particles->progStates = new T[numParticles];
    particles->pcs = new int[numParticles];
    memset(particles->pcs, 0, sizeof(int) * numParticles);
    particles->weights = new floating_t[numParticles];
    memset(particles->weights, 0, sizeof(floating_t) * numParticles);
    #endif

    if (printMemSize) {
        floating_t totalMem = sizeof(T) * numParticles + sizeof(int) * numParticles + sizeof(floating_t) * numParticles;
        #ifdef GPU
        totalMem += sizeof(curandState) * numParticles;
        #endif
        printf("Particles memory size for N=%d: %fMB\n", numParticles, totalMem * 2 / 1000000.0);
    }
    
    return particles;
}

template <typename T>
void freeParticles(particles_t<T>* particles) {
    #ifdef GPU
    cudaSafeCall(cudaFree(particles->progStates));
    cudaSafeCall(cudaFree(particles->pcs));
    cudaSafeCall(cudaFree(particles->weights));
    #else
    delete[] particles->progStates;
    delete[] particles->pcs;
    delete[] particles->weights;
    #endif

    freeMemory(particles);
}

template <typename T>
HOST DEV particles_t<T>* allocateParticlesNested(int numParticles) {
    particles_t<T>* particles = new particles_t<T>[1]; // {}
    particles->progStates = new T[numParticles];
    particles->pcs = new int[numParticles];
    memset(particles->pcs, 0, sizeof(int) * numParticles);
    particles->weights = new floating_t[numParticles];
    memset(particles->weights, 0, sizeof(floating_t) * numParticles);
    return particles;
}

template <typename T>
HOST DEV void freeParticlesNested(particles_t<T>* particles) {
    delete[] particles->progStates;
    delete[] particles->pcs;
    delete[] particles->weights;
    delete[] particles;
}

#endif