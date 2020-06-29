#ifndef PARICLES_MEMORY_HANDLER_INCLUDED
#define PARICLES_MEMORY_HANDLER_INCLUDED

#include <cstring>
#include "../../Utils/misc.cuh"

template <typename T>
particles_t<T>* allocateParticles(bool printMemSize=false) {
    particles_t<T>* particles;
    allocateMemory<particles_t<T>>(&particles, 1);
    
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(&particles->progStates, sizeof(T) * NUM_PARTICLES));
    // cudaSafeCall(cudaMallocManaged(&particles->randStates, sizeof(curandState) * NUM_PARTICLES));
    cudaSafeCall(cudaMallocManaged(&particles->pcs, sizeof(int) * NUM_PARTICLES));
    // cudaSafeCall(cudaMemset(particles->pcs, 0, NUM_PARTICLES * sizeof(int)));
    cudaSafeCall(cudaMallocManaged(&particles->weights, sizeof(floating_t) * NUM_PARTICLES));
    // cudaSafeCall(cudaMemset(particles->weights, 0, NUM_PARTICLES * sizeof(floating_t)));
    // cudaSafeCall(cudaMallocManaged(&particles->resample, sizeof(bool) * NUM_PARTICLES));
    #else
    particles->progStates = new T[NUM_PARTICLES];
    particles->pcs = new int[NUM_PARTICLES];
    memset(particles->pcs, 0, sizeof(int) * NUM_PARTICLES);
    particles->weights = new floating_t[NUM_PARTICLES];
    memset(particles->weights, 0, sizeof(floating_t) * NUM_PARTICLES);
    // particles->resample = new bool[NUM_PARTICLES];
    #endif

    if (printMemSize) {
        floating_t totalMem = sizeof(T) * NUM_PARTICLES + sizeof(int) * NUM_PARTICLES + sizeof(floating_t) * NUM_PARTICLES; // + sizeof(bool) * NUM_PARTICLES;
        #ifdef GPU
        totalMem += sizeof(curandState) * NUM_PARTICLES;
        #endif
        printf("Particles memory size for N=%d: %fMB\n", NUM_PARTICLES, totalMem * 2 / 1000000.0);
    }
    
    return particles;
}

template <typename T>
void freeParticles(particles_t<T>* particles) {
    #ifdef GPU
    cudaSafeCall(cudaFree(particles->progStates));
    // cudaSafeCall(cudaFree(particles->randStates));
    cudaSafeCall(cudaFree(particles->pcs));
    cudaSafeCall(cudaFree(particles->weights));
    // cudaSafeCall(cudaFree(particles->resample));
    #else
    delete[] particles->progStates;
    delete[] particles->pcs;
    delete[] particles->weights;
    // delete[] particles->resample;
    #endif

    freeMemory(particles);
}

template <typename T>
HOST DEV particles_t<T>* allocateParticlesNested() {
    particles_t<T>* particles = new particles_t<T>[1]; // {}
    particles->progStates = new T[NUM_PARTICLES_NESTED];
    //#ifdef GPU
    //particles->randStates = new curandState[NUM_PARTICLES_NESTED];
    //#endif
    particles->pcs = new int[NUM_PARTICLES_NESTED];
    memset(particles->pcs, 0, sizeof(int) * NUM_PARTICLES_NESTED);
    particles->weights = new floating_t[NUM_PARTICLES_NESTED];
    memset(particles->weights, 0, sizeof(floating_t) * NUM_PARTICLES_NESTED);
    //particles->resample = new bool[NUM_PARTICLES_NESTED];
    return particles;
}

template <typename T>
HOST DEV void freeParticlesNested(particles_t<T>* particles) {
    delete[] particles->progStates;
    //#ifdef GPU
    //delete[] particles->randStates;
    //#endif
    delete[] particles->pcs;
    delete[] particles->weights;
    // delete[] particles->resample;
    delete[] particles;
}

#endif