#ifndef AIRPLANE_UTILS_INCLUDED
#define AIRPLANE_UTILS_INCLUDED

#include <iostream>

#include "../Smc/smc.cuh"

using namespace std;

void initMap(floating_t* mapApprox);
void destMap();

__device__ floating_t mapLookupApproxDev(floating_t* mapApprox, floating_t x);
__host__ floating_t mapLookupApprox(floating_t* mapApprox, floating_t x);
void printArray(floating_t* arr, int length);

__host__ __device__ floating_t normalPDFObs(floating_t x, floating_t mean);


template <typename T>
void printStatus(particles_t<T>* particles, floating_t* planeX, int t) {
    int numParticlesClose = 0;
    floating_t minX = 999999;
    floating_t maxX = -1;
    for (int i = 0; i < NUM_PARTICLES; i++) {
        floating_t particleX = particles->progStates[i].x;
        if(abs(particleX - planeX[t]) < 10)
            numParticlesClose++;
        minX = min(minX, particleX);
        maxX = max(maxX, particleX);
    }

    cout << "TimeStep " << t << ", NumClose: " << numParticlesClose << ", MinX: " << minX << ", MaxX: " << maxX << endl;
}

#endif