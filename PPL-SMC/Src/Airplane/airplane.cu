#include <stdio.h>
#include <math.h>
#include <random>
#include <time.h>

#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "airplane.cuh"
#include "airplaneUtils.cuh"
#include "../Utils/misc.cuh"
#include "../cudaErrorUtils.cu"

// nvcc -arch=sm_61 -rdc=true Src/Airplane/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU

using namespace std;


default_random_engine generator;

#ifdef GPU
__device__ floating_t planeXDev[TIME_STEPS], planeObsDev[TIME_STEPS], mapApproxDev[MAP_SIZE]; // make constant memory?
#else
uniform_real_distribution<floating_t> uniDist(0.0, MAP_SIZE);
uniform_real_distribution<floating_t> uDist(0.0, 1.0);
#endif 
floating_t *planeX, *planeObs, *mapApprox;


void initAirplane() {
    generator.seed(time(NULL));

    // Do setup on CPU
    planeX = new floating_t[TIME_STEPS];
    planeObs = new floating_t[TIME_STEPS];
    mapApprox = new floating_t[MAP_SIZE];

    initMap(mapApprox);

    for (int t = 0; t < TIME_STEPS; t++) {
        if(t == 0) {
            planeX[t] = STARTING_POINT;
        } else {
            normal_distribution<floating_t> transDist(planeX[t-1] + VELOCITY, TRANSITION_STD);
            planeX[t] = transDist(generator);
        }
        normal_distribution<floating_t> obsDist(mapLookupApprox(mapApprox, planeX[t]), OBSERVATION_STD);
        planeObs[t] = obsDist(generator);
    }

    #ifdef GPU

    // Copy data to device pointers, so that they can be accessed from kernels
    cudaSafeCall(cudaMemcpyToSymbol(planeXDev, planeX, TIME_STEPS * sizeof(floating_t), 0, cudaMemcpyHostToDevice));
    cudaSafeCall(cudaMemcpyToSymbol(planeObsDev, planeObs, TIME_STEPS * sizeof(floating_t), 0, cudaMemcpyHostToDevice));
    cudaSafeCall(cudaMemcpyToSymbol(mapApproxDev, mapApprox, MAP_SIZE * sizeof(floating_t)));
    #endif
}

#ifdef GPU
__device__ void updateWeight(particles_t<stateType>* particles, int i, int t) { // inline?
    particles->weights[i] = normalPDFObs(planeObsDev[t], mapLookupApproxDev(mapApproxDev, particles->progStates[i].x));
}

#else

void updateWeight(particles_t<stateType>* particles, int i, int t) { // inline?
    particles->weights[i] = normalPDFObs(planeObs[t], mapLookupApprox(mapApprox, particles->progStates[i].x));
}
#endif

void printStatusFunc(particles_t<stateType>* particles, int t) {
    printStatus<stateType>(particles, planeX, t);
}


#ifdef GPU
__device__
#endif
void particleInit(particles_t<stateType>* particles, int i, int t) {
    #ifdef GPU
    particles->progStates[i].x = curand_uniform(&particles->randStates[i]) * MAP_SIZE;
    #else
    particles->progStates[i].x = uniDist(generator);
    #endif

    updateWeight(particles, i, t);

    particles->pcs[i] = 1;
    particles->resample[i] = false;
}

#ifdef GPU
__device__
#endif
void propagateAndWeight(particles_t<stateType>* particles, int i, int t) {

    // Propagate
    #ifdef GPU
    particles->progStates[i].x += VELOCITY + (curand_normal(&particles->randStates[i]) * TRANSITION_STD);
    #else
    normal_distribution<floating_t> transitionDist (particles->progStates[i].x + VELOCITY, TRANSITION_STD);
    particles->progStates[i].x = transitionDist(generator);
    #endif

    // Weight
    updateWeight(particles, i, t);

    // p->progState.t += 1;
    if(t >= TIME_STEPS - 1)
        particles->pcs[i] = 2;

    particles->resample[i] = true;
}

#ifdef GPU
__device__ pplFunc_t<stateType> initDev = particleInit;
__device__ pplFunc_t<stateType> propWeightDev = propagateAndWeight;
#endif

int main(int argc, char** argv) {

    initAirplane();

    // Host pointers, (to device if GPU)
    pplFunc_t<stateType> initHost;
    pplFunc_t<stateType> propWeightHost;

    #ifdef GPU
    // Cannot directly handle device func pointers from host code, this solves this problem
    // Allow host code to handle pointer to device address
    cudaSafeCall(cudaMemcpyFromSymbol(&initHost, initDev, sizeof(pplFunc_t<stateType>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&propWeightHost, propWeightDev, sizeof(pplFunc_t<stateType>)));

    #else
    initHost = particleInit;
    propWeightHost = propagateAndWeight;
    #endif

    pplFunc_t<stateType>* funcArr;
    allocateMemory<pplFunc_t<stateType>>(&funcArr, 3);
    
    funcArr[0] = initHost;
    funcArr[1] = propWeightHost;
    funcArr[2] = NULL;

    runSMC<stateType>(funcArr, printStatusFunc);

    freeMemory<pplFunc_t<stateType>>(funcArr);

    delete[] planeX;
    delete[] planeObs;
    delete[] mapApprox;
    
    return 0;
}
