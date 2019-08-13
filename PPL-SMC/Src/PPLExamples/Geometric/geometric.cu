#include <iostream>
#include <vector>

#include "../../Smc/smc.cuh"
#include "../../Smc/smcImpl.cuh"
#include "geometric.cuh"
#include "../../Utils/misc.cuh"
#include "../../cudaErrorUtils.cu"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU

#ifdef GPU
__device__
#endif
void particleInit(particles_t<progState_t>* particles, int i, int t) {
    
    particles->progStates[i].res = 0;

    particles->pcs[i] = 1;
}


#ifdef GPU
__device__
#endif
void geometric(particles_t<progState_t>* particles, int i, int t) {

    int numFlips = 0;
    int currFlip = 1;
    
    while(currFlip) {

        numFlips++;
        currFlip = flipK<progState_t>(particles, i);
    }

    particles->weights[i] = numFlips > 2 ? 1 : 0;

    particles->progStates[i].res = numFlips;

    particles->pcs[i]++;
}

#ifdef GPU
__device__
#endif
void nop(particles_t<progState_t>* particles, int i, int t) {
    particles->pcs[i]++;
}

void statusFuncGeometric(particles_t<progState_t>* particles, int t) {
    int tally[100] = {0};

    for(int i = 0; i < NUM_PARTICLES; i++)
        tally[particles->progStates[i].res]++;

    for(int i = 1; i < 10; i++)
        printf("%d: %f\n", i, tally[i] / (double)NUM_PARTICLES);
}


// These pointers and "__device__" in front of funcs are the only things stopping from being independent from GPU vs CPU
#ifdef GPU
__device__ pplFunc_t<progState_t> initDev = particleInit;
__device__ pplFunc_t<progState_t> geometricDev = geometric;
__device__ pplFunc_t<progState_t> nopDev = nop;
#endif

int main() {

    srand(time(NULL)); 
    initGen();

    pplFunc_t<progState_t> initHost;
    pplFunc_t<progState_t> geometricHost;
    pplFunc_t<progState_t> nopHost;

    #ifdef GPU
    cudaSafeCall(cudaMemcpyFromSymbol(&initHost, initDev, sizeof(pplFunc_t<progState_t>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&geometricHost, geometricDev, sizeof(pplFunc_t<progState_t>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&nopHost, nopDev, sizeof(pplFunc_t<progState_t>)));
    #else
    initHost = particleInit;
    geometricHost = geometric;
    nopHost = nop;
    #endif

    pplFunc_t<progState_t>* funcArr;
    allocateMemory<pplFunc_t<progState_t>>(&funcArr, 4);
    
    funcArr[0] = initHost;
    funcArr[1] = geometricHost;
    funcArr[2] = nopHost;
    funcArr[3] = NULL;

    bool resample[] = {false, true, false, NULL};

    runSMC<progState_t>(funcArr, resample, statusFuncGeometric);

    freeMemory<pplFunc_t<progState_t>>(funcArr);

    return 0;
}
