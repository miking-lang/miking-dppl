#include <iostream>
#include <vector>

#include "../../Smc/smc.cuh"
#include "../../Smc/smcImpl.cuh"
#include "geometric.cuh"
#include "../../Utils/misc.cuh"
#include "../../cudaErrorUtils.cu"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU


BBLOCK(particleInit, {

    particles->progStates[i].res = 0;

    particles->pcs[i] = 1;
    particles->resample[i] = false;
})

BBLOCK(geometric, {
    int numFlips = 0;
    int currFlip = 1;
    
    while(currFlip) {

        numFlips++;
        currFlip = flipK<progState_t>(particles, i);
    }

    particles->weights[i] = numFlips > 2 ? 1 : 0;

    particles->progStates[i].res = numFlips;

    particles->pcs[i]++;
    particles->resample[i] = true;
})

BBLOCK(nop, {
    particles->pcs[i]++;
    particles->resample[i] = false;
})


void statusFuncGeometric(particles_t<progState_t>* particles, int t) {
    int tally[100] = {0};

    for(int i = 0; i < NUM_PARTICLES; i++)
        tally[particles->progStates[i].res]++;

    for(int i = 1; i < 10; i++)
        printf("%d: %f\n", i, tally[i] / (double)NUM_PARTICLES);
}


// These pointers and "__device__" in front of funcs are the only things stopping from being independent from GPU vs CPU
#ifdef GPU
__device__ pplFunc_t<progState_t> initDev = bblock_particleInit;
__device__ pplFunc_t<progState_t> geometricDev = bblock_geometric;
__device__ pplFunc_t<progState_t> nopDev = bblock_nop;
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
    initHost = bblock_particleInit;
    geometricHost = bblock_geometric;
    nopHost = bblock_nop;
    #endif

    pplFunc_t<progState_t>* funcArr;
    allocateMemory<pplFunc_t<progState_t>>(&funcArr, 4);
    
    funcArr[0] = initHost;
    funcArr[1] = geometricHost;
    funcArr[2] = nopHost;
    funcArr[3] = NULL;

    runSMC<progState_t>(funcArr, statusFuncGeometric);

    freeMemory<pplFunc_t<progState_t>>(funcArr);

    return 0;
}
