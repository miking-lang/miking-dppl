#include <iostream>
#include <list>

#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "examples.cuh"
#include "../Utils/misc.cuh"
#include "../cudaErrorUtils.cu"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/PPLExamples/*.cu Src/Utils/*.cpp Src/Utils/*.cu -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU

#ifdef GPU
__device__
#endif
void particleInit(particles_t<stateType>* particles, int i, int t) {
    
    printf("init1\n");
    particles->progStates[i].res = 0;
    //printf("init2\n");
    //particles->progStates[i].states;
    printf("init3\n");

    particles->pcs[i] = 1;
    printf("left init!\n");
}

#ifdef GPU
__device__
#endif
void geometric(particles_t<stateType>* particles, int i, int t) {

    int numFlips = 0;
    while(true) {
        numFlips++;
        #ifdef GPU
        int random = flipDev(&particles->randStates[i]);
        #else
        int random = flip();
        #endif
        if(random)
            break;
    }

    printf("geometric...\n");
    particles->weights[i] = numFlips > 2 ? 1 : 0;
    printf("weighted!\n");

    particles->progStates[i].res = numFlips;
    printf("updated res!\n");

    particles->pcs[i]++;
    printf("updated PC!\n");
}

#ifdef GPU
__device__
#endif
hmmState_t hmmRec(int n) {
    hmmState_t prev;
    if(n == 1) {
        printf("base case..\n");
        prev.states = {true};
        prev.observations = {};
        printf("base case done\n");
    } else {
        printf("recurse..\n");
        prev = hmmRec(n-1);
        printf("recurse done\n");
    }

    // Transition and observe
    #ifdef GPU
    bool newState = prev.states.back() ? flipDev(&particles->randStates[i], 0.7) : flipDev(&particles->randStates[i], 0.3);
    bool newObs = newState ? flipDev(&particles->randStates[i], 0.9) : flipDev(&particles->randStates[i], 0.1);
    #else
    bool newState = prev.states.back() ? flip(0.7) : flip(0.3);
    bool newObs = newState ? flip(0.9) : flip(0.1);
    #endif

    hmmState_t ret;
    ret.states = prev.states;
    ret.states.push_back(newState);
    ret.observations = prev.observations;
    ret.observations.push_back(newObs);

    return ret;
}

#ifdef GPU
__device__
#endif
void hmm(particles_t<stateType>* particles, int i, int t) {
    hmmState_t hmmState = hmmRec(4);
    particles->progStates[i].states = hmmState.states;
}

#ifdef GPU
__device__
#endif
void nop(particles_t<stateType>* particles, int i, int t) {
    particles->pcs[i]++;
}

void statusFunc(particles_t<stateType>* particles, int t) {
    int tally[100] = {0};

    for(int i = 0; i < NUM_PARTICLES; i++) {
        // printf("Ret %d: %d\n", i, particles->progStates[i].res);
        tally[particles->progStates[i].res]++;
    }

    for(int i = 1; i < 10; i++)
        printf("%d: %f\n", i, tally[i] / (double)NUM_PARTICLES);
}


#ifdef GPU
__device__ pplFunc_t<stateType> initDev = particleInit;
__device__ pplFunc_t<stateType> geometricDev = geometric;
__device__ pplFunc_t<stateType> nopDev = nop;
#endif


int main() { // TODO: Normalize result and try with larger number of particles

    srand(time(NULL)); 
    initGen();

    hmmState_t hmmState = hmmRec(4);
    cout << "begin.." << endl;
    printList(hmmState.states);
    printList(hmmState.observations);

    cout << "end!" << endl;
    printf("hmmRec done!\n");

    pplFunc_t<stateType> initHost;
    pplFunc_t<stateType> geometricHost;
    pplFunc_t<stateType> nopHost;

    #ifdef GPU
    cudaSafeCall(cudaMemcpyFromSymbol(&initHost, initDev, sizeof(pplFunc_t<stateType>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&geometricHost, geometricDev, sizeof(pplFunc_t<stateType>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&nopHost, nopDev, sizeof(pplFunc_t<stateType>)));
    #else
    initHost = particleInit;
    geometricHost = geometric;
    nopHost = nop;
    #endif

    pplFunc_t<stateType>* funcArr;
    allocateMemory<pplFunc_t<stateType>>(&funcArr, 4);
    
    funcArr[0] = initHost;
    funcArr[1] = geometricHost;
    funcArr[2] = nopHost;
    funcArr[3] = NULL;

    bool resample[] = {false, true, false, NULL};

    runSMC<stateType>(funcArr, resample, statusFunc);

    printf("freeing...\n");
    freeMemory<pplFunc_t<stateType>>(funcArr);
    printf("freed!\n");

    return 0;
}