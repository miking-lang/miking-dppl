#include <iostream>
#include <vector>

#include "../../Smc/smc.cuh"
#include "../../Smc/smcImpl.cuh"
#include "hmm.cuh"
#include "../../Utils/misc.cuh"
#include "../../cudaErrorUtils.cu"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Hmm/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPUn

const vector<bool> TRUE_OBSERVATIONS = {true, false, false, false};

#ifdef GPU
__device__
#endif
void particleInit(particles_t<progState_t>* particles, int i, int t) {

    particles->pcs[i] = 1;
    particles->resample[i] = false;
}

#ifdef GPU
__device__
#endif
hmmState_t hmmRec(particles_t<progState_t>* particles, int i, int n) {

    hmmState_t prev;

    if(n == 1)
        prev.states.push_back(true);
    else
        prev = hmmRec(particles, i, n-1);


    // Transition and observe

    // lambda func could make this look nicer? Replaces template arg and "particles, i"
    bool newState = prev.states.back() ? flipK<progState_t>(particles, i, 0.7) : flipK<progState_t>(particles, i, 0.3);
    bool newObs = newState ? flipK<progState_t>(particles, i, 0.9) : flipK<progState_t>(particles, i, 0.1);

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
void hmm(particles_t<progState_t>* particles, int i, int t) {

    hmmState_t hmmState = hmmRec(particles, i, 4);
    particles->progStates[i].states = hmmState.states;

    particles->weights[i] = hmmState.observations == TRUE_OBSERVATIONS ? 1 : 0;

    particles->pcs[i]++;
    particles->resample[i] = true;
}

#ifdef GPU
__device__
#endif
void nop(particles_t<progState_t>* particles, int i, int t) {
    particles->pcs[i]++;
    particles->resample[i] = false;
}


// Calc and print frequency of different states
void statusFuncHmm(particles_t<progState_t>* particles, int t) {

    printList(TRUE_OBSERVATIONS, "True Observations: ");

    vector<vector<bool>> results;

    for(int i = 0; i < NUM_PARTICLES; i++) {
        vector<bool> sts = particles->progStates[i].states;
        bool contains = false;
        for(int j = 0; j < results.size(); j++) {
            if(results[j] == sts)
                contains = true;
        }
        if(! contains)
            results.push_back(sts);
    }

    for(vector<bool> vec : results) {
        int freq = 0;
        for(int i = 0; i < NUM_PARTICLES; i++) {
            if(particles->progStates[i].states == vec)
                freq++;
        }
        printList(vec, to_string(freq / (double)NUM_PARTICLES));
    }
}

// These pointers and "__device__" in front of funcs are the only things stopping from being independent from GPU vs CPU
#ifdef GPU
__device__ pplFunc_t<progState_t> initDev = particleInit;
__device__ pplFunc_t<progState_t> nopDev = nop;
__device__ pplFunc_t<progState_t> hmmDev = hmm;
#endif


int main() {

    srand(time(NULL)); 
    initGen();

    pplFunc_t<progState_t> initHost;
    pplFunc_t<progState_t> nopHost;
    pplFunc_t<progState_t> hmmHost;

    #ifdef GPU
    cudaSafeCall(cudaMemcpyFromSymbol(&initHost, initDev, sizeof(pplFunc_t<progState_t>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&nopHost, nopDev, sizeof(pplFunc_t<progState_t>)));
    cudaSafeCall(cudaMemcpyFromSymbol(&hmmHost, hmmDev, sizeof(pplFunc_t<progState_t>)));
    #else
    initHost = particleInit;
    nopHost = nop;
    hmmHost = hmm;
    #endif

    pplFunc_t<progState_t>* funcArr;
    allocateMemory<pplFunc_t<progState_t>>(&funcArr, 4);
    
    funcArr[0] = initHost;
    funcArr[1] = hmmHost;
    funcArr[2] = nopHost;
    funcArr[3] = NULL;

    runSMC<progState_t>(funcArr, statusFuncHmm);

    freeMemory<pplFunc_t<progState_t>>(funcArr);

    return 0;
}
