#include <iostream>
//#include <vector>

#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "hmm.cuh"
#include "../../../Utils/misc.cuh"
#include "../../../Utils/distributions.cuh"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/Models/RandomExamples/Hmm/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPUn

// Preprocess this file only: nvcc -arch=sm_61 -rdc=true Src/RandomExamples/Hmm/hmm.cu -E -o pp.i -lcudadev -std=c++11 -D GPUn

// Currently not supported for GPU!

BBLOCK_DATA(TRUE_OBSERVATIONS, bool, 4)

BBLOCK(particleInit, progState_t, {

    PC = 1;
    RESAMPLE = false;
})


BBLOCK_HELPER(hmmRec, {

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

}, hmmState_t, int n)


BBLOCK(hmm, progState_t, {

    hmmState_t hmmState = hmmRec(particles, i, 4);
    
    PSTATE.states = hmmState.states;

    WEIGHT(hmmState.observations == DATA_POINTER(TRUE_OBSERVATIONS) ? 0 : -INFINITY);

    PC++;
    RESAMPLE = true;
})

BBLOCK(nop, progState_t, {
    PC++;
    RESAMPLE = false;
})


// Calc and print frequency of different states
STATUSFUNC({
    if(t == 2) {

        printArray<bool>(DATA_POINTER(TRUE_OBSERVATIONS), 4, "True Observations: ");

        list_t<list_t<bool>> results;
        results.initList(20);

        for(int i = 0; i < NUM_PARTICLES; i++) {
            list_t<bool> sts = PSTATE.states;
            bool contains = false;
            for(int j = 0; j < results.size(); j++) {
                if(results[j] == sts)
                    contains = true;
            }
            if(! contains)
                results.push_back(sts);
        }

        for(int li = 0; li < results.size(); li++) {
            int freq = 0;
            for(int i = 0; i < NUM_PARTICLES; i++) {
                if(PSTATE.states == results[li])
                    freq++;
            }
            printList(results[li], to_string(freq / (double)NUM_PARTICLES));
        }

    }
})

int main() {

    srand(time(NULL)); 
    initGen();

    /*
    (*TRUE_OBSERVATIONS).initList(10);
    (*TRUE_OBSERVATIONS).push_back(true);
    (*TRUE_OBSERVATIONS).push_back(false);
    (*TRUE_OBSERVATIONS).push_back(false);
    (*TRUE_OBSERVATIONS).push_back(false);
    */

    //*TRUE_OBSERVATIONS = {true, false, false, false};
    TRUE_OBSERVATIONS[0] = true;
    TRUE_OBSERVATIONS[1] = false;
    TRUE_OBSERVATIONS[2] = false;
    TRUE_OBSERVATIONS[3] = false;

    COPY_DATA_GPU(TRUE_OBSERVATIONS, bool, 4)

    SMCSTART(progState_t)

    INITBBLOCK(particleInit, progState_t)
    INITBBLOCK(hmm, progState_t)
    INITBBLOCK(nop, progState_t)

    SMCEND(progState_t)
}
