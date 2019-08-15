#include <iostream>
#include <vector>

#include "../../Smc/smc.cuh"
#include "../../Smc/smcImpl.cuh"
#include "hmm.cuh"
#include "../../Utils/misc.cuh"
#include "../../Utils/distributions.cuh"

using namespace std;

// nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Hmm/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPUn

// Preprocess this file only: nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Hmm/hmm.cu -E -o geo.i -lcudadev -std=c++11 -D GPUn

const vector<bool> TRUE_OBSERVATIONS = {true, false, false, false};

BBLOCK(particleInit, {

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


BBLOCK(hmm, {

    hmmState_t hmmState = hmmRec(particles, i, 4);
    
    PSTATE.states = hmmState.states;

    WEIGHT(hmmState.observations == TRUE_OBSERVATIONS ? 0 : -INFINITY);

    PC++;
    RESAMPLE = true;
})

BBLOCK(nop, {
    PC++;
    RESAMPLE = false;
})


// Calc and print frequency of different states
STATUSFUNC({
    if(t == 2) {

        printList(TRUE_OBSERVATIONS, "True Observations: ");

        vector<vector<bool>> results;

        for(int i = 0; i < NUM_PARTICLES; i++) {
            vector<bool> sts = PSTATE.states;
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
                if(PSTATE.states == vec)
                    freq++;
            }
            printList(vec, to_string(freq / (double)NUM_PARTICLES));
        }
    }
})

int main() {

    srand(time(NULL)); 
    initGen();

    MAINSTART()

    INITBBLOCK(particleInit)
    INITBBLOCK(hmm)
    INITBBLOCK(nop)

    MAINEND()
}
