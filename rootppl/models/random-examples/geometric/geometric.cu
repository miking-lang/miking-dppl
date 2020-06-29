#include <iostream>
#include <vector>
#include <time.h>

#include "../../../inference/smc/smc.cuh"
#include "../../../inference/smc/smc_impl.cuh"
#include "geometric.cuh"
#include "../../../utils/distributions.cuh"

using namespace std;

// Compile: nvcc -arch=sm_61 -rdc=true Src/Models/RandomExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU

// Preprocess this file only: nvcc -arch=sm_75 -rdc=true Src/Models/RandomExamples/Geometric/geometric.cu -E -o geo.i -lcudadev -std=c++11 -D GPUn

// Compile CPU: g++ -x c++ Src/Models/RandomExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3


BBLOCK(particleInit, progState_t, {

    PSTATE.res = 0;

    PC = 1;
    RESAMPLE = false;
})

BBLOCK(geometric, progState_t, {
    int numFlips = 0;
    int currFlip = 1;
    
    while(currFlip) {

        numFlips++;
        currFlip = flipK<progState_t>(particles, i);
    }

    WEIGHT(numFlips > 2 ? 0 : -INFINITY);

    PSTATE.res = numFlips;

    PC++;
    RESAMPLE = true;
})

BBLOCK(nop, progState_t, {
    PC++;
    RESAMPLE = false;
})


STATUSFUNC({
    if(t == 2) {
        int tally[100] = {0};

        for(int i = 0; i < NUM_PARTICLES; i++)
            tally[PSTATE.res]++;

        for(int i = 1; i < 10; i++)
            printf("%d: %f\n", i, tally[i] / (double)NUM_PARTICLES);
    }
})


int main() {

    srand(time(NULL)); 
    initGen();

    SMCSTART(progState_t)

    INIT_BBLOCK(particleInit, progState_t)
    INIT_BBLOCK(geometric, progState_t)
    INIT_BBLOCK(nop, progState_t)

    SMCEND(progState_t)
}
