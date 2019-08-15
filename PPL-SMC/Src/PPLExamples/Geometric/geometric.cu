#include <iostream>
#include <vector>
#include <time.h>

#include "../../Smc/smc.cuh"
#include "../../Smc/smcImpl.cuh"
#include "geometric.cuh"
#include "../../Utils/distributions.cuh"

using namespace std;

// Compile: nvcc -arch=sm_61 -rdc=true Src/PPLExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O4 -D GPU

// Preprocess this file only: nvcc -arch=sm_75 -rdc=true Src/PPLExamples/Geometric/geometric.cu -E -o geo.i -lcudadev -std=c++11 -D GPUn

// Compile CPU: g++ -x c++ Src/PPLExamples/Geometric/*.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O4


BBLOCK(particleInit, {

    PSTATE.res = 0;

    PC = 1;
    RESAMPLE = false;
})

BBLOCK(geometric, {
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

BBLOCK(nop, {
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

    MAINSTART()

    INITBBLOCK(particleInit)
    INITBBLOCK(geometric)
    INITBBLOCK(nop)

    MAINEND()
}
