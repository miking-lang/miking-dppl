/*
 * File geometric.cu defines the geometric distribution model. 
 */

#include <iostream>
#include <vector>

#include "inference/smc/smc_impl.cuh"

using namespace std;

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/geometric.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/random-examples/geometric.cu -o smc.exe -std=c++11 -O3
*/

struct progState_t {
    int res;
};

#define NUM_BBLOCKS 2
INIT_GLOBAL(progState_t, NUM_BBLOCKS)

BBLOCK_DECLARE(particleInit, progState_t)
BBLOCK_DECLARE(geometric, progState_t)

BBLOCK(particleInit, progState_t, {

    PSTATE.res = 0;

    PC = 1;
    BBLOCK_CALL(geometric, NULL);
})

BBLOCK(geometric, progState_t, {
    int numFlips = 0;
    int currFlip = 1;
    
    while(currFlip) {

        numFlips++;
        // currFlip = flipK<progState_t>(particles, i);
        currFlip = SAMPLE(bernoulli, 0.5);
    }

    WEIGHT(numFlips > 2 ? 0 : -INFINITY);

    PSTATE.res = numFlips;

    PC++;
})


CALLBACK_HOST(callback, progState_t, {

    int tally[100] = {0};

    for(int i = 0; i < NUM_PARTICLES; i++)
        tally[PSTATE.res]++;

    for(int i = 1; i < 10; i++)
        printf("%d: %f\n", i, tally[i] / (double)NUM_PARTICLES);

})


MAIN({

    INIT_BBLOCK(particleInit, progState_t)
    INIT_BBLOCK(geometric, progState_t)

    SMC(progState_t, callback)

})
