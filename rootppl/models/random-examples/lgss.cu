/*
 * File lgss.cu defines the lgss model. 
 */

#include <cstring>

#include "inference/smc/smc_impl.cuh"

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/lgss.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/random-examples/lgss.cu -o smc.exe -std=c++11 -O3
*/

const int NUM_OBS = 3;

struct progState_t {
    floating_t x;
    int t;
};

#define NUM_BBLOCKS 2
INIT_MODEL(progState_t, NUM_BBLOCKS)

BBLOCK_DATA(data, floating_t, NUM_OBS);

BBLOCK_DECLARE(init, progState_t)
BBLOCK_DECLARE(lgss, progState_t)

BBLOCK(init, progState_t, {
    PSTATE.x = SAMPLE(normal, 0.0, 100);
    PSTATE.t = 0;

    PC++;
    BBLOCK_CALL(lgss, NULL);
})

BBLOCK(lgss, progState_t, {

    floating_t* dataP = DATA_POINTER(data);
    
    PSTATE.x = SAMPLE(normal, PSTATE.x + 2.0, 1);
    WEIGHT(logPDFNormal(dataP[PSTATE.t], PSTATE.x, 1.0));

    if(++PSTATE.t == NUM_OBS)
        PC++;

})

CALLBACK(callback, {
    
    floating_t xSum = 0;
    for (int i = 0; i < N; i++)
        xSum += PSTATE.x;
    int i = N / 2;
    printf("Mean x: %f, Median x: %f\n", xSum / N, PSTATE.x);
    
})

void setup() {
    initGen();
    const floating_t obs[NUM_OBS] = {24.39, 25.47, 29.62};
    for (int i = 0; i < NUM_OBS; i++)
        data[i] = obs[i];
}

MAIN({
    setup();

    COPY_DATA_GPU(data, floating_t, NUM_OBS);

    INIT_BBLOCK(init, progState_t);
    INIT_BBLOCK(lgss, progState_t);

    SMC(progState_t, callback);
})
