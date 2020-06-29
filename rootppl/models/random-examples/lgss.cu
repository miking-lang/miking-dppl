#include <cstring>

#include "../../inference/smc/smc_impl.cuh"
#include "../../utils/distributions.cuh"

// g++ -x c++ Src/Models/RandomExamples/lgss.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

// nvcc -arch=sm_75 -rdc=true Src/Models/RandomExamples/lgss.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

const int NUM_OBS = 3;

struct progState_t {
    floating_t x;
    int t;
};

BBLOCK_DATA(data, floating_t, NUM_OBS);

BBLOCK(init, progState_t, {
    PSTATE.x = BBLOCK_CALL(sampleNormal, 0.0, 100);
    PSTATE.t = 0;

    PC++;
    RESAMPLE = false;
})

BBLOCK(lgss, progState_t, {

    floating_t* dataP = DATA_POINTER(data);
    
    PSTATE.x = BBLOCK_CALL(sampleNormal, PSTATE.x + 2.0, 1);
    WEIGHT(logPDFNormal(dataP[PSTATE.t], PSTATE.x, 1.0));

    if(++PSTATE.t == NUM_OBS)
        PC++;

    RESAMPLE = true;
})

STATUSFUNC({
    
    floating_t xSum = 0;
    for (int i = 0; i < NUM_PARTICLES; i++)
        xSum += PSTATE.x;
    int i = NUM_PARTICLES / 2;
    printf("Mean x: %f, Median x: %f\n", xSum / NUM_PARTICLES, PSTATE.x);
    
})

void setup() {
    initGen();
    const floating_t obs[NUM_OBS] = {24.39, 25.47, 29.62};
    for (int i = 0; i < NUM_OBS; i++)
        data[i] = obs[i];
}

int main() {
    setup();

    printf("%f\n", PI);

    COPY_DATA_GPU(data, floating_t, NUM_OBS);

    SMCSTART(progState_t);

    INITBBLOCK(init, progState_t);
    INITBBLOCK(lgss, progState_t);

    SMCEND(progState_t);
}
