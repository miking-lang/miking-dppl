#include "../../inference/mcmc/mcmc.cuh"
#include "../../inference/mcmc/mcmc_impl.cuh"
// #include "../../Utils/distributions.cuh"
#include "../../utils/misc.cuh"

#include <random>
#include <time.h>

// g++ -x c++ Src/Models/RandomExamples/lgssMcmc.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

// nvcc -arch=sm_75 -rdc=true Src/Models/RandomExamples/lgssMcmc.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

default_random_engine generatorDists;

uniform_real_distribution<floating_t> uniformDist(0.0, 1.0);
normal_distribution<floating_t> normalDist(0, 1);

// [min, max)
floating_t sampleUniform(floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

floating_t sampleNormal(floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}


const int NUM_OBS = 3;
const int NUM_BBLOCKS = 2;

struct progState_t {
    floating_t x;
};

BBLOCK_DATA(data, floating_t, NUM_OBS);

void lgss(samplesMcmc_t<progState_t>* samples, int pc, int it, void* arg = NULL) {
    // progState_t state = samples->traces[it][pc-1];
    floating_t prevX = samples->traces[it][pc-1].x;
    samples->traces[it][pc].x = sampleNormal(prevX + 2.0, 1);
    // state.t += 1;
    if(pc+1 < NUM_OBS) // Limit here?
        lgss(samples, pc+1, it, arg);
    else 
        samples->traceIdxs[it] = pc;
}

void init(samplesMcmc_t<progState_t>* samples, int pc, int it, void* arg = NULL) {
    // progState_t state = samples->traces[it][0]; // pc should be 0 here
    samples->traces[it][pc].x = sampleNormal(0.0, 100);
    // state.t = 0;
    // printf("x: %f\n", samples->traces[it][pc].x);
    lgss(samples, pc+1, it, arg);
}


/*
BBLOCK(init, progState_t, {
    // PSTATE.x = BBLOCK_CALL(sampleNormal, 0.0, 100);
    // PSTATE.t = 0;

    // PC++;
})
*/

/*
BBLOCK(lgss, progState_t, {

    floating_t* dataP = DATA_POINTER(data);
    
    // PSTATE.x = BBLOCK_CALL(sampleNormal, PSTATE.x + 2.0, 1);
    // WEIGHT(logPDFNormal(dataP[PSTATE.t], PSTATE.x, 1.0));
})
*/

void statusFunc(samplesMcmc_t<progState_t>* samples, int lastSampleIdx) {
    progState_t lastState = samples->traces[lastSampleIdx][NUM_BBLOCKS-1];
    printf("LastState: x=%f\n", lastState.x);
}

/*
STATUSFUNC({

    
    floating_t xSum = 0;
    for (int i = 0; i < NUM_PARTICLES; i++)
        xSum += PSTATE.x;
    int i = NUM_PARTICLES / 2;
    printf("Mean x: %f, Median x: %f\n", xSum / NUM_PARTICLES, PSTATE.x);
    
})
*/

void setup() {
    generatorDists.seed(time(NULL));
    const floating_t obs[NUM_OBS] = {24.39, 25.47, 29.62};
    for (int i = 0; i < NUM_OBS; i++)
        data[i] = obs[i];
}

int main() {
    setup();

    COPY_DATA_GPU(data, floating_t, NUM_OBS);

    MCMCSTART(progState_t);

    INIT_BBLOCK(init, progState_t);
    INIT_BBLOCK(lgss, progState_t);

    MCMCEND(progState_t);
}
