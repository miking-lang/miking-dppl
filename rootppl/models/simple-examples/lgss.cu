/*
 * File lgss.cu defines the lgss model.
 */

#include <stdio.h>
#include <cstring>
#include <math.h>

#include "inference/smc/smc.cuh"

const int NUM_OBS = 3;

struct progState_t {
    floating_t x;
    int t;
};

INIT_MODEL(progState_t)

BBLOCK_DATA(data, floating_t, NUM_OBS);

BBLOCK_DECLARE(init)
BBLOCK_DECLARE(lgss)

BBLOCK(init, progState_t, {
    PSTATE.x = SAMPLE(normal, 0.0, 100);
    PSTATE.t = 0;

    NEXT = lgss;
    BBLOCK_CALL(lgss, NULL);
})

BBLOCK(lgss, progState_t, {

    floating_t* dataP = DATA_POINTER(data);

    PSTATE.x = SAMPLE(normal, PSTATE.x + 2.0, 1);
    // WEIGHT(normalScore(dataP[PSTATE.t], PSTATE.x, 1.0));
    OBSERVE(normal, PSTATE.x, 1.0, dataP[PSTATE.t]);

    if(++PSTATE.t == NUM_OBS)
        NEXT = NULL;

})

CALLBACK(callback, {

    floating_t weightedSum = 0;
    for (int i = 0; i < N; i++)
        weightedSum += PSTATES[i].x * exp(WEIGHTS[i]);

    printf("Estimated Mean x: %f\n", weightedSum);
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

    FIRST_BBLOCK(init);

    SMC(callback);
})
