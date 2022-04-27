/*
 * File airplane.cu defines the airplane model which is a common example to demonstrate SMC.
 */

#include <stdio.h>
#include <math.h>
#include <random>
#include <time.h>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"

using namespace std;

#include "airplane.cuh"
#include "airplane_utils.cuh"


#define NUM_BBLOCKS 2
INIT_MODEL(progState_t)

floating_t planeX[TIME_STEPS];

BBLOCK_DATA(planeObs, floating_t, TIME_STEPS)
BBLOCK_DATA(mapApprox, floating_t, MAP_SIZE)


void initAirplane() {

    initMap(mapApprox);

    initObservations(planeX, planeObs, mapApprox);

    // Copy data to device pointers, so that they can be accessed from kernels
    COPY_DATA_GPU(planeObs, floating_t, TIME_STEPS)
    COPY_DATA_GPU(mapApprox, floating_t, MAP_SIZE)
}

BBLOCK(propagateAndWeight, {

    // Propagate
    PSTATE.x += SAMPLE(normal, VELOCITY, TRANSITION_STD);

    // Weight
    // printf("t: %d\n", PSTATE.t);
    WEIGHT(logNormalPDFObs(DATA_POINTER(planeObs)[PSTATE.t], mapLookupApprox(DATA_POINTER(mapApprox), PSTATE.x)));
    PSTATE.t++;

    if(PSTATE.t >= TIME_STEPS - 1)
        NEXT = NULL;

})

BBLOCK(particleInit, {

    PSTATE.x = SAMPLE(uniform, 0, MAP_SIZE);
    PSTATE.t = 0;

    NEXT = propagateAndWeight;
    BBLOCK_CALL(propagateAndWeight);
})

// Checks how many particles are close to actual airplane to check for correctness
CALLBACK(callback, {
    int numParticlesClose = 0;
    floating_t minX = 999999;
    floating_t maxX = -1;
    for (int i = 0; i < N; i++) {
        floating_t particleX = PSTATES[i].x;
        if(abs(particleX - planeX[TIME_STEPS-1]) < 10)
            numParticlesClose++;
        minX = min(minX, particleX);
        maxX = max(maxX, particleX);
    }

    cout << "Num particles close to target: " << 100 * static_cast<floating_t>(numParticlesClose) / N << "%, MinX: " << minX << ", MaxX: " << maxX << endl;

})


MAIN(
    initAirplane();

    FIRST_BBLOCK(particleInit)

    SMC(callback)
)
