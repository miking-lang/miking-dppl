#include <stdio.h>
#include <math.h>
#include <random>
#include <time.h>

#include "../../Inference/Smc/smc.cuh"
#include "../../Inference/Smc/smcImpl.cuh"
#include "../../Utils/distributions.cuh"
#include "../../Utils/misc.cuh"
#include "airplane.cuh"
#include "airplaneUtils.cuh"

// nvcc -arch=sm_75 -rdc=true Src/Models/Airplane/*.cu -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

using namespace std;

#define NUM_BBLOCKS 2
INIT_GLOBAL(progState_t, NUM_BBLOCKS)

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

BBLOCK(propagateAndWeight, progState_t, {

    // Propagate
    PSTATE.x += BBLOCK_CALL(sampleNormal, VELOCITY, TRANSITION_STD);

    // Weight
    WEIGHT(logNormalPDFObs(DATA_POINTER(planeObs)[PSTATE.t], mapLookupApprox(DATA_POINTER(mapApprox), PSTATE.x)));
    PSTATE.t++;

    if(PSTATE.t >= TIME_STEPS - 1)
        PC = 2;

    // RESAMPLE = true;
})

BBLOCK(particleInit, progState_t, {

    PSTATE.x = BBLOCK_CALL(sampleUniform, 0, MAP_SIZE);
    PSTATE.t = 0;

    PC = 1;
    // RESAMPLE = false;
    BBLOCK_CALL(propagateAndWeight);
})

CALLBACK_HOST(callback, progState_t, {
    // Checks how many particles are close to actual airplane to check for correctness
    int numParticlesClose = 0;
    floating_t minX = 999999;
    floating_t maxX = -1;
    for (int i = 0; i < NUM_PARTICLES; i++) {
        floating_t particleX = PSTATE.x;
        if(abs(particleX - planeX[TIME_STEPS-1]) < 10)
            numParticlesClose++;
        minX = min(minX, particleX);
        maxX = max(maxX, particleX);
    }

    cout << "Num particles close to target: " << 100 * static_cast<floating_t>(numParticlesClose) / NUM_PARTICLES << "%, MinX: " << minX << ", MaxX: " << maxX << endl;
}, void* arg = NULL)

int main(int argc, char** argv) {

    initAirplane();

    // SMCSTART(progState_t, NUM_BBLOCKS)

    INITBBLOCK(particleInit, progState_t)
    INITBBLOCK(propagateAndWeight, progState_t)

    SMC(progState_t, callback)
}
