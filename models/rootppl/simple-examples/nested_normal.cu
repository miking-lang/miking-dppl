/*
 * File nestedNormal.cu defines a model that is used to test multiple layers of nested inference.
 * TODO(dlunde,2021-11-05): ADD_BBLOCK_NESTED does not seem to work right now. Maybe this example model is deprecated?
 */

#include <iostream>
#include <cstring>
#include <limits>
#include <math.h>

#include "inference/smc/smc.cuh"

struct progState_t {
    double val;
};

INIT_MODEL(progState_t)
#define NUM_BBLOCKS_NESTED 1


CALLBACK_NESTED(calcResult, progState_t, {
    double sum = 0.0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i].val;

    // printf("Sample average: %f\n", sum / static_cast<double>(NUM_PARTICLES_NESTED));
    double* retP = static_cast<double*>(ret);
    *retP = sum / static_cast<double>(N);

}, void* ret)

template <typename T>
DEV T runNestedInference(particles_t& particles, int i, pplFunc_t bblock) {
//BBLOCK_HELPER(runNestedInference, {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMC_PREPARE_NESTED(progState_t, NUM_BBLOCKS_NESTED)

    ADD_BBLOCK_NESTED(bblock, progState_t)

    unsigned long long seed = static_cast<unsigned long long>(SAMPLE(uniform, 0 ,1) * 99999999999);
    // int seed = i;
    // printf("Seed: %llu\n", seed);

    SMC_NESTED(progState_t, 100, parallelExec, parallelResampling, seed, calcResult, ret, NULL)

    return ret;
}
//, T, int seed, pplFunc_t<progState_t> bblock)

BBLOCK(normal8, progState_t, {

    floating_t val = SAMPLE(normal, 8.0, 1);
    PSTATE.val = val;
    // printf("val: %f\n", val);

    NEXT = NULL;
})

BBLOCK(normal4, progState_t, {

    PSTATE.val = SAMPLE(normal, 4.0, 1);
    PSTATE.val += runNestedInference<double>(particles, particleIdx, normal8);

    NEXT = NULL;
})

BBLOCK(normal2, progState_t, {

    PSTATE.val = SAMPLE(normal, 2.0, 1);
    PSTATE.val += runNestedInference<double>(particles, particleIdx, normal4);

    NEXT = NULL;
})


CALLBACK(callback, {
    double sum = 0.0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i].val;

    printf("Sample average: %f\n", sum / static_cast<double>(N));
})


MAIN({

    initGen();

    FIRST_BBLOCK(normal2)
    // FIRST_BBLOCK(normal4, progState_t)
    // FIRST_BBLOCK(normal8, progState_t)

    SMC(callback)

})
