/*
 * File nestedNormal.cu defines a model that is used to test multiple layers of nested inference. 
 */

#include <iostream>
#include <cstring>
#include <limits>

#include "inference/smc/smc_impl.cuh"

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/nestedNormal.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/random-examples/nestedNormal.cu -o smc.exe -std=c++11 -O3
*/

struct progState_t {
    double val;
};

#define NUM_BBLOCKS 1
INIT_GLOBAL(progState_t, NUM_BBLOCKS)
#define NUM_BBLOCKS_NESTED 1


CALLBACK(calcResult, progState_t, {
    double sum = 0.0;
    for(int i = 0; i < NUM_PARTICLES_NESTED; i++)
        sum += PSTATE.val;

    // printf("Sample average: %f\n", sum / static_cast<double>(NUM_PARTICLES_NESTED));
    double* retP = static_cast<double*>(ret);
    *retP = sum / static_cast<double>(NUM_PARTICLES_NESTED);
    
}, void* ret)

template <typename T>
DEV T runNestedInference(particles_t<progState_t>* particles, int i, pplFunc_t<progState_t> bblock) {
//BBLOCK_HELPER(runNestedInference, {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMC_PREPARE_NESTED(progState_t, NUM_BBLOCKS_NESTED)

    INIT_BBLOCK_NESTED(bblock, progState_t)

    unsigned long long seed = static_cast<unsigned long long>(SAMPLE(uniform, 0 ,1) * ULLONG_MAX);
    // int seed = i;
    // printf("Seed: %llu\n", seed);
    
    SMC_NESTED(progState_t, calcResult, ret, NULL, parallelExec, parallelResampling, seed)

    return ret;
}
//, T, int seed, pplFunc_t<progState_t> bblock)

BBLOCK(normal8, progState_t, {
    
    floating_t val = SAMPLE(normal, 8.0, 1);
    PSTATE.val = val;
    // printf("val: %f\n", val);

    PC++;
})

BBLOCK(normal4, progState_t, {
    
    PSTATE.val = SAMPLE(normal, 4.0, 1);
    PSTATE.val += runNestedInference<double>(particles, i, normal8);

    PC++;
})

BBLOCK(normal2, progState_t, {
    
    PSTATE.val = SAMPLE(normal, 2.0, 1);
    PSTATE.val += runNestedInference<double>(particles, i, normal4);

    PC++;
})


CALLBACK_HOST(callback, progState_t, {
    double sum = 0.0;
    for(int i = 0; i < NUM_PARTICLES; i++)
        sum += PSTATE.val;

    printf("Sample average: %f\n", sum / static_cast<double>(NUM_PARTICLES));
})


MAIN({

    initGen();

    INIT_BBLOCK(normal2, progState_t)
    // INIT_BBLOCK(normal4, progState_t)
    // INIT_BBLOCK(normal8, progState_t)

    SMC(progState_t, callback)

})