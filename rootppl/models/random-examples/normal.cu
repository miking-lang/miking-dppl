#include <iostream>
#include <cstring>
#include <limits>

#include "../../inference/smc/smc_impl.cuh"
#include "../../utils/distributions.cuh"

// nvcc -arch=sm_75 -rdc=true Src/Models/RandomExamples/normal.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

struct progState_t {
    double val;
};

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
    bool parallelExec = true, parallelResampling = false;

    T ret;

    SMCSTART(progState_t)

    INIT_BBLOCK_NESTED(bblock, progState_t)

    unsigned long long seed = static_cast<unsigned long long>(BBLOCK_CALL(sampleUniform, 0 ,1) * ULLONG_MAX);
    // printf("Seed: %llu\n", seed);
    
    SMCEND_NESTED(progState_t, calcResult, ret, NULL, parallelExec, parallelResampling, seed)

    return ret;
}
//, T, int seed, pplFunc_t<progState_t> bblock)

BBLOCK(normal8, progState_t, {
    
    floating_t val = BBLOCK_CALL(sampleNormal, 8.0, 1);
    PSTATE.val = val;
    // printf("val: %f\n", val);

    PC++;
    RESAMPLE = false;
})

BBLOCK(normal4, progState_t, {
    
    PSTATE.val = BBLOCK_CALL(sampleNormal, 4.0, 1);
    PSTATE.val += runNestedInference<double>(particles, i, normal8);

    PC++;
    RESAMPLE = false;
})

BBLOCK(normal2, progState_t, {
    
    PSTATE.val = BBLOCK_CALL(sampleNormal, 2.0, 1);
    PSTATE.val += runNestedInference<double>(particles, i, normal4);

    PC++;
    RESAMPLE = false;
})


STATUSFUNC({
    double sum = 0.0;
    for(int i = 0; i < NUM_PARTICLES; i++)
        sum += PSTATE.val;

    printf("Sample average: %f\n", sum / static_cast<double>(NUM_PARTICLES));
})


int main() {

    initGen();
    
    SMCSTART(progState_t)

    INIT_BBLOCK(normal2, progState_t)
    // INIT_BBLOCK(normal4, progState_t)
    // INIT_BBLOCK(normal8, progState_t)

    SMCEND(progState_t)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}