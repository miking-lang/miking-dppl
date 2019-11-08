#include <iostream>
#include <cstring>
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "../Utils/array.cuh"

// nvcc -arch=sm_75 -rdc=true Src/PPLExamples/normal.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

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
DEV T runNestedInference(int parentIndex, pplFunc_t<progState_t> bblock) {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMCSTART(progState_t)

    INITBBLOCK_NESTED(bblock, progState_t)
    
    SMCEND_NESTED(progState_t, calcResult, ret, NULL, parallelExec, parallelResampling, parentIndex)

    return ret;
}

BBLOCK(normal8, progState_t, {
    
    PSTATE.val = BBLOCK_CALL(normal, 8.0, 0.001);

    PC++;
    RESAMPLE = false;
})

BBLOCK(normal4, progState_t, {
    
    PSTATE.val = BBLOCK_CALL(normal, 4.0, 0.001);
    PSTATE.val += runNestedInference<double>(i, normal8);

    PC++;
    RESAMPLE = false;
})

BBLOCK(normal2, progState_t, {
    
    PSTATE.val = BBLOCK_CALL(normal, 2.0, 1);
    PSTATE.val += runNestedInference<double>(i, normal4);

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

    INITBBLOCK(normal2, progState_t)
    // INITBBLOCK(normal4, progState_t)
    // INITBBLOCK(normal8, progState_t)

    SMCEND(progState_t)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}