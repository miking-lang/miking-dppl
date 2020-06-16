#include "../../../Inference/Smc/smcImpl.cuh"

#include "../TreeUtils/treeUtils.cuh"
#include "simulations.cuh"
#include "stack.cuh"

/**
    This model traverses the tree with a DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/test.cu -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/test.cu -o smc.exe -std=c++11 -O3

struct progState_t {
    floating_t x;
};

#define NUM_BBLOCKS 5
INIT_GLOBAL(progState_t, NUM_BBLOCKS)



BBLOCK(test, progState_t, {
    PSTATE.x = BBLOCK_CALL(sampleBeta, 1, 1);
    PC = 1;
})

CALLBACK_HOST(resFunc, progState_t, {
    floating_t sum = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        sum += PSTATE.x;
        printf("beta[%d]: %f\n", i, PSTATE.x);
    }
    printf("avg: %f\n", sum / NUM_PARTICLES);
})


int main() {

    initGen();

    INITBBLOCK(test, progState_t)

    SMC(progState_t, resFunc)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}
