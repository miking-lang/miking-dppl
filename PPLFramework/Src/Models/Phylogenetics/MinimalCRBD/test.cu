#include "../../../Inference/Smc/smcImpl.cuh"

/**
    This model traverses the tree with a DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/test.cu -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/test.cu -o smc.exe -std=c++11 -O3

struct progState_t {
    // floating_t x;
    int x[4];
};

#define NUM_BBLOCKS 5
INIT_GLOBAL(progState_t, NUM_BBLOCKS)


BBLOCK(test, progState_t, {
    
    /*floating_t ps[4];
    ps[0] = 0.1;
    ps[1] = 0.2;
    ps[2] = 0.4;
    ps[3] = 0.3;*/
    
    // BBLOCK_CALL(sampleMultinomial, ps, 100, PSTATE.x, 4);
    int n = 3;
    /*
    floating_t cov[n][n] = {
        {2, 1, 0},
        {1, 2, 0},
        {0, 0, 1}
    };
    */
    floating_t cov[3][3];
    cov[0][0] = 4;
    cov[0][1] = 12;
    cov[0][2] = -16;
    cov[1][0] = 12;
    cov[1][1] = 37;
    cov[1][2] = -43;
    cov[2][0] = -16;
    cov[2][1] = -43;
    cov[2][2] = 98;

    /*
    cov[0][0] = 2.1;
    cov[0][1] = 1;
    cov[0][2] = 0;
    cov[1][0] = 1;
    cov[1][1] = 2.3;
    cov[1][2] = 0;
    cov[2][0] = 0;
    cov[2][1] = 0;
    cov[2][2] = 1;
    */
    
    // printf("%f\n", cov[0][0]);
    // printf("%f\n", cov[0][1]);
    
    floating_t res[3][3];
    choleskyDecomposition(cov, n, res);
    // printArray(res, 4);
    PC = 1;
})

CALLBACK_HOST(resFunc, progState_t, {
    
    //floating_t sum = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        //sum += PSTATE.x;
        //printf("dist[%d]: %f\n", i, PSTATE.x);
        //printArray(PSTATE.x, 4);
    }
    //printf("avg: %f\n", sum / NUM_PARTICLES);
    
    
})


MAIN(
    INITBBLOCK(test, progState_t)

    SMC(progState_t, resFunc)
)

/*
int main() {

    initGen();

    INITBBLOCK(test, progState_t)

    SMC(progState_t, resFunc)

    cout << "log(normalizationConstant) = " << res << endl;

    return 0;
}
*/
