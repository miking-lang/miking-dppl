/*
 * File simple.cu defines a simple example that is useful for demonstrating RootPPL. 
 */


#include "inference/smc/smc_impl.cuh"

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/simple.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/random-examples/simple.cu -o smc.exe -std=c++11 -O3
*/

/* 
 * Initialize array for storing BBLOCK function pointers and define progState to be used in BBLOCKS. 
 * First argument is the program state, second argument is 
 * the number of BBLOCKS in the model. 
 */
INIT_MODEL(floating_t, 2) // Change name of this?
 
/*
 * Define BBLOCK (fragment of model)
 */
BBLOCK(init, {
    PSTATE = SAMPLE(normal, 0.0, 100);
    PC++;
})

BBLOCK(next, {
    PC++;
})
 

CALLBACK(callback, {
    
    floating_t sum = 0;
    for(int i = 0; i < N; i++) {
        sum += PSTATE;
    }
    printf("Average result: %f\n", sum / N);
    
})

MAIN({
    INIT_BBLOCK(init);
    INIT_BBLOCK(next);

    SMC(callback);
})
 