/*
 * File simple.cu defines a simple example that is useful for demonstrating RootPPL. 
 */


#include "inference/smc/smc_impl.cuh"

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/coin_flip_simple.cu -o smc.exe -std=c++14 -O3
g++ -x c++ -I . models/random-examples/coin_flip_simple.cu -o smc.exe -std=c++14 -O3
*/

/* 
* Initialize array for storing BBLOCK function pointers and define progState to be used in BBLOCKS. 
* First argument is the program state, second argument is 
* the number of BBLOCKS in the model. 
*/
INIT_MODEL(int, 1) // Change name of this?
  
/*
 * Define BBLOCK (fragment of model)
 */
BBLOCK(coinFlip, {
    PSTATE = SAMPLE(bernoulli, 0.6);
    PC++;
})
 
CALLBACK(sampleMean, {

    double mean = meanArray(PSTATES, N);
    printf("Sample mean: %f\n", mean);
     
})
 
MAIN({
    ADD_BBLOCK(coinFlip);
 
    SMC(sampleMean);
})
  