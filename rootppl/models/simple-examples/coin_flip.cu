/*
 * File coin_flip.cu defines a coin flip example model. 
 */

#include "inference/smc/smc_impl.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/simple-examples/coin_flip.cu -o smc.exe -std=c++14 -O3
g++ -x c++ -I . models/simple-examples/coin_flip.cu -o smc.exe -std=c++14 -O3
*/
 
// Initialize the model with program state type and number of bblocks.
INIT_MODEL(double, 1)
 
 
// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(coinFlip, {
     
    double x = SAMPLE(beta, 2, 2);
    // if(! SAMPLE(bernoulli, x))
        // WEIGHT(-INFINITY);
    // CONDITION(SAMPLE(bernoulli, x));
    OBSERVE(bernoulli, x, true);

    PSTATE = x;
    PC++;
})
 
// WebPPL N=100K: -0.6949150213133173, -0.6918362896430406

 // Use result after inference. 
CALLBACK(callback, {
 
    // printHistogram(PSTATES, N, 10, 0.0, 1.0);
 
})
 
// Wrapper for main function
MAIN({
     // Add the bblock to the model
    ADD_BBLOCK(coinFlip);
 
     // Run SMC inference
    SMC(callback);
})
 