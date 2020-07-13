/*
 * File geometric.cu defines the geometric distribution model. 
 */

#include "inference/smc/smc_impl.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/simple-examples/geometric.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/simple-examples/geometric.cu -o smc.exe -std=c++11 -O3
*/

// Initialize the model with program state type and number of bblocks.
INIT_MODEL(int, 1)


BBLOCK_HELPER(geometricRecursive, {

    if(SAMPLE(bernoulli, p))
        return 1;
    else
        return BBLOCK_CALL(geometricRecursive, p) + 1;

}, int, floating_t p)

// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(geometric, {
    
    int numFlips = 0;
    int currFlip = 0;
    
    while(! currFlip) {

        numFlips++;
        currFlip = SAMPLE(bernoulli, 0.6);
    }

    PSTATE = numFlips;
    

    // PSTATE = BBLOCK_CALL(geometricRecursive, 0.6);
    PC++;
})

// Use result after inference. 
CALLBACK(callback, {

    printHistogram(PSTATES, N, 10, 1, 10);

})

// Wrapper for main function
MAIN({
    // Initialize bblock, add it to the array of bblocks to be executed in inference
    // Essentially, it adds the BBLOCK to the model
    ADD_BBLOCK(geometric);

    // Run SMC inference
    SMC(callback);
})
