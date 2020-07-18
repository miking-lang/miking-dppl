/*
 * File geometric.cu defines the geometric distribution model. 
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"

// Initialize the model with program state type and number of bblocks.
INIT_MODEL(int, 1)


BBLOCK_HELPER(geometricRecursive, {

    if(SAMPLE(bernoulli, p))
        return 1;
    else
        return BBLOCK_CALL(geometricRecursive, p) + 1;

}, int, double p)

// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(geometric, {
    int numFlips = 1;
    while(! SAMPLE(bernoulli, 0.6))
        numFlips++;

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
