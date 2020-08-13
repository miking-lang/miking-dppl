/*
 * File geometric_recursive.cu defines the recursive geometric distribution model. 
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"

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
    PSTATE = BBLOCK_CALL(geometricRecursive, 0.6);
    PC++;
})

// Use result after inference. Prints frequencies that represents resulting distribution. 
CALLBACK(callback, {

    int frequencies[10] = {0};

    for(int i = 0; i < N; i++) {
        int outcome = PSTATES[i];
        if(outcome <= 10)
            frequencies[outcome-1]++;
    }

    for(int i = 0; i < 10; i++)
        printf("%d: %f\n", i+1, frequencies[i] / (double)N);

})

// Wrapper for main function
MAIN({
    // Initialize bblock, add it to the array of bblocks to be executed in inference
    // Essentially, it adds the BBLOCK to the model
    ADD_BBLOCK(geometric);

    // Run SMC inference
    SMC(callback);
})
