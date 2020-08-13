/*
 * File geometric_iterative.cu defines the iterative geometric distribution model. 
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"

// Initialize the model with program state type and number of bblocks.
INIT_MODEL(int, 1)

// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(geometric, {
    int numFlips = 1;
    while(! SAMPLE(bernoulli, 0.6))
        numFlips++;

    PSTATE = numFlips;
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
