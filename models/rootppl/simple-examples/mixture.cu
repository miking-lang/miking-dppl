/*
 * File mixture.cu defines a mixture model example.
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"


// Initialize the model with program state type and number of bblocks.
INIT_MODEL(double)


// Define the model (or fragment of model) with a BBLOCK.
BBLOCK(mixture, {

    double x;
    if(SAMPLE(bernoulli, 0.7))
        x = SAMPLE(normal, -2, 1);
    else
        x = SAMPLE(normal, 3, 1);

    PSTATE = x;
    NEXT = NULL;
})

// Use result after inference. Calculates sample mean, which perhaps is not very meaningful for a mixture model.
CALLBACK(callback, {

    double sum = 0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i];
    double mean = sum / N;
    printf("Sample mean: %f\n", mean);

})

// Wrapper for main function
MAIN({
    // Initialize bblock, add it to the array of bblocks to be executed in inference
    // Essentially, it adds the BBLOCK to the model
    FIRST_BBLOCK(mixture);

    // Run SMC inference
    SMC(callback);
})

