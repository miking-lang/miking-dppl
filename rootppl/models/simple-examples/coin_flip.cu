/*
 * File coin_flip.cu defines a coin flip example model. 
 */

#include <stdio.h>
#include <math.h>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"

 
// Initialize the model with program state type and number of bblocks.
INIT_MODEL(double, 1)
 
 
// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(coinFlip, {
     
    double x = SAMPLE(beta, 2, 2);
    OBSERVE(bernoulli, x, true);

    PSTATE = x;
    PC++;
})

// Use result after inference. 
CALLBACK(mean, {
 
    double weightedSum = 0;
    for(int i = 0; i < N; i++)
        weightedSum += PSTATES[i] * exp(WEIGHTS[i]);

    printf("Estimated Mean: %f\n", weightedSum);
 
})
 
// Wrapper for main function
MAIN({
     // Add the bblock to the model
    ADD_BBLOCK(coinFlip);
 
     // Run SMC inference
    SMC(mean);
})
 