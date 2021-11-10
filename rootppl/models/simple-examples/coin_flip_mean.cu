/*
 * File coin_flip_mean.cu defines a simple example that is useful for demonstrating RootPPL.
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"

/*
* Initialize array for storing BBLOCK function pointers and define progState to be used in BBLOCKS.
* First argument is the program state, second argument is
* the number of BBLOCKS in the model.
*/
INIT_MODEL(int)

/*
 * Define BBLOCK (fragment of model)
 */
BBLOCK(coinFlip, {
    PSTATE = SAMPLE(bernoulli, 0.6);
    NEXT = NULL;
})

/*
 * Define callback function that calculates and prints mean of coin flips before particles are deleted.
 */
CALLBACK(sampleMean, {
    double sum = 0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i];
    double mean = sum / N;
    printf("Sample mean: %f\n", mean);
})

/*
 * The main function, any code can be added here. In order to run SMC, blocks must be added
 * and SMC be called with the callback function as argument (or NULL if no callback is used).
 */
MAIN({
    FIRST_BBLOCK(coinFlip);

    SMC(sampleMean);
})

