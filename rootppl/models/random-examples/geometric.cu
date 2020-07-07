/*
 * File geometric.cu defines the geometric distribution model. 
 */

#include "inference/smc/smc_impl.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/random-examples/geometric.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/random-examples/geometric.cu -o smc.exe -std=c++11 -O3
*/


#define NUM_BBLOCKS 2
INIT_MODEL(int, NUM_BBLOCKS)


BBLOCK(geometric, {
    int numFlips = 0;
    int currFlip = 1;
    
    while(currFlip) {

        numFlips++;
        currFlip = SAMPLE(bernoulli, 0.5);
    }

    PSTATE = numFlips;
    PC++;
})


CALLBACK(callback, {

    // Calculate frequencies
    int tally[100];
    calculateFrequencies(PSTATES, N, 100, tally);

    for(int i = 1; i < 10; i++)
        printf("%d: %f\n", i, tally[i] / (double)N);

    printNormalizedFrequencies(PSTATES, N, 100, tally);

    printHistogram(PSTATES, N, 10, 1, 10);
    printHistogram(PSTATES, N, 10);

})


MAIN({
    INIT_BBLOCK(geometric)

    SMC(callback)
})
