/*
 * File mixture.cu defines a mixture model example. 
 */

#include "inference/smc/smc_impl.cuh"


/*
Compile commands:
 
nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/simple-examples/mixture.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/simple-examples/mixture.cu -o smc.exe -std=c++11 -O3
*/
  
// Initialize the model with program state type and number of bblocks.
INIT_MODEL(double, 1)
  
  
// Define the model (or fragment of model) with a BBLOCK. 
BBLOCK(mixture, {
      
    double x;
    if(SAMPLE(bernoulli, 0.7))
        x = SAMPLE(normal, -2, 1);
    else
        x = SAMPLE(normal, 3, 1);
 
    PSTATE = x;
    PC++;
})
  
  // Use result after inference. 
CALLBACK(callback, {
  
    printHistogram(PSTATES, N, 50, -6.0, 6.0);
  
})
  
// Wrapper for main function
MAIN({
    // Initialize bblock, add it to the array of bblocks to be executed in inference
    // Essentially, it adds the BBLOCK to the model
    ADD_BBLOCK(mixture);
  
    // Run SMC inference
    SMC(callback);
})
  