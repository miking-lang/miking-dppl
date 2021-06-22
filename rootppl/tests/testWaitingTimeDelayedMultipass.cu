/**
 * Tests waitingTimeDelayed
 */

#include <iostream>
#include <cstring>
#include <string>
#include <iostream>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "utils/math.cuh"
#include "dists/delayed.cuh"

const floating_t k = 1000;
const floating_t theta = 0.0001;
const floating_t factor = 2;
const floating_t observedTime = 10;
const floating_t nEvents = 5;
const floating_t elapsedTime = 10;
const int passes = 1000;

const std::string testName = "testWaitingTimeDelayedMultipass";
int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line


INIT_MODEL(floating_t, 1);



BBLOCK(testWaitingTimeDelayedRef, {
    /* We will sample two waiting times (so that we have an update on the rate),
       and then check the distribution of the second waiting time against WebPPL.*/
    gamma_t lambda(k, theta);
    //ret_delayed_t ret0 = BBLOCK_CALL(waitingTimeDelayed, lambda.k, lambda.theta, lambda.factor);
    //ret_delayed_t ret1 = BBLOCK_CALL(waitingTimeDelayed, ret0.k, ret0.theta, lambda.factor);

    floating_t t0 = 1;
    
    for (int i = 0; i < passes; i++) {
      t0 = sample_GammaExponential(lambda, factor);
      // if (i%100000 == 0) {
      // 	std::cout << "Iteration " << i << "\n"; //lambda.k << " " << lambda.theta;
      // }
    }
    //std::cout << "k: " << lambda.k << "theta: " << lambda.theta << "\n";
  
    
    PSTATE = t0 ;
    
    PC++;
  });





CALLBACK(stats, {
    std::string fileName = "tests/" + testName + ".csv";
    std::ofstream resultFile (fileName, std::ios_base::app);
    if(resultFile.is_open()) {
      for(int i = 0; i < N; i++) {
	resultFile << PSTATES[i] << ", " << exp(WEIGHTS[i])/numRuns << "\n";
      }
      resultFile.close();
    } else {
      printf("Couldnot open file %s\n", fileName.c_str());
    }
})



MAIN({
    if(argc > 2) { 
      numRuns = atoi(argv[2]);			
    }
    else {
      numRuns = 1;
    }
    
    
    ADD_BBLOCK(testWaitingTimeDelayedRef);
    
  
    SMC(stats);
  })
