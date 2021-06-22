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
const std::string testName = "testWaitingTimeMultipass";
const int passes = 1000;

int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line


INIT_MODEL(floating_t, 1);

BBLOCK(testWaitingTime, {
  floating_t lambda = SAMPLE(gamma, k, theta);

  floating_t t0 = 1;
  
  for (int i = 0; i < passes; i++) {
    t0 = SAMPLE(exponential, lambda*factor);
  }
    
  PSTATE = t0;
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
    
    ADD_BBLOCK(testWaitingTime);
   
    SMC(stats);
  })
