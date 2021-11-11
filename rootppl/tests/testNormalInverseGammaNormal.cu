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

const floating_t m0 = 0.1; // mean
const floating_t v = 0.9; // variance multiplier
const floating_t a = 10; // scale
const floating_t b = 0.2; // scale

const std::string testName = "testNormalInverseGammaNormal";

int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line


INIT_MODEL(floating_t);

BBLOCK(testNormalInverseGammaNormal, {
  /* We will sample two waiting times (so that we have an update on the rate),
     and then check the distribution of the second waiting time against WebPPL.*/
    normalInverseGamma_t prior(m0, v, a, b);
    
    floating_t statistic = sample_NormalInverseGammaNormal(prior);
  
    // TODO do it with SAMPLE, instead of invoking the function directly
       
    PSTATE = statistic;
    NEXT = NULL;
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
    
    FIRST_BBLOCK(testNormalInverseGammaNormal);
  
    SMC(stats);
  })
