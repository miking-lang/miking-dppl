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

const floating_t m0 = 0.0; // mean
const floating_t v = 1.0; // variance multiplier
const floating_t a = 1.0; // scale
const floating_t b = 0.2; // scale

const std::string testName = "testLogAlphaSigmaSquaredDelayed";

int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line


INIT_MODEL(floating_t, 1);

BBLOCK(testLogAlphaSigmaSquaredDelayed, {
  /* We will sample two waiting times (so that we have an update on the rate),
     and then check the distribution of the second waiting time against WebPPL.*/

    // floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, a / b);
    //floating_t sigma = sqrt(sigmaSquared);
    //floating_t alpha = exp(SAMPLE(normal, m0, sigma));

    normalInverseGamma_t prior(m0, v, a, b);
       
    floating_t f1 = sample_NormalInverseGammaNormal(prior);
    
    PSTATE = f1;
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
    
    ADD_BBLOCK(testLogAlphaSigmaSquaredDelayed);
  
    SMC(stats);
  })
