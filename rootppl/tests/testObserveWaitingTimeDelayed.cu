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

const std::string testName = "testObserveWaitingTimeDelayed";
int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line

INIT_MODEL(floating_t);

BBLOCK(testObserveWaitingTimeDelayedRef, {
    gamma_t lambda(k, theta);

    //floating_t ret0 = score_GammaExponential(observedTime, lambda, factor);
    //WEIGHT(ret0);
    OBSERVE(gammaExponential, observedTime, lambda, factor);

    floating_t ret1 = SAMPLE(gammaExponential, lambda, factor);
    PSTATE = ret1;
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
     
    FIRST_BBLOCK(testObserveWaitingTimeDelayedRef);

    SMC(stats);
  })
