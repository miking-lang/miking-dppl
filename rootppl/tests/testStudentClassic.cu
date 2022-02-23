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

const floating_t k = 13; // degrees of freedom
const std::string testName = "testStudentClassic";

int numParts; // number of particles, supplied by first argument
int numRuns; // number of runs supplied by the command line


INIT_MODEL(floating_t);

BBLOCK(testStudentClassic, {
  /* We will sample two waiting times (so that we have an update on the rate),
     and then check the distribution of the second waiting time against WebPPL.*/
  floating_t statistic = SAMPLE(student_t, k);

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

    FIRST_BBLOCK(testStudentClassic);

    SMC(stats);
  })
