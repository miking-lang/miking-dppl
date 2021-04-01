/**
 * Tests for the ClaDS2 Helper Functions
 */

#include <iostream>
#include <cstring>
#include <string>
#include <fstream>
#include "inference/smc/smc.cuh"
#include "utils/math.cuh"

#include "../tree-utils/tree_utils.cuh"
typedef bisse32_tree_t tree_t;

#include "clads2-delayed.cuh"
#include "helper-factor.cuh"    

const floating_t k = 1000;
const floating_t theta = 0.0008;
const floating_t factor = 10;
const floating_t epsilon = 0.5;

const floating_t startTime = 1;
const floating_t stopTime = 0.5;

const floating_t alpha = 0.9;
const floating_t sigma = 0.5;
const floating_t rho = 0.5;

INIT_MODEL(unsigned char, 1);

//  bool, floating_t startTime, floating_t lambda, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
BBLOCK(testClads2GoesUndetected, {
  floating_t lambda0 = SAMPLE(gamma, k, theta);
  floating_t lambda = lambda0*factor;

  bool result0 = BBLOCK_CALL(clads2GoesUndetected, startTime, lambda, alpha, sigma, epsilon, rho);
  bool result = BBLOCK_CALL(clads2GoesUndetected, startTime, lambda, alpha, sigma, epsilon, rho);

  PSTATE = result;
  PC++;
});


//  bool, floating_t startTime, floating_t lambda, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
BBLOCK(testClads2GoesUndetectedFactor, {
  floating_t lambda0 = SAMPLE(gamma, k, theta);
 // floating_t lambda = lambda0*factor;

  bool result0 = BBLOCK_CALL(clads2GoesUndetectedFactor, startTime, lambda0, factor, alpha, sigma, epsilon, rho);
  bool result = BBLOCK_CALL(clads2GoesUndetectedFactor, startTime, lambda0, factor, alpha, sigma, epsilon, rho);

  PSTATE = result;
  PC++;
});


BBLOCK(testClads2GoesUndetectedDelayed, {
  rate_t lambda0(k, theta);

  bool result0 = BBLOCK_CALL(clads2GoesUndetectedDelayed, startTime, lambda0, factor, alpha, sigma, epsilon, rho);
  bool result = BBLOCK_CALL(clads2GoesUndetectedDelayed, startTime, lambda0, factor, alpha, sigma, epsilon, rho);
       
  PSTATE = result;
  PC++;
});




/*simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)*/
BBLOCK(testSimBranchProb, {
  floating_t lambda0 = SAMPLE(gamma, k, theta);
  //floating_t lambda = lambda0*factor;

  simBranchRet_t result0 = BBLOCK_CALL(simBranch, startTime, stopTime, lambda0, alpha, sigma, epsilon, rho);
  floating_t lambda = result0.r0;

  simBranchRet_t result = BBLOCK_CALL(simBranch, startTime, stopTime, lambda, alpha, sigma, epsilon, rho);

  PSTATE = result.r2;
  PC++;
});


/*simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)*/
    BBLOCK(testSimBranchFactorProb, {
      floating_t lambda0 = SAMPLE(gamma, k, theta);
      //floating_t lambda = lambda0*factor;
    
      simBranchRet_t result0 = BBLOCK_CALL(simBranchFactor, startTime, stopTime, lambda0, 1.0, alpha, sigma, epsilon, rho);
      floating_t factor = result0.r0;
    
      simBranchRet_t result = BBLOCK_CALL(simBranchFactor, startTime, stopTime, lambda0, factor, alpha, sigma, epsilon, rho);
    
      PSTATE = result.r2;
      PC++;
    });



BBLOCK(testSimBranchProbDelayed, {
  rate_t lambda0(k, theta);

  simBranchRet_t result0 = BBLOCK_CALL(simBranchDelayed, startTime, stopTime, lambda0, 1.0, alpha, sigma, epsilon, rho);

  simBranchRet_t result = BBLOCK_CALL(simBranchDelayed, startTime, stopTime, lambda0, result0.r0, alpha, sigma, epsilon, rho);  

  PSTATE = result.r2; // this is the probability accmmulated along the branch
  // TODO need to test the terminal lambda
  PC++;
});


/*simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)*/
BBLOCK(testSimBranchLambda, {
  floating_t lambda0 = SAMPLE(gamma, k, theta);
  simBranchRet_t result0 = BBLOCK_CALL(simBranch, startTime, stopTime, lambda0, alpha, sigma, epsilon, rho);
  floating_t lambda = result0.r0;
  simBranchRet_t result = BBLOCK_CALL(simBranch, startTime, stopTime, lambda, alpha, sigma, epsilon, rho);
  PSTATE = result.r0; 
  PC++;
});


/*simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)*/
BBLOCK(testSimBranchFactorLambda, {
  floating_t lambda0 = SAMPLE(gamma, k, theta);
  simBranchRet_t result0 = BBLOCK_CALL(simBranchFactor, startTime, stopTime, lambda0, 1.0, alpha, sigma, epsilon, rho);
  floating_t factor = result0.r0;
  simBranchRet_t result = BBLOCK_CALL(simBranchFactor, startTime, stopTime, lambda0, factor, alpha, sigma, epsilon, rho);
  floating_t finalLambda = lambda0*result.r0;
  PSTATE = finalLambda; 
  PC++;
});


BBLOCK(testSimBranchLambdaDelayed, {
  rate_t lambda0(k, theta);
  simBranchRet_t result0 = BBLOCK_CALL(simBranchDelayed, startTime, stopTime, lambda0, 1.0, alpha, sigma, epsilon, rho);
  floating_t factor = result0.r0;
  simBranchRet_t result = BBLOCK_CALL(simBranchDelayed, startTime, stopTime, lambda0, factor, alpha, sigma, epsilon, rho);  
  floating_t finalLambda = SAMPLE(gamma, lambda0.k, lambda0.theta*result.r0); 
  PSTATE = finalLambda; // this is the probability accmmulated along the branch
  // TODO need to test the terminal lambda
  PC++;
});


CALLBACK(stats, {
    double sum = 0;
    
    for(int i = 0; i < N; i++) {
        sum += PSTATES[i];
    }
    double mean = sum / N;

    double sumSqDist = 0;
    for(int i = 0; i < N; i++) {
      sumSqDist += (PSTATES[i] - mean)*(PSTATES[i] - mean);    
    }
    double var = sumSqDist / N;
    
    printf("Sample mean: %f\n", mean);
    printf("Sample var: %f\n", var);
    printf("Sample stddev: %f\n", sqrt(var));
})

MAIN({
    //printf("clads2GoesUndetected:\n"); ADD_BBLOCK(testClads2GoesUndetected);
    //printf("clads2GoesUndetectedFactor:\n"); ADD_BBLOCK(testClads2GoesUndetectedFactor);
    printf("clads2GoesUndetectedDelayed:\n"); ADD_BBLOCK(testClads2GoesUndetectedDelayed);
    

    //ADD_BBLOCK(testSimBranchProb);
    //ADD_BBLOCK(testSimBranchProbDelayed);
    //ADD_BBLOCK(testSimBranchFactorProb);

    //printf("simBranchLambda:\n"); ADD_BBLOCK(testSimBranchLambda);
    //printf("simBranchFactorLambda:\n"); ADD_BBLOCK(testSimBranchFactorLambda);
    //printf("simBranchLambdaDelayed:\n"); ADD_BBLOCK(testSimBranchLambdaDelayed);
    

    SMC(stats);
  })
