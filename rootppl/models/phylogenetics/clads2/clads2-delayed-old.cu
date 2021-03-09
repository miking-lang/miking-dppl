/*
 * File clads2.cu defines the ClaDS2 model
 * uses delayed sampling, encoded manually. 
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */

#include <iostream>
#include <cstring>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

typedef bisse32_tree_t tree_t;
//typedef primate_tree_t tree_t;
//typedef moth_div_tree_t tree_t;

#include "clads2-delayed.cuh"


#define NUM_BBLOCKS 2
INIT_MODEL(progStateDelayed_t, NUM_BBLOCKS)
BBLOCK_DATA(tree, tree_t, 1);

 






// Not called on root as in WebPPL, instead root is handled in simClaDS2 bblock
BBLOCK(simTree, {
    // Fetch tree data
    tree_t* treeP = DATA_POINTER(tree);
    treeIdx_t treeIdx = PSTATE.treeIdx;
    
    // Terminate if tree is fully traversed
    if(treeIdx == -1) {
      
      PC++;
      BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
      return;
    }
    
    PSTATE.treeIdx = treeP->idxNext[treeIdx];
    
    int indexParent = treeP->idxParent[treeIdx];
    
    // floating_t lambda = PSTATE.stack.pop();
  
    floating_t factor = PSTATE.factor.pop();
    
  //  floating_t parentAge = treeP->ages[indexParent];
  //  floating_t branchAge = treeP->ages[treeIdx];
  //  floating_t k = PSTATE.lambda0.k;
  //  floating_t theta = PSTATE.lambda0.theta;
  //  printf(">%d %f %f %f %f f:%f ", treeIdx, parentAge, branchAge, k, theta, factor);
    
    simBranchRet_t ret = BBLOCK_CALL(simBranchDelayed, treeP->ages[indexParent], treeP->ages[treeIdx], PSTATE.lambda0, factor, PSTATE.alpha, PSTATE.sigma, PSTATE.epsilon, PSTATE.rho);
  
    floating_t factorEnd = ret.r0;  // updates the factor

    //printf(" pb:%f f: %f ", branchProb, factor);
    
    // floating_t lambdaEnd = ret.r0;
    // we don't have lambdaEnd anymore. Instead we have the lambda_factor that has been accumulated
    // floating_t end_lambda_factor = ret.lambda_factor;

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;

    //printf("?%d ", interiorNode);
    
    floating_t lnTerminalProb;

    //floating_t lnTerminalProb = interiorNode ? log(lambdaEnd) : log(PSTATE.rho);
    if (interiorNode) {
      // the terminal probability is constructed by observing 0 from exponential with the lambda rate at the end
      lnTerminalProb = BBLOCK_CALL(observeWaitingTimeDelayedRef, 0, PSTATE.lambda0, factorEnd);

    } else {
      lnTerminalProb = log(PSTATE.rho);
    }
    //printf("pt:%f ", lnTerminalProb);
    
    // add the weight accummulated in the side-branch simulations + terminal weight
    WEIGHT(ret.r2 + lnTerminalProb);
    // printf("%f...", branchProb + lnTerminalProb);
    
    if (interiorNode) {
      floating_t f1 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);
      floating_t f2 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);
      floating_t leftf = factorEnd*exp(f1);
      floating_t rightf = factorEnd*exp(f2);

     PSTATE.factor.push(rightf);
     PSTATE.factor.push(leftf);
     
     // printf("%f %f", leftf, rightf);
    }
    //printf(";\n");
    // Collect node and branch info, todo?
})
 
 
BBLOCK(simClaDS2, {

    
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    // Test settings
    floating_t k = 1;
    floating_t theta = 0.2;
    rate_t lambda0(k, theta);
    floating_t factor = 1.0;
    floating_t rho      = 1.0;
    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, 1.0 / 0.2);
    floating_t sigma = sqrt(sigmaSquared);
    
    floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));
    floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    
    PSTATE.alpha = alpha;
    PSTATE.sigma = sigma;
    PSTATE.epsilon = epsilon;
    PSTATE.rho = rho;
    PSTATE.lambda0 = lambda0;
    
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);
    floating_t leftf = factor*exp(f1);
    floating_t rightf = factor*exp(f2);
    
    // bblockArgs_t args(lambda1, lambda2);
    // one for each child, and one for Survivorship Bias after tree simulations

    //printf("%f\t%f\t%f\n", factor, leftf, rightf);
    //printf("push 3 %f %f %f", lambda_factor, k_lambda, theta_lambda);
    PSTATE.factor.push(leftf);
    PSTATE.factor.push(rightf);
    PSTATE.factor.push(factor);
    
    PC++;
    BBLOCK_CALL(simTree);

    // Condition on detection (clads2GoesUndetected simulations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
})

// // Should be equivalent to forward sampling
// BBLOCK(conditionOnDetection, {
    
//     tree_t* treeP = DATA_POINTER(tree);
//     floating_t treeAge = treeP->ages[ROOT_IDX];

//     floating_t lambdaFactor = PSTATE.factor.pop();
//     floating_t k = PSTATE.k;
//     floating_t theta = PSTATE.theta;
//     rate_t lambdaRate(k, theta, lambdaFactor);
    
//     //floating_t lambda0 = PSTATE.lambda_0 * lambda_factor;
    
//     floating_t alpha = PSTATE.alpha;
//     floating_t sigma = PSTATE.sigma;
//     floating_t epsilon = PSTATE.epsilon;
//     floating_t rho = PSTATE.rho;
    
//     int numSamples = 100;
//     int numDetected = 0;
//     for(int i = 0; i < numSamples; i++) {
//       undetectedDelayed_t ret = BBLOCK_CALL(clads2GoesUndetectedDelayed, treeAge, lambdaRate, alpha, sigma, epsilon, rho);
//       bool undetected = ret.res;
//       if(! undetected)
// 	numDetected++;
//     }
//     WEIGHT(-2.0 * log(numDetected / static_cast<floating_t>(numSamples)));
    
//     PC++;
    
//   })

// Write particle data to file
CALLBACK(saveResults, {
    std::string fileName = "parameterDataCladsDelayed.csv";

    std::ofstream resultFile (fileName);
    if(resultFile.is_open()) {

        for(int i = 0; i < N; i++)
            resultFile << 
               "na" << " " << PSTATES[i].sigma << " " << PSTATES[i].alpha << " " << PSTATES[i].epsilon << " " << 
                exp(WEIGHTS[i]) << "\n";

        resultFile.close();
    } else {
        printf("Could not open file %s\n", fileName.c_str());
    }

})

MAIN({
    
    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    //ADD_BBLOCK(conditionOnDetection);
    // TODO add a block for sampling of the final lambda sampleFinalLambda (use the CRBD delayed)
    //    SMC(saveResults);
    SMC(NULL);
})
 
 
