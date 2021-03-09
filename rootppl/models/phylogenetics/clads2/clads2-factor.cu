/*
 * File clads2-factor.cu defines the ClaDS2 model
 * - lambda-factorization but not delayed sampling.
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
// typedef moth_div_tree_t tree_t;
#include "clads2.cuh"
#include "helper-factor.cuh"
 

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
 
BBLOCK_HELPER_DECLARE(clads2GoesUndetected, bool, floating_t, floating_t, floating_t, floating_t, floating_t, floating_t);

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

    floating_t factor = PSTATE.stack.pop();
    floating_t lambda_0 = PSTATE.lambda_0;

    simBranchRet_t ret = BBLOCK_CALL(simBranchFactor, treeP->ages[indexParent], treeP->ages[treeIdx], lambda_0, factor, PSTATE.alpha, PSTATE.sigma, PSTATE.epsilon, PSTATE.rho);

    floating_t factorEnd = ret.r0;

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnTerminalProb = interiorNode ? log(lambda_0*factorEnd) : log(PSTATE.rho);
    printf("%f %f\n", ret.r2, lnTerminalProb);
    WEIGHT(ret.r2 + lnTerminalProb);

    // Collect node and branch info, todo?

    if(interiorNode) {
        //floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);
        //floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);
        floating_t f1 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);
        floating_t f2 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);
        floating_t leftf = factorEnd*exp(f1);
        floating_t rightf = factorEnd*exp(f2);


        PSTATE.stack.push(rightf);
        PSTATE.stack.push(leftf);
    }

})
 
 
BBLOCK(simClaDS2, {
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
 
    // Test settings
    /*
    floating_t lambda_0 = 0.2;
    floating_t alpha    = 1.0;
    floating_t sigma    = 0.0000001;
    floating_t epsilon  = 0.5;   // Corresponds to mu = epsilon*lambda = 0.1
    */
    floating_t rho      = 1.0;

    floating_t k = 1;
    floating_t theta = 0.2;

    floating_t lambda_0 = SAMPLE(gamma, k, theta);

    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, 1.0 / 0.2);

    floating_t sigma = sqrt(sigmaSquared);

    floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));

    floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    floating_t factor = 1.0;
    
    PSTATE.lambda_0 = lambda_0;
    PSTATE.alpha = alpha;
    PSTATE.sigma = sigma;
    PSTATE.epsilon = epsilon;
    PSTATE.rho = rho;
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    //floating_t lambda1 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);
    //floating_t lambda2 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);
    floating_t leftf = factor*exp(f1);
    floating_t rightf = factor*exp(f2);

    // bblockArgs_t args(lambda1, lambda2);
    // one for each child, and one for Survivorship Bias after tree simulations
    PSTATE.stack.push(rightf);
    PSTATE.stack.push(leftf);
    PSTATE.stack.push(factor);

    PC++;
    BBLOCK_CALL(simTree);

    // Condition on detection (clads2GoesUndetected simulations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
})

// Should be equivalent to forward sampling
BBLOCK(conditionOnDetection, {

    tree_t* treeP = DATA_POINTER(tree);
    floating_t treeAge = treeP->ages[ROOT_IDX];

    floating_t lambda0 = PSTATE.stack.pop();

    floating_t alpha = PSTATE.alpha;
    floating_t sigma = PSTATE.sigma;
    floating_t epsilon = PSTATE.epsilon;
    floating_t rho = PSTATE.rho;

    int numSamples = 100;
    int numDetected = 0;
    for(int i = 0; i < numSamples; i++) {
        bool undetected = BBLOCK_CALL(clads2GoesUndetected, treeAge, lambda0, alpha, sigma, epsilon, rho);
        if(! undetected)
            numDetected++;
    }
    WEIGHT(-2.0 * log(numDetected / static_cast<floating_t>(numSamples)));

    PC++;

})
 
// Write particle data to file
CALLBACK(saveResults, {
    std::string fileName = "parameterData";

    std::ofstream resultFile (fileName);
    if(resultFile.is_open()) {

        for(int i = 0; i < N; i++)
            resultFile << 
                PSTATES[i].lambda_0 << " " << PSTATES[i].sigma << " " << PSTATES[i].alpha << " " << PSTATES[i].epsilon << " " << 
                exp(WEIGHTS[i]) << "\n";

        resultFile.close();
    } else {
        printf("Could not open file %s\n", fileName.c_str());
    }

})

MAIN({

    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    ADD_BBLOCK(conditionOnDetection);

    SMC(saveResults);
})
 
 
