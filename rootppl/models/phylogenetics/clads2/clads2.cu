/*
 * File clads2.cu defines the ClaDS2 model
 * as defined in WebPPL in the script linked to below. 
 * 
 * https://github.com/phyppl/probabilistic-programming/blob/master/webppl/phywppl/models/clads2.wppl
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */


#include <iostream>
#include <cstring>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

// typedef bisse32_tree_t tree_t;
// typedef bisse32precision_tree_t tree_t;
typedef primate_tree_t tree_t;
#include "clads2.cuh"
 

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
 
BBLOCK_HELPER_DECLARE(clads2GoesUndetected, bool, floating_t, floating_t, floating_t, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1);

const int MAX_DIV = 5;
const int MAX_LAM = 5;
const floating_t MIN_LAM = 1e-5;
// const bool PANDA = false;

// Since PANDA is false
#define LAMBDA_CHOOSER(lambda, lambdaEnd, alpha, sigma) exp(SAMPLE(normal, log(alpha*lambdaEnd), sigma))
 
BBLOCK_HELPER(clads2GoesUndetected, {
 

    floating_t mu = epsilon * lambda;

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
		return false;

    if (lambda < MIN_LAM)
        return ! SAMPLE(bernoulli, rho);
    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambda + mu);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho); 


    bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    if(exctinction)
        return true;

    floating_t lambda1 = exp(SAMPLE(normal, log(alpha * lambda), sigma));
    floating_t lambda2 = exp(SAMPLE(normal, log(alpha * lambda), sigma));

    return BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda1, alpha, sigma, epsilon, rho)
        && BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda2, alpha, sigma, epsilon, rho);
 
}, bool, floating_t startTime, floating_t lambda, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 
BBLOCK_HELPER(simBranch, {

    floating_t t1 = startTime - stopTime;
    floating_t mu = epsilon * lambda;

    // extreme values patch 2/2
    if(lambda > MAX_LAM) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }

    if(lambda < MIN_LAM) {
        simBranchRet_t ret(lambda, lambda*t1, -mu*t1);
        return ret;
    }
    // extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        simBranchRet_t ret(lambda, lambda*t1, -mu*t1);
        return ret;
    }

    floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);
    floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);

    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda2, alpha, sigma, epsilon, rho);

    if(! sideUndetected) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }

    simBranchRet_t ret = BBLOCK_CALL(simBranch, currentTime, stopTime, lambda1, alpha, sigma, epsilon, rho);
    simBranchRet_t rt(ret.r0, ret.r1 + lambda*t, ret.r2 + log(2.0) - mu*t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 
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

    floating_t lambda = PSTATE.stack.pop();

    simBranchRet_t ret = BBLOCK_CALL(simBranch, treeP->ages[indexParent], treeP->ages[treeIdx], lambda, PSTATE.alpha, PSTATE.sigma, PSTATE.epsilon, PSTATE.rho);

    floating_t lambdaEnd = ret.r0;

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnTerminalProb = interiorNode ? log(lambdaEnd) : log(PSTATE.rho);
    WEIGHT(ret.r2 + lnTerminalProb);

    // Collect node and branch info, todo?

    if(interiorNode) {
        floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);
        floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);

        PSTATE.stack.push(lambda1);
        PSTATE.stack.push(lambda2);
    }

})
 
 
BBLOCK(simClaDS2, {
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
 
    // Test settings
    floating_t lambda_0 = 0.2;
    floating_t alpha    = 1.0;
    floating_t sigma    = 0.0000001;
    floating_t epsilon  = 0.5;   // Corresponds to mu = epsilon*lambda = 0.1
    floating_t rho      = 1.0;

    PSTATE.alpha = alpha;
    PSTATE.sigma = sigma;
    PSTATE.epsilon = epsilon;
    PSTATE.rho = rho;
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    floating_t lambda1 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);
    floating_t lambda2 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);

    // bblockArgs_t args(lambda1, lambda2);
    // one for each child, and one for Survivorship Bias after tree simulations
    PSTATE.stack.push(lambda1);
    PSTATE.stack.push(lambda2);
    PSTATE.stack.push(lambda_0);

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
 
 
CALLBACK(callback, {
    // printf("Done!\n");
})

MAIN({

    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    ADD_BBLOCK(conditionOnDetection);

    SMC(callback);
})
 
 