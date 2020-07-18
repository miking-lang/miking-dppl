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
#include "clads2.cuh"
 

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
 
BBLOCK_HELPER_DECLARE(clads2GoesUndetected, bool, floating_t, floating_t, floating_t, floating_t, floating_t, floating_t);

typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
BBLOCK_DATA(tree, tree_t, 1);

const int MAX_DIV = 5;
const int MAX_LAMBDA = 5;
const floating_t MIN_LAMBDA = 1e-5;
const bool PANDA = false;
 
BBLOCK_HELPER(clads2GoesUndetected, {
 

    floating_t mu = epsilon * lambda;

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
		return false;

    if (lambda < MIN_LAMBDA)
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



}, simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 
BBLOCK(simTree, {

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
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    bblockArgs_t args(lf_0, mu_0, eta, rho);
    // one for each child, and one for Survivorship Bias after tree simulations
    PSTATE.stack.push(args);
    PSTATE.stack.push(args);
    PSTATE.stack.push(args);

    PC++;
    BBLOCK_CALL(simTree);

    // Condition on detection (clads2GoesUndetected simlations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
 })
 
 
CALLBACK(callback, {
    // printf("Done yay!\n");
})

MAIN({
    initGen();

    ADD_BBLOCK(simClaDS2)
    ADD_BBLOCK(simTree)

    SMC(callback)
})
 
 