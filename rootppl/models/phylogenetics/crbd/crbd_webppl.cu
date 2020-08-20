/*
 * File crbd_webppl.cu defines the constant rate birth death model 
 * as defined in WebPPL in the script linked to below. 
 * 
 * https://github.com/phyppl/probabilistic-programming/blob/master/webppl/phywppl/models/crbd.wppl
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

typedef short treeIdx_t;
struct progState_t {
    floating_t lambda;
    floating_t mu;
    treeIdx_t treeIdx;
};
// typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
typedef moth_div_tree_t tree_t;

const int MAX_DIV = 5;
const int MAX_LAM = 5;

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)

BBLOCK_HELPER_DECLARE(crbdGoesUndetected, bool, floating_t, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1)

BBLOCK_DATA_CONST(rho, floating_t, 1.0)


BBLOCK_HELPER(M_crbdGoesUndetected, {

    if(maxM == 0) {
        printf("Aborting crbdGoesUndetected simulation, too deep!\n");
        return 1; // What do return instead of NaN?
    }

    if(! BBLOCK_CALL(crbdGoesUndetected, startTime, lambda, mu, rho) && ! BBLOCK_CALL(crbdGoesUndetected, startTime, lambda, mu, rho))
        return 1;
    else
        return 1 + BBLOCK_CALL(M_crbdGoesUndetected, startTime, maxM - 1, lambda, mu, rho);

}, int, floating_t startTime, int maxM, floating_t lambda, floating_t mu, floating_t rho)

BBLOCK_HELPER(crbdGoesUndetected, {

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
        return false;
    
    if (lambda == 0.0) 
        return ! SAMPLE(bernoulli, rho);

    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambda + mu);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho);
    
    bool speciation = SAMPLE(bernoulli, lambda / (lambda + mu));
    if (! speciation)
        return true;
    
    return BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu, rho) && BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu, rho);

}, bool, floating_t startTime, floating_t lambda, floating_t mu, floating_t rho)


BBLOCK_HELPER(simBranch, {

    // extreme values patch 2/2
	if (lambda > MAX_LAM) {
	    return -INFINITY;
	}
	
	if (lambda == 0.0) {
        return 0.0;
	}
	// extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideDetection = BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu, rho);
    if(! sideDetection)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch, currentTime, stopTime, lambda, mu, rho) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime, floating_t lambda, floating_t mu, floating_t rho)


BBLOCK(simTree, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    floating_t lambdaLocal = PSTATE.lambda;
    floating_t muLocal = PSTATE.mu;
    // floating_t rhoLocal = *DATA_POINTER(rho);
    floating_t rhoLocal = DATA_CONST(rho);

    int indexParent = treeP->idxParent[treeIdx];
    
    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];

    floating_t lnProb1 = - muLocal * (parentAge - age);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnProb2 = interiorNode ? log(lambdaLocal) : log(rhoLocal);

    floating_t lnProb3 = BBLOCK_CALL(simBranch, parentAge, age, lambdaLocal, muLocal, rhoLocal);

    WEIGHT(lnProb1 + lnProb2 + lnProb3);

    // Instead of recurring, use pre-processed traversal order
    int nextIdx = treeP->idxNext[treeIdx];
    PSTATE.treeIdx = nextIdx;

    if(nextIdx == -1)
        PC++;
})


BBLOCK(simCRBD, {

    // PSTATE.lambda = SAMPLE(gamma, 1.0, 1.0);
    PSTATE.lambda = 0.2;
    // floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    floating_t epsilon = 0.5;
    // PSTATE.mu = epsilon * PSTATE.lambda;
    PSTATE.mu = 0.1;

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    PC++;
})

BBLOCK(survivorshipBias, {
    floating_t age = DATA_POINTER(tree)->ages[ROOT_IDX];
    int MAX_M = 10000;
    int M = BBLOCK_CALL(M_crbdGoesUndetected, age, MAX_M, PSTATE.lambda, PSTATE.mu, DATA_CONST(rho));
    WEIGHT(LOG(M));
    PC++;
})


MAIN(
    
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    ADD_BBLOCK(survivorshipBias)

    SMC(NULL)
)

