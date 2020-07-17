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

/**
    This file traverses the tree with a precomputed DFS path that corresponds to the recursive calls. 
*/

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/crbd/crbd_webppl.cu -o smc.exe -std=c++14 -O3
g++ -x c++ -I . models/phylogenetics/crbd/crbd_webppl.cu -o smc.exe -std=c++14 -O3
*/

// Bisse-32 tree
// This model on local WebPPL with 10000 particles took ~42 sec
// This program on CPU took ~0.11 sec
// This program on GPU took ~0.265 sec

// Primate tree tree
// This model on local WebPPL with 10000 particles took ~323 sec
// This program on CPU took ~1.04 sec
// This program on GPU took ~0.606 sec

typedef short treeIdx_t;
struct progState_t {
    floating_t lambda;
    floating_t mu;
    treeIdx_t treeIdx;
};
// typedef bisse32_tree_t tree_t;
// typedef bisse32precision_tree_t tree_t;
typedef primate_tree_t tree_t;

const int MAX_DIV = 5;
const int MAX_LAM = 5;

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)

BBLOCK_HELPER_DECLARE(crbdGoesUndetected, bool, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1)

// BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
// BBLOCK_DATA(mu, floating_t, 1)
BBLOCK_DATA(rho, floating_t, 1)


void initCBD() {
    // lambda = SAMPLE(gamma, 1.0, 1.0);
    // *mu = SAMPLE();
    // *lambda = 0.2; // birth rate
    // *mu = 0.1; // death rate
    *rho = 1.0;

    // COPY_DATA_GPU(tree, tree_t, 1)
    // COPY_DATA_GPU(lambda, floating_t, 1)
    // COPY_DATA_GPU(mu, floating_t, 1)
    COPY_DATA_GPU(rho, floating_t, 1)

}

BBLOCK_HELPER(M_crbdGoesUndetected, {

    if(max_M == 0) {
        printf("Aborting crbdGoesUndetected simulation, too deep!\n");
        return 1; // What do return instead of NaN?
    }

    // if(max_M < 9400)
        // printf("Max_M: %d\n", max_M);

    if(! BBLOCK_CALL(crbdGoesUndetected, startTime, lambda, mu) && ! BBLOCK_CALL(crbdGoesUndetected, startTime, lambda, mu))
        return 1;
    else
        return 1 + BBLOCK_CALL(M_crbdGoesUndetected, startTime, max_M - 1, lambda, mu);

}, int, floating_t startTime, int max_M, floating_t lambda, floating_t mu)

BBLOCK_HELPER(crbdGoesUndetected, {

    // floating_t lambdaLocal = PSTATE.lambda;
    // floating_t muLocal = PSTATE.mu;

    floating_t rhoLocal = *DATA_POINTER(rho);

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
        return false;
    
    if (lambda == 0.0) {
        return ! SAMPLE(bernoulli, rhoLocal);
        /*
        if (flip(rhoLocal))
            return false
        else
            return true
        */
    }
    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambda + mu);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rhoLocal);
    
    bool speciation = SAMPLE(bernoulli, lambda / (lambda + mu));
    if (! speciation)
        return true;
    
    return BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu) && BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu);

}, bool, floating_t startTime, floating_t lambda, floating_t mu)


BBLOCK_HELPER(simBranch, {

    // floating_t lambdaLocal = *DATA_POINTER(lambda);
    // floating_t lambdaLocal = PSTATE.lambda;

    // extreme values patch 2/2
	if (lambda > MAX_LAM) {
	    //console.log( "lambda: ", lambda )
	    return -INFINITY;
	}
	
	if (lambda == 0.0) {
	    // var t1 = startTime - stopTime
        return 0.0;
	}
	// extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideDetection = BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu);
    if(! sideDetection)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch, currentTime, stopTime, lambda, mu) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime, floating_t lambda, floating_t mu)


BBLOCK(simTree, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    floating_t lambdaLocal = PSTATE.lambda;
    floating_t muLocal = PSTATE.mu;

    int indexParent = treeP->idxParent[treeIdx];
    
    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];

    floating_t lnProb1 = - muLocal * (parentAge - age);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnProb2 = interiorNode ? log(lambdaLocal) : log(*DATA_POINTER(rho));

    floating_t lnProb3 = BBLOCK_CALL(simBranch, parentAge, age, lambdaLocal, muLocal);

    WEIGHT(lnProb1 + lnProb2 + lnProb3);

    // Instead of recurring, use pre-processed traversal order
    int nextIdx = treeP->idxNext[treeIdx];
    PSTATE.treeIdx = nextIdx;

    if(nextIdx == -1) {
        PC++;
        return;
    }

})


BBLOCK(simCRBD, {

    PSTATE.lambda = SAMPLE(gamma, 1.0, 1.0);
    floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    PSTATE.mu = epsilon * PSTATE.lambda;

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    PC++;
})

BBLOCK(survivorshipBias, {
    // Survivorship Bias, is done after simCRBD
    floating_t age = DATA_POINTER(tree)->ages[ROOT_IDX];
    // int MAX_M = 10000;
    int MAX_M = 10000;
    int M = BBLOCK_CALL(M_crbdGoesUndetected, age, MAX_M, PSTATE.lambda, PSTATE.mu);
    WEIGHT(log(static_cast<floating_t>(M)));
    PC++;
})


MAIN(
    initCBD();
    
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    ADD_BBLOCK(survivorshipBias)

    SMC(NULL)
)

