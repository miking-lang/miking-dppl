#include <iostream>
#include <cstring>
#include "inference/smc/smc_impl.cuh"
#include "../tree-utils/tree_utils.cuh"

/**
    This file traverses the tree with a precomputed DFS path that corresponds to the recursive calls. 
*/

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/crbd/crbd_webppl.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/phylogenetics/crbd/crbd_webppl.cu -o smc.exe -std=c++11 -O3
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
    treeIdx_t treeIdx;
};
typedef primate_tree_t tree_t;

BBLOCK_HELPER_DECLARE(crbdGoesUndetected, progState_t, bool);

const int MAX_DIV = 5;
const int MAX_LAM = 5;

#define NUM_BBLOCKS 2
INIT_GLOBAL(progState_t, NUM_BBLOCKS)

BBLOCK_DATA(tree, tree_t, 1)

BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)
BBLOCK_DATA(rho, floating_t, 1)


void initCBD() {
    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )
    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate
    *rho = 1.0;

    COPY_DATA_GPU(tree, tree_t, 1)
    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)
    COPY_DATA_GPU(rho, floating_t, 1)

}

BBLOCK_HELPER(M_crbdGoesUndetected, {

    if(max_M == 0) {
        printf("Aborting crbdGoesUndetected simulation, too deep!\n");
        return 0; // What do return instead of NaN?
    }

    if(! BBLOCK_CALL(crbdGoesUndetected<T>, startTime) && ! BBLOCK_CALL(crbdGoesUndetected<T>, startTime))
        return 1;
    else
        return 1 + BBLOCK_CALL(M_crbdGoesUndetected, startTime, max_M - 1);

}, int, floating_t startTime, int max_M)

BBLOCK_HELPER(crbdGoesUndetected, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    floating_t muLocal = *DATA_POINTER(mu);
    floating_t rhoLocal = *DATA_POINTER(rho);

    // extreme values patch 1/2
    if (lambdaLocal - muLocal > MAX_DIV)
        return false;
    
    if (lambdaLocal == 0.0) {
        return ! SAMPLE(bernoulli, rhoLocal);
        /*
        if (flip(rhoLocal))
            return false
        else
            return true
        */
    }
    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambdaLocal + muLocal);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rhoLocal);
    
    bool speciation = SAMPLE(bernoulli, lambdaLocal / (lambdaLocal + muLocal));
    if (! speciation)
        return true;
    
    return BBLOCK_CALL(crbdGoesUndetected<T>, currentTime) && BBLOCK_CALL(crbdGoesUndetected<T>, currentTime);

}, bool, floating_t startTime)


BBLOCK_HELPER(simBranch, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);

    // extreme values patch 2/2
	if (lambdaLocal > MAX_LAM) {
	    //console.log( "lambda: ", lambda )
	    return -INFINITY;
	}
	
	if (lambdaLocal == 0.0) {
	    // var t1 = startTime - stopTime
        return 0.0;
	}
	// extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideDetection = BBLOCK_CALL(crbdGoesUndetected<T>, currentTime);
    if(! sideDetection)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch<T>, currentTime, stopTime) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime)


BBLOCK(simTree, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    int indexParent = treeP->idxParent[treeIdx];
    
    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];

    floating_t lnProb1 = - (*DATA_POINTER(mu)) * (parentAge - age);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnProb2 = interiorNode ? log(*DATA_POINTER(lambda)) : log(*DATA_POINTER(rho));

    floating_t lnProb3 = BBLOCK_CALL(simBranch<progState_t>, parentAge, age);

    WEIGHT(lnProb1 + lnProb2 + lnProb3);

    // Instead of recurring, use pre-processed traversal order
    int nextIdx = treeP->idxNext[treeIdx];
    PSTATE.treeIdx = nextIdx;

    if(nextIdx == -1) {
        PC++;
        return;
    }

})

/*
BBLOCK(simCRBD1, progState_t, {
    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    PC++;
})

BBLOCK(simCRBD2, progState_t, {
    tree_t* treeP = DATA_POINTER(tree);

    const int MAX_M = 10000;
    int M = BBLOCK_CALL(M_crbdGoesUndetected, treeP->ages[PSTATE.treeIdx], MAX_M);
    WEIGHT(log(static_cast<floating_t>(M)));

    PC++;
})
*/

BBLOCK(simCRBD, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);
    // printf("corrFactor: %f\n", corrFactor);

    // Resample here perhaps?

    const int MAX_M = 10000;
    int M = BBLOCK_CALL(M_crbdGoesUndetected, treeP->ages[PSTATE.treeIdx], MAX_M);
    WEIGHT(log(static_cast<floating_t>(M)));
    // printf("log(M): %f\n", log(static_cast<floating_t>(M)));

    PC++;
    // BBLOCK_CALL(simTree);
})


MAIN(
    initCBD();
    
    INIT_BBLOCK(simCRBD, progState_t)
    // INIT_BBLOCK(simCRBD1, progState_t)
    // INIT_BBLOCK(simCRBD2, progState_t)
    INIT_BBLOCK(simTree, progState_t)

    SMC(progState_t, NULL)
)

