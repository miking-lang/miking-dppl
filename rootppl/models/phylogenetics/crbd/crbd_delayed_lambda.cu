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
#include <string>
#include <fstream>
#include <math.h>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

typedef short treeIdx_t;
struct progState_t {
    floating_t mu;
    floating_t kLambda;
    floating_t thetaLambda;
    floating_t lambda;
    treeIdx_t treeIdx;
};

struct ret_delayed_t {
    floating_t res;
    floating_t k;
    floating_t theta;

    DEV ret_delayed_t(){};
    DEV ret_delayed_t(floating_t res_, floating_t k_, floating_t theta_) {
        res = res_;
        k = k_;
        theta = theta_;
    }
};


typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
// typedef moth_div_tree_t tree_t;

// const int MAX_DIV = 5;
// const int MAX_LAM = 5;

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)

BBLOCK_HELPER_DECLARE(crbdGoesUndetectedDelayed, bool, floating_t, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1)

BBLOCK_DATA_CONST(rho, floating_t, 1.0)

BBLOCK_HELPER(delayedSample, {
    floating_t t = SAMPLE(lomax, 1/theta, k);
    ret_delayed_t ret(t, k+1, theta / (1 + t * theta));
    return ret;

}, ret_delayed_t, floating_t k, floating_t theta)

BBLOCK_HELPER(delayedObserve, {
    ret_delayed_t ret(lomaxScore(x, 1/theta, k), k+1, theta / (1 + x * theta));
    return ret;

}, ret_delayed_t, floating_t x, floating_t k, floating_t theta)

/*
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
*/
 
BBLOCK_HELPER(crbdGoesUndetectedDelayed, {

    ret_delayed_t ret0 = BBLOCK_CALL(delayedSample, kLambda, thetaLambda);
    floating_t tLambda = ret0.res;

    // floating_t t = SAMPLE(exponential, lambda + mu);
    floating_t tMu = SAMPLE(exponential, mu);

    floating_t t = MIN(tLambda, tMu);

    bool speciation = tLambda < tMu;
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0) {
        if (SAMPLE(bernoulli, 1 - rho))
            return ret_delayed_t(true, ret0.k, ret0.theta);
        else
            return ret_delayed_t(false, ret0.k, ret0.theta);
    }
    
    bool extinction = !speciation;
    if (extinction)
        return ret_delayed_t(true, ret0.k, ret0.theta);

    ret_delayed_t ret1 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret0.k, ret0.theta, mu, rho);

    bool leftDetection = !ret1.res;

    if (leftDetection)
        return ret1;

    ret_delayed_t ret2 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret1.k, ret1.theta, mu, rho);

    return ret2;

}, ret_delayed_t, floating_t startTime, floating_t kLambda, floating_t thetaLambda, floating_t mu, floating_t rho)


BBLOCK_HELPER(simBranch, {

    ret_delayed_t ret0 = BBLOCK_CALL(delayedSample, kLambda, thetaLambda);
    floating_t t = ret0.res;

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return ret_delayed_t(0.0, ret0.k, ret0.theta);

    ret_delayed_t ret1 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret0.k, ret0.theta, mu, rho);
    
    bool sideDetection = !ret1.res;
    if(sideDetection)
        return ret_delayed_t(-INFINITY, ret1.k, ret1.theta);

    ret_delayed_t ret2 = BBLOCK_CALL(simBranch, currentTime, stopTime, ret1.k, ret1.theta, mu, rho);
    floating_t newProb = ret2.res + log(2.0);
    
    return ret_delayed_t(newProb, ret2.k, ret2.theta);

}, ret_delayed_t, floating_t startTime, floating_t stopTime, floating_t kLambda, floating_t thetaLambda, floating_t mu, floating_t rho)


BBLOCK(simTree, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    // floating_t lambdaLocal = PSTATE.lambda;
    floating_t muLocal = PSTATE.mu;
    // floating_t kLambdaLocal = PSTATE.kLambda;
    // floating_t thetaLambdaLocal = PSTATE.thetaLambda;
    // floating_t rhoLocal = *DATA_POINTER(rho);
    floating_t rhoLocal = DATA_CONST(rho);

    int indexParent = treeP->idxParent[treeIdx];
    
    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];

    floating_t lnProb1 = - muLocal * (parentAge - age);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    // floating_t lnProb2 = interiorNode ? log(lambdaLocal) : log(rhoLocal);

    ret_delayed_t ret0;
    if (interiorNode)
        ret0 = BBLOCK_CALL(delayedObserve, 0, PSTATE.kLambda, PSTATE.thetaLambda);
    else
        ret0 = ret_delayed_t(log(rho), PSTATE.kLambda, PSTATE.thetaLambda);

    floating_t lnProb2 = ret0.res;

    ret_delayed_t ret1 = BBLOCK_CALL(simBranch, parentAge, age, ret0.k, ret0.theta, muLocal, rhoLocal);
    floating_t lnProb3 = ret1.res;

    WEIGHT(lnProb1 + lnProb2 + lnProb3);

    PSTATE.kLambda = ret1.k;
    PSTATE.thetaLambda = ret1.theta;

    // Instead of recurring, use pre-processed traversal order
    int nextIdx = treeP->idxNext[treeIdx];
    PSTATE.treeIdx = nextIdx;

    if(nextIdx == -1)
        PC++;
})

 
BBLOCK(simCRBD, {

    // PSTATE.lambda = SAMPLE(gamma, 1.0, 1.0);
    // PSTATE.lambda = 0.2;
    floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    // floating_t epsilon = 0.5;
    // PSTATE.mu = epsilon * PSTATE.lambda;
    PSTATE.mu = 0.1;
    // floating_t k_lambda = 1;
    // floating_t theta_lambda = 0.2;
    PSTATE.kLambda = 1;
    PSTATE.thetaLambda = 0.2;

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    PC++;
    BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
})

/*
BBLOCK(survivorshipBias, {
    floating_t age = DATA_POINTER(tree)->ages[ROOT_IDX];
    int MAX_M = 10000;
    int M = BBLOCK_CALL(M_crbdGoesUndetected, age, MAX_M, PSTATE.lambda, PSTATE.mu, DATA_CONST(rho));
    WEIGHT(LOG(M));
    PC++;
})
*/

BBLOCK(sampleFinalLambda, {
    PSTATE.lambda = SAMPLE(gamma, PSTATE.kLambda, PSTATE.thetaLambda);
    PC++;
})

// Write particle data to file. 
CALLBACK(saveResults, {
    
    std::string fileName = "parameterDataDelayed";
    std::ofstream resFile (fileName);
    if(resFile.is_open()) {

        for(int i = 0; i < N; i++)
            resFile << PSTATES[i].lambda << " " << PSTATES[i].mu << " " << exp(WEIGHTS[i]) << "\n";

        resFile.close();
    } else {
        printf("Could not open file %s\n", fileName.c_str());
    }
    
})


MAIN(
    
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    // ADD_BBLOCK(survivorshipBias)
    ADD_BBLOCK(sampleFinalLambda)
    
    SMC(saveResults)
)
 
 