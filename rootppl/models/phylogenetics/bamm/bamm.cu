/*
 * File bamm.cu defines the Bayesian Analysis of Macroevolutionary Mixtures (BAMM) model
 * as defined in WebPPL in the script linked to below. 
 * 
 * https://github.com/phyppl/probabilistic-programming/blob/master/webppl/phywppl/models/bamm.wppl
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */


#include <iostream>
#include <cstring>

#include "inference/smc/smc_impl.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "bamm.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/bamm/bamm.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/phylogenetics/bamm/bamm.cu -o smc.exe -std=c++11 -O3
*/

#define NUM_BBLOCKS 3
INIT_GLOBAL(progState_t, NUM_BBLOCKS)

#define MIN(a, b) a <= b ? a : b
#define MAX(a, b) a >= b ? a : b

#define DIST_LAMBDA() exp(SAMPLE(uniform, log(1e-2), log(1e1)))
#define DIST_Z() SAMPLE(normal, 0, 0.001)
#define DIST_MU(lam0) SAMPLE(uniform, 0.0, 0.1) * lam0

typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
BBLOCK_DATA(tree, tree_t, 1);


DEV inline floating_t lambdaFun(lambdaFun_t lf, floating_t t) {
    return lf.lambda * exp(lf.z * (lf.t1 - t));
}

DEV inline floating_t robustExponentialSampler(RAND_STATE_DECLARE floating_t a) {
    if(a <= 0) return INFINITY;
    if(a == INFINITY) return 0;
    return SAMPLE(exponential, a);
}

BBLOCK_HELPER(M_bammGoesUndetected, {

    if(max_M == 0) { printf("M_bammGoesUndetected: MAX DEPTH REACHED!\n"); return 0;}

    if(! BBLOCK_CALL(bammGoesUndetected, startTime, lf, mu, eta, rho) 
    && ! BBLOCK_CALL(bammGoesUndetected, startTime, lf, mu, eta, rho)) {
        return 1;
    } 
    else return 1 + BBLOCK_CALL(M_bammGoesUndetected, startTime, lf, mu, eta, rho, max_M-1);

}, int, floating_t startTime, lambdaFun_t lf, floating_t mu, floating_t eta, floating_t rho, int max_M)

BBLOCK_HELPER(bammLambdaWait, {

    floating_t startLambda = lambdaFun(lf, startTime);
    floating_t stopLambda = lambdaFun(lf, stopTime);

    floating_t topLambda = MAX(startLambda, stopLambda);

    floating_t t = startTime - robustExponentialSampler(RAND_STATE topLambda);

    if(t < stopTime) return INFINITY;
    if(SAMPLE(bernoulli, lambdaFun(lf, t) / topLambda)) return startTime - t;

    return startTime - t + BBLOCK_CALL(bammLambdaWait, lf, t, stopTime);

}, floating_t, lambdaFun_t lf, floating_t startTime, floating_t stopTime)

BBLOCK_HELPER(bammGoesUndetected, {

    floating_t t1 = robustExponentialSampler(RAND_STATE mu + eta);
    floating_t tLambda = BBLOCK_CALL(bammLambdaWait, lf, startTime, 0);

    floating_t t = MIN(t1, tLambda);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho);
        
    if(t1 < tLambda) {
        bool extinction = SAMPLE(bernoulli, mu / (mu + eta));
        if(extinction) return true;

        // No exctinction, so rateshift
        floating_t lambda2 = DIST_LAMBDA();
        floating_t z2 = DIST_Z();
        floating_t mu2 = DIST_MU(lambda2);
        lambdaFun_t lf2(lambda2, z2, currentTime);

        return BBLOCK_CALL(bammGoesUndetected, currentTime, lf2, mu2, eta, rho);
    }

    return BBLOCK_CALL(bammGoesUndetected, currentTime, lf, mu, eta, rho) && BBLOCK_CALL(bammGoesUndetected, currentTime, lf, mu, eta, rho);

}, bool, floating_t startTime, lambdaFun_t lf, floating_t mu, floating_t eta, floating_t rho)


BBLOCK_HELPER(simBranch, {

    floating_t tLambda = BBLOCK_CALL(bammLambdaWait, lf, startTime, stopTime);
    floating_t tEta = robustExponentialSampler(RAND_STATE eta);
    floating_t t = MIN(tLambda, tEta);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        floating_t t1 = startTime - stopTime;
        floating_t meanLambda = (lambdaFun(lf, startTime), lambdaFun(lf, stopTime)) / 2.0;
        simBranchRet_t rt(lf, z, mu, meanLambda * t1, z * t1, mu * t1, 0, - mu * t1);
        return rt;
    }

    // Check whether this is a rate shift, and handle this case
    if(tEta < tLambda) {
        floating_t lambda0_2 = DIST_LAMBDA();
        floating_t z2 = DIST_Z();
        floating_t mu2 = DIST_MU(lambda0_2);
        lambdaFun_t lf2(lambda0_2, z2, currentTime);

        simBranchRet_t ret = BBLOCK_CALL(simBranch, currentTime, stopTime, lf2, z2, mu2, eta, rho);

        floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;
        simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda * t, ret.r4 + z * t, ret.r5 + mu * t, ret.r6 + 1, ret.r7 - mu * t);
        return rt;
    }

    bool sideDetection = BBLOCK_CALL(bammGoesUndetected, currentTime, lf, mu, eta, rho);
    if(! sideDetection) {
        simBranchRet_t rt(lf, 0.0, 0.0, 0.0, 0.0, 0.0, 0, -INFINITY);
        return rt;
    }

    simBranchRet_t ret = BBLOCK_CALL(simBranch, currentTime, stopTime, lf, z, mu, eta, rho);

    floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;
    simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda * t, ret.r4 + z * t, ret.r5 + mu * t, ret.r6, ret.r7 + log(2.0) - mu * t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, lambdaFun_t lf, floating_t z, floating_t mu, floating_t eta, floating_t rho)


// TODO: Should return tree info as string?
BBLOCK(simTree, {

    // Fetch tree data
    tree_t* treeP = DATA_POINTER(tree);
    treeIdx_t treeIdx = PSTATE.treeIdx;
    PSTATE.treeIdx = treeP->idxNext[treeIdx];

    // Terminate if tree is fully traversed
    if(treeIdx == -1) {
        PC++;
        BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
        return;
    }

    int indexParent = treeP->idxParent[treeIdx];

    if(indexParent == -1) {
        // Recursive call under diversification rates
        
        // Return total subtree with diversification info attached as an 'extended newick' character string

        // Root seems to only recurse and conclude tree info?
        bblockArgs_t args = PSTATE.stack.peek();
        PSTATE.stack.push(args);
        // RESAMPLE = false;
        BBLOCK_CALL(simTree);
        return;
    }

    bblockArgs_t args = PSTATE.stack.pop();

    floating_t treeAge = treeP->ages[treeIdx];
    simBranchRet_t ret = BBLOCK_CALL(simBranch<progState_t>, treeP->ages[indexParent], treeAge, args.lf, args.lf.z, args.mu, args.eta, args.rho);

    // Collect node info
    lambdaFun_t lambdaFun2 = ret.lf;
    floating_t lambda2 = lambdaFun(lambdaFun2, treeAge);
    // floating_t z2 = ret.r1;
    floating_t mu2 = ret.r2;
    // nodeInfo = ...

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnSpecProb = interiorNode ? log(lambdaFun(lambdaFun2, treeAge)) : log(args.rho);
    WEIGHT(ret.r7 + lnSpecProb);
    
    // Collect branch info
    // floating_t length = ...
    // branchInfo  = ...

    if(interiorNode) {
        bblockArgs_t args2(ret.lf, mu2, args.eta, args.rho); // lf contains z, lf.z always seem to be equal to z in WebPPL-script
        PSTATE.stack.push(args2);
        PSTATE.stack.push(args2);

        // Recursive call under final diversification rates (handled by pre-processed tree and SMC)

        // Return total subtree with diversification info attached as an 'extended newick' character string
        return;
    } else {
        // Return leaf with diversification info attached as an 'extended newick' character string
        return;
    }

})


BBLOCK(simBAMM, {
    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
    floating_t age = treeP->ages[ROOT_IDX];

    // Test settings
    /*
    floating_t lambda_0 = 0.2;
    floating_t z_0 = 0.0;
    lambdaFun_t lf_0(lambda_0, z_0, age);
    floating_t mu_0 = 0.1;
    floating_t eta = 0.000001;
    */
    floating_t lambda_0 = DIST_LAMBDA();
    floating_t z_0 = DIST_Z();
    lambdaFun_t lf_0(lambda_0, z_0, age);
    floating_t mu_0 = DIST_MU(lambda_0);
    floating_t eta = SAMPLE(gamma, 1, 1);
    floating_t rho = 1.0;

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
})

BBLOCK(survivorshipBias, {
    // Survivorship Bias, is done after simBAMM
    floating_t age = DATA_POINTER(tree)->ages[ROOT_IDX];
    bblockArgs_t args = PSTATE.stack.pop();
    int MAX_M = 10000;
    int M = BBLOCK_CALL(M_bammGoesUndetected, age, args.lf, args.mu, args.eta, args.rho, MAX_M);
    WEIGHT(log(static_cast<floating_t>(M)));
    PC++;
})


CALLBACK(callback, {
    // printf("Done yay!\n");
})

MAIN({
    initGen();

    INIT_BBLOCK(simBAMM)
    INIT_BBLOCK(simTree)
    INIT_BBLOCK(survivorshipBias)

    SMC(callback)
})

