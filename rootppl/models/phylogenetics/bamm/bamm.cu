#include <iostream>
#include <cstring>

#include "inference/smc/smc_impl.cuh"
#include "tree-utils/tree_utils.cuh"
#include "bamm.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/bamm/bamm.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/phylogenetics/bamm/bamm.cu -o smc.exe -std=c++11 -O3
*/


#define MIN(a, b) a <= b ? a : b
#define MAX(a, b) a >= b ? a : b

#define DIST_LAMBDA() exp(SAMPLE(uniform, log(1e-2), log(1e1)))
#define DIST_Z() SAMPLE(normal, 0, 0.001)
#define DIST_MU(lam0) SAMPLE(uniform, 0.0, 0.1) * lam0

typedef bisse32_tree_t tree_t;
BBLOCK_DATA(tree, tree_t, 1);
// BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
// BBLOCK_DATA(mu, floating_t, 1)

// BBLOCK_DATA(nestedArgs, bblockArgs_t, NUM_PARTICLES)


floating_t corrFactor;


void initBamm() {
    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )
    // *lambda = 0.2; // birth rate
    // *mu = 0.1; // death rate

    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, NUM_NODES);
    corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);

    COPY_DATA_GPU(tree, tree_t, 1);
    // COPY_DATA_GPU(lambda, floating_t, 1)
    // COPY_DATA_GPU(mu, floating_t, 1)

    //COPY_DATA_GPU(nestedArgs, bblockArgs_t, NUM_PARTICLES)
}

DEV HOST inline floating_t lambdaFun(lambdaFun_t lf, floating_t t) {
    return lf.lambda * exp(lf.z * (lf.t1 - t));
}

DEV HOST inline floating_t robustExponentialSampler(floating_t a) {
    if(a <= 0) return INFINITY;
    if(a == INFINITY) return 0;
    return SAMPLE(exponential, a);
}

BBLOCK_HELPER(M_bammGoesUndetected, {

    if(max_M == 0) { printf("M_bammGoesUndetected: MAX DEPTH REACHED!\n"); return 0;}

    if(! BBLOCK_CALL(bammGoesUndetected, startTime, lf, mu, eta, rho) 
    && ! BBLOCK_CALL(bammGoesUndetected, startTime, lf, mu, eta, rho)) return 1;
    else return 1 + BBLOCK_CALL(M_bammGoesUndetected, startTime, lf, mu, eta, rho, max_M-1);

}, int, floating_t startTime, lambdaFun_t lf, floating_t mu, floating_t eta, floating_t rho, int max_M)

BBLOCK_HELPER(bammLambdaWait, {

    floating_t startLambda = lambdaFun(lf, startTime);
    floating_t stopLambda = lambdaFun(lf, stopTime);

    floating_t topLambda = MAX(startLambda, stopLambda);

    floating_t t = startTime - robustExponentialSampler(topLambda);

    if(t < stopTime) return INFINITY;
    if(SAMPLE(bernoulli, lambdaFun(lf, t) / topLambda)) return startTime - t;

    return startTime - t + BBLOCK_CALL(bammLambdaWait, lf, t, stopTime);

}, lambdaFun_t lf, floating_t startTime, floating_t stopTime)

BBLOCK_HELPER(bammGoesUndetected, {

    floating_t t1 = robustExponentialSampler(mu + eta);
    floating_t tLambda = bammLambdaWait(lf, startTime, 0);

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

        return BBLOCK_CALL(bammGoesUndetected, currentTime, lf2, mu2, eta, rho));
    }

    return BBLOCK_CALL(bammGoesUndetected, currentTime, lf, mu, eta, rho) && BBLOCK_CALL(bammGoesUndetected, currentTime, lf, mu, eta, rho));

}, floating_t startTime, lambdaFun_t lf, floating_t mu, floating_t eta, floating_t rho)


BBLOCK_HELPER(simBranch, {

    floating_t tLambda = BBLOCK_CALL(bammLambdaWait, lambdaFun, startTime, stopTime);
    floating_t tEta = robustExponentialSampler(eta);
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

/*
BBLOCK_HELPER(simBranch, {

    floating_t tLambda = BBLOCK_CALL(lambdaWait, lf, startTime, stopTime);

    // floating_t tSigma = BBLOCK_CALL(sampleExponential, sigma)
    floating_t tSigma = tLambda + 1.0; // For testing
    floating_t t = MIN(tLambda, tSigma);

    floating_t currentTime = startTime - t;

    // This is the terminating condition, return final values and appropriate accumulators
    // for number of shifts and probability
    if (currentTime <= stopTime) {
        floating_t t1 = startTime - stopTime;
        floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, stopTime)) / 2.0;
        
        simBranchRet_t rt(lf, z, mu, meanLambda*t1, z*t1, mu*t1, 0, -mu*t1);
        return rt;
    }

    // Check whether this is a rate shift, and handle this case
    if (tSigma < tLambda) {
        floating_t lambda0_2 = BBLOCK_CALL(sampleGamma, 1.0, 1.0);
        floating_t z2 = BBLOCK_CALL(sampleNormal, 0.0, 0.001);
        floating_t mu2 = BBLOCK_CALL(sampleGamma, 1.0, 1.0);

        // Recursive call
        lambdaFun_t lf2(lambda0_2, z2, currentTime);
        simBranchRet_t ret = BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lf2, z2, mu2, sigma);

        // Return accumulated values
        // Add this rate shift to the total number of rate shifts
        // No-extinction probability is dependent on mu before shift
        floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;
        
        simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6 + 1, ret.r7 - mu*t);
        return rt;
    }

    // We have a speciation event; handle this case
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime, lf, mu, sigma);
    if (sideExtinction == false) {
        simBranchRet_t rt(lf, 0.0, 0.0, 0.0, 0.0, 0.0, 0, -INFINITY);
        return rt;
    }

    // Recursive call
    simBranchRet_t ret = BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lf, z, mu, sigma);

    // Return accumulated values
    // Factor 2 because we do not care whether extinction is on left or right side branch
    floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;

    simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6, ret.r7 + log(2.0) - mu*t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, lambdaFun_t lf, floating_t z, floating_t mu, floating_t sigma)
*/

// TODO: Should return tree info as string?
BBLOCK(simTree, progState_t, {

    // Fetch tree data
    tree_t* treeP = DATA_POINTER(tree);
    treeIdx_t treeIdx = PSTATE.treeIdx;
    PSTATE.treeIdx = treeP->idxNext[treeIdx];

    // Terminate if tree is fully traversed
    if(treeIdx == -1) {
        PC++;
        RESAMPLE = false;
        return;
    }

    int indexParent = treeP->idxParent[treeIdx];

    if(indexParent == -1) {
        // Recursive call under diversification rates
        
        // Return total subtree with diversification info attached as an 'extended newick' character string

        // Root seems to only recurse and conclude tree info?
        bblockArgs_t args = PSTATE.stack.peek();
        PSTATE.stack.push(args);
        RESAMPLE = false;
        return;
    }

    bblockArgs_t args = PSTATE.stack.pop();


    floating_t treeAge = treeP->ages[treeIdx];
    simBranchRet_t ret = BBLOCK_CALL(simBranch<progState_t>, treeP->ages[indexParent], treeAge, args.lf, args.lf.z, args.mu, args.sigma);

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnSpecProb = interiorNode ? log(lambdaFun(args.lf, treeAge)) : 0.0;
    WEIGHT(ret.r7 + lnSpecProb);

    // Collect node info
    
    // Collect branch info

    RESAMPLE = true;
    if(interiorNode) {
        bblockArgs_t args2(ret.lf, ret.r2, args.sigma);
        PSTATE.stack.push(args2);
        PSTATE.stack.push(args2);

        // Recursive call under final diversification rates (done automatically by particle PC framework)

        // Return total subtree with diversification info attached as an 'extended newick' character string
        return;
    } else {
        // Return leaf with diversification info attached as an 'extended newick' character string
        return;
    }

})


BBLOCK(simBAMM, progState_t, {
    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    //    var lambda_0    = DistLambda.sample()
    //    var z_0         = DistZ.sample()
    //    var lambdaFun_0 = function( t ) { lambda_0 * Math.exp( z_0 * (tree.age - t ) ) }
    //    var mu_0        = DistMu.sample()
    //    var sigma       = gamma( {shape:1, scale:1} )

    // Test settings
    floating_t lambda_0 = 0.2;
    floating_t z_0 = 0.0;
    floating_t age = treeP->ages[ROOT_IDX];
    floating_t mu_0 = 0.1;
    floating_t sigma = 0.000001;

    lambdaFun_t lf(lambda_0, z_0, age);
    bblockArgs_t args(lf, mu_0, sigma);

    // one for each child, and one for nested inference after tree simulations
    /*
    for(int i = 0; i < 3; i++)
        PSTATE.stack.push(args);
    */
    PSTATE.stack.push(args);
    PSTATE.stack.push(args);
    PSTATE.stack.push(args);

    PC++;
    RESAMPLE = false;
})

CALLBACK(calcResult, nestedProgState_t, {
    int numExtinct = 0;
    for(int i = 0; i < NUM_PARTICLES_NESTED; i++)
        numExtinct += PSTATE.extinct;

    int numSurvived = NUM_PARTICLES_NESTED - numExtinct;
    return_t* retP = static_cast<return_t*>(ret);
    *retP = numSurvived / (double)NUM_PARTICLES_NESTED;
    
}, void* ret)

template <typename T>
DEV T runNestedInference(int parentIndex, bblockArgs_t* arg) {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMCSTART(nestedProgState_t)

    INIT_BBLOCK_NESTED(goesExtinctBblock, nestedProgState_t)
    
    SMCEND_NESTED(nestedProgState_t, calcResult, ret, arg, parallelExec, parallelResampling, parentIndex)

    return ret;
}

BBLOCK(survivalConditioning, progState_t, {
    bblockArgs_t args = PSTATE.stack.pop();
    bblockArgs_t* argsAlloc = new bblockArgs_t;
    *argsAlloc = args;
    double survivalRate = runNestedInference<double>(i, argsAlloc);
    free(argsAlloc);

    WEIGHT(-2.0 * log(survivalRate));

    PC++;
    RESAMPLE = false;
})

STATUSFUNC({
    
})

int main() {

    initGen();
    initBamm();
    
    SMCSTART(progState_t)

    INIT_BBLOCK(simBAMM, progState_t)
    INIT_BBLOCK(simTree, progState_t)
    INIT_BBLOCK(survivalConditioning, progState_t)

    SMCEND(progState_t)

    res += corrFactor;  

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}

