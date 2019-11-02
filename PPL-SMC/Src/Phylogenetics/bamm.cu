#include <iostream>
#include <cstring>
// #include <math.h>
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "../Utils/array.cuh"
#include "TreeUtils/treeUtils.cuh"
#include "bamm.cuh"

// nvcc -arch=sm_75 -rdc=true Src/Phylogenetics/bamm.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Phylogenetics/bamm.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

#define MIN(a, b) a <= b ? a : b
#define MAX(a, b) a >= b ? a : b

BBLOCK_DATA(tree, tree_t, 1);
// BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
// BBLOCK_DATA(mu, floating_t, 1)

//BBLOCK_DATA(nestedArgs, bblockArgs_t, NUM_PARTICLES)


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

//DEV HOST inline floating_t lambdaFun(floating_t lambda2, floating_t z2, floating_t t1, floating_t t) {
DEV HOST inline floating_t lambdaFun(lambdaFun_t lf, floating_t t) {
    return lf.lambda * exp(lf.z * (lf.t1 - t));
}


BBLOCK_HELPER(lambdaWait, {

    //printf("Will write lambda vals...\n");
    floating_t startLambda = lambdaFun(lf, startTime);
    floating_t stopLambda = lambdaFun(lf, stopTime);

    floating_t topLambda = MAX(startLambda, stopLambda);
    //printf("Wrote lambda vals!\n");

    floating_t t = startTime - BBLOCK_CALL(exponential, topLambda);
    // printf("Made exponential call!\n");

    if(t < stopTime || BBLOCK_CALL(flipK, lambdaFun(lf, t) / topLambda))
        return startTime - t;

    return startTime - BBLOCK_CALL(lambdaWait, lf, t, stopTime);

}, floating_t, lambdaFun_t lf, floating_t startTime, floating_t stopTime) //, floating_t lambda, floating_t lambdaZ, floating_t lambdaT) 

// Forward simulation from a starting time, returning extinction (true) or survival (false)
BBLOCK_HELPER(goesExtinct, {

    floating_t t1 = BBLOCK_CALL(exponential, mu + sigma);
    floating_t tLambda = BBLOCK_CALL(lambdaWait, lf, startTime, 0);

    floating_t t = MIN(t1, tLambda);
    // floating_t t = t1 < tLambda ? t1 : tLambda;

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return false;
    
    if(t1 < tLambda) {
        bool extinction = BBLOCK_CALL(flipK, mu / (mu+sigma));
        if (extinction)
            return true;

        // No extinction, so rateshift
        floating_t lambda2 = BBLOCK_CALL(gamma, 1.0, 1.0);
        floating_t z2 = BBLOCK_CALL(normal, 0.0, 0.001);
        floating_t mu2 = BBLOCK_CALL(gamma, 1.0, 1.0);
        // lambdaFun_t lf2 = lambdaFun_t{lambda2, z2, t1};
        // lambdaFun_t lf2 = {lambda2, z2, t1};
        lambdaFun_t lf2(lambda2, z2, t1);
        //lf2.lambda = lambda2;
        //lf2.z = z2;
        //lf2.t1 = t1;
        return BBLOCK_CALL(goesExtinct<T>, currentTime, lf2, mu2, sigma);
    }

    return BBLOCK_CALL(goesExtinct<T>, currentTime, lf, mu, sigma)
        && BBLOCK_CALL(goesExtinct<T>, currentTime, lf, mu, sigma);

}, bool, floating_t startTime, lambdaFun_t lf, floating_t mu, floating_t sigma)

/*
BBLOCK(goesExtinctBblock, nestedProgState_t, { // NEEDS LAMBAFUN
    tree_t* treeP = DATA_POINTER(tree);
    double age = treeP->ages[ROOT_IDX];
    
    PSTATE.extinct = BBLOCK_CALL(goesExtinct<nestedProgState_t>, age);
    PC++;
    RESAMPLE = true;
})
*/

BBLOCK_HELPER(simBranch, {

    floating_t tLambda = BBLOCK_CALL(lambdaWait, lf, startTime, stopTime);

    // floating_t tSigma = BBLOCK_CALL(exponential, sigma)
    floating_t tSigma = tLambda + 1.0; // For testing
    floating_t t = MIN(tLambda, tSigma);
    //floating_t t = tLambda < tSigma ? tLambda : tSigma;

    floating_t currentTime = startTime - t;

    // This is the terminating condition, return final values and appropriate accumulators
    // for number of shifts and probability
    if (currentTime <= stopTime) {
        floating_t t1 = startTime - stopTime;
        floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, stopTime)) / 2.0;
        
        /*
        simBranchRet_t rt;
        rt.lf = lf;
        rt.r1 = z;
        rt.r2 = mu;
        rt.r3 = meanLambda*t1;
        rt.r4 = z*t1;
        rt.r5 = mu*t1;
        rt.r6 = 0;
        rt.r7 = -mu * t1;
        // return ;
        */
        simBranchRet_t rt(lf, z, mu, meanLambda*t1, z*t1, mu*t1, 0, -mu*t1);
        return rt;
    }

    // Check whether this is a rate shift, and handle this case
    if (tSigma < tLambda) {
        floating_t lambda0_2 = BBLOCK_CALL(gamma, 1.0, 1.0);
        floating_t z2 = BBLOCK_CALL(normal, 0.0, 0.001);
        floating_t mu2 = BBLOCK_CALL(gamma, 1.0, 1.0);
        // lambdaFun_t lf2 = lambdaFun_t{lambda0_2, z2, mu2};

        // Recursive call
        lambdaFun_t lf2(lambda0_2, z2, currentTime);
        /*lf2.lambda = lambda0_2;
        lf2.z = z2;
        lf2.t1 = currentTime;*/
        simBranchRet_t ret = BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lf2, z2, mu2, sigma);

        // Return accumulated values
        // Add this rate shift to the total number of rate shifts
        // No-extinction probability is dependent on mu before shift
        floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;
        //simBranchRet_t toRet(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6 + 1, ret.r7 - mu*t);
        //return toRet;
        /*simBranchRet_t toRet;
        toRet.lf = ret.lf;
        toRet.r1 = ret.r1;
        toRet.r2 = ret.r2;
        toRet.r3 = ret.r3 + meanLambda * t;
        toRet.r4 = ret.r4 + z * t;
        toRet.r5 = ret.r5 + mu * t;
        toRet.r6 = ret.r6 + 1;
        toRet.r7 = ret.r7 - mu * t;
        return toRet;
        */
        simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6 + 1, ret.r7 - mu*t);
        return rt;
        //return simBranchRet_t{ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6 + 1, ret.r7 - mu*t};
    }

    // We have a speciation event; handle this case
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime, lf, mu, sigma);
    if (sideExtinction == false) {
        /*
        simBranchRet_t toRet;
        toRet.lf = lf;
        toRet.r1 = 0.0;
        toRet.r2 = 0.0;
        toRet.r3 = 0.0;
        toRet.r4 = 0.0;
        toRet.r5 = 0.0;
        toRet.r6 = 0;
        toRet.r7 = -INFINITY;
        return toRet;
        */
        simBranchRet_t rt(lf, 0.0, 0.0, 0.0, 0.0, 0.0, 0, -INFINITY);
        return rt;
        // return simBranchRet_t{lf, 0.0, 0.0, 0.0, 0.0, 0.0, 0, -INFINITY};
    }

    // Recursive call
    simBranchRet_t ret = BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lf, z, mu, sigma);

    // Return accumulated values
    // Factor 2 because we do not care whether extinction is on left or right side branch
    floating_t meanLambda = (lambdaFun(lf, startTime) + lambdaFun(lf, currentTime)) / 2.0;

    /*
    simBranchRet_t toRet;
    toRet.lf = ret.lf;
    toRet.r1 = ret.r1;
    toRet.r2 = ret.r2;
    toRet.r3 = ret.r3 + meanLambda * t;
    toRet.r4 = ret.r4 + z * t;
    toRet.r5 = ret.r5 + mu * t;
    toRet.r6 = ret.r6;
    toRet.r7 = ret.r7 + log(2.0) - mu * t;
    return toRet;
    */
    simBranchRet_t rt(ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6, ret.r7 + log(2.0) - mu*t);
    return rt;
    // return simBranchRet_t{ret.lf, ret.r1, ret.r2, ret.r3 + meanLambda*t, ret.r4 + z*t, ret.r5 + mu*t, ret.r6, ret.r7 + log(2.0) - mu*t};

}, simBranchRet_t, floating_t startTime, floating_t stopTime, lambdaFun_t lf, floating_t z, floating_t mu, floating_t sigma)



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
        /*
        args2.lf = ret.lf;
        args2.mu = ret.r2;
        args2.sigma = args.sigma;
        */
        //PSTATE.stack.push(bblockArgs_t{ret.lf, ret.r2, args.sigma});
        //PSTATE.stack.push(bblockArgs_t{ret.lf, ret.r2, args.sigma});
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
    //lambdaFun_t lf{lambda_0, z_0, age};
    floating_t mu_0 = 0.1;
    floating_t sigma = 0.000001;

    lambdaFun_t lf(lambda_0, z_0, age);
    /*
    lf.lambda = lambda_0;
    lf.z = z_0;
    lf.t1 = age;
    */
    bblockArgs_t args(lf, mu_0, sigma);
    /*
    args.lf = lf;
    args.mu = mu_0;
    args.sigma = sigma;
    */

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

BBLOCK(survivalConditioning, progState_t, {
    // double survivalRate = runNestedInference<double>(i);

    // WEIGHT(-2.0 * log(survivalRate));

    // printf("StackPointer: %d\n", PSTATE.stack.stackPointer);
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

/*
template <typename T>
DEV T runNestedInference(int parentIndex) {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMCSTART(nestedProgState_t)

    INITBBLOCK_NESTED(goesExtinctBblock, nestedProgState_t)
    
    SMCEND_NESTED(nestedProgState_t, calcResult, ret, parallelExec, parallelResampling, parentIndex)

    return ret;
}
*/

STATUSFUNC({
    
})

int main() {

    initGen();
    initBamm();
    
    SMCSTART(progState_t)

    INITBBLOCK(simBAMM, progState_t)
    INITBBLOCK(simTree, progState_t)
    INITBBLOCK(survivalConditioning, progState_t)

    SMCEND(progState_t)

    res += corrFactor;

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}

