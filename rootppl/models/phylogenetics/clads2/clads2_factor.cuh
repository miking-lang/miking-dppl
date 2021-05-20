/*
 * ClaDS2 Model with Immediate sampling, but uses factors.
   (uses a stack for the factors)
   TODO change to an array
 */

typedef short treeIdx_t;

struct progState_t {
    pStack_t stack;
    floating_t alpha;
    floating_t sigma;
    floating_t epsilon;
    floating_t rho;
    floating_t lambda_0;
    treeIdx_t treeIdx;
};

struct simBranchRet_t {
    floating_t r0;
    floating_t r1;
    floating_t r2;

    DEV simBranchRet_t(){};

    DEV simBranchRet_t(floating_t r0_, floating_t r1_, floating_t r2_) {
        r0 = r0_;
        r1 = r1_;
        r2 = r2_;
    }
};


BBLOCK_HELPER(clads2GoesUndetectedFactor, {
    floating_t lambda = lambda0*factor;
    floating_t mu = epsilon*lambda;

    // extreme values patch
    if (factor > 1e5) {
      return false; // detected for sure with insane div. rate
    }
    
    if (factor < 1e-5) {
      // lambda is very small, so nothing will happen to the lineage in terms of speciation
      // it will hit present and then we see
      bool undetected = !SAMPLE(bernoulli, rho);  
      return undetected;
    }
    // end extreme values patch 1

    floating_t t = SAMPLE(exponential, lambda + mu);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho); 


    bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    if(exctinction)
        return true;


    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    return BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho)
        && BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);
 
}, bool, floating_t startTime, floating_t lambda0, floating_t factor, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 

/*
 * Simulates Hidden Speciation Events Along Branch
 */ 
BBLOCK_HELPER(simBranchFactor, {
    floating_t lambda = lambda0*factor;

    floating_t t1 = startTime - stopTime;
    floating_t mu = epsilon * lambda;

    // extreme values patch 2/2
    if (factor > 1e5 ) {
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }
    
    if (factor < 1e-5) {
      
      simBranchRet_t ret(factor, lambda*t1, -mu*t1);
      return ret;
    }
    // end extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        simBranchRet_t ret(factor, lambda*t1, -mu*t1);
        return ret;
    }

    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho);

    if(! sideUndetected) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }

    simBranchRet_t ret = BBLOCK_CALL(simBranchFactor, currentTime, stopTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);
    simBranchRet_t rt(ret.r0, ret.r1 + lambda*t, ret.r2 + log(2.0) - mu*t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda0, floating_t factor, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)

 

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
 
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
    //    printf("%f %f\n", ret.r2, lnTerminalProb);
    WEIGHT(ret.r2 + lnTerminalProb);

    // Collect node and branch info, todo?

    if(interiorNode) {
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
 
   

    floating_t lambda_0 = SAMPLE(gamma, k, theta);

    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, 1.0 / 0.2);

    floating_t sigma = sqrt(sigmaSquared);

    floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));

    floating_t epsilon = SAMPLE(uniform, epsMin, epsMax);
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

    floating_t factor = PSTATE.stack.pop();
    floating_t lambda_0 = PSTATE.lambda_0;

    floating_t alpha = PSTATE.alpha;
    floating_t sigma = PSTATE.sigma;
    floating_t epsilon = PSTATE.epsilon;
    floating_t rho = PSTATE.rho;

    int numSamples = 100;
    int numDetected = 0;
    for(int i = 0; i < numSamples; i++) {
      bool undetected = BBLOCK_CALL(clads2GoesUndetectedFactor, treeAge, lambda_0, factor, alpha, sigma, epsilon, rho);
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

    //    SMC(saveResults);
    SMC(NULL);
})
 
 
