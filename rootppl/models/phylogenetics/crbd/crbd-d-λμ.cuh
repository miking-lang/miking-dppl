
typedef short treeIdx_t;
// V: changed the progstate to add delayed mu
struct progState_t {
    floating_t mu;
    floating_t kLambda;
    floating_t thetaLambda;
    floating_t kMu;
    floating_t thetaMu;
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
 
 // V: I am introducing this new structure for the return type of crbdGoesUndetectedDelayed
struct ret_delayed_t_bothways {
    floating_t res;
    floating_t kLambda;
    floating_t thetaLambda;
    floating_t kMu;
    floating_t thetaMu;

    DEV ret_delayed_t_bothways(){};
    DEV ret_delayed_t_bothways(floating_t res_, floating_t k_lambda, floating_t theta_lambda, floating_t k_mu, floating_t theta_mu) {
        res = res_;
        kLambda = k_lambda;
        thetaLambda = theta_lambda;
        kMu = k_mu;
        thetaMu = theta_mu;
    }
};
 
 
 

// const int MAX_DIV = 5;
// const int MAX_LAM = 5;

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)

// V: This seemed to be correct; returning four floats, two for each parameter and the bool value, no change needed.
BBLOCK_HELPER_DECLARE(crbdGoesUndetectedDelayed, ret_delayed_t_bothways, floating_t, floating_t, floating_t, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1)

BBLOCK_DATA_CONST(rho, floating_t, rhoConst)

BBLOCK_HELPER(delayedSample, {
    floating_t t = SAMPLE(lomax, 1/theta, k);
    ret_delayed_t ret(t, k+1, theta / (1 + t * theta));
    return ret;

}, ret_delayed_t, floating_t k, floating_t theta)


BBLOCK_HELPER(delayedObserve, {
    ret_delayed_t ret(lomaxScore(x, 1/theta, k), k+1, theta / (1 + x * theta));
    return ret;

}, ret_delayed_t, floating_t x, floating_t k, floating_t theta)
 

// V: we  need a new delayed observe BBLOCK to observe from a poisson
// See Jan's thesis, page 60
// x: the value
// t: how much time the poisson process runs
// k: delay paramater
// theta: delay parameter
BBLOCK_HELPER(delayedObservePoisson, {
    ret_delayed_t ret(negativeBinomialScore(x, k, 1/(1 + t*theta)),
            k,
            theta / (1 + t * theta));
    return ret;

}, ret_delayed_t, floating_t x, floating_t t, floating_t k, floating_t theta)
 
 
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
    // floating_t tMu = SAMPLE(exponential, mu);

    // V: Introduce delayed sampling for mu
    ret_delayed_t mu_ret0 = BBLOCK_CALL(delayedSample, kMu, thetaMu);
    floating_t tMu = mu_ret0.res;

    floating_t t = MIN(tLambda, tMu);

    bool speciation = tLambda < tMu;
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0) {
        if (SAMPLE(bernoulli, 1 - rho))
            // V: return type is now ret_delayed_t_bothways
            return ret_delayed_t_bothways(true, ret0.k, ret0.theta, mu_ret0.k, mu_ret0.theta);
        else
            // V: return type is now ret_delayed_t_bothways
            return ret_delayed_t_bothways(false, ret0.k, ret0.theta, mu_ret0.k, mu_ret0.theta);
    }
    
    bool extinction = !speciation;
    if (extinction)
        // V: return type is now ret_delayed_t_bothways
        return ret_delayed_t_bothways(true, ret0.k, ret0.theta, mu_ret0.k, mu_ret0.theta);

    // V: the recursive call now has the new return type
    ret_delayed_t_bothways ret1 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret0.k, ret0.theta, mu_ret0.k, mu_ret0.theta, rho);

    bool leftDetection = !ret1.res;

    if (leftDetection)
        return ret1;

    // V: same, but ret1 now has both of the delayed parameters
    ret_delayed_t_bothways ret2 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret1.kLambda, ret1.thetaLambda, ret1.kMu, ret1.thetaMu, rho); 

    return ret2;
    // V: I am not sure if the return type is the first argument after the }. I think all the subsequent arguments are the input values?
    // I asuming the first argument after the } is the return type, so I changed it from ret_delayed_t to ret_delayed_t_bothways

}, ret_delayed_t_bothways, floating_t startTime, floating_t kLambda, floating_t thetaLambda, floating_t kMu, floating_t thetaMu, floating_t rho)
 
 
BBLOCK_HELPER(simBranch, {

    ret_delayed_t ret0 = BBLOCK_CALL(delayedSample, kLambda, thetaLambda);
    floating_t t = ret0.res;

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        // V: changed the return type to incorporate delayed mu
        return ret_delayed_t_bothways(0.0, ret0.k, ret0.theta, kMu, thetaMu);
    }

    // V: mu is now delayed as well; also crbdGoesUndetectedDelayed returns ret_delayed_t_bothways
    ret_delayed_t_bothways ret1 = BBLOCK_CALL(crbdGoesUndetectedDelayed, currentTime, ret0.k, ret0.theta, kMu, thetaMu, rho);

    bool sideDetection = !ret1.res;
    if(sideDetection) {
        // V: need to change the return here to delay mu
        return ret_delayed_t_bothways(-INFINITY, ret1.kLambda, ret1.thetaLambda, ret1.kMu, ret1.thetaMu);
    }

    // V: change the return type and delay mu
    ret_delayed_t_bothways ret2 = BBLOCK_CALL(simBranch, currentTime, stopTime, ret1.kLambda, ret1.thetaLambda, ret1.kMu, ret1.thetaMu, rho);
    floating_t newProb = ret2.res + log(2.0);

    // V: delay mu and return type 
    return ret_delayed_t_bothways(newProb, ret2.kLambda, ret2.thetaLambda, ret2.kMu, ret2.thetaMu );

// V: changed the function signature to reflect new input types and return type
}, ret_delayed_t_bothways, floating_t startTime, floating_t stopTime, floating_t kLambda, floating_t thetaLambda, floating_t kMu, floating_t thetaMu, floating_t rho)
 
 
BBLOCK(simTree, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    // floating_t lambdaLocal = PSTATE.lambda;
    // V: mu is now delayed, so we cannot rely on muLocal
    //floating_t muLocal = PSTATE.mu;
    // floating_t kLambdaLocal = PSTATE.kLambda;
    // floating_t thetaLambdaLocal = PSTATE.thetaLambda;
    // floating_t rhoLocal = *DATA_POINTER(rho);
    floating_t rhoLocal = DATA_CONST(rho);

    int indexParent = treeP->idxParent[treeIdx];
    
    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];

    // V: instead of using muLocal, we use the Poisson observe
    //floating_t lnProb1 = - muLocal * (parentAge - age);
    // ret_delayed_t my_ret_0;
    ret_delayed_t mu_ret_0 = BBLOCK_CALL(delayedObservePoisson, 0, parentAge - age, PSTATE.kMu, PSTATE.thetaMu);
    floating_t lnProb1 = mu_ret_0.res;
                    

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    // floating_t lnProb2 = interiorNode ? log(lambdaLocal) : log(rhoLocal);

    ret_delayed_t ret0;
    if (interiorNode)
        ret0 = BBLOCK_CALL(delayedObserve, 0, PSTATE.kLambda, PSTATE.thetaLambda);
    else
        ret0 = ret_delayed_t(log(rho), PSTATE.kLambda, PSTATE.thetaLambda);

    floating_t lnProb2 = ret0.res;

    // V: this now includes the delay of mu as well
    ret_delayed_t_bothways ret1 = BBLOCK_CALL(simBranch, parentAge, age, ret0.k, ret0.theta, mu_ret_0.k, mu_ret_0.theta, rhoLocal);
    floating_t lnProb3 = ret1.res;

    WEIGHT(lnProb1 + lnProb2 + lnProb3);

    // V: Changed the states to reflect that both are delayed
    PSTATE.kLambda = ret1.kLambda;
    PSTATE.thetaLambda = ret1.thetaLambda;
    PSTATE.kMu = ret1.kMu;
    PSTATE.thetaMu = ret1.thetaMu;

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

    // V: Instead of fixing mu, we draw it from gamma now
    //PSTATE.mu = 0.1;
    PSTATE.kMu = kMu;
    PSTATE.thetaMu = thetaMu;
    
    // floating_t k_lambda = 1;
    // floating_t theta_lambda = 0.2;
    PSTATE.kLambda = kLambda;
    PSTATE.thetaLambda = thetaLambda;


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
    PSTATE.mu = SAMPLE(gamma, PSTATE.kMu, PSTATE.thetaMu);
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


