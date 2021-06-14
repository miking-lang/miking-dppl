/** CRBD Model with immediate sampling 
    - must be included from an example */

typedef short treeIdx_t;
struct progState_t {
    floating_t lambda;
    floating_t mu;
    treeIdx_t treeIdx;
};

#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
BBLOCK_HELPER_DECLARE(crbdGoesUndetected, bool, floating_t, floating_t, floating_t, floating_t);
BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA_CONST(rho, floating_t, rhoConst)


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


// uses Jan's walk
BBLOCK_HELPER(crbdGoesUndetected, {
    floating_t duration = SAMPLE(exponential, mu);

    if (duration > startTime) {
      if (SAMPLE(bernoulli, rho)) return false;
    }

    floating_t branchLength = MIN(duration, startTime);

    floating_t nSpPoints = SAMPLE(poisson, lambda*branchLength);

    for (int n = 0; n < nSpPoints; n++) {
      floating_t eventTime = SAMPLE(uniform, startTime - branchLength, startTime);
      if (!BBLOCK_CALL(crbdGoesUndetected, eventTime, lambda, mu, rho)) return false;
    }

    return true;
}, bool, floating_t startTime, floating_t lambda, floating_t mu, floating_t rho)


// uses Jan's walk
BBLOCK_HELPER(simBranch, { 
    floating_t nSpPoints = SAMPLE(poisson, lambda*(startTime - stopTime));

    for (int n = 0; n < nSpPoints; n++) {
      floating_t currentTime = SAMPLE(uniform, stopTime, startTime);
      if (!BBLOCK_CALL(crbdGoesUndetected, currentTime, lambda, mu, rho)) return -INFINITY;
    }

    return nSpPoints*log(2.0);
    
}, floating_t, floating_t startTime, floating_t stopTime, floating_t lambda, floating_t mu, floating_t rho)


BBLOCK(simTree, {
    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    floating_t lambdaLocal = PSTATE.lambda;
    floating_t muLocal = PSTATE.mu;
    // floating_t rhoLocal = *DATA_POINTER(rho);
    // floating_t rhoLocal = DATA_CONST(rho);
    floating_t rhoLocal = rho;

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
    PSTATE.lambda = SAMPLE(gamma, k, theta);
    PSTATE.mu =  SAMPLE(gamma, kMu, thetaMu);

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    PC++;
    BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
})


BBLOCK(survivorshipBias, {
    floating_t age = DATA_POINTER(tree)->ages[ROOT_IDX];
    int MAX_M = 10000;
    // int M = BBLOCK_CALL(M_crbdGoesUndetected, age, MAX_M, PSTATE.lambda, PSTATE.mu, DATA_CONST(rho));
    int M = BBLOCK_CALL(M_crbdGoesUndetected, age, MAX_M, PSTATE.lambda, PSTATE.mu, rho);
    WEIGHT(LOG(M));
    PC++;
})


// Write particle data to file. 
CALLBACK(saveResults, {    
    std::string fileName = "crbd-immediate.out";
    std::ofstream resFile (fileName);
    if(resFile.is_open()) {
        for(int i = 0; i < N; i++)
            resFile << PSTATES[i].lambda << " " << PSTATES[i].mu << " " << WEIGHTS[i] << "\n";
        resFile.close();
    } else {
        printf("Could not open file %s\n", fileName.c_str());
    }
})

