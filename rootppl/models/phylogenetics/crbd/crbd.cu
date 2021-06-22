/** A constant-rate birth-death model.

    This is a simple model, given as an example here.
    For more advanced phylogenetic models, look at
    miking-benchmarks and phy-rootppl. */

#include <stdio.h>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "utils/math.cuh"


/** Tree stuff */
const int ROOT_IDX = 0;

// Bisse-32 tree, nodes: 63, maxDepth: 9, higher precision
struct bisse32_tree_t {
    static const int NUM_NODES = 63;
    static const int MAX_DEPTH = 9;
    const floating_t ages[NUM_NODES] =  {13.015999999999613,10.625999999999765,8.957999999999615,8.351999999999904,10.53600000000035,3.7479999999996982,7.775000000000737,7.6789999999999035,7.360999999999658,8.291000000000485,8.192000000000883,0,0.03300000000000002,0.5840000000000004,1.5889999999999358,5.187000000000067,5.19600000000007,3.8179999999998175,1.8680000000000012,1.3959999999999808,0,0.5600000000000004,0,0,0,0,0,0,0,0,0,0,4.870999999999686,1.143000000000001,1.8129999999999613,0.8660000000000007,1.059999999999994,0.21500000000000016,0,0,0,2.6009999999998246,0,0.8290000000000006,0,0.45200000000000035,0,0,0.001,0,0,0,0,0,0,0,0,0.20300000000000015,0,0,0,0,0};
    const int idxLeft[NUM_NODES] =      {1,3,5,7,9,11,13,15,17,19,21,-1,23,25,27,29,31,33,35,37,-1,39,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,41,43,45,47,49,51,-1,-1,-1,53,-1,55,-1,57,-1,-1,59,-1,-1,-1,-1,-1,-1,-1,-1,61,-1,-1,-1,-1,-1};
    const int idxRight[NUM_NODES] =     {2,4,6,8,10,12,14,16,18,20,22,-1,24,26,28,30,32,34,36,38,-1,40,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,42,44,46,48,50,52,-1,-1,-1,54,-1,56,-1,58,-1,-1,60,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,-1,-1};
    const int idxParent[NUM_NODES] =    {-1,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19,21,21,32,32,33,33,34,34,35,35,36,36,37,37,41,41,43,43,45,45,48,48,57,57};
    const int idxNext[NUM_NODES] =      {1,3,5,7,9,11,13,15,17,19,21,12,23,25,27,29,31,33,35,37,10,39,2,24,6,26,14,28,-1,30,16,32,41,43,45,47,49,51,20,40,22,53,8,55,34,57,18,48,59,50,4,52,38,54,42,56,44,61,46,60,36,62,58};
};

HOST DEV int countLeaves(const int* leftIdx, const int* rightIdx, int size) {
    int numLeaves = 0;
    for(int i = 0; i < size; i++) {
        if(leftIdx[i] == -1 && rightIdx[i] == -1)
            numLeaves++;
    }
    return numLeaves;
}

typedef bisse32_tree_t tree_t;

typedef short treeIdx_t;


/** Setup global parameters */
const floating_t k = 1.0;
const floating_t theta = 1.0;
const floating_t kMu = 1.0;
const floating_t thetaMu = 0.5;
const floating_t epsMin = 0.0;
const floating_t epsMax = 1.0;
const floating_t rhoConst = 1.0;


/** Model */
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

MAIN(
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    ADD_BBLOCK(survivorshipBias)

    SMC(saveResults)
)