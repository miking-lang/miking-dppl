#include <iostream>
#include <cstring>
#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "../../../Utils/distributions.cuh"
// #include "cbd.cuh"
#include "../TreeUtils/treeUtils.cuh"

/**
    This file traverses the tree with a precomputed DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/crbdNextPtr.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/crbdNextPtr.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3


BBLOCK_DATA(tree, tree_t, 1)


floating_t corrFactor;

typedef short treeIdx_t;
struct progState_t {
    floating_t lambda;
    floating_t mu;
    treeIdx_t treeIdx;
};


void initCBD() {
    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, NUM_NODES);
    corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);

    COPY_DATA_GPU(tree, tree_t, 1);
}

BBLOCK_HELPER(goesExtinct, {

    floating_t t = BBLOCK_CALL(sampleExponential, PSTATE.lambda + PSTATE.mu);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return false;
    
    bool speciation = BBLOCK_CALL(flipK, PSTATE.lambda / (PSTATE.lambda + PSTATE.mu));
    if (! speciation)
        return true;
    else 
        return BBLOCK_CALL(goesExtinct<T>, currentTime) && BBLOCK_CALL(goesExtinct<T>, currentTime);

}, bool, floating_t startTime)


BBLOCK_HELPER(simBranch, {

    floating_t t = BBLOCK_CALL(sampleExponential, PSTATE.lambda);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime);
    if(! sideExtinction)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch<T>, currentTime, stopTime) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime)



BBLOCK(simTree, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    if(treeIdx == -1) {
        PC = 2;
        return;
    }

    int indexParent = treeP->idxParent[treeIdx];

    floating_t lnProb1 = - PSTATE.mu * (treeP->ages[indexParent] - treeP->ages[treeIdx]);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnProb2 = interiorNode ? log(PSTATE.lambda) : 0.0;

    floating_t lnProb3 = BBLOCK_CALL(simBranch<progState_t>, treeP->ages[indexParent], treeP->ages[treeIdx]);

    WEIGHT(lnProb1 + lnProb2 + lnProb3);
    PSTATE.treeIdx = treeP->idxNext[treeIdx];
    // Resample
})

BBLOCK(simCRBD, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
    PSTATE.lambda = 0.2;
    PSTATE.mu = 0.1;

    PC++;
    // No resample => execute next bblock
    BBLOCK_CALL(simTree);
})

STATUSFUNC({})

int main() {

    initGen();
    initCBD();
    
    SMCSTART(progState_t)

    INITBBLOCK(simCRBD, progState_t)
    INITBBLOCK(simTree, progState_t)

    SMCEND(progState_t)

    res += corrFactor;

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}

