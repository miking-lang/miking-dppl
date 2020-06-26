#include <iostream>
#include <cstring>
#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "../../../Utils/distributions.cuh"
// #include "cbd.cuh"
#include "../TreeUtils/treeUtils.cuh"
#include "simulations.cuh"

#define WORDS
#include "stack.cuh"

/**
    This model traverses the tree with a DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/condBDExampleWordStack.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/condBDExampleWordStack.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

BBLOCK_DATA(tree, tree_t, 1)

struct progState_t {
    int treeIdx;
    int parentIdx;
    floating_t lambda;
    floating_t mu;
    // int pc;
};

/*
BBLOCK_HELPER(pushProgState, {

    int numWords = sizeof(progState_t) / 4; // Fix case with non multiple of 4
    void* pStateVoid = static_cast<void*>(&pState);
    int* pStateInt = static_cast<int*>(pStateVoid);
    for (int w = 0; w < numWords; w++) { 
        PSTATE.push(pStateInt[w]);
    }

}, void, progState_t pState)

BBLOCK_HELPER(popProgState, {

    int numWords = sizeof(progState_t) / 4; // Fix case with non multiple of 4
    int pStateInt[numWords]; 

    for (int w = numWords-1; w >= 0; w--) {
        pStateInt[w] = PSTATE.pop();
    }

    void* pStateVoid = static_cast<void*>(pStateInt);
    progState_t pState = *static_cast<progState_t*>(pStateVoid);
    return pState;

}, progState_t)
*/

BBLOCK_HELPER(pushChild, {

    progState_t pState;
    pState.parentIdx = nodeIdx;
    pState.treeIdx = childIdx;
    pState.lambda = 0.2;
    pState.mu = 0.1;
    // BBLOCK_CALL(pushProgState, pState);
    PSTATE.pushType(pState);

}, void, int nodeIdx, int childIdx)

BBLOCK_HELPER(pushChildren, {

    tree_t* treeP = DATA_POINTER(tree);

    int rightIdx = treeP->idxRight[nodeIdx];
    progState_t pStateR;
    pStateR.parentIdx = nodeIdx;
    pStateR.treeIdx = rightIdx;
    pStateR.lambda = 0.2;
    pStateR.mu = 0.1;
    // BBLOCK_CALL(pushProgState, pStateR);
    PSTATE.pushType(pStateR);

    int leftIdx = treeP->idxLeft[nodeIdx];
    progState_t pStateL;
    pStateL.parentIdx = nodeIdx;
    pStateL.treeIdx = leftIdx;
    pStateL.lambda = 0.2;
    pStateL.mu = 0.1;
    // BBLOCK_CALL(pushProgState, pStateL);
    PSTATE.pushType(pStateL);

}, void, int nodeIdx)



BBLOCK(condBD_1, stack_t, {

    if(PSTATE.empty()) {
        PC = 5;
        return;
    }

    // PSTATE.numVisits += 1;
    // printf("NumVis: %d\n", PSTATE.numVisits);
    tree_t* treeP = DATA_POINTER(tree);

    // progState_t pState = PSTATE.peek();
    // progState_t pState = BBLOCK_CALL(popProgState);
    progState_t pState = PSTATE.popType<progState_t>();
    // BBLOCK_CALL(pushProgState, pState);
    PSTATE.pushType<progState_t>(pState);
    // printf("parIdx: %d, childIdx: %d\n", pState.parentIdx, pState.treeIdx);

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    WEIGHT(- pState.mu * (parentAge - treeAge));

    PC++; // condBD_2
    // Resamples here
})

BBLOCK(condBD_2, stack_t, {
    tree_t* treeP = DATA_POINTER(tree);

    // progState_t pState = PSTATE.pop();
    // progState_t pState = BBLOCK_CALL(popProgState);
    progState_t pState = PSTATE.popType<progState_t>();

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    // printf("AgeDiff: %f\n", parentAge - treeAge);
    floating_t w = BBLOCK_CALL(simBranch<stack_t>, parentAge, treeAge, pState.lambda, pState.mu);
    WEIGHT(w);
    // Why not resample here?

    if(treeP->idxLeft[pState.treeIdx] != -1) { // Interior node, keep DFSingg++ -x c++ Src/Models/Phylogenetics/CBD/cbdNoStackPrecomputeNext.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3
        WEIGHT(log(2.0 * pState.lambda));
        PC++; // condBD_3
        // PSTATE.push(pState);
        // BBLOCK_CALL(pushProgState, pState);
        PSTATE.pushType<progState_t>(pState);
        // Resamples here
    } else {
        PC = 1; // cond_BD1 instead?
        // condBD_4
        // Does not resample here
    }
})

BBLOCK(condBD_3, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);
    // progState_t pState = PSTATE.pop();
    // progState_t pState = BBLOCK_CALL(popProgState);
    progState_t pState = PSTATE.popType<progState_t>();
    BBLOCK_CALL(pushChildren, pState.treeIdx);
    //int childIdxR = treeP->idxRight[pState.treeIdx];
    //BBLOCK_CALL(pushChild, pState.treeIdx, childIdxR);
    //int childIdx = treeP->idxLeft[pState.treeIdx];
    //BBLOCK_CALL(pushChild, pState.treeIdx, childIdx);
    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_4, stack_t, {

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_init, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    //int rightIdx = treeP->idxRight[ROOT_IDX];
    //int leftIdx = treeP->idxLeft[ROOT_IDX];

    //BBLOCK_CALL(pushChild, ROOT_IDX, rightIdx);
    //BBLOCK_CALL(pushChild, ROOT_IDX, leftIdx);
    BBLOCK_CALL(pushChildren, ROOT_IDX);
    
    PC++;
    BBLOCK_CALL(condBD_1);
})


int main() {

    initGen();
    
    SMCSTART(stack_t)

    INITBBLOCK(condBD_init, stack_t)
    INITBBLOCK(condBD_1, stack_t)
    INITBBLOCK(condBD_2, stack_t)
    INITBBLOCK(condBD_3, stack_t)
    INITBBLOCK(condBD_4, stack_t)

    SMCEND(stack_t, NULL)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}
