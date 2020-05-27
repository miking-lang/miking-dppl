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

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/condBDExample.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/condBDExample.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

BBLOCK_DATA(tree, tree_t, 1)

struct progState_t {
    int treeIdx;
    int parentIdx;
    floating_t lambda;
    floating_t mu;
};

BBLOCK_HELPER(pushChild, {

    progState_t pState;
    pState.parentIdx = nodeIdx;
    pState.treeIdx = childIdx;
    pState.lambda = 0.2;
    pState.mu = 0.1;
    PSTATE.pushType(pState);

}, void, int nodeIdx, int childIdx)


BBLOCK(condBD_1, stack_t, {

    if(PSTATE.empty()) {
        PC = 5;
        return;
    }

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.popType<progState_t>();
    PSTATE.pushType<progState_t>(pState);

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    WEIGHT(- pState.mu * (parentAge - treeAge));

    PC++; // condBD_2
    // Resamples here
})

BBLOCK(condBD_2, stack_t, {
    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.popType<progState_t>();

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    floating_t w = BBLOCK_CALL(simBranch<stack_t>, parentAge, treeAge, pState.lambda, pState.mu);
    WEIGHT(w);
    // Why not resample here?

    if(treeP->idxLeft[pState.treeIdx] != -1) { // Interior node, keep DFSing
        WEIGHT(log(2.0 * pState.lambda));
        PC++; // condBD_3
        PSTATE.pushType<progState_t>(pState);
        // Resamples here
    } else {
        // PC = 1; // cond_BD1 instead?
        // condBD_4
        PC = PSTATE.popType<int>();
        // BBLOCK_CALL(condBD_1);
        // Does not resample here
    }
})

// Keep DFSing left
BBLOCK(condBD_3, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.popType<progState_t>();
    
    // int rightIdx = treeP->idxRight[ROOT_IDX];
    // BBLOCK_CALL(pushChild, ROOT_IDX, rightIdx);

    PSTATE.pushType<progState_t>(pState); // Need to use this state again when exploring right child
    PSTATE.pushType<int>(4); // PC = condBD_4

    int leftIdx = treeP->idxLeft[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, leftIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

// Keep DFSing right
BBLOCK(condBD_4, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.popType<progState_t>();

    // What should the return address be here? Do I really need one?

    int rightIdx = treeP->idxRight[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, rightIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_init, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    int rightIdx = treeP->idxRight[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, rightIdx);

    int leftIdx = treeP->idxLeft[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, leftIdx);
    
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
