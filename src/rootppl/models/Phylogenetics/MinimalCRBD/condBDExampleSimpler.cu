#include <iostream>
#include <cstring>
#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "../../../Utils/distributions.cuh"
// #include "cbd.cuh"
#include "../TreeUtils/treeUtils.cuh"
#include "simulations.cuh"
#include "stack.cuh"

/**
    This model traverses the tree with a DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/condBDExampleSimpler.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/condBDExampleSimpler.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

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
    PSTATE.push(pState);

}, void, int nodeIdx, int childIdx)


BBLOCK(condBD_1, stack_t<progState_t>, {

    if(PSTATE.empty()) {
        PC = 5;
        return;
    }

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.peek();

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    WEIGHT(- pState.mu * (parentAge - treeAge));

    PC++; // condBD_2
    // Resamples here
})

BBLOCK(condBD_2, stack_t<progState_t>, {
    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState = PSTATE.pop();

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    
    floating_t simBranchWeight = BBLOCK_CALL(simBranch<stack_t<progState_t>>, parentAge, treeAge, pState.lambda, pState.mu);
    WEIGHT(simBranchWeight);
    // Why not resample here?

    if(treeP->idxLeft[pState.treeIdx] != -1) { // Interior node, keep DFSing
        WEIGHT(log(2.0 * pState.lambda));
        PC++; // condBD_3
        PSTATE.push(pState);
        // Resamples here
    } else {
        PC = 1; // cond_BD1 instead?
        BBLOCK_CALL(condBD_1);
        // condBD_4
        // Does not resample here
    }
})

BBLOCK(condBD_3, stack_t<progState_t>, {

    tree_t* treeP = DATA_POINTER(tree);
    progState_t pState = PSTATE.pop();
    int childIdxR = treeP->idxRight[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState.treeIdx, childIdxR);
    int childIdx = treeP->idxLeft[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState.treeIdx, childIdx);
    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_4, stack_t<progState_t>, {

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_init, stack_t<progState_t>, {

    tree_t* treeP = DATA_POINTER(tree);

    int rightIdx = treeP->idxRight[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, rightIdx);
    int leftIdx = treeP->idxLeft[ROOT_IDX];
    BBLOCK_CALL(pushChild, ROOT_IDX, leftIdx);
    
    // Push PC termination idx to stack
    PC++;
    BBLOCK_CALL(condBD_1);
})


int main() {

    initGen();
    
    SMCSTART(stack_t<progState_t>)

    INITBBLOCK(condBD_init, stack_t<progState_t>)
    INITBBLOCK(condBD_1, stack_t<progState_t>)
    INITBBLOCK(condBD_2, stack_t<progState_t>)
    INITBBLOCK(condBD_3, stack_t<progState_t>)
    INITBBLOCK(condBD_4, stack_t<progState_t>)

    SMCEND(stack_t<progState_t>, NULL)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}
