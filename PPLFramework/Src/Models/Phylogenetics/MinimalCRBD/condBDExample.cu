#include "../../../Inference/Smc/smcImpl.cuh"

#include "../TreeUtils/treeUtils.cuh"
#include "simulations.cuh"
#include "stack.cuh"

/**
    This model traverses the tree with a DFS path that corresponds to the recursive calls. 
*/

// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/MinimalCRBD/condBDExample.cu -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

// Compile CPU: g++ -x c++ Src/Models/Phylogenetics/MinimalCRBD/condBDExample.cu -o smc.exe -std=c++11 -O3

#define NUM_BBLOCKS 5
INIT_GLOBAL(stack_t, NUM_BBLOCKS)


BBLOCK_DATA(tree, tree_t, 1)

struct progState_t {
    int treeIdx;
    int parentIdx;
    floating_t lambda;
    floating_t mu;
};

BBLOCK_HELPER(pushChild, {

    progState_t pState;
    pState.parentIdx = parent.treeIdx;
    pState.treeIdx = childIdx;
    pState.lambda = parent.lambda;
    pState.mu = parent.mu;
    PSTATE.pushType(pState);

}, void, progState_t parent, int childIdx)


BBLOCK(condBD_1, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);
    PSTATE.pushType<progState_t>(pState);


    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    WEIGHT(- pState.mu * (parentAge - treeAge));

    PC++; // condBD_2
    // Resamples here
})

BBLOCK(condBD_2, stack_t, {
    
    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    floating_t w = BBLOCK_CALL(simBranch<stack_t>, parentAge, treeAge, pState.lambda, pState.mu);
    WEIGHT(w);
    

    if(treeP->idxLeft[pState.treeIdx] != -1) { // Interior node, keep DFSing
        WEIGHT(log(2.0 * pState.lambda));
        PC++; // condBD_3
        PSTATE.pushType<progState_t>(pState);
        // Resamples here
    } else {
        PC = PSTATE.pop();
        if (PC < NUM_BBLOCKS) // Does not resample here
            DATA_POINTER(bblocksArr)[PC](particles, i, NULL);
    }
})

// Keep DFSing left
BBLOCK(condBD_3, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    PSTATE.pushType<progState_t>(pState); // Need to use this state again when exploring right sibling
    PSTATE.pushType<int>(4); // PC = condBD_4

    int leftIdx = treeP->idxLeft[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState, leftIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

// Keep DFSing right
BBLOCK(condBD_4, stack_t, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    // What should the return address be here? Do I really need one?

    int rightIdx = treeP->idxRight[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState, rightIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_init, stack_t, {

    PSTATE.pushType<int>(NUM_BBLOCKS); // Go to PC=5 when top-level condBD_1 is done (terminate)

    progState_t pState;
    pState.parentIdx = -1;
    pState.treeIdx = ROOT_IDX;
    pState.lambda = 0.2;
    pState.mu = 0.1;
    PSTATE.pushType<progState_t>(pState);
    
    WEIGHT(log(2.0));

    PC = 3;
    BBLOCK_CALL(condBD_3);
})

MAIN(
    INITBBLOCK(condBD_init, stack_t)
    INITBBLOCK(condBD_1, stack_t)
    INITBBLOCK(condBD_2, stack_t)
    INITBBLOCK(condBD_3, stack_t)
    INITBBLOCK(condBD_4, stack_t)

    SMC(stack_t, NULL)
)

/*
int main() {

    initGen();
    
    // SMCSTART(stack_t, NUM_BBLOCKS)

    INITBBLOCK(condBD_init, stack_t)
    INITBBLOCK(condBD_1, stack_t)
    INITBBLOCK(condBD_2, stack_t)
    INITBBLOCK(condBD_3, stack_t)
    INITBBLOCK(condBD_4, stack_t)

    SMC(stack_t, NULL)

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}
*/
