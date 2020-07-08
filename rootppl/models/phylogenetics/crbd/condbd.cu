/*
 * File condbd.cu defines the example model used when discussing the compilation from TreePPL with a general explicit stack. 
 * This model traverses the tree with a DFS path that corresponds to the recursive calls in the original model. 
 */


#include "inference/smc/smc_impl.cuh"

#include "../tree-utils/tree_utils.cuh"
#include "stack.cuh"

/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/crbd/condbd.cu -o smc.exe -std=c++11 -O3 
g++ -x c++ -I . models/phylogenetics/crbd/condbd.cu -o smc.exe -std=c++11 -O3
*/

#define NUM_BBLOCKS 5
INIT_MODEL(pStack_t, NUM_BBLOCKS)

typedef primate_tree_t tree_t;
BBLOCK_DATA(tree, tree_t, 1)

struct progState_t {
    floating_t lambda;
    floating_t mu;
    int treeIdx;
    int parentIdx;
};

/* Simulations */

BBLOCK_HELPER(goesExtinct, {

    floating_t t = SAMPLE(exponential, lambda + mu);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return false;
    
    bool speciation = SAMPLE(bernoulli, lambda / (lambda + mu));
    if (! speciation)
        return true;
    else 
        return BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu) && BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu);

}, bool, floating_t startTime, floating_t lambda, floating_t mu)


BBLOCK_HELPER(simBranch, {

    floating_t t = SAMPLE(exponential, lambda);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu);
    if(! sideExtinction)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lambda, mu) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime, floating_t lambda, floating_t mu)

/**/

BBLOCK_HELPER(pushChild, {

    progState_t pState;
    pState.parentIdx = parent.treeIdx;
    pState.treeIdx = childIdx;
    pState.lambda = parent.lambda;
    pState.mu = parent.mu;
    PSTATE.pushType(pState);

}, void, progState_t parent, int childIdx)


BBLOCK(condBD_1, {

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

BBLOCK(condBD_2, {
    
    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    floating_t parentAge = treeP->ages[pState.parentIdx];
    floating_t treeAge = treeP->ages[pState.treeIdx];
    floating_t w = BBLOCK_CALL(simBranch<pStack_t>, parentAge, treeAge, pState.lambda, pState.mu);
    WEIGHT(w);
    

    if(treeP->idxLeft[pState.treeIdx] != -1) { // Interior node, keep DFSing
        WEIGHT(log(2.0 * pState.lambda));
        PC++; // condBD_3
        PSTATE.pushType<progState_t>(pState);
        // Resamples here
    } else {
        PC = PSTATE.pop();
        if (PC < NUM_BBLOCKS) { // Does not resample here
            // pplFunc_t<pStack_t> funcToCall = DATA_POINTER(bblocksArr)[PC];
            // BBLOCK_CALL(funcToCall, NULL);
            BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
            return;
        }
    }
})

// Keep DFSing left
BBLOCK(condBD_3, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    PSTATE.pushType<progState_t>(pState); // Need to use this state again when exploring right sibling
    PSTATE.push(4); // PC = condBD_4

    int leftIdx = treeP->idxLeft[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState, leftIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

// Keep DFSing right
BBLOCK(condBD_4, {

    tree_t* treeP = DATA_POINTER(tree);

    progState_t pState;
    PSTATE.popType<progState_t>(&pState);

    // What should the return address be here? Do I really need one?

    int rightIdx = treeP->idxRight[pState.treeIdx];
    BBLOCK_CALL(pushChild, pState, rightIdx);

    PC = 1;
    BBLOCK_CALL(condBD_1);
})

BBLOCK(condBD_init, {

    //PSTATE.arr = DATA_POINTER(globalStack);
    //PSTATE.stackPointer = i;
    //printf("arr: %p, sp: %d\n", PSTATE.arr, PSTATE.stackPointer);

    //printf("sp: %d\n", PSTATE.stackPointer);
    PSTATE.push(NUM_BBLOCKS); // Go to PC=5 when top-level condBD_1 is done (terminate)

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

    ADD_BBLOCK(condBD_init)
    ADD_BBLOCK(condBD_1)
    ADD_BBLOCK(condBD_2)
    ADD_BBLOCK(condBD_3)
    ADD_BBLOCK(condBD_4)

    SMC(NULL)
)
