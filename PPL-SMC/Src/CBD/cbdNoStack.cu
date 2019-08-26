#include <iostream>
#include <cstring>
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "cbd.cuh"
#include "cbdUtils.cuh"

// nvcc -arch=sm_75 -rdc=true Src/CBD/cbdNoStack.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)
// BBLOCK_DATA(corrFactor, floating_t, 1)


void initCBD() {

    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )

    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate

    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, NUM_NODES);
    // *corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    setLogCorrectionFactor(corrFactor);

    COPY_DATA_GPU(tree, tree_t, 1)
    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)
    // COPY_DATA_GPU(corrFactor, floating_t, 1)

}

BBLOCK_HELPER(survival, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    floating_t muLocal = *DATA_POINTER(mu);

    floating_t t = BBLOCK_CALL(exponential, lambdaLocal + muLocal);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return true;
    else {
        bool speciation = BBLOCK_CALL(flipK, lambdaLocal / (lambdaLocal + muLocal));
        if (speciation)
            return BBLOCK_CALL(survival, currentTime) || BBLOCK_CALL(survival, currentTime);
        else
            return false;
    }

}, bool, floating_t startTime)


BBLOCK_HELPER(simBranch, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    // floating_t muLocal = *DATA_POINTER(mu);

    floating_t t = BBLOCK_CALL(exponential, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return;
    
    // WEIGHT(log(2.0));
    
    if(BBLOCK_CALL(survival, currentTime)) {
        WEIGHT(-INFINITY);
        return;
    }

    WEIGHT(log(2.0)); // was previously done above survival call, no reason to do it before though (unless resample occurrs there)
    
    BBLOCK_CALL(simBranch, currentTime, stopTime);

}, void, floating_t startTime, floating_t stopTime)


BBLOCK(condBD2, {

    int treeIdx = PSTATE.treeIdx;

    tree_t* treeP = DATA_POINTER(tree);

    int indexParent = treeP->idxParent[treeIdx];

    BBLOCK_CALL(simBranch, treeP->ages[indexParent], treeP->ages[treeIdx]);

    int nextIdx;
    if(treeP->idxLeft[treeIdx] != -1) { // If left branch exists, so does right..
        
        WEIGHT(log(*DATA_POINTER(lambda)));

        nextIdx = treeP->idxLeft[treeIdx]; // Go left if possible

    } else { // Find first possible right turn

        nextIdx = treeIdx;

        while(true) {
            // printf("While..., treeidx: %d, leftIdx: %d, rightIdx: %d, parentIdx: %d\n", treeIdx, treeP->idxLeft[treeIdx], treeP->idxRight[treeIdx], treeP->idxParent[treeIdx]);
            int parentIdx = treeP->idxParent[nextIdx];
            bool wasLeftChild = treeP->idxLeft[parentIdx] == nextIdx;

            if(wasLeftChild) {
                nextIdx = treeP->idxRight[parentIdx];
                break;
            } else {
                nextIdx = parentIdx;
                if(parentIdx == -1) // We are at root and done, let condCBD1 terminate
                    break;
            }
        }
    }

    PSTATE.treeIdx = nextIdx;


    PC--;
    RESAMPLE = true;

})


BBLOCK(condBD1, {

    tree_t* treeP = DATA_POINTER(tree);
    /*
    if(PSTATE.stack.stackPointer == 0) {
        PC = 3;
        return;
    }
    */
    int treeIdx = PSTATE.treeIdx;

    // if(treeIdx == ROOT_IDX && (source == right || treeP->idxRight[treeIdx]) == -1) { // Done
    if(treeIdx == -1) {
        PC = 3;
        RESAMPLE = false;
        return;
    }


    // MÅSTE JAG VIKTA EFTER FÖRSTA BRANCHEN AV ROTEN ÄR KLAR?
    if(treeIdx == 2)
        WEIGHT(log(*(DATA_POINTER(lambda)))); 
    

    int indexParent = treeP->idxParent[treeIdx];

    WEIGHT(- (*DATA_POINTER(mu)) * (treeP->ages[indexParent] - treeP->ages[treeIdx]));


    PC++;
    RESAMPLE = true;

})


BBLOCK(cbd, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    /*
    if(treeP->idxRight[ROOT_IDX] != -1) {
        PSTATE.stack.push(treeP->idxRight[ROOT_IDX]);
    }
    
    PSTATE.stack.push(treeP->idxLeft[ROOT_IDX]);
    */


    //PSTATE.treeIdx = tree->idxLeft[ROOT_IDX];
    //PSTATE.parentIdx = ROOT_IDX;

    // WEIGHT(log(2.0)); 
    // WEIGHT(log(*(DATA_POINTER(lambda)))); 
    // Now assuming that root has 2 children, and weighting before first condBD instead of after. Check if correct?

    /*BBLOCK_CALL(condBD1);
    if(tree->idxRight[ROOT_IDX] != -1) {
        WEIGHT(log(2.0));
        BBLOCK_CALL(condBD, tree->idxRight[ROOT_IDX], ROOT_IDX);
        
    }*/

    PC++;
    RESAMPLE = false;
})


STATUSFUNC({

    /*
    int numInf = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        if(PWEIGHT == -INFINITY)
            numInf++;
    }

    printf("NumNonInf: %d\n", NUM_PARTICLES - numInf);
    */
})


int main() {

    initGen();
    initCBD();
    

    MAINSTART()

    INITBBLOCK(cbd)
    INITBBLOCK(condBD1)
    INITBBLOCK(condBD2)

    MAINEND()
}
