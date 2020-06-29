#include <iostream>
#include <cstring>
#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "../../../Utils/distributions.cuh"
#include "../TreeUtils/treeUtils.cuh"


// nvcc -arch=sm_75 -rdc=true Src/Models/Phylogenetics/CBD/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)
BBLOCK_DATA(corrFactor, floating_t, 1)


void initCBD() {

    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )

    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate

    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, NUM_NODES);
    *corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);

    COPY_DATA_GPU(tree, tree_t, 1)
    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)
    COPY_DATA_GPU(corrFactor, floating_t, 1)

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
    
    WEIGHT(log(2.0));
    
    if(BBLOCK_CALL(survival, currentTime)) {
        WEIGHT(-INFINITY);
        return;
    }
    
    BBLOCK_CALL(simBranch, currentTime, stopTime);

}, void, floating_t startTime, floating_t stopTime)


BBLOCK(condBD2, {

    treeLoc_t loc = PSTATE.stack.pop();
    int treeIdx = loc.treeIdx;

    tree_t* treeP = DATA_POINTER(tree);

    BBLOCK_CALL(simBranch, treeP->ages[loc.parentIdx], treeP->ages[treeIdx]);

    // match tree with...
    if(treeP->idxLeft[treeIdx] != -1) { // If left branch exists, so does right..
        WEIGHT(log(2.0 * (*DATA_POINTER(lambda))));
        // WEIGHT(log(*DATA_POINTER(lambda)));

        treeLoc_t locR;
        locR.treeIdx = treeP->idxRight[treeIdx];
        locR.parentIdx = treeIdx;
        PSTATE.stack.push(locR);

        treeLoc_t locL;
        locL.treeIdx = treeP->idxLeft[treeIdx];
        locL.parentIdx = treeIdx;
        PSTATE.stack.push(locL);

        //BBLOCK_CALL(condBD, treeP->idxLeft[treeIdx], treeIdx);
        //BBLOCK_CALL(condBD, treeP->idxRight[treeIdx], treeIdx);

    }

    PC--;
    RESAMPLE = true;

})


BBLOCK(condBD1, {


    if(PSTATE.stack.stackPointer == 0) {
        PC = 3;
        return;
    }

    treeLoc_t loc = PSTATE.stack.peek();

    // MÅSTE JAG VIKTA EFTER FÖRSTA BRANCHEN AV ROTEN ÄR KLAR?
    if(loc.treeIdx == 2)
        WEIGHT(log(2.0)); 
    

    tree_t* treeP = DATA_POINTER(tree);

    WEIGHT(- (*DATA_POINTER(mu)) * (treeP->ages[loc.parentIdx] - treeP->ages[loc.treeIdx]));


    PC++;
    RESAMPLE = true;

})


BBLOCK(cbd, {

    tree_t* treeP = DATA_POINTER(tree);

    treeLoc_t locR;
    locR.treeIdx = treeP->idxRight[ROOT_IDX];
    locR.parentIdx = ROOT_IDX;
    PSTATE.stack.push(locR);
    
    treeLoc_t locL;
    locL.treeIdx = treeP->idxLeft[ROOT_IDX];
    locL.parentIdx = ROOT_IDX;
    PSTATE.stack.push(locL);


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

BBLOCK(nop, {
    //WEIGHT(*(DATA_POINTER(corrFactor)));
    //printf("corrf: %f\n", corrFactor);
    PC++;
    RESAMPLE = true;
})

STATUSFUNC({
    // cout << "lol" << endl;

    /*
    double sum = 0;
    int numNonInf = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        if(PWEIGHT != -INFINITY) {
            sum += PWEIGHT;
            numNonInf++;
        }
    }

    if(numNonInf == 0)
        cout << "Average weight: " << sum / numNonInf << ", #nonInf: " << numNonInf << endl;
    */
    
})


int main() {

    initGen();
    initCBD();
    

    MAINSTART()

    INITBBLOCK(cbd)
    INITBBLOCK(condBD1)
    INITBBLOCK(condBD2)
    INITBBLOCK(nop)

    MAINEND()
}
