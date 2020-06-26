#include <iostream>
#include <cstring>
#include "../../../Inference/Smc/smc.cuh"
#include "../../../Inference/Smc/smcImpl.cuh"
#include "../../../Utils/distributions.cuh"
#include "../TreeUtils/treeUtils.cuh"

// Uses precomputed next state, and no stack

// nvcc -arch=sm_61 -rdc=true Src/Models/Phylogenetics/CBD/cbdNestedInf.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)

floating_t corrFactor;

void initCBD() {

    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )

    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate

    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, NUM_NODES);
    corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);

    COPY_DATA_GPU(tree, tree_t, 1)
    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)

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

    floating_t t = BBLOCK_CALL(exponential, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return;
    
    if(BBLOCK_CALL(survival, currentTime)) {
        WEIGHT(-INFINITY);
        return;
    }

    WEIGHT(log(2.0)); // was previously done above survival call, no reason to do it before though (unless resample occurrs there)
    
    BBLOCK_CALL(simBranch, currentTime, stopTime);

}, void, floating_t startTime, floating_t stopTime)


BBLOCK(condBD, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    if(treeIdx == -1) {
        PC = 3;
        RESAMPLE = false;
        return;
    }

    // MÅSTE JAG VIKTA EFTER FÖRSTA BRANCHEN AV ROTEN ÄR KLAR?
    if(treeIdx == 2) // Denna är menad att vikta endast om det är ett stalked tree? Troligen en aning fel atm
        WEIGHT(log(*(DATA_POINTER(lambda)))); 
    

    int indexParent = treeP->idxParent[treeIdx];

    WEIGHT(- (*DATA_POINTER(mu)) * (treeP->ages[indexParent] - treeP->ages[treeIdx]));
    
    PSTATE.treeIdx = treeP->idxNext[treeIdx];

    BBLOCK_CALL(simBranch, treeP->ages[indexParent], treeP->ages[treeIdx]);

    if(treeP->idxLeft[treeIdx] != -1) { // If left branch exists, so does right..
        
        WEIGHT(log(*DATA_POINTER(lambda)));

    }

    // PC++;
    RESAMPLE = true;

})


BBLOCK(cbd, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    PC++;
    RESAMPLE = false;
})


BBLOCK(condSurvival, {

    // BBLOCK_CALL(survival, currentTime);

})

STATUSFUNC({
    
})


int main() {

    initGen();
    initCBD();
    

    MAINSTART()

    INITBBLOCK(cbd)
    INITBBLOCK(condBD)
    INITBBLOCK(condSurvival)

    MAINEND()

    res += corrFactor;

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}
