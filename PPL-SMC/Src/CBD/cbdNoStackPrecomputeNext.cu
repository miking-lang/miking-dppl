#include <iostream>
#include <cstring>
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "../Utils/array.cuh"
#include "cbd.cuh"
#include "cbdUtils.cuh"

// nvcc -arch=sm_75 -rdc=true Src/CBD/cbdNoStackPrecomputeNext.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)

floating_t corrFactor;

struct nestedProgState_t {
    bool survived;
};
typedef double return_t;

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
        if (speciation) {
            printf("making recursive calls!\n");
            bool tempRes = BBLOCK_CALL(survival<T>, currentTime) || BBLOCK_CALL(survival<T>, currentTime);
            printf("Made recursive calls\n");
            return tempRes;
        } else
            return false;
    }

}, bool, floating_t startTime)


BBLOCK(survivalBblock, nestedProgState_t, {
    printf("Inside survival Bblock nested!\n");
    tree_t* treeP = DATA_POINTER(tree);
    printf("Dereffed tree!\n");
    double age = treeP->ages[ROOT_IDX];
    printf("Fetched age!\n");
    bool tempRes = BBLOCK_CALL(survival<nestedProgState_t>, age);
    printf("returned from bblock call!\n");
    PSTATE.survived = tempRes;
    printf("Wrote result!\n");
    PC++;
    RESAMPLE = true;
})


BBLOCK_HELPER(simBranch, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);

    floating_t t = BBLOCK_CALL(exponential, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return;
    
    if(BBLOCK_CALL(survival<T>, currentTime)) {
        WEIGHT(-INFINITY);
        return;
    }

    WEIGHT(log(2.0)); // was previously done above survival call, no reason to do it before though (unless resample occurrs there)
    
    BBLOCK_CALL(simBranch<T>, currentTime, stopTime);

}, void, floating_t startTime, floating_t stopTime)



BBLOCK(condBD, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    if(treeIdx == -1) {
        PC = 2;
        RESAMPLE = false;
        return;
    }

    int indexParent = treeP->idxParent[treeIdx];

    // Weight here only on once, if root has right child
    if(treeIdx == 2 && indexParent == ROOT_IDX)
        WEIGHT(log(2.0));
    //WEIGHT(log(*(DATA_POINTER(lambda)))); 
    

    WEIGHT(- (*DATA_POINTER(mu)) * (treeP->ages[indexParent] - treeP->ages[treeIdx]));
    
    PSTATE.treeIdx = treeP->idxNext[treeIdx];

    BBLOCK_CALL(simBranch<progState_t>, treeP->ages[indexParent], treeP->ages[treeIdx]);

    if(treeP->idxLeft[treeIdx] != -1) { // If left branch exists, so does right..
        
        WEIGHT(log(*DATA_POINTER(lambda)));

    }

    // PC++;
    RESAMPLE = true;

})

CALLBACK(calcResult, nestedProgState_t, {
    int numSurvived = 0;
    for(int i = 0; i < NUM_PARTICLES; i++)
        numSurvived += PSTATE.survived;

    return_t* retP = static_cast<return_t*>(ret);
    *retP = numSurvived / (double)NUM_PARTICLES;
    
}, void* ret)

template <typename T>
DEV T runNestedInference() {
    bool parallel = true;

    T ret;

    SMCSTART(nestedProgState_t)

    INITBBLOCK_NESTED(survivalBblock, nestedProgState_t)
    
    SMCEND_NESTED(nestedProgState_t, calcResult, ret, parallel)

    return ret;
}

BBLOCK(cbd, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    
    double survivalRate = runNestedInference<double>();

    WEIGHT(-survivalRate);

    // PC++;
    PC = 2;
    RESAMPLE = false;
})


STATUSFUNC({
    
})


int main() {

    initGen();
    initCBD();
    

    SMCSTART(progState_t)

    INITBBLOCK(cbd, progState_t)
    INITBBLOCK(condBD, progState_t)

    SMCEND(progState_t)

    res += corrFactor;

    cout << "log(MarginalLikelihood) = " << res << endl;

    return 0;
}

