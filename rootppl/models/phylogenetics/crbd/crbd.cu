/*
 * File crbd.cu defines an older variant of the constant 
 * rate birth death model with nested inference. 
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the originial model. 
 */


#include <iostream>
#include <cstring>
#include "inference/smc/smc_impl.cuh"
#include "../tree-utils/tree_utils.cuh"


/*
Compile commands:

nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/phylogenetics/crbd/crbd.cu -o smc.exe -std=c++11 -O3
g++ -x c++ -I . models/phylogenetics/crbd/crbd.cu -o smc.exe -std=c++11 -O3
*/


typedef short treeIdx_t;
struct progState_t {
    treeIdx_t treeIdx;
};

struct nestedProgState_t {
    bool extinct;
};
typedef double return_t;


#define NUM_BBLOCKS 2
INIT_MODEL(progState_t, NUM_BBLOCKS)
#define NUM_BBLOCKS_NESTED 1

typedef primate_tree_t tree_t;
BBLOCK_DATA(tree, tree_t, 1)

BBLOCK_DATA(lambda, floating_t, 1) // prolly faster to just pass these as args... they should be generated in particle anyway?
BBLOCK_DATA(mu, floating_t, 1)

floating_t corrFactor;


void initCBD() {
    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )
    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate

    int numLeaves = countLeaves(tree->idxLeft, tree->idxRight, tree->NUM_NODES);
    corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);

    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)

}

BBLOCK_HELPER(goesExtinct, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    floating_t muLocal = *DATA_POINTER(mu);

    floating_t t = SAMPLE(exponential, lambdaLocal + muLocal);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return false;
    
    bool speciation = SAMPLE(bernoulli, lambdaLocal / (lambdaLocal + muLocal));
    if (! speciation)
        return true;
    else 
        return BBLOCK_CALL(goesExtinct<T>, currentTime) && BBLOCK_CALL(goesExtinct<T>, currentTime);

}, bool, floating_t startTime)



BBLOCK(goesExtinctBblock, nestedProgState_t, {
    tree_t* treeP = DATA_POINTER(tree);
    double age = treeP->ages[ROOT_IDX];
    
    PSTATE.extinct = BBLOCK_CALL(goesExtinct<nestedProgState_t>, age);
    PC++;
    // RESAMPLE = true;
})


BBLOCK_HELPER(simBranch, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);

    floating_t t = SAMPLE(exponential, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime);
    if(! sideExtinction)
        return -INFINITY;

    // WEIGHT(log(2.0)); // was previously done above survival call, no reason to do it before though (unless resample occurrs there)
    
    return BBLOCK_CALL(simBranch<T>, currentTime, stopTime) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime)



BBLOCK(simTree, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);
    int treeIdx = PSTATE.treeIdx;

    if(treeIdx == -1) {
        PC = 2;
        // RESAMPLE = false;
        return;
    }

    int indexParent = treeP->idxParent[treeIdx];

    // Weight here only on once, if root has right child
    /*
    if(treeIdx == 2 && indexParent == ROOT_IDX)
        WEIGHT(log(2.0));
    */
    //WEIGHT(log(*(DATA_POINTER(lambda)))); 

    floating_t parentAge = treeP->ages[indexParent];
    floating_t age = treeP->ages[treeIdx];
    

    floating_t lnProb1 = - (*DATA_POINTER(mu)) * (parentAge - age);

    // Interior if at least one child
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnProb2 = interiorNode ? log(*DATA_POINTER(lambda)) : 0.0;

    floating_t lnProb3 = BBLOCK_CALL(simBranch<progState_t>, parentAge, age);

    /*
    if(treeP->idxLeft[treeIdx] != -1) { // If left branch exists, so does right..
        WEIGHT(log(*DATA_POINTER(lambda)));
    */

    WEIGHT(lnProb1 + lnProb2 + lnProb3);
    PSTATE.treeIdx = treeP->idxNext[treeIdx];
    // RESAMPLE = true;

})

CALLBACK_NESTED(calcResult, nestedProgState_t, {
    int numExtinct = 0;
    for(int i = 0; i < numParticles; i++)
        numExtinct += PSTATE.extinct;

    int numSurvived = numParticles - numExtinct;
    return_t* retP = static_cast<return_t*>(ret);
    *retP = numSurvived / (double)numParticles;
    
}, void* ret)

template <typename T>
DEV T runNestedInference(RAND_STATE_DECLARE int parentIndex) {
    bool parallelExec = false, parallelResampling = false;

    T ret;

    SMC_PREPARE_NESTED(nestedProgState_t, NUM_BBLOCKS_NESTED)

    INIT_BBLOCK_NESTED(goesExtinctBblock, nestedProgState_t)
    
    SMC_NESTED(nestedProgState_t, 100, parallelExec, parallelResampling, parentIndex, calcResult, ret, NULL)

    return ret;
}

// Correction Factor moved out to main
BBLOCK(simCRBD, progState_t, {

    tree_t* treeP = DATA_POINTER(tree);

    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];

    double survivalRate = runNestedInference<double>(RAND_STATE i);

    WEIGHT(-2.0 * log(survivalRate));

    PC++;
    // PC = 2;
    BBLOCK_CALL(simTree);
})


MAIN(
    initCBD();
    
    INIT_BBLOCK(simCRBD, progState_t)
    INIT_BBLOCK(simTree, progState_t)

    SMC(NULL)

    res += corrFactor;
)

