#include <iostream>
#include <cstring>
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "cbd.cuh"


// nvcc -arch=sm_61 -rdc=true Src/CBD/*.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU

BBLOCK_DATA(tree, tree_t, 1)
BBLOCK_DATA(lambda, floating_t, 1)
BBLOCK_DATA(mu, floating_t, 1)


void initCBD() {

    // lambda ~ gamma( 1.0, 1.0 )
    // mu     ~ gamma( 1.0, 1.0 )

    *lambda = 0.2; // birth rate
    *mu = 0.1; // death rate

    COPY_DATA_GPU(tree, tree_t, 1)
    COPY_DATA_GPU(lambda, floating_t, 1)
    COPY_DATA_GPU(mu, floating_t, 1)

}

BBLOCK_HELPER(survival, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    floating_t muLocal = *DATA_POINTER(mu);

    floating_t t = exponential(particles, i, lambdaLocal + muLocal);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return true;
    else {
        bool speciation = flipK(particles, i, lambdaLocal / (lambdaLocal + muLocal));
        if (speciation)
            return survival(particles, i, currentTime) || survival(particles, i, currentTime);
        else
            return false;
    }

}, bool, floating_t startTime)


BBLOCK_HELPER(simBranch, {

    floating_t lambdaLocal = *DATA_POINTER(lambda);
    floating_t muLocal = *DATA_POINTER(mu);

    floating_t t = exponential(particles, i, lambdaLocal);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return;
    
    WEIGHT(log(2.0));
    
    floating_t w = survival(particles, i, currentTime) ? -INFINITY : 0;
    WEIGHT(w);
    
    simBranch(currentTime, stopTime);

}, void, floating_t startTime, floating_t stopTime)


BBLOCK_HELPER(condBD, {

    floating_t lambdaLocal = *DATA_POINTER(lambda); // prolly faster to just pass these as args...
    floating_t muLocal = *DATA_POINTER(mu);

    tree_t* tree = DATA_POINTER(tree);
    floating_t parentAge = tree->ages[parentIdx];
    floating_t treeAge = tree->ages[treeIdx];

    WEIGHT(- muLocal * (parentAge - treeAge));

    simBranch(parentAge, treeAge);

    // match tree with...

}, void, int treeIdx, int parentIdx)


BBLOCK(test, {
    
    // PSTATE.res = exponential(particles, i, 2);
    //printf("%f\n", PSTATE.res);

    bool survived = survival(particles, i, 3);
    PSTATE.res = survived;
    // printf("Survived: %s\n", survived ? "true" : "false");

    PC++;
    RESAMPLE = false;
})


STATUSFUNC({

    /*
    double sum = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        sum += PSTATE.res;
    }
    cout << "Average: " << sum / NUM_PARTICLES << endl;
    */

    double sum = 0;
    for(int i = 0; i < NUM_PARTICLES; i++) {
        sum += PSTATE.res;
    }
    cout << "Average: " << sum / NUM_PARTICLES << endl;
})


int main() {

    initGen();
    initCBD();
    

    MAINSTART()

    //INITBBLOCK(particleInit)
    INITBBLOCK(test)

    MAINEND()
}
