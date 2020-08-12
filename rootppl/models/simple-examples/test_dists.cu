/*
 * This model tests most distributions. 
 * NOTE: This model seems to be broken, fix the comma separator problem again? Related to new way of compiling or new c++ version?
 */

#include <stdio.h>

#include "inference/smc/smc.cuh"

// const int n = 3;
struct progState_t {
    floating_t x;
    // floating_t x[n];
};

#define NUM_BBLOCKS 1
INIT_MODEL(progState_t, NUM_BBLOCKS)


BBLOCK(test, progState_t, {

    floating_t xLcl = 0;

    //int intSample = SAMPLE(randomInteger, 100);
    // PSTATE.x = 0;
    //for(int t = 0; t < intSample; t++) {
    //    xLcl += SAMPLE(uniform, 0, 1);
    //}
    
    
    printf("uniform: %f\n", SAMPLE(uniform, 0, 1));
    printf("normal: %f\n", SAMPLE(normal, 0, 1));
    printf("exponential: %f\n", SAMPLE(exponential, 3));
    
    for(int t = 0; t < 1; t++) {
        // PSTATE.x += gamma(randState, 0.1, 2);
        xLcl += SAMPLE(normal, 0.1, 2);
        // xLcl += normal(randState, 0.1, 2);
        // xLcl += uniformRef(randStateLcl, 0.3, 7.6);
    }
    
    printf("gamma: %f\n", SAMPLE(gamma, 1, 2));
    printf("bernoulli: %d\n", SAMPLE(bernoulli, 0.3));
    printf("poisson: %d\n", SAMPLE(poisson, 5.3));

    floating_t res[2];
    floating_t alpha[2] = {1 CMA 5.3};
    SAMPLE(dirichlet, alpha, 2, res);
    printArrayF(res, 2);

    floating_t dist[2] = {0.3 CMA 0.7};
    int idx = SAMPLE(discrete, dist, 2);
    printf("discrete: %d\n", idx);
    
    floating_t vals[2] = {3.2 CMA 77};
    floating_t val = SAMPLE(categorical<floating_t>, dist, 2, vals);
    printf("categorical: %f\n", val);

    printf("randInt: %d\n", SAMPLE(randomInteger, 1000));

    printf("beta: %f\n", SAMPLE(beta, 3, 5.5));

    printf("binomial: %d\n", SAMPLE(binomial, 0.7, 100));

    printf("cauchy: %f\n", SAMPLE(cauchy, 7, 0.3));

    printf("laplace: %f\n", SAMPLE(laplace, 78, 0.7));

    int resI[3];
    floating_t dist2[3] = {0.1 CMA 0.2 CMA 0.7};

    SAMPLE(multinomial, dist2, 1000, 3, resI);
    printArrayI(resI, 3);

    floating_t mu[3] = {4 CMA 5 CMA 6};
    floating_t sigma[3] = {1 CMA 2 CMA 3};
    floating_t resF[3];
    SAMPLE(diagCovNormal, mu, sigma, 3, resF);
    printArrayF(resF, 3);

    SAMPLE(multivariateStandardNormal, 3, resF);
    printArrayF(resF, 3);

    floating_t cov[3][3] = {
        {2 CMA 1 CMA 0} CMA
        {1 CMA 3 CMA 2} CMA
        {0 CMA 2 CMA 5}
    };
    SAMPLE(multivariateNormal<3>, mu, cov, resF);
    printArrayF(resF, 3);

    // PSTATE.x = xLcl;
    PC = 1;
})

CALLBACK(resFunc, {
    
    //floating_t sum = 0;
    //for(int i = 0; i < NUM_PARTICLES; i++) {
        //sum += PSTATE.x;
        //printf("dist[%d]: %f\n", i, PSTATE.x);
        // printArray(PSTATE.x, n);
        //printf("x: %d\n", PSTATE.x);
    //}
    //printf("avg: %f\n", sum / NUM_PARTICLES);
    
    
})


MAIN(
    ADD_BBLOCK(test)

    SMC(resFunc)
)

