/**
 * Immediate sampling template.
 * File clads2.cu defines the ClaDS2 model
 * as defined in WebPPL in the script linked to below. 
 *  https://github.com/phyppl/probabilistic-programming/blob/master/webppl/phywppl/models/clads2.wppl
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */

#include <iostream>
#include <cstring>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "utils/math.cuh"
#include "../tree-utils/tree_utils.cuh"

//typedef bisse32_tree_t tree_t;
//typedef primate_tree_t tree_t;
//typedef moth_div_tree_t tree_t;
typedef Accipitridae_tree_t tree_t;

// Test settings
// #ifndef PRIORS
// #define PRIORS
// const floating_t lambda_0 = 0.2;
// const floating_t alpha    = 1.0;
// const floating_t sigma    = 0.0000001;
// const floating_t epsilon  = 0.5;
// #endif

const floating_t kLambda  = 1.0;
const floating_t thetaLambda = 1.0;
const floating_t epsMin = 0.0;
const floating_t epsMax = 1.0;
const floating_t a = 1.0;
const floating_t b = 0.2;

const floating_t rho      = 0.7142857142857143;

#include "../clads2/clads2.cuh"

MAIN({

    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    ADD_BBLOCK(conditionOnDetection);

    //SMC(saveResults);
    SMC(NULL);
})
 
 
