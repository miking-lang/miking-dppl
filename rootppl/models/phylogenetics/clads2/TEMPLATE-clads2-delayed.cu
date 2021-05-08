/*
 * ClaDS2 Model
 * - uses the framework
 * - delayed sampling for lambda0
 *
 */

#include <iostream>
#include <cstring>
#include <cassert>
#include <string>
#include <fstream>
#include <algorithm>
#include <random>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"
#include "utils/stack.cuh"
#include "dists/delayed.cuh"

//typedef bisse32_tree_t tree_t;
//typedef toy_tree_t tree_t;
//typedef primate_tree_t tree_t;
//typedef moth_div_tree_t tree_t;
typedef Accipitridae_tree_t tree_t;
//typedef Alcedinidae_tree_t tree_t;
 
// Test settings

//floating_t rho      = 0.7142857142857143;
const floating_t rho = 1.0;

const floating_t k = 1;
const floating_t theta = 1;
const floating_t kMu = 1;
const floating_t thetaMu = 0.5;

const floating_t m0 = 0;
const floating_t v = 1;
const floating_t a = 1.0;
const floating_t b = 0.2;

std::string analysisName = "Bisse32";

#include "../clads2/clads2-delayed.cuh"



MAIN({

    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    ADD_BBLOCK(conditionOnDetection);
    ADD_BBLOCK(justResample);
    ADD_BBLOCK(sampleFinalLambda);
    //SMC(saveResults);
    SMC(NULL)
})
 