/*
 * ClaDS2 Model
 * - uses the framework
 * - delayed sampling for lambda0
 *
 */

#include <iostream>
#include <cstring>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "../../models/phylogenetics/tree-utils/tree_utils.cuh"
#include "utils/math.cuh"
#include "utils/stack.cuh"
#include "dists/delayed.cuh"

//typedef bisse32_tree_t tree_t;
//typedef primate_tree_t tree_t;
//typedef moth_div_tree_t tree_t;
typedef BC7_tree_t tree_t;
//typedef Alcedinidae_tree_t tree_t;

// Test settings
floating_t kLambda  = 1.0;
floating_t thetaLambda = 1.0;
floating_t epsMin = 0.0;
floating_t epsMax = 1.0;
floating_t a = 1.0;
floating_t b = 0.2;

floating_t rho      = 0.4891304347826087;


#include "../../models/phylogenetics/clads2/clads2-d-λ.cuh"


MAIN({

    ADD_BBLOCK(simClaDS2);
    ADD_BBLOCK(simTree);
    ADD_BBLOCK(conditionOnDetection);
    ADD_BBLOCK(sampleFinalLambda);
    //SMC(saveResults);
    SMC(NULL)
})
 