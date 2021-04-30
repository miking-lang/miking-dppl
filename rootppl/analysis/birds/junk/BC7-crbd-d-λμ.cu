/*
 * Delayed version of CRBD model.
 * Both parameters are delayed.
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */

#include <stdio.h>
#include <string>
#include <fstream>
#include <math.h>

#include "inference/smc/smc.cuh"
#include "../../models/phylogenetics/tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

// typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
// typedef moth_div_tree_t tree_t;
typedef BC7_tree_t tree_t;
 
floating_t kMu = 1;
floating_t thetaMu = 0.5;
floating_t kLambda = 1;
floating_t thetaLambda = 1.0;


floating_t rhoConst      = 0.4891304347826087;


#include "../../models/phylogenetics/crbd/crbd-d-λμ.cuh"

MAIN(    
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    //ADD_BBLOCK(survivorshipBias)
    ADD_BBLOCK(sampleFinalLambda)
    
    SMC(saveResults)
)
  
