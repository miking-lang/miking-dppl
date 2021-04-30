/*
 * File crbd_webppl.cu defines the constant rate birth death model 
 * as defined in WebPPL in the script linked to below. 
 * 
 * https://github.com/phyppl/probabilistic-programming/blob/master/webppl/phywppl/models/crbd.wppl
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */

#include <stdio.h>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "../../models/phylogenetics/tree-utils/tree_utils.cuh"
#include "utils/math.cuh"

//typedef bisse32_tree_t tree_t;
// typedef primate_tree_t tree_t;
// typedef moth_div_tree_t tree_t;
typedef P20b_tree_t tree_t;

floating_t k = 1.0;
floating_t theta = 1.0;
floating_t epsMin = 0.0;
floating_t epsMax = 1.0;

floating_t rhoConst      = 0.7668711656441718;

//floating_t mu = 0.1;

#include "../../models/phylogenetics/crbd/crbd.cuh"

MAIN(
    ADD_BBLOCK(simCRBD)
    ADD_BBLOCK(simTree)
    ADD_BBLOCK(survivorshipBias)

    SMC(saveResults)
)

