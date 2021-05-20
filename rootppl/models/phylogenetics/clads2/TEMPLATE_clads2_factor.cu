/*
 * File clads2-factor.cu defines the ClaDS2 model
 * - lambda-factorization but not delayed sampling.
 *
 * This model traverses the tree with a pre-computed DFS path (defined by the next 
 * pointer in the tree) that corresponds to the recursive calls in the original model. 
 */


#include <iostream>
#include <cstring>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "../tree-utils/tree_utils.cuh"
#include "utils/math.cuh"
#include "utils/stack.cuh"

//typedef Accipitridae_tree_t tree_t;

typedef bisse32_tree_t tree_t;
//typedef primate_tree_t tree_t;
//typedef moth_div_tree_t tree_t;

// Test settings
const floating_t rho      = 1.0;

const floating_t k = 1.0;
const floating_t theta = 1.0;
const floating_t epsMin = 0.0;
const floating_t epsMax = 1.0;

#include "clads2_factor.cuh"

