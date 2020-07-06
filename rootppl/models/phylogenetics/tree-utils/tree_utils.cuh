#ifndef TREE_UTILS_INCLUDED
#define TREE_UTILS_INCLUDED

/*
 * File tree_utils.cuh contains helper functions for trees. 
 */

#include "trees.cuh"

HOST DEV int countLeaves(const int* leftIdx, const int* rightIdx, int size) {
    int numLeaves = 0;
    for(int i = 0; i < size; i++) {
        if(leftIdx[i] == -1 && rightIdx[i] == -1)
            numLeaves++;
    }
    return numLeaves;
}

#endif


