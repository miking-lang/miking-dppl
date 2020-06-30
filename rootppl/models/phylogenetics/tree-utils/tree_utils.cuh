#ifndef CRBDUTILS_INCLUDED
#define CRBDUTILS_INCLUDED

#include <math.h>
#include "trees.cuh"

HOST DEV int countLeaves(const int* leftIdx, const int* rightIdx, int size) {
    int numLeaves = 0;
    for(int i = 0; i < size; i++) {
        if(leftIdx[i] == -1 && rightIdx[i] == -1)
            numLeaves++;
    }
    return numLeaves;
}


HOST DEV double lnFactorial(int n) {
    if(n == 1) {
        return 0.0;
    } else {
        return log(static_cast<double>(n)) + lnFactorial(n-1);
    }
}

#endif


