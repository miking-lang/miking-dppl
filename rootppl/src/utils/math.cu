
/*
 * File math.cuh contains mathematical helper functions.
 */


#include <math.h>
// V: needed for assert
#include <cassert>

#include "math.cuh"
#include "../dists/scores.cuh"

HOST DEV double lnFactorial(int n) {
    /*if(n == 1) {
        return 0.0;
    } else {
        return log(static_cast<double>(n)) + lnFactorial(n-1);
    }*/
    double res = 0;
    while(n > 1) {
        res += log(static_cast<double>(n));
        n--;
    }
    return res;
}

HOST DEV floating_t maxNaive(const floating_t* arr, const int n) {
    floating_t maxVal = -INFINITY;
    for(int i = 0; i < n; i++) {
        maxVal = arr[i] > maxVal ? arr[i] : maxVal;
    }
    return maxVal;
}

// V: Logarithm of the binomial coefficient.
// Based on: https://github.com/lawmurray/Birch/blob/master/libraries/Standard/src/math/special.birch
HOST DEV floating_t lchoose(const int x, const int y) {
    assert(0 <= x);
    assert(0 <= y);
    assert(x >= y);

    if (y == 0) {
        return 0.0;
    } else {
        // Birch comment: see Boost binomial_coefficient function for this implementation
        // V: replaced Real(y) and so one with static casts like in lnFactorial
        return -log(static_cast<double>(y))
            - logBeta(static_cast<double>(y), static_cast<double>(x - y + 1));
    }
}
