
/*
 * File math.cuh contains mathematical helper functions. 
 */


#include <math.h>

#include "math.cuh"

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
