#include "math.h"
#include "../utils.h"
#include "utilsGPU.cuh"

__device__ floating_t mapLookupApproxDev(floating_t x, floating_t* mapApproxDev) {
    int f = floor(x);
    int c = ceil(x);
    if(c >= MAP_SIZE || f < 0)
        return 999999;
    return (mapApproxDev[f] + mapApproxDev[c]) / 2.0;
}

__device__ floating_t normalPDFObsDev(floating_t x, floating_t mean) {
    return exp(-pow(x - mean, 2) / (TWO_OBS_STD_SQUARED)) / (SQRT_TWO_PI_OBS_STD);
}

__device__ floating_t logNormalPDFObsDev(floating_t x, floating_t mean) {
    return -log(SQRT_TWO_PI_OBS_STD) - (pow(x - mean, 2) / TWO_OBS_STD_SQUARED); // should be compile-time optimized (log)
}
