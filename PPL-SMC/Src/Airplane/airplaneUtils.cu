
#include <math.h>
#include <iostream>
#include "../Smc/smc.cuh"
#include "airplane.cuh"


floating_t mapLookup(floating_t x) {
    if(x > 35 && x < 80)
        return 0;

    floating_t c1 = x < 20 || x > 60 ? 0.5 : 1.5;
    floating_t c2 = x < 45 || x > 90 ? 1 : 2;

    return sin(x / 4.0) / c1 + x / (7 * c2) - (x * x) / 400.0 + (x * x * x) / 100000.0;
}

void initMap(floating_t* mapApprox) {
    for (int x = 0; x < MAP_SIZE; x++) {
        mapApprox[x] = ALTITUDE - mapLookup(x);
    }
}

__host__ floating_t mapLookupApprox(floating_t* mapApprox, floating_t x) {
    int f = floor(x);
    int c = ceil(x);
    if(c >= MAP_SIZE || f < 0)
        return 999999;
    return (mapApprox[f] + mapApprox[c]) / 2.0;
}

__device__ floating_t mapLookupApproxDev(floating_t* mapApprox, floating_t x) {
    int f = floor(x);
    int c = ceil(x);
    if(c >= MAP_SIZE || f < 0)
        return 999999;
    floating_t toRet = (mapApprox[f] + mapApprox[c]) / 2.0;
    return toRet;
}

void printArray(floating_t* arr, int length) {
	cout << "[ ";
	for (int i = 0; i < length; i++)
		cout << arr[i] << " ";

	cout << "]" << endl;
}

__host__ __device__
floating_t normalPDFObs(floating_t x, floating_t mean) {
    return exp(-pow(x - mean, 2) / (TWO_OBS_STD_SQUARED)) / (SQRT_TWO_PI_OBS_STD);
}
