#ifndef AIRPLANE_UTILS_INCLUDED
#define AIRPLANE_UTILS_INCLUDED


#include <math.h>
#include <iostream>
#include "../Smc/smc.cuh"
#include "airplane.cuh"

default_random_engine generator;


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

HOST DEV
floating_t mapLookupApprox(floating_t* mapApprox, floating_t x) {
    int f = floor(x);
    int c = ceil(x);
    if(c >= MAP_SIZE || f < 0)
        return 999999;
    return (mapApprox[f] + mapApprox[c]) / 2.0;
}

void initObservations(floating_t* planeX, floating_t* planeObs, floating_t* mapApprox) {
    generator.seed(time(NULL));
    for (int t = 0; t < TIME_STEPS; t++) {
        if(t == 0) {
            planeX[t] = STARTING_POINT;
        } else {
            normal_distribution<floating_t> transDist(planeX[t-1] + VELOCITY, TRANSITION_STD);
            planeX[t] = transDist(generator);
        }
        normal_distribution<floating_t> obsDist(mapLookupApprox(mapApprox, planeX[t]), OBSERVATION_STD);
        planeObs[t] = obsDist(generator);
    }
}


void printArray(floating_t* arr, int length) {
	cout << "[ ";
	for (int i = 0; i < length; i++)
		cout << arr[i] << " ";

	cout << "]" << endl;
}

/*__host__ __device__
floating_t normalPDFObs(floating_t x, floating_t mean) {
    return exp(-pow(x - mean, 2) / (TWO_OBS_STD_SQUARED)) / (SQRT_TWO_PI_OBS_STD);
}*/

HOST DEV
floating_t logNormalPDFObs(floating_t x, floating_t mean) {
    return log(exp(-pow(x - mean, 2) / (TWO_OBS_STD_SQUARED)) / (SQRT_TWO_PI_OBS_STD));
}

#endif