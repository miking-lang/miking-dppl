#include <iostream>
#include <math.h>
#include <random>
#include "utils.h"

using namespace std;

floating_t* mapApprox;

floating_t mapLookup(floating_t x);

struct timespec start, finish;
double elapsed;

void initMap() {
    mapApprox = new floating_t[MAP_SIZE];
    for (int x = 0; x < MAP_SIZE; x++) {
        mapApprox[x] = ALTITUDE - mapLookup(x);
    }
}

void destMap() {
    delete[] mapApprox;
}

floating_t mapLookup(floating_t x) {
    if(x > 35 && x < 80)
        return 0;

    floating_t c1 = x < 20 || x > 60 ? 0.5 : 1.5;
    floating_t c2 = x < 45 || x > 90 ? 1 : 2;

    return sin(x / 4.0) / c1 + x / (7 * c2) - (x * x) / 400.0 + (x * x * x) / 100000.0;
}

floating_t mapLookupApprox(floating_t x) {
    int f = floor(x);
    int c = ceil(x);
    if(c >= MAP_SIZE || f < 0)
        return 999999;
    return (mapApprox[f] + mapApprox[c]) / 2.0;
}

void printArray(floating_t* arr, int length) {
	cout << "[ ";
	for (int i = 0; i < length; i++)
		cout << arr[i] << " ";

	cout << "]" << endl;
}

/*
void printArray(double* arr, int length) {
	cout << "[ ";
	for (int i = 0; i < length; i++)
		cout << arr[i] << " ";

	cout << "]" << endl;
}
*/

floating_t normalPDFObs(floating_t x, floating_t mean) {
    return exp(-pow(x - mean, 2) / (TWO_OBS_STD_SQUARED)) / (SQRT_TWO_PI_OBS_STD);
}

floating_t logNormalPDFObs(floating_t x, floating_t mean) {
    return -log(SQRT_TWO_PI_OBS_STD) - (pow(x - mean, 2) / TWO_OBS_STD_SQUARED); // should be compile-time optimized (log)
}

floating_t maxPDFObs() {
    return 1 / (SQRT_TWO_PI_OBS_STD);
}

void startTimer() {
	clock_gettime(CLOCK_MONOTONIC, &start);
}

double getTimeElapsed() {
	clock_gettime(CLOCK_MONOTONIC, &finish);

	elapsed = (finish.tv_sec - start.tv_sec);
	elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;

	return elapsed;
}

floating_t* getMapApproxArr() {
    return mapApprox;
}

void printStatus(floating_t* x, floating_t* w, floating_t* planeX, int t) {
    int numParticlesClose = 0;
    floating_t minX = 999999;
    floating_t maxX = -1;
    for (int i = 0; i < NUM_PARTICLES; i++) {
        if(abs(x[i] - planeX[t]) < 10)
            numParticlesClose++;
        minX = min(minX, x[i]);
        maxX = max(maxX, x[i]);
    }

    cout << "TimeStep " << t << ", NumClose: " << numParticlesClose << ", MinX: " << minX << ", MaxX: " << maxX << endl;
}

floating_t maxValue(floating_t* arr, int length) {
    floating_t maxVal = std::numeric_limits<floating_t>::lowest();

    for(int i = 0; i < length; i++)
        maxVal = max(maxVal, arr[i]);

    return maxVal;
}
