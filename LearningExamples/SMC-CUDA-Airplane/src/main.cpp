#include <iostream>
#include <random>
#include "utils.h"
#include "sequential.h"
#include "cuda/parallel.h"

using namespace std;

// Compile: nvcc -arch=sm_75 -rdc=true src/*.cpp src/cuda/*.cu -o smc.exe -lcudadevrt -std=c++11 -O3

floating_t* planeX;
floating_t* planeObs;

void init() {
    initMap();
    default_random_engine generator;
    // generator.seed(time(NULL));

    planeX = new floating_t[TIME_STEPS];
    planeObs = new floating_t[TIME_STEPS];
    
    for (int t = 0; t < TIME_STEPS; t++) {
        if(t == 0) {
            planeX[t] = STARTING_POINT;
        } else {
            normal_distribution<floating_t> dist(planeX[t-1] + VELOCITY, TRANSITION_STD);
            planeX[t] = dist(generator);
        }
        normal_distribution<floating_t> dist(mapLookupApprox(planeX[t]), OBSERVATION_STD);
        planeObs[t] = dist(generator);
    }
    //printArray(planeX, TIME_STEPS);
    //printArray(planeObs, TIME_STEPS);
    //cout << endl;
}

double bench(bool gpu) {
	startTimer();
	if(gpu)
		smc(planeX, planeObs);
	else
		smcSeq(planeX, planeObs);

	double duration = getTimeElapsed();
	return duration;
}

double benchSequence(bool gpu, int numTrials) {
    double minTime = 99999999;
    for(int i = 0; i < numTrials; i++) {
        double duration = bench(gpu);
        minTime = min(minTime, duration);
        
        if(gpu)
            cout << "GPU: ";
        else
            cout << "CPU: ";

        cout << "Duration: " << duration << " seconds" << endl;
        
    }
    cout << "Duration (min): " << minTime << " seconds" << endl;
    return minTime;
}


int main(int argc, char** argv) {
    if (argc < 3) {
		cout << "Enter arguments: runOnGPU(0 for CPU, 1 for GPU, 2 for both) numTrials" << endl;
		return 0;
	}
	int gpu = atoi(argv[1]);
    int numTrials = atoi(argv[2]);

    init();
    
    if(gpu == 2) {
        double minCPU = benchSequence(0, numTrials);
        double minGPU = benchSequence(1, numTrials);
        cout << "Speedup: " << minCPU / minGPU << endl;
    } else {
        benchSequence(gpu, numTrials);
    }

    destMap();
    delete[] planeX;
    delete[] planeObs;
    if(gpu)
        freeRandStates();
}
