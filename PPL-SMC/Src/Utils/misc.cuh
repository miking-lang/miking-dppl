#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <iostream>
#include <random>
#include <vector>
#include <cstring>
#include <time.h>

#ifdef GPU
#include <curand_kernel.h>
#endif

#include "../Smc/smc.cuh"

using namespace std;


default_random_engine generatorMisc;

uniform_real_distribution<floating_t> uDistMisc(0.0, 1.0);

void initGen() {
    generatorMisc.seed(time(NULL));
}

template <typename T>
#ifdef GPU
__device__ int flipK(particles_t<T>* particles, int i, double p = 0.5) {
    
    return curand_uniform(&particles->randStates[i]) < p ? 1 : 0;
}

#else

int flipK(particles_t<T>* particles, int i, double p = 0.5) {
    
    return uDistMisc(generatorMisc) < p ? 1 : 0;
}
#endif

int flip(double p = 0.5) {
    return uDistMisc(generatorMisc) < p ? 1 : 0;
}

#ifdef GPU
__device__ int flipDev(curandState* randState, double p = 0.5) {
    return curand_uniform(randState) < p ? 1 : 0;
}
#endif

void printList(vector<bool> l, string title="") {
    if(title.length() > 0)
        cout << title << ": ";
    cout << "[ ";
    for(bool b : l) {
        cout << b << " ";
    }
    cout << "]\n" << endl;
}

#endif