#include <iostream>
#include <random>
#include <curand_kernel.h>
#include <list>

#include "../Smc/smc.cuh"

using namespace std;


default_random_engine generator;

uniform_real_distribution<floating_t> uDist(0.0, 1.0);

void initGen() {
    generator.seed(time(NULL));
}

int flip(double p = 0.5) {
    return uDist(generator) < p ? 1 : 0;
}

#ifdef GPU
__device__ int flipDev(curandState* randState, double p = 0.5) {
    return curand_uniform(randState) < p ? 1 : 0;
}
#endif

void printList(list<bool> l) {
    cout << "[ \n";
    for(bool b : l) {
        cout << b << " ";
    }
    cout << "]\n" << endl;
}
