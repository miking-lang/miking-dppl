#ifndef SMC_INCLUDED
#define SMC_INCLUDED

//#define GPU

#include <cstddef>
#ifdef GPU
#include <curand_kernel.h>
#endif

using namespace std;

const int NUM_PARTICLES = 1 << 4;

const int NUM_THREADS_PER_BLOCK = 128;
const int NUM_BLOCKS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

typedef double floating_t;

template <typename T>
struct particles_t {
    // void* progState;
    T progStates[NUM_PARTICLES];
    #ifdef GPU
    curandState randStates[NUM_PARTICLES];
    #endif
    int pcs[NUM_PARTICLES];
    floating_t weights[NUM_PARTICLES];
};

template <typename T>
using pplFunc_t = void (*)(particles_t<T>*, int, int);

template <typename T>
using statusFunc_t = void (*)(particles_t<T>*, int);

#endif