#ifndef MCMC_INCLUDED
#define MCMC_INCLUDED

#include <cstddef>
#include "../../macros.cuh"
#ifdef GPU
#include "../../Utils/cudaErrorUtils.cu"
#include <curand_kernel.h>
#endif

using namespace std;

const int NUM_ITERATIONS = 1;


template <class T>
struct samplesMcmc_t {
    // T progState;
    T** traces;
    int* traceIdxs;
    // int pc;
    floating_t* weights;
    
    // floating_t probability;
};

/*template <typename T>
struct particles_t {

    T* progStates;
    #ifdef GPU
    curandState* randStates;
    #endif
    floating_t* weights;
};*/


// BBLOCK function
// Share with SMC?
template <typename T>
using pplFunc_t = void (*)(samplesMcmc_t<T>*, int, int, void*);

template <typename T>
using statusFunc_t = void (*)(samplesMcmc_t<T>*, int);

template <typename T>
using callbackFunc_t = void (*)(samplesMcmc_t<T>*, int, void*);


#endif
