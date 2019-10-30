#ifndef SMC_INCLUDED
#define SMC_INCLUDED

#include <cstddef>
#include "macros.cuh"
#ifdef GPU
#include "../cudaErrorUtils.cu"
#include <curand_kernel.h>
#endif

using namespace std;

/* 
~0.9 sec avg over 6 runs with only some separate num_particles in nested and not
~0.915 sec avg over 6 runs with separate num_particles in nested and not


Primate tree execution time: avg +- std over 3 runs, for N particles
No stack precompute

CPU:
N=100:       0,01336  +- 0,00224 seconds time elapsed  ( +- 16,76% )
N=1000:      0,084143 +- 0,000972 seconds time elapsed ( +-  1,15% )
N=10 000:    0,8350   +- 0,0253 seconds time elapsed   ( +-  3,02% )
N=100 000:   8,0278   +- 0,0282 seconds time elapsed   ( +-  0,35% )
N=300 000:   24,1590  +- 0,0736 seconds time elapsed   ( +-  0,30% )
N=1 000 000: 80,6065  +- 0,0148 seconds time elapsed   ( +-  0,02% )

GPU:
N=100:       0,32938  +- 0,00919 seconds time elapsed  ( +-  2,79% )
N=1000:      0,3470   +- 0,0122 seconds time elapsed   ( +-  3,53% )
N=10 000:    0,38759  +- 0,00768 seconds time elapsed  ( +-  1,98% ) 
N=100 000:   0,62697  +- 0,00486 seconds time elapsed  ( +-  0,78% )
N=300 000:   1,4377   +- 0,0193 seconds time elapsed   ( +-  1,34% )
N=1 000 000: 5,6439   +- 0,0202 seconds time elapsed   ( +-  0,36% )


(Seems to take ~0.2sec before even initialization (initRandStates) can begin)
N=100K  => 1.7%  initRandStates, 78.1% execFuncs
N=300k  => 11.5% initRandStates, 76.9% execFuncs
N=1M    => 39.6% initRandStates, 55.1% execFuncs

N=100K: ~19 speedup CPU -> GPU, without init costs   (~13 with costs)
N=1M: ~30 speedup without init costs                 (~15 with costs)

*/

const int NUM_PARTICLES = 1000;
const int NUM_PARTICLES_NESTED = 50;

const int NUM_THREADS_PER_BLOCK = 32;
const int NUM_BLOCKS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

const int NUM_THREADS_PER_BLOCK_FUNCS = 128;
const int NUM_BLOCKS_FUNCS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK_FUNCS - 1) / NUM_THREADS_PER_BLOCK_FUNCS;

const int NUM_THREADS_PER_BLOCK_INITRAND = 32;
const int NUM_BLOCKS_INITRAND = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK_INITRAND - 1) / NUM_THREADS_PER_BLOCK_INITRAND;

const int NUM_THREADS_PER_BLOCK_NESTED = 32;
const int NUM_BLOCKS_NESTED = (NUM_PARTICLES_NESTED + NUM_THREADS_PER_BLOCK_NESTED - 1) / NUM_THREADS_PER_BLOCK_NESTED;


typedef double floating_t;

/*
template <typename T>
struct particles_t {

    T progStates[NUM_PARTICLES];
    #ifdef GPU
    curandState randStates[NUM_PARTICLES];
    #endif
    int pcs[NUM_PARTICLES] = {0};
    floating_t weights[NUM_PARTICLES] = {0};
    bool resample[NUM_PARTICLES];
};
*/

template <typename T>
struct particles_t {

    T* progStates;
    #ifdef GPU
    curandState* randStates;
    #endif
    int* pcs;
    floating_t* weights;
    bool* resample;
};

template <typename T>
using pplFunc_t = void (*)(particles_t<T>*, int, int);

template <typename T>
using statusFunc_t = void (*)(particles_t<T>*, int);

template <typename T>
using callbackFunc_t = void (*)(particles_t<T>*, int, void*);


#endif