#ifndef SMC_INCLUDED
#define SMC_INCLUDED

//#define GPU

#include <cstddef>
#ifdef GPU
#include "../cudaErrorUtils.cu"
#include <curand_kernel.h>
#endif

using namespace std;

const int NUM_PARTICLES = 1 << 10;

const int NUM_THREADS_PER_BLOCK = 128;
const int NUM_BLOCKS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;


typedef double floating_t;

template <typename T>
struct particles_t {

    T progStates[NUM_PARTICLES];
    #ifdef GPU
    curandState randStates[NUM_PARTICLES];
    #endif
    int pcs[NUM_PARTICLES];
    floating_t weights[NUM_PARTICLES] = {0};
    bool resample[NUM_PARTICLES];
};

template <typename T>
using pplFunc_t = void (*)(particles_t<T>*, int, int);

template <typename T>
using statusFunc_t = void (*)(particles_t<T>*, int);



// MACROS

#ifdef GPU

#define DEV __device__
#define DEV_POINTER(funcName) __device__ pplFunc_t<progState_t> funcName ## Dev = funcName;
#define FUN_REF(funcName) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progState_t>))); 

#else

#define DEV
#define DEV_POINTER(funcName)
#define FUN_REF(funcName) funcName ## Host = funcName;

#endif

#define BBLOCK(funcName, body) DEV void funcName(particles_t<progState_t>* particles, int i, int t) \
body \
DEV_POINTER(funcName)

#define STATUSFUNC(body) void statusFunc(particles_t<progState_t>* particles, int t) body

#define INITBBLOCK(funcName) \
pplFunc_t<progState_t> funcName ## Host; \
FUN_REF(funcName) \
bblocks.push_back(funcName ## Host);


#define MAINSTART() vector<pplFunc_t<progState_t>> bblocks;

#define MAINEND() \
bblocks.push_back(NULL); \
pplFunc_t<progState_t>* bblocksArr; \
allocateMemory<pplFunc_t<progState_t>>(&bblocksArr, bblocks.size()); \
copy(bblocks.begin(), bblocks.end(), bblocksArr); \
runSMC<progState_t>(bblocksArr, statusFunc); \
freeMemory<pplFunc_t<progState_t>>(bblocksArr); \
return 0;

#define WEIGHT(w) particles->weights[i] += w; // SHOULD ADD? (AND BE ZEROED AFTER RESAMPLE)
#define PC particles->pcs[i]
#define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]

//

#endif