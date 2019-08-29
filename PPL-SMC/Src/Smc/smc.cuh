#ifndef SMC_INCLUDED
#define SMC_INCLUDED

#include <cstddef>
#ifdef GPU
#include "../cudaErrorUtils.cu"
#include <curand_kernel.h>
#endif

using namespace std;

/*

Monkeytree execution time: avg +- std in over 3 runs

CPU:
N=100:          0,002861 +- 0,000322 seconds time elapsed ( +- 11,24% )
N=1000:         0,01281  +- 0,00217 seconds time elapsed  ( +- 16,98% )
N=10 000:       0,10172  +- 0,00161 seconds time elapsed  ( +-  1,58% )
N=100 000:      1,12494  +- 0,00987 seconds time elapsed  ( +-  0,88% )
N=500 000:      5,72949  +- 0,00919 seconds time elapsed  ( +-  0,16% ) 
N=1 000 000:    11,528   +- 0,121   seconds time elapsed  ( +-  1,05% )

GPU:
N=100:          0,22958  +- 0,00724 seconds time elapsed  ( +-  3,15% )
N=1000:         0,22348  +- 0,00665 seconds time elapsed  ( +-  2,97% )
N=10 000:       0,23183  +- 0,00269 seconds time elapsed  ( +-  1,16% )
N=100 000:      0,29160  +- 0,00829 seconds time elapsed  ( +-  2,84% )
N=500 000:      1,1551   +- 0,0170 seconds time elapsed   ( +-  1,47% )
N=1 000 000:    2,8126   +- 0,0127  seconds time elapsed  ( +-  0,45% )


Which kernels contribute to execution time? (GPU)

N=100K => 8.8% initRandStates, 26.7% execFuncs, 42% copyStates (resample)
N=500K => 75.1% initRandStates, 9.0% execFuncs, 12.2% copyStates (resample)
N=1M   => 82.6% initRandStates, 6.4% execFuncs, 9% copyStates (resample)

*/

const int NUM_PARTICLES = 100000;// 1 << 17;


const int NUM_THREADS_PER_BLOCK = 32;
const int NUM_BLOCKS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

const int NUM_THREADS_PER_BLOCK_FUNCS = 128;
const int NUM_BLOCKS_FUNCS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK_FUNCS - 1) / NUM_THREADS_PER_BLOCK_FUNCS;

const int NUM_THREADS_PER_BLOCK_INITRAND = 32;
const int NUM_BLOCKS_INITRAND = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK_INITRAND - 1) / NUM_THREADS_PER_BLOCK_INITRAND;

typedef double floating_t;

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

template <typename T>
using pplFunc_t = void (*)(particles_t<T>*, int, int);

template <typename T>
using statusFunc_t = void (*)(particles_t<T>*, int);



/* MACROS */

#ifdef GPU

#define HOST __host__
#define DEV __device__
#define DEV_POINTER(funcName) __device__ pplFunc_t<progState_t> funcName ## Dev = funcName;
#define FUN_REF(funcName) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progState_t>))); 
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];\
__device__ type pointerName ## Dev[n];
#define COPY_DATA_GPU(pointerName, type, n) cudaSafeCall(cudaMemcpyToSymbol(pointerName ## Dev, pointerName, n * sizeof(type)));
#define DATA_POINTER(pointerName) pointerName ## Dev

#else

#define HOST
#define DEV
#define DEV_POINTER(funcName)
#define FUN_REF(funcName) funcName ## Host = funcName;
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];
#define COPY_DATA_GPU(pointerName, type, n) // Would be nice to solve this cleaner
#define DATA_POINTER(pointerName) pointerName

#endif


#define BBLOCK(funcName, body) DEV void funcName(particles_t<progState_t>* particles, int i, int t) \
body \
DEV_POINTER(funcName)


#define BBLOCK_HELPER(funcName, body, returnType, ...) DEV returnType funcName(particles_t<progState_t>* particles, int i, __VA_ARGS__) \
body

#define BBLOCK_CALL(funcName, ...) funcName(particles, i, __VA_ARGS__)

//#define BBLOCK_HELPER_HOSTDEV(funcName, body, returnType, ...) HOST DEV returnType funcName(__VA_ARGS__) \
//body

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
double dur = runSMC<progState_t>(bblocksArr, statusFunc, bblocks.size()); \
freeMemory<pplFunc_t<progState_t>>(bblocksArr); \
return 0;

#define WEIGHT(w) particles->weights[i] += w // SHOULD ADD? (AND BE ZEROED AFTER RESAMPLE)
#define PWEIGHT particles->weights[i]
#define PC particles->pcs[i]
#define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]


/*
double sum = 0, min = 99999999; \
int numTrials = 10;\
for(int i = 0; i < numTrials; i++) {\
    double dur = runSMC<progState_t>(bblocksArr, statusFunc, bblocks.size()); \
    if(dur < min) min = dur; sum += dur;\
    }\
    printf("Avg time(sec): %f, Min time: %f\n", sum/numTrials, min);\
*/

//

#endif