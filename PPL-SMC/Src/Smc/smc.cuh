#ifndef SMC_INCLUDED
#define SMC_INCLUDED

#include <cstddef>
#ifdef GPU
#include "../cudaErrorUtils.cu"
#include <curand_kernel.h>
#endif

using namespace std;



const int NUM_PARTICLES = 100000;// 1 << 17;

// Odd since warp size is 32
const int NUM_THREADS_PER_BLOCK = 32;
const int NUM_BLOCKS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK - 1) / NUM_THREADS_PER_BLOCK;

const int NUM_THREADS_PER_BLOCK_FUNCS = 128;
const int NUM_BLOCKS_FUNCS = (NUM_PARTICLES + NUM_THREADS_PER_BLOCK_FUNCS - 1) / NUM_THREADS_PER_BLOCK_FUNCS;


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
double sum = 0, min = 99999999;\
int numTrials = 10;\
for(int i = 0; i < numTrials; i++) {\
double dur = runSMC<progState_t>(bblocksArr, statusFunc, bblocks.size()); \
if(dur < min) min = dur; sum += dur;\
}\
printf("Avg time(sec): %f, Min time: %f\n", sum/numTrials, min);\
freeMemory<pplFunc_t<progState_t>>(bblocksArr); \
return 0;

#define WEIGHT(w) particles->weights[i] += w // SHOULD ADD? (AND BE ZEROED AFTER RESAMPLE)
#define PWEIGHT particles->weights[i]
#define PC particles->pcs[i]
#define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]

//

#endif