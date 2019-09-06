#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED

/* MACROS used by problem-specifik code, acts as an interface to the SMC framework*/

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
double res = runSMC<progState_t>(bblocksArr, statusFunc, bblocks.size()); \
freeMemory<pplFunc_t<progState_t>>(bblocksArr);

#define WEIGHT(w) particles->weights[i] += w // SHOULD ADD? (AND BE ZEROED AFTER RESAMPLE)
#define PWEIGHT particles->weights[i]
#define PC particles->pcs[i]
#define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]


#endif