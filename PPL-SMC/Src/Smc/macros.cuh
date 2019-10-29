#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED

/* MACROS used by problem-specific code, acts as an interface to the SMC framework*/

#ifdef GPU

#define HOST __host__
#define DEV __device__
#define DEV_POINTER(funcName, progStateType) __device__ pplFunc_t<progStateType> funcName ## Dev = funcName;
#define FUN_REF(funcName, progStateType) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progStateType>))); 
// #define FUN_REF_TYPE(funcName, progStateType) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progStateType>))); 
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];\
__device__ type pointerName ## Dev[n];
#define COPY_DATA_GPU(pointerName, type, n) cudaSafeCall(cudaMemcpyToSymbol(pointerName ## Dev, pointerName, n * sizeof(type)));
#define DATA_POINTER(pointerName) pointerName ## Dev

#else

#define HOST
#define DEV
#define DEV_POINTER(funcName, progStateType)
#define FUN_REF(funcName, progStateType) funcName ## Host = funcName;
// #define FUN_REF_TYPE(funcName, progStateType) funcName ## Host = funcName;
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];
#define COPY_DATA_GPU(pointerName, type, n) // Would be nice to solve this cleaner
#define DATA_POINTER(pointerName) pointerName

#endif

// #define FUN_REF_NESTED(funcName, progStateType) funcName ## Host = funcName;

/*
#define BBLOCK(funcName, body) \
template <typename T> \
DEV void funcName(particles_t<T>* particles, int i, int t) \
body \
DEV_POINTER(funcName)
*/

#define BBLOCK(funcName, progStateType, body) \
DEV void funcName(particles_t<progStateType>* particles, int i, int t) \
body \
DEV_POINTER(funcName, progStateType)


#define BBLOCK_HELPER(funcName, body, returnType, ...) \
template <typename T> \
DEV returnType funcName(particles_t<T>* particles, int i, ##__VA_ARGS__) \
body

#define BBLOCK_CALL(funcName, ...) funcName(particles, i, ##__VA_ARGS__)

//#define BBLOCK_HELPER_HOSTDEV(funcName, body, returnType, ...) HOST DEV returnType funcName(__VA_ARGS__) \
//body

#define STATUSFUNC(body) void statusFunc(particles_t<progState_t>* particles, int t) body
// Just a general statusfunc
#define CALLBACK(funcName, progStateType, body, arg) DEV void funcName(particles_t<progStateType>* particles, int t, arg) body

#define INITBBLOCK(funcName, progStateType) \
pplFunc_t<progStateType> funcName ## Host; \
FUN_REF(funcName, progStateType) \
bblocks.push_back(funcName ## Host);


#define INITBBLOCK_NESTED(funcName, progStateType) \
pplFunc_t<progStateType> funcName ## Host = funcName; \
bblocks.push_back(funcName ## Host);


/*
#define INITBBLOCK_NESTED(funcName, bblockArr, index, progStateType) \
pplFunc_t<progStateType> funcName ## Host; \
FUN_REF_TYPE(funcName, progStateType) \
bblockArr[index] = funcName ## Host; \
index++;
*/

// #define MAINSTART() vector<pplFunc_t<progState_t>> bblocks;
#define SMCSTART(progStateType) arr10_t<pplFunc_t<progStateType>> bblocks;

#define SMCEND(progStateType) \
bblocks.push_back(NULL); \
pplFunc_t<progStateType>* bblocksArr; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, bblocks.size()); \
copy(bblocks.begin(), bblocks.end(), bblocksArr); \
double res = runSMC<progStateType>(bblocksArr, statusFunc, bblocks.size()); \
freeMemory<pplFunc_t<progStateType>>(bblocksArr);

#define SMCEND_NESTED(progStateType, callback, retStruct, parallelExec, parallelResampling, parentIndex) \
bblocks.push_back(NULL); \
pplFunc_t<progStateType>* bblocksArr; \
/*allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, bblocks.size());*/ \
bblocksArr = new pplFunc_t<progStateType>[bblocks.size()]; \
/*copy(bblocks.begin(), bblocks.end(), bblocksArr);{}*/ \
for(int i = 0; i < bblocks.size(); i++) \
    bblocksArr[i] = bblocks[i]; \
double res = runSMCNested<nestedProgState_t>(bblocksArr, callback, (void*)&retStruct, parallelExec, parallelResampling, parentIndex); \
delete[] bblocksArr;
/*freeMemory<pplFunc_t<progStateType>>(bblocksArr);*/

#define WEIGHT(w) particles->weights[i] += w // SHOULD ADD? (AND BE ZEROED AFTER RESAMPLE)
#define PWEIGHT particles->weights[i]
#define PC particles->pcs[i]
#define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]


#endif