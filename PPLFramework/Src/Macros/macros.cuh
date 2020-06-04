#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED

#include "macrosAdaptive.cuh"

typedef double floating_t; // To be able to switch between single and double precision easily

/*
*** MACROS used by problem-specific code, acts as an interface to the SMC framework ***

- Call SMCSTART with programState and numBblocks type as argument
- INITBBLOCK with BBLOCK function (defined with BBLOCK macro) and programState type as arguments for each BBLOCK
- SMCEND with program state type and callback as arguments to start inference
- Result (right now only normalizationConstant) available through local variable "res"
*/

#define BBLOCK(funcName, progStateType, body) \
DEV void funcName(particles_t<progStateType>* particles, int i, void* arg = NULL) \
body \
DEV_POINTER(funcName, progStateType)


#define BBLOCK_HELPER(funcName, body, returnType, ...) \
template <typename T> \
DEV returnType funcName(particles_t<T>* particles, int i, ##__VA_ARGS__) \
body

#define BBLOCK_CALL(funcName, ...) funcName(particles, i, ##__VA_ARGS__)

// #define STATUSFUNC(body) void statusFunc(particles_t<progState_t>* particles) body
// Just a general statusfunc
#define CALLBACK_HOST(funcName, progStateType, body, arg) void funcName(particles_t<progStateType>* particles, arg) body
#define CALLBACK(funcName, progStateType, body, arg) DEV void funcName(particles_t<progStateType>* particles, arg) body

#define INITBBLOCK(funcName, progStateType) \
pplFunc_t<progStateType> funcName ## Host; \
FUN_REF(funcName, progStateType) \
bblocksArr[bbIdx] = funcName ## Host; \
bbIdx++;
//bblocks.push_back(funcName ## Host);
// bblocksArrDev[bbIdx] = funcName ## Dev; \
// cudaSafeCall(cudaMemcpy(funcName ## Ptr, funcName ## Dev, sizeof(pplFunc_t<progStateType>), cudaMemcpyDeviceToDevice)); \
//pplFunc_t<progStateType>* funcName ## Ptr = bblocksArr; \
//funcName ## Ptr += bbIdx; \
// cudaSafeCall(cudaMemcpyToSymbol(bblocksArrDev, &funcName ## Dev, sizeof(pplFunc_t<progStateType>), bbIdx * sizeof(pplFunc_t<progStateType>), cudaMemcpyDeviceToDevice)); \
// cudaSafeCall(cudaMemcpyToSymbol(bblocksArrDev[bbIdx], &funcName ## Dev, sizeof(pplFunc_t<progStateType>), 0, cudaMemcpyDeviceToDevice)); \


// #define INITBBLOCK_NESTED(funcName, progStateType) bblocks.push_back(funcName);
#define INITBBLOCK_NESTED(funcName, progStateType) bblocks[bbIdx] = funcName; bbIdx++;

//pplFunc_t<progStateType> funcName ## Host = funcName; \


// *** Array needed to handle stuff =)
template <typename T>
struct arr100_t {
    int currIndex = 0;
    T arr[100];

    HOST DEV T operator [] (int i) const {return arr[i];}
    HOST DEV T& operator [] (int i) {return arr[i];}

    HOST DEV void push_back(T obj) {
        arr[currIndex] = obj;
        currIndex++;
    }

    HOST DEV int size() {
        return currIndex;
    }

    HOST DEV T* begin() {
        return arr;
    }

    HOST DEV T* end() {
        return &arr[currIndex];
    }
};
// ***


// #define INIT_BBLOCKS_ARRAY(progStateType, numBblocks) BBLOCK_DATA(bblocksArr, pplFunc_t<progStateType>, numBblocks+1)
// #define INIT_GLOBAL(progStateType) pplFunc_t<progStateType>* bblocksArr;
#define INIT_GLOBAL(progStateType, numBblocks) \
BBLOCK_DATA(bblocksArr, pplFunc_t<progStateType>, numBblocks+1)
//pplFunc_t<progStateType>* bblocksArr; \
//__device__ pplFunc_t<progStateType> bblocksArrDev[numBblocks];
// BBLOCK_DATA_PTR(bblocksArr, pplFunc_t<progStateType>);
// BBLOCK_DATA(bblocksArr, pplFunc_t<progStateType>, numBblocks+1)



// #define SMCSTART(progStateType) arr100_t<pplFunc_t<progStateType>> bblocks;
#define SMCSTART(progStateType, numBblocks) \
int bbIdx = 0;
// ALLOCATE_BBLOCKS(bblocksArr, progStateType, numBblocks)
// bblocksArr = new pplFunc_t<progStateType>[numBblocks+1]; // {}

// cudaMemcpy(bblocksArrDev, bblocksArr, numBblocks * sizeof(pplFunc_t<progStateType>), cudaMemcpyHostToDevice); \
// DATA_POINTER(bblocksArr) = bblocksArr; \
// COPY_DATA_GPU(bblocksArr, pplFunc_t<progStateType>*, 1); \
// COPY_DATA_GPU(bblocksArr, pplFunc_t<progStateType>, numBblocks); \

#define SMCEND(progStateType, callback) \
bblocksArr[bbIdx] = NULL; \
int numBblocks = bbIdx+1; \
configureMemSizeGPU(); \
COPY_DATA_GPU(bblocksArr, pplFunc_t<progStateType>, numBblocks) \
pplFunc_t<progStateType>* bblocksArrCudaManaged; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArrCudaManaged, numBblocks); \
for(int i = 0; i < numBblocks; i++) \
    bblocksArrCudaManaged[i] = bblocksArr[i]; \
double res = runSMC<progStateType>(bblocksArrCudaManaged, numBblocks, callback); \
freeMemory<pplFunc_t<progStateType>>(bblocksArrCudaManaged);


/*
#define SMCEND(progStateType, callback) \
bblocks.push_back(NULL); \
pplFunc_t<progStateType>* bblocksArr; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, bblocks.size()); \
copy(bblocks.begin(), bblocks.end(), bblocksArr); \
configureMemSizeGPU(); \
double res = runSMC<progStateType>(bblocksArr, bblocks.size(), callback);
freeMemory<pplFunc_t<progStateType>>(bblocksArr);
*/
// #define SMCSTART_NESTED(progStateType) arr100_t<pplFunc_t<progStateType>> bblocks;
#define SMCSTART_NESTED(progStateType, numBblocks) \
pplFunc_t<progStateType>* bblocks = new pplFunc_t<progStateType>[numBblocks]; /*{}*/ \
int bbIdx = 0;

#define SMCEND_NESTED(progStateType, callback, retStruct, arg, parallelExec, parallelResampling, parentIndex) \
bblocks[bbIdx] = NULL; \
double res = runSMCNested<progStateType>(bblocks, callback, (void*)&retStruct, (void*)arg, parallelExec, parallelResampling, parentIndex); \
delete[] bblocks;


//#define SMCEND_NESTED(progStateType, callback, retStruct, arg, parallelExec, parallelResampling, parentIndex) \
//bblocks.push_back(NULL); \
//pplFunc_t<progStateType>* bblocksArr; \
///*{}*/ \
//bblocksArr = new pplFunc_t<progStateType>[bblocks.size()]; \
///*{}*/ \
//for(int i = 0; i < bblocks.size(); i++) \
//    bblocksArr[i] = bblocks[i]; \
//double res = runSMCNested<progStateType>(bblocksArr, callback, (void*)&retStruct, (void*)arg, parallelExec, parallelResampling, parentIndex); \
//delete[] bblocksArr;


#define WEIGHT(w) particles->weights[i] += w
#define PWEIGHT particles->weights[i]
#define PC particles->pcs[i]
// #define RESAMPLE particles->resample[i]
#define PSTATE particles->progStates[i]




/* MCMC */
#define MCMCSTART(progStateType) arr100_t<pplFunc_t<progStateType>> bblocks;
#define MCMCEND(progStateType) \
bblocks.push_back(NULL); \
pplFunc_t<progStateType>* bblocksArr; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, bblocks.size()); \
copy(bblocks.begin(), bblocks.end(), bblocksArr); \
runMCMC<progStateType>(bblocksArr, statusFunc, bblocks.size()); \
freeMemory<pplFunc_t<progStateType>>(bblocksArr);

#endif

