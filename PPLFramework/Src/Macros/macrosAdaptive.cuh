#ifndef MACROS_ADAPTIVE_INCLUDED
#define MACROS_ADAPTIVE_INCLUDED

#ifdef GPU

#define HOST __host__
#define DEV __device__
#define DEV_POINTER(funcName, progStateType) __device__ pplFunc_t<progStateType> funcName ## Dev = funcName;
#define FUN_REF(funcName, progStateType) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progStateType>))); 
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];\
__device__ type pointerName ## Dev[n];
#define BBLOCK_DATA_PTR(pointerName, type) type* pointerName;\
__device__ type* pointerName ## Dev;
#define BBLOCK_DATA_2D(pointerName, type, n, m) type pointerName[n][m];\
__device__ type pointerName ## Dev[n][m];

#define COPY_DATA_GPU(pointerName, type, n) cudaSafeCall(cudaMemcpyToSymbol(pointerName ## Dev, pointerName, n * sizeof(type)));
#define DATA_POINTER(pointerName) pointerName ## Dev
#define ALLOCATE_BBLOCKS(bblocksArr, progStateType, numBblocks) \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, numBblocks+1);
// cudaSafeCall(cudaMalloc(&bblocksArrDev, sizeof(pplFunc_t<progStateType>) * (numBblocks+1)));
// allocateMemory<pplFunc_t<progStateType>>(&bblocksArrDev, numBblocks+1);


// #define SAMPLE(distrName, ...) distrName(curandState* randState, ##__VA_ARGS__)
#else
// #define SAMPLE(distrName, ...) distrName(##__VA_ARGS__)


#define HOST
#define DEV
#define DEV_POINTER(funcName, progStateType)
#define FUN_REF(funcName, progStateType) funcName ## Host = funcName;
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];
#define BBLOCK_DATA_PTR(pointerName, type) type* pointerName;
#define BBLOCK_DATA_2D(pointerName, type, n, m) type pointerName[n][m];
#define COPY_DATA_GPU(pointerName, type, n) // Would be nice to solve this cleaner
#define DATA_POINTER(pointerName) pointerName

#define ALLOCATE_BBLOCKS(bblocksArr, progStateType, numBblocks) \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, numBblocks+1);

#endif

#endif
