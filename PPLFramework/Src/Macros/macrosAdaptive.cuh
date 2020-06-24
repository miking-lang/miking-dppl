#ifndef MACROS_ADAPTIVE_INCLUDED
#define MACROS_ADAPTIVE_INCLUDED

#ifdef GPU

// Defines that BBLOCKS should create a local curandState by default, efficient with many samples within it
#define USE_LOCAL_RAND_STATES

#define HOST __host__
#define DEV __device__

// A device pointer to a bblock
#define DEV_POINTER(funcName, progStateType) __device__ pplFunc_t<progStateType> funcName ## Dev = funcName;

// Copies a reference to a device function to a host pointer, necessary to handle GPU function pointers on CPU
#define FUN_REF(funcName, progStateType) cudaSafeCall(cudaMemcpyFromSymbol(&funcName ## Host, funcName ## Dev, sizeof(pplFunc_t<progStateType>))); 

// Allocate data on host and device, should be followed by a COPY_DATA_GPU call before inference
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];\
__device__ type pointerName ## Dev[n];
// __device__ type pointerName ## Dev[n];

// Same as BBLOCK_DATA, but 2D-array
#define BBLOCK_DATA_2D(pointerName, type, n, m) type pointerName[n][m];\
__device__ type pointerName ## Dev[n][m];

// Same as above, but 3D-array
#define BBLOCK_DATA_3D(pointerName, type, n, m, z) type pointerName[n][m][z];\
__device__ type pointerName ## Dev[n][m][z];

// Declare pointers on host and device
#define BBLOCK_DATA_PTR(pointerName, type) type* pointerName;\
__device__ type* pointerName ## Dev;

// Copy the data from the host pointer to the device pointer (to the GPU), pointers should be allocated with a BBLOCK_DATA macro
#define COPY_DATA_GPU(pointerName, type, n) cudaSafeCall(cudaMemcpyToSymbol(pointerName ## Dev, pointerName, n * sizeof(type)));

// Access the data allocated with a BBLOCK_DATA macro
#define DATA_POINTER(pointerName) pointerName ## Dev


// Used when declaring dists working on both CPU and GPU, assumes dist has at least one more parameter
#define RAND_STATE_DECLARE curandState* randState,

// Used when calling SAMPLE from dists working on both CPU and GPU since they rely on other dists only, assumes dist has at least one more parameter
#define RAND_STATE_ACCESS randState,


/*** Optimization to use local CUDA random states in BBLOCKS ***/
#ifdef USE_LOCAL_RAND_STATES

// Used in SAMPLE macro to provide the CUDA random generating state to the function call, assumes at least one more argument follows
#define RAND_STATE randState,

// Efficient when many random calls are done in the same function, to not access global memory repeatedly

#define RAND_STATE_LOCAL \
curandState randStateLocal = randStates[i]; \
curandState* randState = &randStateLocal;

// curandState randStateLocal = particles->randStates[i];

// #define RAND_STATE_RESTORE  particles->randStates[i] = randStateLocal;
#define RAND_STATE_RESTORE  randStates[i] = randStateLocal;

#else

// #define RAND_STATE &particles->randStates[i],
#define RAND_STATE randStates[i],
#define RAND_STATE_LOCAL
#define RAND_STATE_RESTORE

/*** ***/

#endif

#else

// The macros below are equivalent to the GPU variants above, but for CPU

#define HOST
#define DEV
#define DEV_POINTER(funcName, progStateType)
#define FUN_REF(funcName, progStateType) funcName ## Host = funcName;
#define BBLOCK_DATA(pointerName, type, n) type pointerName[n];
#define BBLOCK_DATA_PTR(pointerName, type) type* pointerName;
#define BBLOCK_DATA_2D(pointerName, type, n, m) type pointerName[n][m];
#define COPY_DATA_GPU(pointerName, type, n)
#define DATA_POINTER(pointerName) pointerName
#define RAND_STATE
#define RAND_STATE_DECLARE
#define RAND_STATE_ACCESS
#define RAND_STATE_LOCAL
#define RAND_STATE_RESTORE

#endif

#endif
