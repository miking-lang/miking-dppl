#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED

#include "macros_adaptive.cuh"

#define COMMA ,
#define CMA ,
typedef double floating_t; // To be able to switch between single and double precision easily

/*
*** MACROS used by problem-specific code, acts as an interface to the SMC framework ***

- INIT_GLOBAL (mandatory) will set up globally accessible bblocks
- BBLOCK_DATA (optional) will set up globally accessible data
- Call SMCSTART with programState and numBblocks type as argument
- INIT_BBLOCK with BBLOCK function (defined with BBLOCK macro) and programState type as arguments for each BBLOCK
- SMCEND with program state type and callback as arguments to start inference
- Result (right now only normalizationConstant) available through local variable "res"
*/

// Used by BBLOCK, BBLOCK_HELPER and BBLOCK_CALL macros
#define BBLOCK_PARAMS(progStateType) RAND_STATE_DECLARE particles_t<progStateType>* particles, int i
#define BBLOCK_ARGS RAND_STATE_ACCESS particles, i

// These will be executed by the framework
#define BBLOCK(funcName, progStateType, body) \
DEV void funcName(BBLOCK_PARAMS(progStateType), void* arg = NULL) \
body \
DEV_POINTER(funcName, progStateType)

// Regular helper functions that takes the particles as argument (syntactic sugar)
#define BBLOCK_HELPER(funcName, body, returnType, ...) \
template <typename T> \
DEV returnType funcName(BBLOCK_PARAMS(T), ##__VA_ARGS__) \
body

// Call functions that takes the particles as argument (syntactic sugar)
#define BBLOCK_CALL(funcName, ...) funcName(BBLOCK_ARGS, ##__VA_ARGS__)

// #define BBLOCK_CALL_FNC_PTR(funcName, ...) funcName(RAND_STATE_ACCESS particles, i, ##__VA_ARGS__)

// Functions that can be called from the framework, usually to use resulting particle distributions before clean up
#define CALLBACK_HOST(funcName, progStateType, body) void funcName(particles_t<progStateType>* particles, void* arg=NULL) body
#define CALLBACK(funcName, progStateType, body, arg) DEV void funcName(particles_t<progStateType>* particles, arg) body

/* 
Initialize the basic block (add it to the array of bblocks), the order of bblocks matters!
The first bblock to be initialized will be the first to be executed, then the execution follows the
index (PC) specified by the model (bblocks)
*/
#define INIT_BBLOCK(funcName, progStateType) \
pplFunc_t<progStateType> funcName ## Host; \
FUN_REF(funcName, progStateType) \
bblocksArr[bbIdx] = funcName ## Host; \
bbIdx++;

// Same as above, but for nested inference
#define INIT_BBLOCK_NESTED(funcName, progStateType) bblocks[bbIdx] = funcName; bbIdx++;

// Sets up globally accessible bblock array, that can be accessed from the bblocks
#define INIT_GLOBAL(progStateType, numBblocks) \
BBLOCK_DATA(bblocksArr, pplFunc_t<progStateType>, numBblocks) \
int bbIdx = 0;

// Samples from distributions, which should all take the curandState as argument first if on GPU.
#define SAMPLE(distName, ...) distName(RAND_STATE __VA_ARGS__ )
// #define SAMPLE(distName, ...) distName(randState, __VA_ARGS__ )

// #define SMCSTART(progStateType, numBblocks) \
// int bbIdx = 0;

// Run SMC with given program state type (mandatory) and callback function (optional, can be declared with CALLBACK macro)
#define SMC(progStateType, callback) \
int numBblocks = bbIdx; \
configureMemSizeGPU(); \
COPY_DATA_GPU(bblocksArr, pplFunc_t<progStateType>, numBblocks) \
pplFunc_t<progStateType>* bblocksArrCudaManaged; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArrCudaManaged, numBblocks); \
for(int i = 0; i < numBblocks; i++) \
    bblocksArrCudaManaged[i] = bblocksArr[i]; \
res = runSMC<progStateType>(bblocksArrCudaManaged, numBblocks, callback); \
freeMemory<pplFunc_t<progStateType>>(bblocksArrCudaManaged);

// Prepare bblock array for initialization of bblocks, for nested inference only
#define SMC_PREPARE_NESTED(progStateType, numBblocks) \
pplFunc_t<progStateType>* bblocks = new pplFunc_t<progStateType>[numBblocks]; /*{}*/ \
int bbIdx = 0;

/* 
Run the nested inference with arguments:
- progStateType: The program state used by particles/bblocks in nested inference
- callback: Callback function to use resulting particle distribution before clean up
- retStruct: structure to fill with result in callback (passed to callback, handled by model, just passed by framework)
- arg: argument to nested inference bblocks
- parallelExec: boolean, whether new CUDA-kernels should be launched for nested inference, otherwise just run sequentially (on GPU threads if top-level inference runs on the GPU)
- parallelResampling: boolean, whether new CUDA-kernels should be launched for nested resampling, otherwise run sequential variant (on GPU threads if top-level inference runs on the GPU)
- parentIndex: 
*/
#define SMC_NESTED(progStateType, callback, retStruct, arg, parallelExec, parallelResampling, parentIndex) \
int numBblocks = bbIdx; \
double res = runSMCNested<progStateType>(RAND_STATE_ACCESS bblocks, callback, numBblocks, (void*)&retStruct, (void*)arg, parallelExec, parallelResampling, parentIndex); \
delete[] bblocks;

// Add log-weight to the particle
#define WEIGHT(w) particles->weights[i] += w

// Access particle weight
#define PWEIGHT particles->weights[i]

// Access particle program counter (bblock index)
#define PC particles->pcs[i]

// Access the particle's program/model specific state
#define PSTATE particles->progStates[i]

// Main function
#define MAIN(body) \
int main() { \
    initGen(); \
    double res = 0; \
    body \
    cout << "log(normalizationConstant) = " << res << endl; \
    return 0; \
}


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

