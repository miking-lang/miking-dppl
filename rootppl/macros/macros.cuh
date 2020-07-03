#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED

/*
 * File macros.cuh defines macros that acts as an interface to the SMC framework.
 * These macros helps define the intermediate language RootPPL, in which the models are programmed. 
 * 
 * Short summary of how some of the most fundamental macros are used in models:
 *
 * - INIT_GLOBAL (mandatory) will set up globally accessible bblocks, takes number of bblocks as argument
 * - BBLOCK_DATA (optional) will set up globally accessible data
 * - MAIN will set up main function and some necessary variables. 
 * - INIT_BBLOCK with BBLOCK function (defined with BBLOCK macro) and programState type as arguments for each BBLOCK
 * - SMC with program state type and callback as arguments to start inference
 * - Result (currently only normalizationConstant) available through local variable "res" in MAIN
 */

#ifdef __NVCC__
#define GPU
#endif

#include <stdlib.h>
#include "macros_adaptive.cuh"

// Using a macro for "," is unfortunately necessary for some longer lists within bblocks. For example when doing array initialization.
#define COMMA ,
#define CMA ,

// To be able to switch between single and double precision easily.
typedef double floating_t; 

// Used by BBLOCK, BBLOCK_HELPER and BBLOCK_CALL macros.
#define BBLOCK_PARAMS(progStateType) RAND_STATE_DECLARE particles_t<progStateType>& particles, int i
#define BBLOCK_ARGS RAND_STATE_ACCESS particles, i

// Declarations of BBLOCK and BBLOCK_HELPER functions.
#define BBLOCK_DECLARE(funcName, progStateType) DEV void funcName(RAND_STATE_SIGNATURE particles_t<progStateType>&, int, void*);
#define BBLOCK_HELPER_DECLARE(funcName, progStateType, returnType, ...) \
template <typename T> \
DEV returnType funcName(RAND_STATE_SIGNATURE particles_t<T>, int, ##__VA_ARGS__);

// These will be executed by the framework.
#define BBLOCK(funcName, progStateType, body) \
DEV void funcName(BBLOCK_PARAMS(progStateType), void* arg = NULL) \
body \
DEV_POINTER(funcName, progStateType)

// Regular helper functions that takes the particles as argument (syntactic sugar).
#define BBLOCK_HELPER(funcName, body, returnType, ...) \
template <typename T> \
DEV returnType funcName(BBLOCK_PARAMS(T), ##__VA_ARGS__) \
body

// Call functions that takes the particles as argument (syntactic sugar).
#define BBLOCK_CALL(funcName, ...) funcName(BBLOCK_ARGS, ##__VA_ARGS__)

// Functions that can be called from the framework, usually to use resulting particle distributions before clean up.
#define CALLBACK_HOST(funcName, progStateType, body) void funcName(particles_t<progStateType>& particles, int numParticles, void* arg=NULL) body
#define CALLBACK(funcName, progStateType, body, arg) DEV void funcName(particles_t<progStateType>& particles, int numParticles, arg) body

/* 
Initialize the basic block (add it to the array of bblocks), the order of bblocks matters!
The first bblock to be initialized will be the first to be executed, then the execution follows the
index (PC) specified by the model (bblocks).
*/
#define INIT_BBLOCK(funcName, progStateType) \
pplFunc_t<progStateType> funcName ## Host; \
FUN_REF(funcName, progStateType) \
bblocksArr[bbIdx] = funcName ## Host; \
bbIdx++;

// Same as above, but for nested inference.
#define INIT_BBLOCK_NESTED(funcName, progStateType) bblocks[bbIdx] = funcName; bbIdx++;

// Sets up globally accessible bblock array, that can be accessed from the bblocks.
#define INIT_GLOBAL(progStateType, numBblocks) \
BBLOCK_DATA(bblocksArr, pplFunc_t<progStateType>, numBblocks) \
int bbIdx = 0;

// Samples from distributions, which should all take the curandState as argument first if on GPU.
#define SAMPLE(distName, ...) distName(RAND_STATE __VA_ARGS__ )

// Run SMC with given program state type (mandatory) and callback function (optional, can be declared with CALLBACK macro).
#define SMC(progStateType, callback) \
int numBblocks = bbIdx; \
configureMemSizeGPU(numParticles); \
COPY_DATA_GPU(bblocksArr, pplFunc_t<progStateType>, numBblocks) \
pplFunc_t<progStateType>* bblocksArrCudaManaged; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArrCudaManaged, numBblocks); \
for(int i = 0; i < numBblocks; i++) \
    bblocksArrCudaManaged[i] = bblocksArr[i]; \
res = runSMC<progStateType>(bblocksArrCudaManaged, numBblocks, numParticles, callback); \
freeMemory<pplFunc_t<progStateType>>(bblocksArrCudaManaged);

// Prepare bblock array for initialization of bblocks, for nested inference only.
#define SMC_PREPARE_NESTED(progStateType, numBblocks) \
pplFunc_t<progStateType>* bblocks = new pplFunc_t<progStateType>[numBblocks]; /*{}*/ \
int bbIdx = 0;

/* 
Run the nested inference with arguments:
- progStateType: The program state used by particles/bblocks in nested inference.
- callback: Callback function to use resulting particle distribution before clean up.
- retStruct: structure to fill with result in callback (passed to callback, handled by model, just passed by framework).
- arg: argument to nested inference bblocks.
- parallelExec: boolean, whether new CUDA-kernels should be launched for nested inference, otherwise just run sequentially (on GPU threads if top-level inference runs on the GPU).
- parallelResampling: boolean, whether new CUDA-kernels should be launched for nested resampling, otherwise run sequential variant (on GPU threads if top-level inference runs on the GPU).
- parentIndex: 
*/
#define SMC_NESTED(progStateType, numParticles, parallelExec, parallelResampling, parentIndex, callback, retStruct, arg) \
int numBblocks = bbIdx; \
double res = runSMCNested<progStateType>(RAND_STATE_ACCESS bblocks, numBblocks, numParticles, \
    parallelExec, parallelResampling, parentIndex, callback, (void*)&retStruct, (void*)arg); \
delete[] bblocks;

// Add log-weight to the particle.
#define WEIGHT(w) particles.weights[i] += w

// Access particle weight.
#define PWEIGHT particles.weights[i]

// Access particle program counter (bblock index).
#define PC particles.pcs[i]

// Access the particle's program/model specific state.
#define PSTATE particles.progStates[i]

// Main function.
#define MAIN(body) \
int main(int argc, char** argv) { \
    int numParticles = 10000; \
    if(argc > 1) { \
        numParticles = atoi(argv[1]); \
    } \
    initGen(); \
    double res = 0; \
    body \
    cout << "log(normalizationConstant) = " << res << endl; \
    return 0; \
}


/* MCMC, work in progress */
#define MCMCSTART(progStateType) arr100_t<pplFunc_t<progStateType>> bblocks;
#define MCMCEND(progStateType) \
bblocks.push_back(NULL); \
pplFunc_t<progStateType>* bblocksArr; \
allocateMemory<pplFunc_t<progStateType>>(&bblocksArr, bblocks.size()); \
copy(bblocks.begin(), bblocks.end(), bblocksArr); \
runMCMC<progStateType>(bblocksArr, statusFunc, bblocks.size()); \
freeMemory<pplFunc_t<progStateType>>(bblocksArr);

#endif

