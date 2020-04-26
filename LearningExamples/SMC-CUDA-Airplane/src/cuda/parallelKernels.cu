#include <random>
#include <curand_kernel.h>
#include "../utils.h"
#include "utilsGPU.cuh"


__global__ void initRandStates(curandState* states) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;
    curand_init(1234, idx, 0, &states[idx]);
}

__global__ void initX(curandState* states, floating_t* x) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;
    x[idx] = curand_uniform(&states[idx]) * MAP_SIZE;
}

__global__ void weigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    if(LOG_WEIGHTS)
        w[idx] = logNormalPDFObsDev(obs, mapLookupApproxDev(x[idx], mapApproxDev));
    else
        w[idx] = normalPDFObsDev(obs, mapLookupApproxDev(x[idx], mapApproxDev));
}

/*__global__ void propagate(curandState* states, floating_t* x) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    x[idx] += VELOCITY + (curand_normal(&states[idx]) * TRANSITION_STD);
}*/

__global__ void assignPropagate(curandState* states, floating_t* x, floating_t* ancestorX) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    x[idx] = ancestorX[idx] + VELOCITY + (curand_normal(&states[idx]) * TRANSITION_STD);
}

/*__global__ void propagateAndWeigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    x[idx] += VELOCITY + (curand_normal(&states[idx]) * TRANSITION_STD); // prop

    w[idx] = normalPDFObs(obs, mapLookupApproxDev(x[idx], mapApproxDev)); // weigh
}*/

/*__global__ void assignAncestor(floating_t* x, floating_t* ancestorX) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    x[idx] = ancestorX[idx];
}*/

__global__ void assignPropagateWeigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev, floating_t* ancestorX) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    x[idx] = ancestorX[idx] + VELOCITY + (curand_normal(&states[idx]) * TRANSITION_STD); // prop

    if(LOG_WEIGHTS) // weight
        w[idx] = logNormalPDFObsDev(obs, mapLookupApproxDev(x[idx], mapApproxDev));
    else
        w[idx] = normalPDFObsDev(obs, mapLookupApproxDev(x[idx], mapApproxDev));
}

__global__ void normalizeWeights(floating_t* w, double weightSum) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    w[idx] /= weightSum;
}

// Assumes normalized weights for now. Perhaps reverse-sort weights before?
__device__ int sampleCategorical(curandState* state, floating_t* w) {
    floating_t u = curand_uniform(state);
    floating_t acc = w[0];
    int i = 0;
    while(true) { // rip performance
        if(u < acc || i >= NUM_PARTICLES-1)
            return i;
        i++;
        acc += w[i]; // goes out of bounds for high N as weights are low and numerical instability!
    }
    return i;
}

__global__ void sampleAncestorCategorical(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    ancestorX[idx] = x[sampleCategorical(&states[idx], w)];
}

__global__ void systematicCumulativeOffspring(floating_t* prefixSum, int* cumulativeOffspring, floating_t u) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    floating_t expectedCumulativeOffspring = NUM_PARTICLES * prefixSum[idx] / prefixSum[NUM_PARTICLES - 1]; // opimize by saving W[n-1] to constant?
    cumulativeOffspring[idx] = min(NUM_PARTICLES, static_cast<int>(floor(expectedCumulativeOffspring + u)));
}

__global__ void cumulativeOffspringToAncestor(int* cumulativeOffspring, floating_t* ancestorX, floating_t* x) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    int start = idx == 0 ? 0 : cumulativeOffspring[idx - 1];
    int numCurrentOffspring = cumulativeOffspring[idx] - start;
    floating_t xVal = x[idx];
    for(int j = 0; j < numCurrentOffspring; j++)
        ancestorX[start+j] = xVal;
}

__global__ void scaleWeights(floating_t* w, floating_t m) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    w[idx] -= m;
}

__global__ void scaleWeightsAndExp(floating_t* w, floating_t m) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    w[idx] = exp(w[idx] - m);
}

__global__ void rejectionAncestors(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX, floating_t wMax) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    int j = idx;
    curandState state = states[idx]; // doing this locally gave ~20% speedup
    floating_t u = curand_uniform(&state);
    while(u > w[j] / wMax) {
        j = static_cast<int>(curand_uniform(&state) * NUM_PARTICLES - 0.0000000001);
        //if(j >= NUM_PARTICLES || j < 0) 
            //printf("j OUT OF BOUNDS: %d\n", j);
        u = curand_uniform(&state);
    }
    states[idx] = state;
    ancestorX[idx] = x[j]; // coalesced reading? perhaps faster in another kernel?
}

__global__ void rejectionAncestorsLog(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX, floating_t wMax) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if(idx >= NUM_PARTICLES || idx < 0) return;

    int j = idx;
    curandState state = states[idx]; // doing this locally gave ~20% speedup on whole SMC
    floating_t u = curand_uniform(&state);
    while(u > exp(w[j] - wMax)) {
        j = static_cast<int>(curand_uniform(&state) * NUM_PARTICLES - 0.0000000001);
        u = curand_uniform(&state);
    }
    states[idx] = state;
    ancestorX[idx] = x[j]; // coalesced reading? perhaps faster in another kernel?
}
