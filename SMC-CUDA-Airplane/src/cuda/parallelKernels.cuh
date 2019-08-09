

__global__ void initRandStates(curandState* states);

__global__ void initX(curandState* states, floating_t* x);

__global__ void weigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev);

//__global__ void propagate(curandState* states, floating_t* x);

__global__ void assignPropagateWeigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev, floating_t* ancestorX);
// __global__ void propagateAndWeigh(curandState* states, floating_t* x, floating_t* w, floating_t obs, floating_t* mapApproxDev);

__global__ void normalizeWeights(floating_t* w, double weightSum);

__global__ void sampleAncestorCategorical(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX);

// __global__ void assignAncestor(floating_t* x, floating_t* ancestorX);
__global__ void assignPropagate(curandState* states, floating_t* x, floating_t* ancestorX);

__global__ void systematicCumulativeOffspring(floating_t* prefixSum, int* cumulativeOffspring, floating_t u);

__global__ void cumulativeOffspringToAncestor(int* cumulativeOffspring, floating_t* ancestorX, floating_t* x);

__global__ void scaleWeights(floating_t* w, floating_t m);

__global__ void scaleWeightsAndExp(floating_t* w, floating_t m);

__global__ void rejectionAncestors(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX, floating_t wMax);

__global__ void rejectionAncestorsLog(curandState* states, floating_t* x, floating_t* w, floating_t* ancestorX, floating_t wMax);
