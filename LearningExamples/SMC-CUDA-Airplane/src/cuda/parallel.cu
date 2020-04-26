#include <stdio.h>
#include <random>
#include <cuda.h>
// #include <curand.h>
#include <curand_kernel.h>
#include <thrust/reduce.h>
#include <thrust/scan.h>
#include <thrust/execution_policy.h>
#include <thrust/extrema.h>
#include <thrust/sort.h>

#include "device_launch_parameters.h"
#include "cuda_runtime.h"
#include "../utils.h"
#include "utilsGPU.cuh"
#include "cudaErrorUtils.cu"
#include "parallelKernels.cuh"
#include "cuda_profiler_api.h"


floating_t *x, *w, *ancestorX, *prefixSum;
int* cumulativeOffspring;
floating_t *planeXDev, *planeObsDev, *mapApproxDev;
curandState* randStates;

bool initializedRandStates = false;
const bool DEBUG_MSE = false;
double MSE;
double MSEAvg = 0;
int counter = 0;

using namespace std;

floating_t *sortW;
void calcMSE() {
    cudaSafeCall(cudaMemcpy(sortW, w, NUM_PARTICLES * sizeof(floating_t), cudaMemcpyDefault));
    double weightSum = thrust::reduce(thrust::device, sortW, sortW + NUM_PARTICLES);
    if(RESAMPLING_STRATEGY == SYSTEMATIC) {
        for(int i = 0; i < NUM_PARTICLES; i++) {
            double expectedOffspring = (w[i] * NUM_PARTICLES) / weightSum;

            int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
            int numCurrentOffspring = cumulativeOffspring[i] - start;
            MSE += pow(numCurrentOffspring - expectedOffspring, 2);
        }
    } else if(RESAMPLING_STRATEGY == REJECTION) {

        /*for(int i = 0; i < NUM_PARTICLES; i++) {
            double expectedOffspring = (w[i] * NUM_PARTICLES) / weightSum;
        }*/
    }
}

void runSMC(floating_t* planeX, floating_t* planeObs) {
    default_random_engine generator;
    generator.seed(time(NULL));
    uniform_real_distribution<floating_t> uniDist(0.0, 1.0);
    MSE = 0;
    // Initialize
    int numThreadsPerBlock = 64;
    int numBlocks = (NUM_PARTICLES + numThreadsPerBlock - 1) / numThreadsPerBlock;
    if(! initializedRandStates) {
        printf("Initializing rand states...\n");
        initRandStates<<<numBlocks, numThreadsPerBlock>>>(randStates); // Apparently slow!
        cudaDeviceSynchronize();
        printf("Initialized!\n");
        initializedRandStates = true;
    }
    // cudaProfilerStart();
    initX<<<numBlocks, numThreadsPerBlock>>>(randStates, x);
    cudaCheckError();

    //printArray(x, NUM_PARTICLES);

    // First weights update
    weigh<<<numBlocks, numThreadsPerBlock>>>(randStates, x, w, planeObs[0], mapApproxDev);
    cudaCheckError();
    cudaDeviceSynchronize();
    if(LOG_WEIGHTS) {
        if(RESAMPLING_STRATEGY != REJECTION) {
            floating_t m = *(thrust::max_element(thrust::device, w, w + NUM_PARTICLES)); // should be on device
            // floating_t m = log(maxPDFObs()); // not sure if this is okay
            // floating_t m = maxValue(w, NUM_PARTICLES); // just for test
            scaleWeightsAndExp<<<numBlocks, numThreadsPerBlock>>>(w, m);
            cudaCheckError();
        }
    }
    
    if(RESAMPLING_STRATEGY == MULTINOMIAL) {
        floating_t weightSum = thrust::reduce(w, w + NUM_PARTICLES);
        normalizeWeights<<<numBlocks, numThreadsPerBlock>>>(w, weightSum);
        cudaCheckError();
    }

    for(int t = 0; t < TIME_STEPS; t++) {
        // Resample
        if(RESAMPLING_STRATEGY == MULTINOMIAL)
            sampleAncestorCategorical<<<numBlocks, numThreadsPerBlock>>>(randStates, x, w, ancestorX); // rip performance
        else if(RESAMPLING_STRATEGY == SYSTEMATIC) {
            thrust::inclusive_scan(thrust::device, w, w + NUM_PARTICLES, prefixSum); // prefix sum
            floating_t u = uniDist(generator);
            systematicCumulativeOffspring<<<numBlocks, numThreadsPerBlock>>>(prefixSum, cumulativeOffspring, u); // 0.00909617 seconds for N=2^18
            cudaCheckError();
            cumulativeOffspringToAncestor<<<numBlocks, numThreadsPerBlock>>>(cumulativeOffspring, ancestorX, x);
        } else if(RESAMPLING_STRATEGY == REJECTION) {
            // empirical
            // floating_t m = *(thrust::max_element(thrust::device, w, w + NUM_PARTICLES)); // done twice now, if log weights? can we compare log weights instead?
            // alternative: approximated
            if(LOG_WEIGHTS) {
                floating_t m = log(maxPDFObs());
                rejectionAncestorsLog<<<numBlocks, numThreadsPerBlock>>>(randStates, x, w, ancestorX, m);
            } else {
                floating_t m = maxPDFObs();
                rejectionAncestors<<<numBlocks, numThreadsPerBlock>>>(randStates, x, w, ancestorX, m);
            }
        }

        cudaCheckError();

        if(DEBUG_MSE)
            calcMSE();

        // assignAncestor<<<numBlocks, numThreadsPerBlock>>>(x, ancestorX);
        // cudaCheckError();
        
        // Propagate & Update weights
        if(t < TIME_STEPS-1) {
            assignPropagateWeigh<<<numBlocks, numThreadsPerBlock>>>(randStates, x, w, planeObs[t+1], mapApproxDev, ancestorX);
            cudaCheckError();
            if(LOG_WEIGHTS) {
                if(RESAMPLING_STRATEGY != REJECTION) {
                    floating_t m = *(thrust::max_element(thrust::device, w, w + NUM_PARTICLES)); // should be on device
                    // floating_t m = maxValue(w, NUM_PARTICLES); // just for test
                    // floating_t m = log(maxPDFObs()); // not sure if this is okay
                    scaleWeightsAndExp<<<numBlocks, numThreadsPerBlock>>>(w, m);
                    cudaCheckError();
                }
            }
        }
        else
            assignPropagate<<<numBlocks, numThreadsPerBlock>>>(randStates, x, ancestorX);

        cudaCheckError();

        cudaDeviceSynchronize(); // necessary?

        if(RESAMPLING_STRATEGY == MULTINOMIAL) {
            floating_t weightSum = thrust::reduce(w, w + NUM_PARTICLES);
            normalizeWeights<<<numBlocks, numThreadsPerBlock>>>(w, weightSum);
            cudaCheckError();
        }

        // CURAND_CALL(curandDestroyDistribution(catDist));
        // printArray(x, NUM_PARTICLES);
        // printStatus(x, w, planeX, t);
    }
    //cudaProfilerStop();
    if(DEBUG_MSE) {
        MSE /= TIME_STEPS;
        if(MSE/NUM_PARTICLES > 20) {
            printf("MSE: %f, MSE/N: %f\n", MSE, MSE/NUM_PARTICLES);
            printf("AvgMSE/N: %f\n", MSEAvg / counter);
        }
        MSEAvg += MSE / NUM_PARTICLES;
        counter++;
        if(counter >= 20) {
            printf("AvgMSE/N: %f\n", MSEAvg / counter);
        }
    }
    //printStatus(x, w, planeX, TIME_STEPS-1);
}

void smc(floating_t* planeX, floating_t* planeObs) {
    // curandDiscreteDistribution_t catDist;

    // cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    // Transfer map to device
    cudaMalloc(&mapApproxDev, MAP_SIZE * sizeof(floating_t));
    cudaSafeCall(cudaMemcpy(mapApproxDev, getMapApproxArr(), MAP_SIZE * sizeof(floating_t), cudaMemcpyHostToDevice));

    // Allocate GPU compatible memory
    cudaSafeCall(cudaMallocManaged(&x, NUM_PARTICLES * sizeof(floating_t)));
    cudaSafeCall(cudaMallocManaged(&w, NUM_PARTICLES * sizeof(floating_t)));
    cudaSafeCall(cudaMallocManaged(&ancestorX, NUM_PARTICLES * sizeof(floating_t)));

    cudaSafeCall(cudaMallocManaged(&sortW, NUM_PARTICLES * sizeof(floating_t)));

    if(RESAMPLING_STRATEGY == SYSTEMATIC) {
        cudaSafeCall(cudaMallocManaged(&prefixSum, NUM_PARTICLES * sizeof(floating_t)));
        cudaSafeCall(cudaMallocManaged(&cumulativeOffspring, NUM_PARTICLES * sizeof(int)));
    }

    cudaSafeCall(cudaMallocManaged(&planeXDev, TIME_STEPS * sizeof(floating_t)));
    cudaSafeCall(cudaMallocManaged(&planeObsDev, TIME_STEPS * sizeof(floating_t)));

    if(! initializedRandStates)
        cudaSafeCall(cudaMallocManaged(&randStates, NUM_PARTICLES * sizeof(curandState)));

    // Transfer to Cuda managed memory
    cudaSafeCall(cudaMemcpy(planeXDev, planeX, TIME_STEPS * sizeof(floating_t), cudaMemcpyDefault));
    cudaSafeCall(cudaMemcpy(planeObsDev, planeObs, TIME_STEPS * sizeof(floating_t), cudaMemcpyDefault));

    runSMC(planeX, planeObs);

    // Print result to file?

    cudaFree(x);
    cudaFree(w);
    cudaFree(ancestorX);
    if(RESAMPLING_STRATEGY == SYSTEMATIC) {
        cudaFree(prefixSum);
        cudaFree(cumulativeOffspring);
    }

    cudaFree(planeXDev);
    cudaFree(planeObsDev);
    cudaFree(mapApproxDev);
    cudaFree(sortW);
}

void freeRandStates() {
    cudaFree(randStates);
}


/* (min exec time over 10.000 trials) 
N=2^10
    singe_precision
        REJECTION: 0.000804418 seconds
        SYSTEMATIC: 0.00122542 seconds
    double_precision
        REJECTION: 0.000796794 seconds
        SYSTEMATIC: 0.00128205 seconds

N=2^15 (min exec time over 1000 trials)
    singe_precision
        REJECTION: 0.00513635 seconds
        SYSTEMATIC: 0.00403192 seconds
    double_precision
        REJECTION: 0.00510502 seconds
        SYSTEMATIC: 0.00442799 seconds

N=2^15, with log-weights in systematic, without approx max in systematic
    singe_precision
        REJECTION: 0.00517758 seconds
        SYSTEMATIC: 0.00525198 seconds
    double_precision
        REJECTION: 0.00511814 seconds
        SYSTEMATIC: 0.0059867 seconds

N=2^20 (min exec time over 50 trials)
    singe_precision
        REJECTION: 0.119812 seconds
        SYSTEMATIC: 0.0690664 seconds
    double_precision
        REJECTION: 0.118409 seconds
        SYSTEMATIC: 0.083449 seconds

N=2^20, with log-weights in systematic, without approx max in systematic
    single_precision
        REJECTION: 0.119864 seconds
    double_precision
        SYSTEMATIC: 0.0909 seconds
*/
