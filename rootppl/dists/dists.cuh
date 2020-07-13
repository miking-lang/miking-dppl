#ifndef DISTS_INCLUDED
#define DISTS_INCLUDED

/*
 * File dists.cuh contains distributions that builds upon the hardware specific distributions. 
 * These implementations will be the same regardless of the hardware. Most of these distributions
 * are inspired by the WebPPL language (i.e. names and format).
 * 
 * The distribution functions uses a macro for the first parameter which specifies that a 
 * curandState pointer should be passed to the function if it is compiled for GPU. 

 * Sampling from these distributions (calling these functions), should be done
 * by using the macro SAMPLE(distName, args...). This will pass a curandState as argument.
 * This curandState will be present in the BBLOCK and BBLOCK_HELPER functions. 
 */


#include <random>
#include <time.h>
const double PI = 3.1415926535897932384626433832795028841971693993751;

#ifdef GPU
#include <curand_kernel.h>
#endif

#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "utils/math.cuh"
#include "scores.cuh"

// Define the CPU generator and primitives, and initializes the generator state. 
std::default_random_engine generatorDists;
std::uniform_real_distribution<floating_t> uniformDist(0.0, 1.0);
std::normal_distribution<floating_t> normalDist(0, 1);
void initGen() {
    generatorDists.seed(time(NULL));
}

//#ifdef GPU
#ifdef GPU
#include "dists_gpu.cuh"
//#endif
#else

#include "dists_cpu.cuh"

#endif


/**
 * Returns a sample from the Bernoulli distribution.
 * 
 * @param p success probability.
 * @return 1 with probability p, 0 with probability 1-p.
 */
DEV int bernoulli(RAND_STATE_DECLARE floating_t p) {
    return SAMPLE(uniform, 0, 1) < p ? 1 : 0;
}

/**
 * Returns a sample from the Exponential distribution.
 * 
 * @param lambda the rate parameter
 * @return Exp(lambda)
 */
DEV floating_t exponential(RAND_STATE_DECLARE floating_t lambda) {
    
    return -log(1 - SAMPLE(uniform, 0, 1)) / lambda;
}

/**
 * Returns a sample from the Gamma distribution.
 * 
 * @param k the shape parameter
 * @param theta the scale parameter
 * @return Gamma(k, theta)
 */
DEV floating_t gamma(RAND_STATE_DECLARE floating_t k, floating_t theta) {
    // This gamma sampling is based on the implementation used by GSL
    if (k < 1) {
        double u = SAMPLE(uniform, 0, 1);
        return SAMPLE(gamma, 1.0 + k, theta) * pow(u, 1.0 / k);
    }
    
    {
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = SAMPLE(normal, 0, 1);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = SAMPLE(uniform, 0, 1);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        return theta * d * v;
    }
    
}

/**
 * Returns a sample from the Dirichlet distribution.
 * 
 * @param alpha the concentration parameters, each alpha[i] > 0. 
 * @param n the number of categories. 
 * @param ret pointer to the output array.
 */
DEV void dirichlet(RAND_STATE_DECLARE const floating_t* alpha, const int n, floating_t* ret) {
    
    floating_t sum = 0;
    for (int k = 0; k < n; k++) {
        ret[k] = SAMPLE(gamma, alpha[k], 1);
        sum += ret[k];
    }

    for (int k = 0; k < n; k++) 
        ret[k] /= sum;
}

/**
 * Returns a sample from the Discrete distribution.
 * 
 * @param ps the probabilities of getting each index. 
 * @param n the number of categories. 
 * @return An integer in the range [0, n);
 */
DEV int discrete(RAND_STATE_DECLARE const floating_t* ps, const int n) {
    // Could be optimized, if many samples should be done
    floating_t u = SAMPLE(uniform, 0, 1);
    floating_t sum = 0;
    int idx = 0;
    for(idx = 0; idx < n-1; idx++) {
        sum += ps[idx];
        if(u <= sum)
            break;
    }
    return idx;
}

/**
 * Returns a sample from the Categorical distribution.
 * 
 * @param ps the probabilities of getting each index. 
 * @param n the number of categories. 
 * @param arr the values to choose from when returning.
 * @return A value in the array arr with the index sampled from Discrete. 
 */
template <typename T>
DEV T categorical(RAND_STATE_DECLARE const floating_t* ps, const int n, T* arr) {
    int idx = SAMPLE(discrete, ps, n);
    return arr[idx];
}

/**
 * Returns a uniformly random integer.
 * 
 * @param n the exclusive upper limit. 
 * @return A value in the interval [0, n).
 */
DEV int randomInteger(RAND_STATE_DECLARE const int n) {
    floating_t u = SAMPLE(uniform, 0, n) - 0.000000000000001;
    return static_cast<int>(u);
}

/**
 * Returns a sample from the Beta distribution.
 * 
 * @param alpha the first shape parameter.
 * @param beta the second shape parameter.
 * @return B(alpha, beta)
 */
DEV floating_t beta(RAND_STATE_DECLARE floating_t alpha, floating_t beta) {
    floating_t x = SAMPLE(gamma, alpha, 1);
    floating_t y = SAMPLE(gamma, beta, 1);
    return x / (x + y);
}

/**
 * Returns a sample from the Binomial distribution.
 * 
 * @param p the success probability.
 * @param n the number of trials.
 * @return the number of successes over n independent trials with success probability p. 
 */
DEV int binomial(RAND_STATE_DECLARE floating_t p, int n) {
    // Could be done more efficiently, see alias methods or webppl source code
    int numSuccesses = 0;
    for(int t = 0; t < n; t++)
        numSuccesses += SAMPLE(bernoulli, p);

    return numSuccesses;
}

/**
 * Returns a sample from the Cauchy distribution.
 * 
 * @param loc the location parameter of the distribution. 
 * @param scale the scale parameter of the distribution. 
 * @return Cauchy(loc, scale)
 */
DEV floating_t cauchy(RAND_STATE_DECLARE floating_t loc, floating_t scale) {
    return loc + scale * tan(PI * (SAMPLE(uniform, 0, 1) - 0.5));
}

/**
 * Returns a sample from the Laplace distribution.
 * 
 * @param loc the location parameter of the distribution. 
 * @param scale the scale parameter of the distribution. 
 * @return Laplace(loc, scale)
 */
DEV floating_t laplace(RAND_STATE_DECLARE floating_t loc, floating_t scale) {
    floating_t u = SAMPLE(uniform, -0.5, 0.5);
    return loc - scale * sgn(u) * log(1 - 2 * abs(u));
}

/**
 * Returns a sample from the Multinomial distribution. The number of successes for each "bin".
 * 
 * @param ps the success probabilities.
 * @param n the number of trials.
 * @param numCategories the number of categories, should be same as length of ps and returnArr. 
 * @param returnArr the array to fill the sample results. 
 */
DEV void multinomial(RAND_STATE_DECLARE floating_t* ps, int n, int numCategories, int* returnArr) {

    // Can be much more efficient for large n, using host API on GPU (alias methods)
    // Just sorting probabilities in descending order should give performance increase for many samples
    
    for(int k = 0; k < numCategories; k++)
        returnArr[k] = 0;

    for(int t = 0; t < n; t++) {
        int idx = SAMPLE(discrete, ps, numCategories);
        returnArr[idx]++;
    }
}

/**
 * Returns a sample from the multi-variate Normal distribution with a diagonal covariance matrix. 
 * 
 * @param mu the array of mean values. 
 * @param sigma the 1D-array of standard deviations, i.e. the diagonal of the covariance matrix. 
 * @param n the number of trials.
 * @param ret the array to fill the sample results. 
 */
DEV void diagCovNormal(RAND_STATE_DECLARE floating_t* mu, floating_t* sigma, int n, floating_t* ret) {

    // Could be optimized with curand2 or curand4 that samples more than one value at a time.
    for(int k = 0; k < n; k++) {
        ret[k] = SAMPLE(normal, mu[k], sigma[k]);
    }
}

/**
 * Returns a sample from the multi-variate standard Normal distribution. No covariances. 
 * 
 * @param n the size of the 1D-array to be sampled. 
 * @param ret the array to fill the sample results. 
 */
DEV void multivariateStandardNormal(RAND_STATE_DECLARE int n, floating_t* ret) {
    for(int k = 0; k < n; k++) {
        ret[k] = SAMPLE(normal, 0, 1);
    }
}

/**
 * Returns a sample from the multi-variate Normal distribution. 
 * 
 * @param mu the array of mean values. 
 * @param cov the covariance matrix of size n.
 * @param ret the array to fill the sample results. 
 */
template <size_t n>
DEV void multivariateNormal(RAND_STATE_DECLARE floating_t mu[n], floating_t (&cov)[n][n], floating_t ret[n]) {
    floating_t A[n][n];
    choleskyDecomposition<n>(cov, A);
    floating_t z[n];
    SAMPLE(multivariateStandardNormal, n, z);
    transformColumn<n, n>(A, z, mu, ret);
}

#endif