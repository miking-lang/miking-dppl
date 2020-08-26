#ifndef DISTS_INCLUDED
#define DISTS_INCLUDED

#ifdef __NVCC__
#include <curand_kernel.h>
#endif

#include "scores.cuh"

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
 *
 * The distributions are in alphabetical order with the exception of templated functions at the bottom.  
 */

/**
 * Initializes the generator(s) used on CPU. Should be called before inference starts. Done in MAIN macro.
 */
void initGen();

/**
 * Frees the generator(s) used on CPU. Should be called after inference is done. Done in MAIN macro.
 */
void freeGen();


/**
 * Returns a sample from the Bernoulli distribution.
 * 
 * @param p success probability.
 * @return 1 with probability p, 0 with probability 1-p.
 */
DEV int bernoulli(RAND_STATE_DECLARE floating_t p);

 /**
 * Returns a sample from the Beta distribution.
 * 
 * @param alpha the first shape parameter.
 * @param beta the second shape parameter.
 * @return B(alpha, beta)
 */
DEV floating_t beta(RAND_STATE_DECLARE floating_t alpha, floating_t beta);

/**
 * Returns a sample from the Binomial distribution.
 * 
 * @param p the success probability.
 * @param n the number of trials.
 * @return the number of successes over n independent trials with success probability p. 
 */
DEV int binomial(RAND_STATE_DECLARE floating_t p, int n);

/**
 * Returns a sample from the Cauchy distribution.
 * 
 * @param loc the location parameter of the distribution. 
 * @param scale the scale parameter of the distribution. 
 * @return Cauchy(loc, scale)
 */
DEV floating_t cauchy(RAND_STATE_DECLARE floating_t loc, floating_t scale);

/**
 * Returns a sample from the multi-variate Normal distribution with a diagonal covariance matrix. 
 * 
 * @param mu the array of mean values. 
 * @param sigma the 1D-array of standard deviations, i.e. the diagonal of the covariance matrix. 
 * @param n the number of trials.
 * @param ret the array to fill the sample results. 
 */
DEV void diagCovNormal(RAND_STATE_DECLARE floating_t* mu, floating_t* sigma, int n, floating_t* ret);

/**
 * Returns a sample from the Dirichlet distribution.
 * 
 * @param alpha the concentration parameters, each alpha[i] > 0. 
 * @param n the number of categories. 
 * @param ret pointer to the output array.
 */
DEV void dirichlet(RAND_STATE_DECLARE const floating_t* alpha, const int n, floating_t* ret);

/**
 * Returns a sample from the Discrete distribution.
 * 
 * @param ps the probabilities of getting each index. 
 * @param n the number of categories. 
 * @return An integer in the range [0, n);
 */
DEV int discrete(RAND_STATE_DECLARE const floating_t* ps, const int n);

/**
 * Returns a sample from the Exponential distribution.
 * 
 * @param lambda the rate parameter
 * @return Exp(lambda)
 */
DEV floating_t exponential(RAND_STATE_DECLARE floating_t lambda);

/**
 * Returns a sample from the Gamma distribution.
 * 
 * @param k the shape parameter
 * @param theta the scale parameter
 * @return Gamma(k, theta)
 */
DEV floating_t gamma(RAND_STATE_DECLARE floating_t k, floating_t theta);

/**
 * Returns a sample from the Laplace distribution.
 * 
 * @param loc the location parameter of the distribution. 
 * @param scale the scale parameter of the distribution. 
 * @return Laplace(loc, scale)
 */
DEV floating_t laplace(RAND_STATE_DECLARE floating_t loc, floating_t scale);

/**
 * Returns a sample from the Multinomial distribution. The number of successes for each "bin".
 * 
 * @param ps the success probabilities.
 * @param n the number of trials.
 * @param numCategories the number of categories, should be same as length of ps and returnArr. 
 * @param returnArr the array to fill the sample results. 
 */
DEV void multinomial(RAND_STATE_DECLARE floating_t* ps, int n, int numCategories, int* returnArr);

/**
 * Returns a sample from the multi-variate standard Normal distribution. No covariances. 
 * 
 * @param n the size of the 1D-array to be sampled. 
 * @param ret the array to fill the sample results. 
 */
DEV void multivariateStandardNormal(RAND_STATE_DECLARE int n, floating_t* ret);

/**
 * Returns a sample from the Normal/Gaussian distribution. 
 * 
 * @param mean the mean/location of the gaussian distribution
 * @param std the standard deviation/scale of the gaussian destribution
 * @return N(mean, std^2)
 */
DEV floating_t normal(RAND_STATE_DECLARE floating_t mean, floating_t std);

/**
 * Returns a sample from the Poisson distribution. 
 * 
 * @param lambda the rate parameter
 * @return Po(lambda)
 */
DEV unsigned int poisson(RAND_STATE_DECLARE double lambda);

/**
 * Returns a uniformly random integer.
 * 
 * @param n the exclusive upper limit. 
 * @return A value in the interval [0, n).
 */
DEV int randomInteger(RAND_STATE_DECLARE const int n);

/**
 * Returns a sample from the Uniform distribution on the interval (min, max]
 * 
 * @param min the exclusive minimum value
 * @param max the inclusive maximum value
 * @return U(min, max]
 */
DEV floating_t uniform(RAND_STATE_DECLARE floating_t min, floating_t max);

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
 * Returns a sample from the multi-variate Normal distribution. 
 * 
 * @param mu the array of mean values. 
 * @param cov the covariance matrix of size n.
 * @param ret the array to fill the sample results. 
 */
/*template <size_t n>
DEV void multivariateNormal(RAND_STATE_DECLARE floating_t mu[n], floating_t (&cov)[n][n], floating_t ret[n]) {
    floating_t A[n][n];
    choleskyDecomposition<n>(cov, A);
    floating_t z[n];
    SAMPLE(multivariateStandardNormal, n, z);
    transformColumn<n, n>(A, z, mu, ret);
}*/

#endif