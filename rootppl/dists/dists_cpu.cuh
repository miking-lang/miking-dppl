#ifndef DISTS_CPU
#define DISTS_CPU

/*
 * File dists_cpu.cuh contains primitive distributions specific for the CPU. 
 */

#ifdef _OPENMP
#include <omp.h>
#endif

/**
 * Returns a sample from the Uniform distribution on the interval [min, max)
 * 
 * @param min the inclusive minimum value
 * @param max the exclusive maximum value
 * @return U[min, max)
 */
floating_t uniform(floating_t min, floating_t max) {
    floating_t u;
    #ifdef _OPENMP
    u = uniformDist(genWrappers[omp_get_thread_num()].gen);
    #else
    u = uniformDist(gen);
    #endif
    return (u * (max - min)) + min;
}

/**
 * Returns a sample from the Normal/Gaussian distribution. 
 * 
 * @param mean the mean/location of the gaussian distribution
 * @param std the standard deviation/scale of the gaussian destribution
 * @return N(mean, std^2)
 */
floating_t normal(floating_t mean, floating_t std) {
    floating_t n;
    #ifdef _OPENMP
    n = normalDist(genWrappers[omp_get_thread_num()].gen);
    #else
    n = normalDist(gen);
    #endif
    return (n * std) + mean;
}

/**
 * Returns a sample from the Poisson distribution. 
 * 
 * @param lambda the rate parameter
 * @return Po(lambda)
 */
unsigned int poisson(double lambda) {
    // Reuse distribution object when possible? However, does not seem to be that expensive to create dist object
    std::poisson_distribution<unsigned int> poissonDist(lambda);
    #ifdef _OPENMP
    return poissonDist(genWrappers[omp_get_thread_num()].gen);
    #else
    return poissonDist(gen);
    #endif
}

/**
 * Returns a sample from the Binomial distribution.
 *
 * @param n number of trials
 * @param p success probability for each trial
 * @return Binomial(n, p)
 */
int binomial(double p, int n) {
    std::binomial_distribution<int> dist(n,p);
    #ifdef _OPENMP
    return dist(genWrappers[omp_get_thread_num()].gen);
    #else
    return dist(gen);
    #endif
}

/**
 * Returns a sample from Student's t-distribution. Parametrization 2021.
 *
 * @param k > 0 real, degrees of freedom.
 */
DEV floating_t student_t_classic(RAND_STATE_DECLARE floating_t k) {
  std::student_t_distribution<double> dist(k);
  #ifdef _OPENMP
  return dist(genWrappers[omp_get_thread_num()].gen);
  #else
  return dist(gen);
  #endif
}

#endif
