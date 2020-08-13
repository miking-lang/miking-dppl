#ifndef SCORES_INCLUDED
#define SCORES_INCLUDED

/*
 * File scores.cuh contains the log probability/density functions of distributions. 
 * Most of these implementations are inspired from the implementation of WebPPL.
 *
 * NOTE: some of the multi-variate distribution scores are yet to be implemented!
 * TODO: diagCovNormal, dirichlet, multinomial, multiVariateNormal
 */

HOST DEV floating_t logGamma(floating_t xx);

HOST DEV floating_t lnfactExact(int x);

HOST DEV int fact(int x);

HOST DEV floating_t lnfact(int x);

HOST DEV floating_t bernoulliScore(floating_t p, int x);

// Helper function
HOST DEV floating_t logBeta(floating_t a, floating_t b);

HOST DEV floating_t betaScore(floating_t a, floating_t b, floating_t x);

HOST DEV floating_t binomialScore(floating_t p, int n, int x);

template <typename T>
HOST DEV floating_t categoricalScore(const floating_t* ps, const int n, T* arr, T x);

HOST DEV floating_t cauchyScore(floating_t loc, floating_t scale, floating_t x);

// diagCovNormal

// dirichlet

HOST DEV floating_t discreteScore(const floating_t* ps, const int n, int x);

HOST DEV floating_t exponentialScore(floating_t lambda, int x);

HOST DEV floating_t gammaScore(floating_t k, floating_t theta, floating_t x);

HOST DEV floating_t laplaceScore(floating_t loc, floating_t scale, floating_t x);

// multinomial

// multiVariateNormal

HOST DEV floating_t normalScore(floating_t mean, floating_t std, floating_t x);

// The range differs on CPU and GPU unfortunately, should that affect this? Is it negligible?
HOST DEV floating_t uniformScore(floating_t min, floating_t max, floating_t x);

#endif
