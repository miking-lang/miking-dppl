#include <random>
#include <time.h>
#include <cassert>

#ifdef _OPENMP
#include <omp.h>
#endif

#ifdef __NVCC__
#include <curand_kernel.h>
#endif

#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "utils/math.cuh"
#include "scores.cuh"
#include "macros/macros.cuh"

#include "dists/delayed.cuh"

DEV floating_t betaBernoulli(RAND_STATE_DECLARE beta_t& p) {
  // The shapes of the beta distribution need to be non-negative
  assert(0.0 < p.alpha); 
  assert(0.0 < p.beta);

  //floating_t eps = SAMPLE(beta, p.alpha, p.beta);
  floating_t prob = p.alpha/(p.alpha + p.beta);
  int x = SAMPLE(bernoulli, prob);
  
  // Updates
  p.alpha = p.alpha + x;
  p.beta = p.beta + 1.0 - x;

  //printf("updated alpha = %f, updated beta = %f, p = %f\n", p.alpha, p.beta, p.alpha/(p.alpha + p.beta));
  return x;
}

HOST DEV floating_t betaBernoulliScore(beta_t& p, int x) {
  floating_t s0 = log(p.beta);
  if (x) {
    s0 = log(p.alpha);
  }
  
  return s0 - log(p.alpha + p.beta);
}

DEV floating_t gammaExponential(RAND_STATE_DECLARE gamma_t& rate, floating_t f) {
  floating_t t = SAMPLE(lomax, 1/(f*rate.theta), rate.k);

  assert(0.0 < rate.theta/(1 + t*f*rate.theta));					  
  rate.k = rate.k + 1;
  rate.theta = rate.theta/(1 + t*f*rate.theta);
  return t;
}

DEV floating_t gammaExponentialScore( floating_t x, gamma_t& rate,  floating_t f) {
  floating_t score = lomaxScore(x, 1/(f*rate.theta), rate.k);

  assert(0.0 < rate.theta/(1 + x*f*rate.theta));
  
  rate.k = rate.k + 1;
  rate.theta =  rate.theta/(1 + x*f*rate.theta);
  return score;
}

DEV floating_t gammaPoissonScore(floating_t x, floating_t t, gamma_t& rate, floating_t f)
{
  assert(0.0 <= f);
  assert(0.0 <= t);
  assert(0.0 <= rate.theta);
 
  floating_t score = negativeBinomialScore(x, rate.k, 1/(1 + t*f*rate.theta));

  assert(0.0 < rate.theta/(1 + t*f*rate.theta));
  rate.theta = rate.theta / (1 + t*f*rate.theta);
  return score;
}

DEV floating_t normalInverseGammaNormal(RAND_STATE_DECLARE normalInverseGamma_t& prior) {
  floating_t m0 = prior.m0;
  floating_t v = prior.v;
  floating_t a = prior.a;
  floating_t b = prior.b;
  floating_t f = SAMPLE(linearStudent_t, 2.0*a, m0, (1.0 + 1.0/v)*b/a);
  prior.m0 = (v*m0 + f)/(v + 1);
  prior.v  = v + 1;
  prior.a  = a + 0.5;
  prior.b  = b + 0.5*(v/(v + 1)*(f - m0)*(f - m0));
  return f;
}

DEV floating_t linearNormalInverseGammaNormal(RAND_STATE_DECLARE normalInverseGamma_t& prior, floating_t aleph, floating_t c, floating_t s2) {
  floating_t m0 = prior.m0;
  floating_t v = prior.v;
  floating_t a = prior.a;
  floating_t b = prior.b;
  floating_t f = SAMPLE(linearStudent_t, 2.0*a, aleph*m0 + c, (s2 + aleph*aleph/v)*b/a);
  
  floating_t l = 1.0/s2;
  floating_t y = f - c;
  floating_t z = l*y;
  floating_t nu_prime = (v*m0 + aleph*z);

  prior.v  = v + aleph*l*aleph;

  prior.m0 = nu_prime/prior.v;
  prior.a  = a + 0.5;
  prior.b  = b + 0.5*(y*z + v*m0*m0 - nu_prime*nu_prime/prior.v);

  return f;
}
