#include <random>
#include <time.h>

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

#include "delayed.cuh"

DEV floating_t sample_GammaExponential(RAND_STATE_DECLARE gamma_t& rate, floating_t f) {
    floating_t t = SAMPLE(lomax, 1/(f*rate.theta), rate.k);
    rate.k = rate.k + 1;
    rate.theta = rate.theta/(1 + t*f*rate.theta);
    return t;
}


DEV floating_t score_GammaExponential(RAND_STATE_DECLARE floating_t x, gamma_t& rate,  floating_t f) {
  if (f < 1e-5) {
    return -INFINITY;
  } 
  // TODO Maybe guard on f*theta
  // TODO instead of multiply here, add the logs in lomax
  floating_t score = lomaxScore(x, 1/(f*rate.theta), rate.k);
  
  rate.k = rate.k + 1;
  rate.theta =  rate.theta/(1 + x*f*rate.theta);
  return score;
}


DEV floating_t score_GammaPoisson(floating_t x, floating_t t, gamma_t& rate, floating_t f)
{
  if (f < 1e-5) {
    return 0.0;
  }
  // TODO guard maybe on f*theta
  // TODO instead of multiplication do add in negativeBionomial score
  floating_t score = negativeBinomialScore(x, rate.k, 1/(1 + t*f*rate.theta));
  
  rate.theta = rate.theta / (1 + t*f*rate.theta);
  return score;
}

