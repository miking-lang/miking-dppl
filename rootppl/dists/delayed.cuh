// DELAYED SAMPLING HEADER FILE
// TODO split in header and source files and move to framework.

/**
 * Gamma distribution type
 *
 * k    -  shape
 * theta - scale
 */
struct gamma_t {
  floating_t k;
  floating_t theta;
  
  DEV gamma_t(){}; 
  DEV gamma_t(floating_t k_, floating_t theta_) {
    k = k_;
    theta = theta_;
  }
};


/**
 * Delayed Sampling from a GammaExponential Mixture
 *
 * E.g. to sample a waiting time.
 *
 * t - returns waiting time
 * rate - a reference to the prior (will modify)
 * f - factor by which the scale may be multiplied
 */
DEV floating_t sample_GammaExponential(RAND_STATE_DECLARE gamma_t& rate, floating_t f);


/**
 * Log-score of observation from a GammaExponential
 * 
 * E.g. to observe a waiting time.
 * 
 * score - returns the log score
 * x - observation
 * rate - reference to rate (will modify)
 * f - factor by which the scale may be multiplied
 */
DEV floating_t score_GammaExponential(RAND_STATE_DECLARE floating_t x, gamma_t& rate,  floating_t f);



/**
 * Log-score of observation from a GammaPoisson
 * 
 * E.g. to observe a number of speciation points.
 * 
 * score - returns the log score
 * x - observation
 * rate - reference to rate (will modify)
 * f - factor by which the scale may be multiplied
 */
DEV floating_t score_GammaPoisson(floating_t x, floating_t t, gamma_t& rate, floating_t f);

