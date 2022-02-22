/**
 * Beta distribution type
 *
 * @param alpha first shape.
 * @param beta second shape.
 */
struct beta_t {
  floating_t alpha;
  floating_t beta;
  
  DEV beta_t(){}; 
  DEV beta_t(floating_t alpha_, floating_t beta_) {
    alpha = alpha_;
    beta = beta_;
  }
};


/**
 * Gamma Type
 *
 * @param k shape.
 * @param theta scale.
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
 * NormalInverseGamma Type
 *
 * σ^2 | a,b ~ InverseGamma(a, b)
 * m ~ N(m0, v σ^2)
 * 
 * <=> σ^2, m ~ NormalInverseGamma(m0, v, a, b) 
 *
 * @param m0 initial mean.
 * @param v scale by which the variance is multiplied.
 * @param a gamma-parameter of the variance (shape).
 * @param b gamma-parameter of the variance (scale).
 */
struct normalInverseGamma_t {
  floating_t m0;
  floating_t v;
  floating_t a;
  floating_t b;

  DEV normalInverseGamma_t(){};
  DEV normalInverseGamma_t(floating_t m0_, floating_t v_, floating_t a_, floating_t b_) {
    m0 = m0_;
    v = v_;
    a = a_;
    b = b_;
  }
};


/**
 * Returns a sample from the Bernoulli distribution with a Beta prior (BetaBernoulli).
 * 
 * @param p beta_t distribution of the success probability.
 * @return 1 with probability sampled from p, 0 otherwise.
 */
DEV floating_t betaBernoulli(RAND_STATE_DECLARE beta_t& p);


/**
 * Score of betaBernoulli
 * 
 * @param p beta_t distribution of the success probability.
 * @param x variate.
 *
 * @retun log-probability.
 */
HOST DEV floating_t betaBernoulliScore(beta_t& p, int x);


/**
 * Delayed Sampling from a GammaExponential Mixture
 *
 * E.g. to sample a waiting time.
 *
 * @param rate  the prior gamma rate as reference (mutable, side-effect)
 * @param f factor (multiplier) by which the scale will be multiplied
 *
 * @return waiting time until an exponentially distributed event with a gamma prior
 */
DEV floating_t gammaExponential(RAND_STATE_DECLARE gamma_t& rate, floating_t f);


/**
 * Log-score (delayed) of observation from a GammaExponential
 * 
 * E.g. to observe a waiting time.
 * 
 * @param x  observation.
 * @param rate the prior gamma rate as reference (mutable, side effect)
 * @param f factor (multiplier) by which the scale will be multiplied
 *
 * @return the log score.
 */
DEV floating_t gammaExponentialScore(floating_t x, gamma_t& rate,  floating_t f);



/**
 * Log-score (delayed) of observation from a GammaPoisson
 * 
 * E.g. to observe a number of speciation points.
 * 
 * @param x  observation.
 * @param rate prior gamma rate as a reference (mutable, side effect)
 * @param f factor (multiplier) by which the scale will be multiplied
 *
 * @return returns the log score.
 */
DEV floating_t gammaPoissonScore(floating_t x, floating_t t, gamma_t& rate, floating_t f);


/**
 * Returns a Sample from NormalInverseGammaNormal (delayed)
 *
 * E.g. to sample a factor by which the speciation rate is multiplied.
 *
 * m, σ^2 ~ NIG(m0, v, a, b)
 * f ~ N(m, σ^2)
 *
 * @param prior parameters (mutable, will be updated).
 *
 * @retun a sample from normal distribution, with NIG-prior.
 */
DEV floating_t normalInverseGammaNormal(RAND_STATE_DECLARE normalInverseGamma_t& prior);

/**
 * Returns a Sample from LinearNormalInverseGammaNormal (delayed)
 *
 * E.g. to sample a factor by which the speciation rate is multiplied.
 *
 * m, σ^2 ~ NIG(m0, v, a, b)
 * f ~ N(aleph*m + c, s2*σ^2)
 *
 * @param prior struct of m0, v, a, b prior parameters (mutable, will be updated).
 * @param aleph scale of mean
 * @param c offset of mean
 * @param s2 scale of variance
 *
 * @retun a sample from normal distribution, with NIG-prior.
 */
DEV floating_t linearNormalInverseGammaNormal(RAND_STATE_DECLARE normalInverseGamma_t& prior, floating_t aleph, floating_t c, floating_t s2);
