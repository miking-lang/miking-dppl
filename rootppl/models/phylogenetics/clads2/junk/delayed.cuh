/**
 * Delayed sampling functions
 * 
 * TODO split in header and source files and move to framework.
 */

/** Rate type
 *
 * The basic idea here is that the rate parameters,
 * i.e. lambda or mu, are now represented 
 * by three variables: k, theta, f
 * k - shape of Gamma distr
 * theta - scale of Gamma distr
 */
struct rate_t {
  floating_t k;
  floating_t theta;
  
  DEV rate_t(){}; 
  DEV rate_t(floating_t k_, floating_t theta_) {
    k = k_;
    theta = theta_;
  }
};


/** 
 * Next we will create some functions for delayed sampling
 * they will return a value (res) and updated proposal for the rate.
 * - res is either a score (log-weight) or a waiting time, so in both cases positive float
 */ 
struct ret_delayed_t {
  floating_t res;
  floating_t k;
  floating_t theta;
  
  DEV ret_delayed_t(){};
  DEV ret_delayed_t(floating_t res_, floating_t k_, floating_t theta_) {
    // TODO add an assertion here
        res = res_;
	k = k_;
	theta = theta_;
  }
};




/**
 * Delayed sampling of waiting time until next event (with factor)
 * 
 * use 1 if no factor
 *
 * - k: shape parameter of the Gamma prior
 * - theta: scale parameter of the Gamma prior
 * - f: factor by which the scale is multiplied
 * 
 * Returns: ret_delayed_t
 */
BBLOCK_HELPER(sampleWaitingTimeDelayed, {
    floating_t t = SAMPLE(lomax, 1/(f*theta), k);
    ret_delayed_t ret(t, k + 1, theta/(1 + t*f*theta));
    return ret;
    
  }, ret_delayed_t, floating_t k, floating_t theta, floating_t f);		




/**
 * Delayed sampling of waiting time until next event (with side effect)
 * Side-Effect
 * 
 * use 1 if no factor
 *
 * - rate: a reference to the rate (will be updated)
 * - factor: factor by which the scale is multiplied
 * 
 * Returns: ret_delayed_t
 */
BBLOCK_HELPER(sampleWaitingTimeDelayedRef, {
    floating_t t = SAMPLE(lomax, 1/(f*rate.theta), rate.k);
    rate.k = rate.k + 1;
    rate.theta = rate.theta/(1 + t*f*rate.theta);
    return t;
    	   
  }, floating_t, rate_t &rate, floating_t f);		




/**
 * Observe a given waiting time 
 *
 * - x: observed waiting time
 * - k: prior shape
 * - theta: prior scale
 * - f: factor
 * 
 * Returns: ret_delayed_t
 */ 
BBLOCK_HELPER(observeWaitingTimeDelayed, {
    ret_delayed_t ret(lomaxScore(x, 1/(f*theta), k), k + 1,  theta/(1 + x*f*theta));
    return ret;

  }, ret_delayed_t, floating_t x, floating_t k, floating_t theta, floating_t f);
 


/**
 * Observe a given waiting time (with side effect), 
 *
 * - x: observed waiting time
 * - rate: prior rate (will be update)
 * - f: factor
 * 
 * Returns: the score and makes an update on rate
 */ 
 BBLOCK_HELPER(observeWaitingTimeDelayedRef, {
  if (f < 1e-5) {
    return -INFINITY;
  } 
  // TODO Maybe guard on f*theta
  // TODO instead of multiply here, add the logs in lomax
  floating_t score = lomaxScore(x, 1/(f*rate.theta), rate.k);
  
  rate.k = rate.k + 1;
  rate.theta =  rate.theta/(1 + x*f*rate.theta);
  return score;
}, floating_t, floating_t x, rate_t& rate,  floating_t f);




/**
 * Delayed observe a given number of events from the Poisson process
 * See Jan's thesis, page 60
 *
 * - x: number of events
 * - t: how much time the poisson process runs
 * - k: shape paramater
 * - scale: delay parameter
 * - f: factor by which the scale of the gamma is changed
 *
 * Returns: ret_delayed_t
 */ 
  BBLOCK_HELPER(observeXEventsDelayed, {
   
      ret_delayed_t ret(negativeBinomialScore(x, k, 1/(1 + t*f*theta)), k,theta / (1 + t*f*theta));
      return ret;
      
    },
    ret_delayed_t, floating_t x, floating_t t, floating_t k, floating_t theta, floating_t f);





 /**
 * Delayed observe a given number of events from the Poisson process (with side effect)
 * See Jan's thesis, page 60
 *
 * - x: number of events
 * - t: how much time the poisson process runs
 * - rate: prior rate will be update
 * - f: factor by which the scale of the gamma is changed
 *
 * Returns: score and updates
 */ 
  BBLOCK_HELPER(observeXEventsDelayedRef, {
     if (f < 1e-5) {
      return 0.0;
    }
    floating_t score = negativeBinomialScore(x, rate.k, 1/(1 + t*f*rate.theta));
    //rate.k  = k;
    rate.theta = rate.theta / (1 + t*f*rate.theta);
    return score;
  },
    floating_t, floating_t x, floating_t t, rate_t& rate,  floating_t f);


