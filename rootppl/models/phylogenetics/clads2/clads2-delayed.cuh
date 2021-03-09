/**
 * Needed stuff to do delayed sampling.
 * TODO Move this to the framework.
 */

#include "clads2.cuh"
#include "delayed.cuh"

/**
 * The delayed program state now has the latest k
 * and theta, as well as a stack of factors;
 */
struct progStateDelayed_t {
  pStack_t stack;
  rate_t lambda_0;
  floating_t lambda0;
  floating_t alpha;
  floating_t sigma;
  floating_t epsilon;
  floating_t rho;
  treeIdx_t treeIdx;
};








/**
 * This function simulates the side-branches and returns 
 * true if the side branch does not make it to our sapmle.
 *
 * - start_time: when did the lineage speciate
 * - lambda0  (as reference to be updated)
 * - factor: the accummulated lambda factor at start_time
 * - alpha
 * - sigma
 * - epsilon
 * - rho
 * 
 * Returns: TRUE/FALSE
 * also it has side effect that the proposal for lambda0 is updated
 */
BBLOCK_HELPER(clads2GoesUndetectedDelayed, {
    
    // extreme values patch
    if (factor > 1e5) {
      return false; // detected for sure with insane div. rate
    }
    
    if (factor < 1e-5) {
      // lambda is very small, so nothing will happen to the lineage in terms of speciation
      // it will hit present and then we see
      bool undetected = !SAMPLE(bernoulli, rho);  
      return undetected;
    }
    // end extreme values patch 1
    
    // t is the waiting time until the next event (speciation or extinction)
    // TODO is the next line correct?
    floating_t t = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda0, factor*(1 + epsilon));
    
    floating_t currentTime = startTime - t;
    
    if(currentTime < 0) { // we are in the future, rho is the detection probability
        bool undetected = !SAMPLE(bernoulli, rho);  
        return undetected;
    }
            
    //  bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    bool speciation = SAMPLE(bernoulli, 1.0/(1.0 + epsilon));
    bool extinction = !speciation;
    
    if(extinction) {
      return true;
    }
    
    // Realizes the new factor by which the current lambda (= lambda_0 x old factors)
    // is going to be multiplied. One for left and right.
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    //rate_t leftRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f1));

    bool ret1 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho);
    
    bool leftDetection = !ret1;
    if (leftDetection) return ret1; // no need to descend to the right side of the tree
    
    //rate_t rightRate(ret1.rate_upd.k, ret1.rate_upd.theta, lambdaRate.factor*exp(f2));

    bool ret2 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);
    return ret2;
    
  }, bool, floating_t startTime, rate_t& lambda0, floating_t factor, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)




// Simulates the hidden speciation events along a branch
// Returns
//   - the accumulated probability along the branch
//   - the accumulated factors along the branch
// has side-effect that the proposal for lambda0 is updated
BBLOCK_HELPER(simBranchDelayed, {

    floating_t t1 = startTime - stopTime;
    
    // extreme values patch 2/2
    if (factor > 1e5 ) {
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
    }

    if (factor < 1e-5) {
      floating_t ret0 = BBLOCK_CALL(observeXEventsDelayedRef, 0, t1, lambda0, factor*(epsilon));
      simBranchRet_t ret(factor, 0.0, ret0);
    }
    // end extreme values patch 2/2

    floating_t t = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda0, factor);
    floating_t currentTime = startTime - t;
    

    if(currentTime <= stopTime) {
      floating_t ret1 = BBLOCK_CALL(observeXEventsDelayedRef, 0, t1, lambda0, factor*(epsilon));
      
      //floating_t ret2 = BBLOCK_CALL(observeWaitingTimeDelayedRef, 0, lambda0, factor);
      simBranchRet_t ret(factor, 0.0, ret1);
      return ret;
    }
    
    // sample factors for left and right subtrees
    floating_t f1 = SAMPLE(normal, log(alpha), sigma); // left factor
    floating_t f2 = SAMPLE(normal, log(alpha), sigma); // right factor
    
    // we need to check if the side was undetected
    // w.l.o.g. we choose the right side to die
    //rate_t rightRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f2));
    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);

    if(! sideUndetected) {
      // this particle needs to die
      
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }
    
    
    // Now we will enter into the recursion to process the rest of the branch
    // and accummulate the factor
    //    rate_t leftRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f1));
    
    simBranchRet_t ret7 = BBLOCK_CALL(simBranchDelayed, currentTime, stopTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho);

    floating_t extinctionProb = BBLOCK_CALL(observeXEventsDelayedRef, 0, t, lambda0, factor*(epsilon)); // branch didn't go extinct

    
    // Now gather all weights and add 2 for the end of the branch
    // 1 and 2 are probs, 3 is a bool, 4 is a prob again
    //simBranchRetDelayed_t rt(ret7.prob + extinctionProb + log(2.0), ret7.factor);
  simBranchRet_t rt(ret7.r0, 0.0, ret7.r2 + log(2.0) + extinctionProb);

  return rt;
    
  }, simBranchRet_t, floating_t startTime, floating_t stopTime, rate_t& lambda0, floating_t factor, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho);

