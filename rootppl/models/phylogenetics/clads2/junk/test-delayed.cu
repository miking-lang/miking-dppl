/**
 * Tests waitingTimeDelayed
 */

#include <iostream>
#include <cstring>
#include <string>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "utils/math.cuh"
#include "delayed.cuh"


const floating_t k = 1000;
const floating_t theta = 0.0001;
const floating_t factor = 2;
const floating_t observedTime = 10;
const floating_t nEvents = 5;
const floating_t elapsedTime = 10;


INIT_MODEL(floating_t, 1);



BBLOCK(testWaitingTime, {
  /* We will sample two waiting times (so that we have an update on the rate),
     and then check the distribution of the second waiting time against WebPPL.*/
  floating_t lambda = SAMPLE(gamma, k, theta);
  //printf("%f", lambda*factor);
  floating_t t0 = SAMPLE(exponential, lambda*factor);
  floating_t t1 = SAMPLE(exponential, lambda*factor);
       
  PSTATE = t0 + t1;
  PC++;
});




BBLOCK(testWaitingTimeDelayed, {
    /* We will sample two waiting times (so that we have an update on the rate),
       and then check the distribution of the second waiting time against WebPPL.*/
    rate_t lambda(k, theta);

    ret_delayed_t ret0 = BBLOCK_CALL(sampleWaitingTimeDelayed, lambda.k, lambda.theta, factor);
    ret_delayed_t ret1 = BBLOCK_CALL(sampleWaitingTimeDelayed, ret0.k, ret0.theta, factor);
        
    PSTATE = ret0.res + ret1.res;
    
    PC++;
  });


BBLOCK(testWaitingTimeDelayedRef, {
    /* We will sample two waiting times (so that we have an update on the rate),
       and then check the distribution of the second waiting time against WebPPL.*/
    rate_t lambda(k, theta);
    //ret_delayed_t ret0 = BBLOCK_CALL(waitingTimeDelayed, lambda.k, lambda.theta, lambda.factor);
    //ret_delayed_t ret1 = BBLOCK_CALL(waitingTimeDelayed, ret0.k, ret0.theta, lambda.factor);
   
    floating_t t0 = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda, factor);
    floating_t t1 = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda, factor);
    
    PSTATE = t0 + t1;
    
    PC++;
  });





  BBLOCK(testObserveWaitingTime, {
    floating_t lambda = SAMPLE(gamma, k, theta);
    OBSERVE(exponential, lambda*factor, observedTime);

    floating_t t0 = SAMPLE(exponential, lambda*factor);
    PSTATE = t0;
    PC++;
  });





BBLOCK(testObserveWaitingTimeDelayed, {
    rate_t lambda(k, theta);
    ret_delayed_t ret0 = BBLOCK_CALL(observeWaitingTimeDelayed, observedTime, lambda.k, lambda.theta, factor);
    WEIGHT(ret0.res);

    ret_delayed_t ret1 = BBLOCK_CALL(sampleWaitingTimeDelayed, ret0.k, ret0.theta, factor);
    PSTATE = ret1.res;
    PC++;
  });





BBLOCK(testObserveWaitingTimeDelayedRef, {
    rate_t lambda(k, theta);
    floating_t ret0 = BBLOCK_CALL(observeWaitingTimeDelayedRef, observedTime, lambda, factor);
    WEIGHT(ret0);

    floating_t ret1 = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda, factor);
    PSTATE = ret1;
    PC++;
  });





BBLOCK(testObserveXEvents, {
    floating_t lambda = SAMPLE(gamma, k, theta);
    
    OBSERVE(poisson, lambda*factor*elapsedTime, nEvents);

    floating_t t0 = SAMPLE(exponential, lambda*factor);
    
    PSTATE = t0;
    PC++;
  });


BBLOCK(testObserveXEventsDelayed, {
  rate_t lambda(k, theta);

  ret_delayed_t ret0 = BBLOCK_CALL(observeXEventsDelayed, nEvents, elapsedTime, lambda.k, lambda.theta, factor);

  WEIGHT(ret0.res);

  ret_delayed_t ret1 = BBLOCK_CALL(sampleWaitingTimeDelayed, lambda.k, lambda.theta, factor);

  PSTATE = ret1.res;
  PC++;
  });


BBLOCK(testObserveXEventsDelayedRef, {
    rate_t lambda(k, theta);
    
    floating_t ret0 = BBLOCK_CALL(observeXEventsDelayedRef, nEvents, elapsedTime, lambda, factor);
    
    WEIGHT(ret0);
    
    floating_t ret1 = BBLOCK_CALL(sampleWaitingTimeDelayedRef, lambda, factor);
    
    PSTATE = ret1;
    PC++;
  });




CALLBACK(stats, {
    double sum = 0;
    
    for(int i = 0; i < N; i++) {
        sum += PSTATES[i];
    }
    double mean = sum / N;

    double sumSqDist = 0;
    for(int i = 0; i < N; i++) {
      sumSqDist += (PSTATES[i] - mean)*(PSTATES[i] - mean);    
    }
    double var = sumSqDist / N;
    
    printf("Sample mean: %f\n", mean);
    printf("Sample var: %f\n", var);
    printf("Sample stddev: %f\n", sqrt(var));
})

MAIN({
    //ADD_BBLOCK(testWaitingTime);
    //ADD_BBLOCK(testWaitingTimeDelayed);
    //ADD_BBLOCK(testWaitingTimeDelayedRef);
    
    //ADD_BBLOCK(testObserveWaitingTime);
    //ADD_BBLOCK(testObserveWaitingTimeDelayed);
    //ADD_BBLOCK(testObserveWaitingTimeDelayedRef);

    //ADD_BBLOCK(testObserveXEvents);
    //ADD_BBLOCK(testObserveXEventsDelayed);
    ADD_BBLOCK(testObserveXEventsDelayedRef);
    SMC(stats);
  })
