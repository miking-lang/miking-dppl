/**
 * A tutorial program.
 *
 * - sample λ rate from a prior
 * - observe a waiting time with this rate
 * - get the posterior sample of λ
 */

#include <iostream>
#include <cstring>
#include <string>
#include <iostream>
#include <fstream>

#include "inference/smc/smc.cuh"
#include "utils/math.cuh"

INIT_MODEL(floating_t, 1);

BBLOCK(testObserveWaitingTime, {
    const floating_t k = 1;
    const floating_t theta = 1;
    const floating_t observedTime = 1;

    floating_t lambda = SAMPLE(gamma, k, theta);
    OBSERVE(exponential, lambda, observedTime);
    PSTATE = lambda;

    PC++;
  });

CALLBACK(stats, {
    floating_t wmean = 0.0;
    for(int i = 0; i < N; i++) {
      wmean += PSTATES[i]*exp(WEIGHTS[i]);
    }
    std::cout << "Weighted mean is " << wmean;
})

MAIN({
    ADD_BBLOCK(testObserveWaitingTime);
    SMC(stats);
  })
