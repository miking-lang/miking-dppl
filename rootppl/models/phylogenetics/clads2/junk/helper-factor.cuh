
BBLOCK_HELPER(clads2GoesUndetectedFactor, {
    floating_t lambda = lambda0*factor;
    floating_t mu = epsilon*lambda;

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
		return false;

    if (lambda < MIN_LAM)
        return ! SAMPLE(bernoulli, rho);
    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambda + mu);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho); 


    bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    if(exctinction)
        return true;

    //floating_t lambda1 = exp(SAMPLE(normal, log(alpha * lambda), sigma));
    //floating_t lambda2 = exp(SAMPLE(normal, log(alpha * lambda), sigma));
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    return BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho)
        && BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);
 
}, bool, floating_t startTime, floating_t lambda0, floating_t factor, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 

/*
 * Simulates Hidden Speciation Events Along Branch
 */ 
BBLOCK_HELPER(simBranchFactor, {
    floating_t lambda = lambda0*factor;

    floating_t t1 = startTime - stopTime;
    floating_t mu = epsilon * lambda;

    // extreme values patch 2/2
    if(lambda > MAX_LAM) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }
    if(lambda < MIN_LAM) {
        simBranchRet_t ret(factor, lambda*t1, -mu*t1);
        return ret;
    }
    // extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        simBranchRet_t ret(factor, lambda*t1, -mu*t1);
        return ret;
    }

    //floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);
    //floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetectedFactor, currentTime, lambda0, factor*exp(f1), alpha, sigma, epsilon, rho);

    if(! sideUndetected) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }

    simBranchRet_t ret = BBLOCK_CALL(simBranchFactor, currentTime, stopTime, lambda0, factor*exp(f2), alpha, sigma, epsilon, rho);
    simBranchRet_t rt(ret.r0, ret.r1 + lambda*t, ret.r2 + log(2.0) - mu*t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda0, floating_t factor, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
