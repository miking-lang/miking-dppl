#ifndef SIMULATIONS_INCLUDED
#define SIMULATIONS_INCLUDED


BBLOCK_HELPER(goesExtinct, {

    floating_t t = BBLOCK_CALL(sampleExponential, lambda + mu);
    
    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return false;
    
    bool speciation = BBLOCK_CALL(flipK, lambda / (lambda + mu));
    if (! speciation)
        return true;
    else 
        return BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu) && BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu);

}, bool, floating_t startTime, floating_t lambda, floating_t mu)


BBLOCK_HELPER(simBranch, {

    floating_t t = BBLOCK_CALL(sampleExponential, lambda);

    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime)
        return 0.0;
    
    bool sideExtinction = BBLOCK_CALL(goesExtinct<T>, currentTime, lambda, mu);
    if(! sideExtinction)
        return -INFINITY;
    
    return BBLOCK_CALL(simBranch<T>, currentTime, stopTime, lambda, mu) + log(2.0);

}, floating_t, floating_t startTime, floating_t stopTime, floating_t lambda, floating_t mu)

#endif


