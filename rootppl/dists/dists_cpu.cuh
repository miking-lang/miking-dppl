#ifndef DISTS_CPU
#define DISTS_CPU

/*
 * File dists_cpu.cuh contains primitive distributions specific for the CPU. 
 */


/**
 * Returns a sample from the Uniform distribution on the interval [min, max)
 * 
 * @param min the inclusive minimum value
 * @param max the exclusive maximum value
 * @return U[min, max)
 */
floating_t uniform(floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

/**
 * Returns a sample from the Normal/Gaussian distribution. 
 * 
 * @param mean the mean/location of the gaussian distribution
 * @param std the standard deviation/scale of the gaussian destribution
 * @return N(mean, std^2)
 */
floating_t normal(floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}

/**
 * Returns a sample from the Poisson distribution. 
 * 
 * @param lambda the rate parameter
 * @return Po(lambda)
 */
unsigned int poisson(double lambda) {
    // Reuse distribution object when possible? However, does not seem to be that expensive to create dist object
    std::poisson_distribution<unsigned int> poissonDist(lambda);
    return poissonDist(generatorDists);
}

#endif