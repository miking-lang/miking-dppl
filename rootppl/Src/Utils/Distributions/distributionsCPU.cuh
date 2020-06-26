#ifndef DISTS_CPU
#define DISTS_CPU

// [min, max)
floating_t uniform(floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

floating_t normal(floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}

// Reuse distribution object when possible? However, does not seem to be that expensive to create dist object
unsigned int poisson(double lambda) {
    std::poisson_distribution<unsigned int> poissonDist(lambda);
    return poissonDist(generatorDists);
}

#endif