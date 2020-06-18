#ifndef DISTS_CPU
#define DISTS_CPU

// [min, max)
floating_t uniform(floating_t min, floating_t max) {
    return (uniformDist(generatorDists) * (max - min)) + min;
}

floating_t normal(floating_t mean, floating_t std) {
    return (normalDist(generatorDists) * std) + mean;
}

floating_t exponential(floating_t lambda) {
    
    return -log(1 - uniformDist(generatorDists)) / lambda;
}

// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
/*
floating_t gamma(floating_t k, floating_t theta) {
    
    if (k < 1) {
        double u = uniformDist(generatorDists);
        return gamma(1.0 + k, theta) * pow(u, 1.0 / k);
    }
    
    {
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = normalDist(generatorDists);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = uniformDist(generatorDists);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        return theta * d * v;
    }
    
}
*/

//int sampleBernoulli(particles_t<T>* particles, int i, double p = 0.5) {
//template <typename T>
int bernoulli(floating_t p = 0.5) {
    return uniformDist(generatorDists) < p ? 1 : 0;
}

// Reuse distribution object when possible? However, does not seem to be that expensive to create dist object
unsigned int poisson(double lambda) {
    std::poisson_distribution<unsigned int> poissonDist(lambda);
    return poissonDist(generatorDists);
}

#endif