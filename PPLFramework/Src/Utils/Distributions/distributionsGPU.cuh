#ifndef DISTS_GPU
#define DISTS_GPU

// template <typename T>
// __device__ int sampleBernoulli(particles_t<T>* particles, int i, floating_t p = 0.5) {
__device__ int bernoulli(curandState* randState, floating_t p = 0.5) {
    return curand_uniform(randState) < p ? 1 : 0;
    //return curand_uniform(&particles->randStates[i]) < p ? 1 : 0;
}

// (min, max]
__device__ floating_t uniform(curandState* randState, floating_t min, floating_t max) {
    return (curand_uniform(randState) * (max - min)) + min;
}

__device__ floating_t normal(curandState* randState, floating_t mean, floating_t std) {
    return (curand_normal(randState) * std) + mean;
}

__device__ floating_t exponential(curandState* randState, floating_t lambda) {
    
    // particles->randStates[i];
    //curandState localState = particles->randStates[i];
    //curand_uniform(&localState);
    //floating_t u = curand_uniform(&localState);
    // particles->randStates[i] = localState;
    return -log(1 - curand_uniform(randState)) / lambda;
}

// This gamma sampling is based on the implementation used by GSL
// k = shape, theta = scale
/*
__device__ floating_t gamma(curandState* randState, floating_t k, floating_t theta) {

    
    if (k < 1) {
        double u = curand_uniform(randState);
        return gamma(randState, 1.0 + k, theta) * pow (u, 1.0 / k);
    }
    
    {
        curandState localState = *randState;
        double x, v, u;
        double d = k - 1.0 / 3.0;
        double c = (1.0 / 3.0) / sqrt(d);
    
        while (true) {
            do {
                x = curand_normal(&localState);
                v = 1.0 + c * x;
            } while (v <= 0);
    
            v = v * v * v;
            u = curand_uniform(&localState);
    
            if (u < 1 - 0.0331 * x * x * x * x) 
                break;
    
            if (log(u) < 0.5 * x * x + d * (1 - v + log(v)))
                break;
        }
    
        *randState = localState;
        return theta * d * v;
    }
    

    // return exponential(particles, i, 1.0); // exponential(1) is special case of gamma: gamma(1, 1)
}
*/

__device__ unsigned int poisson(curandState* randState, double lambda) {
    return curand_poisson(randState, lambda);
}

#endif