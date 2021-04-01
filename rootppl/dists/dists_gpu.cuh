#ifndef DISTS_GPU
#define DISTS_GPU

/*
 * File dists_gpu.cuh contains primitive distributions specific for the GPU. 
 */


/**
 * Returns a sample from the Uniform distribution on the interval (min, max]
 * 
 * @param randState the curandState pointer required by cuRAND to generate random numbers. 
 * @param min the exclusive minimum value
 * @param max the inclusive maximum value
 * @return U(min, max]
 */
__device__ floating_t uniform(curandState* randState, floating_t min, floating_t max) {
    return (curand_uniform(randState) * (max - min)) + min;
}

/**
 * Returns a sample from the Normal/Gaussian distribution. 
 * 
 * @param randState the curandState pointer required by cuRAND to generate random numbers. 
 * @param mean the mean/location of the gaussian distribution
 * @param std the standard deviation/scale of the gaussian destribution
 * @return N(mean, std^2)
 */
__device__ floating_t normal(curandState* randState, floating_t mean, floating_t std) {
    return (curand_normal(randState) * std) + mean;
}

/**
 * Returns a sample from the Poisson distribution. 
 * 
 * @param randState the curandState pointer required by cuRAND to generate random numbers. 
 * @param lambda the rate parameter
 * @return Po(lambda)
 */
__device__ unsigned int poisson(curandState* randState, double lambda) {
    return curand_poisson(randState, lambda);
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





#endif
