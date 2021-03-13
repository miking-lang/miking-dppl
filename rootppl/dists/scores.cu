
/*
 * File scores.cu contains the log probability/density functions of distributions. 
 * Most of these implementations are inspired from the implementation of WebPPL.
 *
 * NOTE: some of the multi-variate distribution scores are yet to be implemented!
 * TODO: diagCovNormal, dirichlet, multinomial, multiVariateNormal
 */

#include <math.h>
#include <stdio.h>
#include <cassert>

#include "macros/macros.cuh"
#include "utils/math.cuh"
#include "scores.cuh"

HOST DEV floating_t logGamma(floating_t xx) {
    const floating_t gammaCof[6] = {
        76.18009172947146,
       -86.50532032941677,
       24.01409824083091,
       -1.231739572450155,
       0.1208650973866179e-2,
       -0.5395239384953e-5};

    floating_t x = xx - 1.0;
    floating_t tmp = x + 5.5;
    tmp -= (x + 0.5) * log(tmp);
    floating_t ser = 1.000000000190015;
    for(int j = 0; j <= 5; j++) {
        x+= 1;
        ser += gammaCof[j] / x;
    }
    return -tmp + log(2.5066282746310005 * ser);
}

HOST DEV floating_t lnfactExact(int x) {
    if (x < 0)
      printf("lnfactExact called with negative value!\n");
      
    if (x < 1)
      x = 1;

    floating_t t = 0;
    while (x > 1) {
      t += log(static_cast<floating_t>(x));
      x -= 1;
    }
    return t;
}

HOST DEV int fact(int x) {
    int t = 1;
    while (x > 1) {
      t *= x;
      x -= 1;
    }
    return t;
  }

HOST DEV floating_t lnfact(int x) {
    if (x < 1) {
      x = 1;
    }
    if (x < 12) {
      return log(static_cast<floating_t>(fact(round(static_cast<floating_t>(x)))));
    }
    floating_t invx = 1.0 / x;
    floating_t invx2 = invx * invx;
    floating_t invx3 = invx2 * invx;
    floating_t invx5 = invx3 * invx2;
    floating_t invx7 = invx5 * invx2;
    floating_t sum = ((x + 0.5) * log(static_cast<floating_t>(x))) - x;
    sum += log(2 * PI) / 2;
    sum += (invx / 12) - (invx3 / 360);
    sum += (invx5 / 1260) - (invx7 / 1680);
    return sum;
  }

HOST DEV floating_t bernoulliScore(floating_t p, int x) {
    if(x == 1)
        return log(p);
    else if(x == 0)
        return log(1 - p);
    else
        return -INFINITY;
}

// Helper function
HOST DEV floating_t logBeta(floating_t a, floating_t b) {
    return logGamma(a) + logGamma(b) - logGamma(a + b);
}

HOST DEV floating_t betaScore(floating_t a, floating_t b, floating_t x) {
    if(x > 0 && x < 1)
        return (a - 1) * log(x) + (b - 1) * log(1 - x) - logBeta(a, b);
    else
        return -INFINITY;
}

HOST DEV floating_t binomialScore(floating_t p, int n, int x) {
    if(! (x >= 0 && x <= n))
        return -INFINITY;

    floating_t logNumPermutations = 0;
    int m, o;
    if(x < n - x) {
        m = x;
        o = n - x;
    } else {
        m = n - x;
        o = x;
    }

    for (int i = o + 1; i <= n; i++) 
        logNumPermutations += log(static_cast<floating_t>(i));

    logNumPermutations -= lnfactExact(m);

    return (logNumPermutations + 
        (x == 0 ? 0 : x * log(p)) + 
        (n - x == 0 ? 0 : (n - x) * log(1 - p))
    );
}

template <typename T>
HOST DEV floating_t categoricalScore(const floating_t* ps, const int n, T* arr, T x) {
    for (int i = 0; i < n; i++)
        if(arr[i] == x)
            return log(ps[i]);
    return -INFINITY;
}

HOST DEV floating_t cauchyScore(floating_t loc, floating_t scale, floating_t x) {
    return -LOG_PI - log(scale) - log(1 + pow((x - loc) / scale, 2));
}

// diagCovNormal

// dirichlet

HOST DEV floating_t discreteScore(const floating_t* ps, const int n, int x) {
    if(x < 0 || x >= n)
        return -INFINITY;
    return log(ps[x]);
}

HOST DEV floating_t exponentialScore(floating_t lambda, int x) {
    return x >= 0 ? log(lambda) - lambda * x : -INFINITY;
}

HOST DEV floating_t gammaScore(floating_t k, floating_t theta, floating_t x) {
    return (k - 1) * log(x) - x / theta - logGamma(k) - k * log(theta);
}

HOST DEV floating_t laplaceScore(floating_t loc, floating_t scale, floating_t x) {
    return -1 * (log(2 * scale) + abs(x - loc) / scale);
}

HOST DEV floating_t lomaxScore(floating_t x, floating_t lambda, floating_t alpha) {
    return log(alpha) - log(lambda) - (alpha + 1) * log(1 + x / lambda);
}

// multinomial

// multiVariateNormal

HOST DEV floating_t normalScore(floating_t mean, floating_t std, floating_t x) {
    // return log(exp(-pow(x - mean, 2) / (std * std)) / (std * sqrt(2 * PI)));
    return -0.5 * (LOG_2PI + 2 * log(std) + (x - mean) * (x - mean) / (std * std));
}

HOST DEV floating_t poissonScore(floating_t lambda, int x) {
    return x * log(lambda) - lambda - lnFactorial(x);
}

HOST DEV floating_t randomIntegerScore(const int n, int x) {
    if (x < 0 || x >= n)
        return -INFINITY;
    else
        return -log(static_cast<floating_t>(n));
}

// The range differs on CPU and GPU unfortunately, should that affect this? Is it negligible?
HOST DEV floating_t uniformScore(floating_t min, floating_t max, floating_t x) {
    if(x < min || x > max)
        return -INFINITY;
    else
        return -log(max - min);
}

// V: Score function for the negative binomial distribution
// Implementation inspired by Birch
// see https://github.com/lawmurray/Birch/blob/master/libraries/Standard/src/math/logpdf.birch
// It makes use of log() and log1p() which should be in <math.h>
// and lchoose, which I have implemented in 
// - x: The variate (number of failures).
// - k: Number of successes before the experiment is stopped. must be > 0
// - p: Probability of success. must be in [0, 1]
// Returns: the log probability mass.
HOST DEV floating_t negativeBinomialScore(floating_t x, floating_t k, floating_t p) {
    assert(0 < k);
    assert(0.0 <= p && p <= 1.0);
  
    if (x >= 0) {
      return k*log(p) + x*log1p(-p) + lchoose(x + k - 1, x);
    } else {
      return -INFINITY;
    }
    
}
