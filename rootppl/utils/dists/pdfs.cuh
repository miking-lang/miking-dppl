#ifndef PDFS_INCLUDED
#define PDFS_INCLUDED

/*
 * File pdfs.cuh contains probability density functions of distributions. 
 */

// Log of normal pdf
HOST DEV floating_t logPDFNormal(floating_t x, floating_t mean, floating_t std) {
    return log(exp(-pow(x - mean, 2) / (std * std)) / (std * sqrt(2 * PI)));
}

#endif
