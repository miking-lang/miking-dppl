#ifndef UTILSGPU_INCLUDED
#define UTILSGPU_INCLUDED

__device__ floating_t mapLookupApproxDev(floating_t x, floating_t* mapApproxDev);
__device__ floating_t normalPDFObsDev(floating_t x, floating_t mean);
__device__ floating_t logNormalPDFObsDev(floating_t x, floating_t mean);

#endif