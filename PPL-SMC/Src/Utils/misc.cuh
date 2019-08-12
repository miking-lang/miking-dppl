#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <list>

void initGen();
int flip(double p = 0.5);

#ifdef GPU
__device__ int flipDev(curandState* randState, double p = 0.5);
#endif

void printList(list<bool> l);

#endif