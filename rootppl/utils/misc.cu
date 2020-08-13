/*
 * File misc.cuh contains helper functions of various kinds. 
 */

#include <iostream>
#include <vector>
#include <cstring>
#include <math.h>
#include <limits> 
#include <sstream>

#include "misc.cuh"


void allocateMemoryVoid(void** pointer, size_t allocSize) {
     #ifdef __NVCC__
     cudaSafeCall(cudaMallocManaged(pointer, allocSize));
     #else
     *pointer = malloc(allocSize);
     #endif
}

void freeMemoryVoid(void* pointer) {
    #ifdef __NVCC__
    cudaSafeCall(cudaFree(pointer));
    #else
    free(pointer);
    #endif
}

DEV void printArrayF(floating_t* arr, int n) {
    printf("[ ");
    for(int i = 0; i < n; i++)
        printf("%f ", arr[i]);
    printf("]\n");
}

DEV void printArrayI(int* arr, int n) {
    printf("[ ");
    for(int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("]\n");
}
