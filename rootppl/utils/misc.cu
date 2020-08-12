/*
 * File misc.cuh contains helper functions of various kinds. 
 */

#include <iostream>
#include <vector>
#include <cstring>
// #include <bits/stdc++.h> 
#include <math.h>
#include <limits> 
#include <sstream>

#include "misc.cuh"

// using namespace std;

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


/*
HOST DEV void calculateFrequencies(int* arr, int n, int maxVal, int* ret) {
    for(int i = 0; i < maxVal; i++)
        ret[i] = 0;

    for(int i = 0; i < n; i++) {
        int val = arr[i];
        if(val < maxVal)
            ret[val]++;
    }
}

void printNormalizedFrequencies(int* arr, int n, int maxVal, int* ret) {
    calculateFrequencies(arr, n, maxVal, ret);
    floating_t arrF[maxVal];
    for(int i = 0; i < maxVal; i++)
        arrF[i] = ret[i];
    normalizeArray<floating_t>(arrF, maxVal);
    for(int i = 0; i < maxVal; i++)
        ret[i] = (int)(arrF[i] * 100);
    printArray<int>(ret, 10);
    printStars<int>(ret, 10);
}
*/
