#ifndef MISC_INCLUDED
#define MISC_INCLUDED

/*
 * File misc.cuh contains helper functions of various kinds. 
 */

#include <iostream>
#include <vector>
#include <cstring>
#include <bits/stdc++.h> 
#include <math.h>
#include <limits> 
#include <sstream>

#include "math.cuh"

// using namespace std;

/**
 * Allocates memory on host or device depending on compiler. 
 * 
 * @param pointer address of the pointer which should point to the allocated memory. 
 * @param n the number of elements of type T to be allocated. 
 */
template <typename T>
void allocateMemory(T** pointer, size_t n) {
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(pointer, sizeof(T) * n));
    #else
    *pointer = new T[n];
    #endif
}

/**
 * Frees memory on host or device depending on compiler. 
 * 
 * @param pointer address of the allocated memory. 
 */
template <typename T>
void freeMemory(T* pointer) {
    #ifdef GPU
    cudaSafeCall(cudaFree(pointer));
    #else
    delete[] pointer;
    #endif
}

/**
 * This is an attempt to make most of the GPU memory available 
 * from kernels via implicit stacks and device malloc calls
 * When running programs that reaches the memory limit, this could 
 * be tweaked to prioritize the memory type required by the program
 */
 void configureMemSizeGPU() {
    #ifdef GPU

    // Read memory properties and define limits
    cudaDeviceProp devProp;
    cudaGetDeviceProperties(&devProp, 0);
    size_t MAX_THREADS_RESIDENT = devProp.maxThreadsPerMultiProcessor * devProp.multiProcessorCount;
    size_t GPU_MEM_TOT = devProp.totalGlobalMem * 0.95; // Leave 5% of memory for global structures or just to be sure
    size_t GPU_MEM_HEAP = GPU_MEM_TOT * 0.20; // Arbitrarily set 20% of GPU memory to device allocated heap memory
    size_t GPU_MEM_STACK = GPU_MEM_TOT - GPU_MEM_HEAP;
    size_t MAX_LOCAL_MEM_PER_THREAD = 512000; // 512 KB on all compute capabilities according to CUDA docs
    size_t MAX_STACK_SIZE = min(MAX_LOCAL_MEM_PER_THREAD, GPU_MEM_STACK / MAX_THREADS_RESIDENT);
    MAX_STACK_SIZE *= 1.0; // For some reason, with nested inference, this limit must be lower. Also, lower can give better performance.
    
    // Set limits and read the resulting set limits
    size_t heapSize, stackSize;
    cudaDeviceSetLimit(cudaLimitMallocHeapSize, GPU_MEM_HEAP);
    cudaDeviceSetLimit(cudaLimitStackSize, MAX_STACK_SIZE);
    cudaDeviceGetLimit(&heapSize, cudaLimitMallocHeapSize);
    cudaDeviceGetLimit(&stackSize, cudaLimitStackSize);

    if(true) {
        std::cout << "Global Memory size: " << GPU_MEM_TOT / 1000000.0 << " MB" << std::endl;
        std::cout << "Stack per thread max size attempted to set: " << MAX_STACK_SIZE / 1000.0 << " KB" << std::endl;
        std::cout << "Stack per thread max size set: " << stackSize / 1000.0 << " KB" << std::endl;
        std::cout << "Device allocation heap max size: " << heapSize / 1000000.0 << " MB" << std::endl;
    }
    // cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
    #endif
}

/**
 * Prints the array of type T. Works only on the CPU. 
 * 
 * @param arr array of type T.
 * @param n number of elements in arr.
 * @param optional title of the array to be printed. 
 */
template <typename T>
void printArray(T* arr, int n, std::string title="") {
    if(title.length() > 0)
        std::cout << title << ": ";
    std::cout << "[ ";
    for(int i = 0; i < n; i++)
        std::cout << arr[i] << " ";
    std::cout << "]" << std::endl;
}

/**
 * Prints the floating point array.
 * 
 * @param arr floating point array.
 * @param n number of elements in arr.
 */
DEV void printArrayF(floating_t* arr, int n) {
    printf("[ ");
    for(int i = 0; i < n; i++)
        printf("%f ", arr[i]);
    printf("]\n");
}

/**
 * Prints the integer array.
 * 
 * @param arr integer array.
 * @param n number of elements in arr.
 */
DEV void printArrayI(int* arr, int n) {
    printf("[ ");
    for(int i = 0; i < n; i++)
        printf("%d ", arr[i]);
    printf("]\n");
}

/**
 * Prints the value with a given precision.  
 *
 * @param val the value to stringify
 * @param n the precision, corresponds to number of decimals for floating points. 
 */
template <typename T>
std::string to_string_with_precision(const T val, const int n=6) {
    std::ostringstream out;
    out.precision(n);
    out << std::fixed << val;
    return out.str();
}


/**
 * Prints rows of stars according to the numbers in the given array. 
 *
 * @param freqs the array containing the star frequencies. 
 * @param n the number of elements in freqs.
 * @param minVal the starting value for row titles.
 * @param intervalSize the stepsize for row title increments. 
 */
template <typename T>
void printStars(int* freqs, int n, T minVal=0, T intervalSize=1) {
    for(int i = 0; i < n; i++) {
        if(std::is_same<T, int>::value) {
            std::string str;
            if(intervalSize == 1)
                str = std::to_string(minVal + i) + ": ";
            else 
                str = "[" + std::to_string(minVal + i * intervalSize) + " - " + std::to_string(minVal + (i + 1) * (intervalSize) - 1) + "]: ";
            std::cout << std::setw(15) << str;
        } else {
            std::string str = "[" + to_string_with_precision(minVal + intervalSize * static_cast<T>(i), 2) + " - " + to_string_with_precision(minVal + (intervalSize) * static_cast<T>(i + 1), 2) + "): ";
            std::cout << std::setw(20) << str;
        }
        for(int j = 0; j < freqs[i]; j++)
            printf("*");
        printf("\n");
    }
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


/**
 * Calculates and prints a horizontal histogram of the data provided.
 * Prints to stdout. Not very thoroughly tested!
 *
 * @param arr the array containing the numeric data.
 * @param n the number of elements in arr.
 * @param numBins the number of groups to divide the data into and calculate frequencies for.
 * @param minVal the minimum value to consider.
 * @param maxVal the maximum value to consider. 
 */
template <typename T>
void printHistogram(T* arr, int n, int numBins, T minVal, T maxVal) {
    
    int* bins = (int*)malloc(sizeof(int) * numBins);
    for(int i = 0; i < numBins; i++)
        bins[i] = 0;

    T intervalSize;
    if(std::is_same<T, int>::value)
        intervalSize = ceil(static_cast<floating_t>((maxVal - minVal + 1)) / numBins);
    else
        intervalSize = static_cast<floating_t>((maxVal - minVal)) / numBins;

    for(int i = 0; i < n; i++) {
        int bin = (arr[i] - minVal) / intervalSize;
        // bin = bin >= numBins ? numBins - 1 : bin;
        // bin = bin < 0 ? 0 : bin;
        if(bin < 0 || bin >= numBins)
            continue;

        bins[bin]++;
    }

    for(int b = 0; b < numBins; b++) {
        bins[b] = bins[b] * numBins * 10 / n;
    }

    printStars<T>(bins, numBins, minVal, intervalSize);

    free(bins);
}

/**
 * Calculates and prints a horizontal histogram of the data provided.
 * Prints to stdout. Calculates the min and max values in the data and 
 * that data range into intervals.
 *
 * @param arr the array containing the numeric data.
 * @param n the number of elements in arr.
 * @param numBins the number of groups to divide the data into and calculate frequencies for.
 */
template <typename T>
void printHistogram(T* arr, int n, int numBins=10) {
    T minVal = minNaiveCPU<T>(arr, n);
    T maxVal = maxNaiveCPU<T>(arr, n);

    printHistogram<T>(arr, n, numBins, minVal, maxVal);
}

#endif