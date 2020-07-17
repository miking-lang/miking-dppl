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

#include "macros/macros.cuh"
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
    #ifdef __NVCC__
    cudaSafeCall(cudaMallocManaged(pointer, sizeof(T) * n));
    #else
    *pointer = new T[n];
    #endif
}

/**
 * Allocates memory on host or device depending on compiler. 
 * 
 * @param pointer address of the pointer which should point to the allocated memory. 
 * @param allocSize the size of memory to allocate. 
 */
 void allocateMemoryVoid(void** pointer, size_t allocSize);

 /**
 * Frees memory on host or device depending on compiler. 
 * 
 * @param pointer address of the allocated memory. 
 */
template <typename T>
void freeMemory(T* pointer) {
    #ifdef __NVCC__
    cudaSafeCall(cudaFree(pointer));
    #else
    delete[] pointer;
    #endif
}

/**
 * Frees memory on host or device depending on compiler. 
 * 
 * @param pointer address of the allocated memory. 
 */
void freeMemoryVoid(void* pointer);

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
DEV void printArrayF(floating_t* arr, int n);

/**
 * Prints the integer array.
 * 
 * @param arr integer array.
 * @param n number of elements in arr.
 */
DEV void printArrayI(int* arr, int n);

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