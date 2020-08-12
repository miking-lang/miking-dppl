#ifndef MISC_INCLUDED
#define MISC_INCLUDED

/*
 * File misc.cuh contains helper functions of various kinds. 
 */

#include <iostream>
#include <vector>
#include <cstring>
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
 * Calculates and prints a horizontal histogram of the data provided.
 * Prints to stdout. Not very thoroughly tested!
 *
 * @param arr the array containing the numeric data.
 * @param n the number of elements in arr.
 * @param numBins the number of groups to divide the data into and calculate frequencies for.
 * @param minVal the minimum value to consider.
 * @param maxVal the maximum value to consider. 
 */

#endif