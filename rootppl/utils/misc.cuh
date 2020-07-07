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

using namespace std;

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
 * Prints the array of type T. Works only on the CPU. 
 * 
 * @param arr array of type T.
 * @param n number of elements in arr.
 * @param optional title of the array to be printed. 
 */
template <typename T>
void printArray(T* arr, int n, string title="") {
    if(title.length() > 0)
        cout << title << ": ";
    cout << "[ ";
    for(int i = 0; i < n; i++)
        cout << arr[i] << " ";
    cout << "]" << endl;
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

HOST DEV void calculateFrequencies(int* arr, int n, int maxVal, int* ret) {
    for(int i = 0; i < maxVal; i++)
        ret[i] = 0;

    for(int i = 0; i < n; i++) {
        int val = arr[i];
        if(val < maxVal)
            ret[val]++;
    }
}

template <typename T>
string to_string_with_precision(const T a_value, const int n = 6) {
    ostringstream out;
    out.precision(n);
    out << fixed << a_value;
    return out.str();
}

template <typename T>
void printStars(int* freqs, int n, T minVal=0, T intervalSize=1) {
    for(int i = 0; i < n; i++) {
        if(is_same<T, int>::value) {
            string str;
            if(intervalSize == 1)
                str = to_string(minVal + i);
            else 
                str = to_string(minVal + i * intervalSize);
            cout << setw(10) << str;
        } else {
            string str = "[" + to_string_with_precision(minVal + intervalSize * static_cast<T>(i), 2) + " - " + to_string_with_precision(minVal + (intervalSize) * static_cast<T>(i + 1), 2) + "): ";
            cout << setw(20) << str;
        }
        // cout << setw(10) << "[" << minVal + intervalSize * static_cast<T>(i) << " - " <<  minVal + intervalSize+ 1 * static_cast<T>(i) << "): ";
        // printf("%f: ", minVal + intervalSize * i);
        for(int j = 0; j < freqs[i]; j++)
            printf("*");
        printf("\n");
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


/**
 * Calculates and prints a horizontal histogram of the data provided.
 * Prints to stdout. 
 *
 * @param arr the array containing the numeric data
 * @param n the number of elements in arr
 * @param numBins the number of groups to divide the data into and calculate frequencies for
 * @param minVal the minimum value to consider.
 * @param maxVal the maximum value to consider. 
 */
template <typename T>
void printHistogram(T* arr, int n, int numBins, T minVal, T maxVal) {
    
    // int bins[numBins] = {0};
    int* bins = (int*)malloc(sizeof(int) * numBins);
    for(int i = 0; i < numBins; i++)
        bins[i] = 0;

    T intervalSize = ceil(static_cast<floating_t>((maxVal - minVal + 1)) / numBins);
    cout << "IntervalSize: " << intervalSize << endl;

    for(int i = 0; i < n; i++) {
        int bin = 0;
        floating_t acc = minVal;
        while(acc < arr[i]) {
            bin++;
            acc += intervalSize;
        }
        bins[bin]++;
    }

    for(int b = 0; b < numBins; b++) {
        bins[b] = bins[b] * 100 / n;
    }

    printStars<T>(bins, numBins, minVal, intervalSize);

    free(bins);
}

template <typename T>
void printHistogram(T* arr, int n, int numBins=10) {
    T minVal = minNaiveCPU<T>(arr, n);
    T maxVal = maxNaiveCPU<T>(arr, n);

    printHistogram<T>(arr, n, numBins, minVal, maxVal);
}

#endif