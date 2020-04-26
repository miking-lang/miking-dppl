#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <iostream>
#include <vector>
#include <cstring>

// #include "list.cuh"

using namespace std;


template <typename T>
void allocateMemory(T** pointer, size_t n) {
    #ifdef GPU
    cudaSafeCall(cudaMallocManaged(pointer, sizeof(T) * n));
    #else
    *pointer = new T[n];
    #endif
}

template <typename T>
void freeMemory(T* pointer) {
    #ifdef GPU
    cudaSafeCall(cudaFree(pointer));
    #else
    delete[] pointer;
    #endif
}

template <typename T>
HOST DEV T sumArray(T* arr, int n) {
        T sum = 0;
        for (int i = 0; i < n; i++)
            sum += arr[i];
        return sum;
    }

/*
HOST DEV int sumArrayInt(int* arr, int n) {
    int sum = 0;
    for (int i = 0; i < n; i++)
        sum += arr[i];
    return sum;
}

HOST DEV int sumArrayFloat(floating_t* arr, int n) {
    floating_t sum = 0;
    for (int i = 0; i < n; i++)
        sum += arr[i];
    return sum;
}
*/

template <typename T>
HOST DEV void normalizeArray(T* arr, int n) {
    floating_t sum = 0;
    for (int i = 0; i < n; i++)
        sum += arr[i];
    for (int i = 0; i < n; i++)
        arr[i] /= sum;
}

template <typename T>
void printArray(T* arr, int n, string title="") {
    if(title.length() > 0)
        cout << title << ": ";
    cout << "[ ";
    for(int i = 0; i < n; i++)
        cout << arr[i] << " ";
    cout << "]\n" << endl;
}

#endif