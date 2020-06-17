#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <iostream>
#include <vector>
#include <cstring>
#include <bits/stdc++.h> 

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
    cout << "]" << endl;
}

template <typename T>
HOST DEV int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

const int MAX_SIZE_MG = 3;
HOST DEV void choleskyDecomposition(floating_t matrix[MAX_SIZE_MG][MAX_SIZE_MG], int n, floating_t lower[MAX_SIZE_MG][MAX_SIZE_MG]) { 
    // floating_t lower[n][n]; 
    memset(lower, 0, sizeof(floating_t) * n * n); 

    // Decomposing a matrix into Lower Triangular 
    for (int i = 0; i < n; i++) { 
        for (int j = 0; j <= i; j++) { 
            floating_t sum = 0; 
            // printf("[%d][%d]: %f\n", i, j, matrix[i][j]);

            if (j == i) { // summation for diagnols  
                for (int k = 0; k < j; k++) 
                    sum += pow(lower[j][k], 2); 
                lower[j][j] = sqrt(matrix[j][j] - sum); 
            } else { 

                // Evaluating L(i, j) using L(j, j) 
                for (int k = 0; k < j; k++) 
                    sum += (lower[i][k] * lower[j][k]); 
                lower[i][j] = (matrix[i][j] - sum) / lower[j][j]; 
            } 
        }
    } 

    // Displaying Lower Triangular and its Transpose 
    // cout << setw(6) << " Lower Triangular" << setw(30) << "Transpose" << endl; 
    for (int i = 0; i < n; i++) { 

        // Lower Triangular 
        for (int j = 0; j < n; j++) 
            printf("%f ", lower[i][j]);
            // cout << setw(6) << lower[i][j]; 
        printf("\n"); 

        // Transpose of Lower Triangular 
        //for (int j = 0; j < n; j++) 
        //    cout << setw(6) << lower[j][i] << "\t"; 
        //cout << endl; 
    } 
} 


// *** Array needed to handle stuff =)
/*
template <typename T>
struct arr100_t {
    int currIndex = 0;
    T arr[100];

    HOST DEV T operator [] (int i) const {return arr[i];}
    HOST DEV T& operator [] (int i) {return arr[i];}

    HOST DEV void push_back(T obj) {
        arr[currIndex] = obj;
        currIndex++;
    }

    HOST DEV int size() {
        return currIndex;
    }

    HOST DEV T* begin() {
        return arr;
    }

    HOST DEV T* end() {
        return &arr[currIndex];
    }
};
*/
// ***

#endif