#ifndef MATH_INCLUDED
#define MATH_INCLUDED

/*
 * File math.cuh contains mathematical helper functions. 
 */


#include <math.h>
#include <limits> 

#include "macros/macros.cuh"

const double PI = 3.1415926535897932384626433832795028841971693993751;
const floating_t LOG_PI = 1.1447298858494002;
const floating_t LOG_2PI = 1.8378770664093453;

/**
 * Calculates the log factorial of the integer input. 
 * 
 * @param n the input value. 
 */
HOST DEV double lnFactorial(int n);


/**
 * Calculates the sum of the array of type T. 
 * 
 * @param arr array of type T.
 * @param n number of elements in arr.
 * @return the sum of the elements of the array. 
 */
template <typename T>
HOST DEV T sumArray(T* arr, int n) {
    T sum = 0;
    for (int i = 0; i < n; i++)
        sum += arr[i];
    return sum;
}

/**
 * Calculates the mean value of the array of type T. 
 * 
 * @param arr array of type T.
 * @param n number of elements in arr.
 * @return the mean of the elements of the array. 
 */
 template <typename T>
 HOST DEV floating_t meanArray(T* arr, int n) {
     T sum = 0;
     for (int i = 0; i < n; i++)
         sum += arr[i];
     return static_cast<floating_t>(sum) / n;
 }


/**
 * Normalizes the array of type T. 
 * 
 * @param arr array of type T.
 * @param n number of elements in arr.
 */
 template <typename T>
 HOST DEV void normalizeArray(T* arr, int n) {
     floating_t sum = sumArray<T>(arr, n);
     for (int i = 0; i < n; i++)
         arr[i] /= sum;
 }

 /**
 * Calculates the sign of the argument. 
 * 
 * @param val value of type which can be compared. 
 * @return the sign of val. -1 if it is negative, 0 if it is 0, 1 if it is positive. 
 */
template <typename T>
HOST DEV int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

/**
 * Finds the maximum value, sequential implementation. 
 * 
 * @param arr floating point array.
 * @param n number of elements in arr.
 * @return the maximum value. 
 */
HOST DEV floating_t maxNaive(const floating_t* arr, const int n);

/**
 * Finds the maximum value, sequential implementation. 
 * General template version but for CPU only. 
 * 
 * @param arr floating point array.
 * @param n number of elements in arr.
 * @return the maximum value. 
 */
 template <typename T>
T maxNaiveCPU(const T* arr, const int n) {
     T maxVal = - std::numeric_limits<T>::max();
     for(int i = 0; i < n; i++) {
         maxVal = arr[i] > maxVal ? arr[i] : maxVal;
     }
     return maxVal;
 }

/**
 * Finds the maximum value, sequential implementation. 
 * General template version but for CPU only. 
 * 
 * @param arr floating point array.
 * @param n number of elements in arr.
 * @return the maximum value. 
 */
template <typename T>
T minNaiveCPU(const T* arr, const int n) {
    T minVal = std::numeric_limits<T>::max();
    for(int i = 0; i < n; i++) {
        minVal = arr[i] < minVal ? arr[i] : minVal;
    }
    return minVal;
}

/**
 * Does a Cholesky Decomposition of the matrix. This is used when generating multi-variate gaussian RVs. 
 * 
 * @param matrix the square matrix of size n to be decomposed. 
 * @param lower the square matrix of size n that will be filled with the result. 
 */
template <size_t n>
HOST DEV void choleskyDecomposition(floating_t (&matrix)[n][n], floating_t (&lower)[n][n]) {
    memset(lower, 0, sizeof(floating_t) * n * n); 

    // Decomposing a matrix into Lower Triangular 
    for (int i = 0; i < n; i++) { 
        for (int j = 0; j <= i; j++) { 
            floating_t sum = 0; 

            if (j == i) { // summation for diagonals  
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

} 

/**
 * Performs a matrix multiplication of matrices A and B and stores the results in matrix C.
 * Template arguments a, b and c defines the sizes of the matrices. 
 * 
 * @param A the first input matrix
 * @param B the second input matrix
 * @param C the output matrix
 */
template <size_t a, size_t b, size_t c>
HOST DEV void matmul(floating_t A[a][b], floating_t B[b][c], floating_t C[a][c]) {

    for(int i = 0; i < a; i++)
        for(int j = 0; j < c; j++)
            C[i][j] = 0;

    for(int i = 0; i < a; i++)
        for(int j = 0; j < c; j++)
            for(int k = 0; k < b; k++)
                C[i][j] += A[i][k] * B[k][j];

}

/**
 * Linearly transforms a column. Used when generating multi-variate gaussian RVs. 
 * 
 * @param A the transformation matrix that will be multiplied with the column. 
 * @param col the column to be transformed
 * @param toAdd the column of values to be added to the column. 
 * @param C the output, the transformed column. 
 */
template <size_t a, size_t b>
HOST DEV void transformColumn(floating_t A[a][b], floating_t col[b], floating_t toAdd[a], floating_t C[a]) {
    
    for(int i = 0; i < a; i++) {
        C[i] = toAdd[i];
        for(int k = 0; k < b; k++)
            C[i] += A[i][k] * col[k];
    }
}

 #endif
