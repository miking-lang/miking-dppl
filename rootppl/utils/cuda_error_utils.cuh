#ifndef CUDAERRORUTILS_INCLUDED
#define CUDAERRORUTILS_INCLUDED

/*
 * File cuda_error_utils.cuh contains error checking for CUDA. 
 * If the RELEASE flag is defined it will be disabled as it may degrade performance. 
 * These macros and functions are quite standard for CUDA development and I have not written them myself. 
 */

#include <stdio.h>
#include <curand.h>


// Define this to turn on error checking
#ifndef RELEASE
#define CUDA_ERROR_CHECK
#endif

#define cudaSafeCall( err ) __cudaSafeCall( err, __FILE__, __LINE__ )
#define cudaCheckError()    __cudaCheckError( __FILE__, __LINE__ )
#define cudaCheckErrorDev()    __cudaCheckErrorDev( __FILE__, __LINE__ )
#define CURAND_CALL(status) __cudaRandCall( status, __FILE__, __LINE__ )

inline void __cudaSafeCall( cudaError err, const char *file, const int line )
{
#ifdef CUDA_ERROR_CHECK
    if ( cudaSuccess != err ) {
        fprintf( stderr, "cudaSafeCall() failed at %s:%i : %s\n",
                 file, line, cudaGetErrorString( err ) );
        exit( -1 );
    }
#endif

    //return;
}

inline void __cudaCheckError( const char *file, const int line )
{
#ifdef CUDA_ERROR_CHECK
    cudaError err = cudaGetLastError();
    if ( cudaSuccess != err ) {
        fprintf( stderr, "cudaCheckError() failed at %s:%i : %s\n",
                 file, line, cudaGetErrorString( err ) );
        exit( -1 );
    }

    // More careful checking. However, this will affect performance.
    // Comment away if needed.
    err = cudaDeviceSynchronize();
    if( cudaSuccess != err ) {
        fprintf( stderr, "cudaCheckError() with sync failed at %s:%i : %s\n",
                 file, line, cudaGetErrorString( err ) );
        exit( -1 );
    }
#endif
    //return;
}

__device__ inline void __cudaCheckErrorDev( const char *file, const int line )
{
#ifdef CUDA_ERROR_CHECK
    cudaError err = cudaGetLastError();
    if ( cudaSuccess != err ) {
        printf("%s %s %d\n", cudaGetErrorString(err), file, line);
    }

    // More careful checking. However, this will affect performance.
    // Comment away if needed.
    err = cudaDeviceSynchronize();
    if( cudaSuccess != err ) {
        printf("%s %s %d\n", cudaGetErrorString(err), file, line);
    }
    
#endif

    //return;
}

inline void __cudaRandCall(curandStatus err, const char *file, const int line) {
#ifdef CUDA_ERROR_CHECK
    if(err != CURAND_STATUS_SUCCESS) {
        printf("CURAND Error at %s:%d\n", __FILE__, __LINE__);
        exit( -1 );
    }
#endif
}

#endif
