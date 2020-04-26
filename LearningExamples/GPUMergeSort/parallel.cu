#include <algorithm>
#include <iostream>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

#include "device_launch_parameters.h"
#include "cuda_runtime.h"
#include "utils.h"
#include "cudaErrorUtils.cu"


__device__ int binarySearch(int* arr, int val, int low, int high) {
	if (high <= low) 
		return (val > arr[low]) ? (low + 1) : low; 
  
    int mid = (low + high)/2;
  
    //if(val == a[mid]) we dont support duplicates anyway
        //return mid+1; 
  
    if(val > arr[mid])
		return binarySearch(arr, val, mid+1, high);
		
    return binarySearch(arr, val, low, mid); // was mid-1
}

__device__ int getIndex(int* subAux, int ownIndex, int nLow, int nTot) {
	int scanIndex;
	int upperBound;
	bool partOfFirstArr = ownIndex < nLow;

	if(partOfFirstArr) {
		scanIndex = nLow; // Start scanning in 2nd arr
		upperBound = nTot;
	} 
	else {
		scanIndex = 0;
		upperBound = nLow;
	}

	//while (scanIndex < upperBound && subAux[scanIndex] < subAux[ownIndex])
		//scanIndex++;

	scanIndex = binarySearch(subAux, subAux[ownIndex], scanIndex, upperBound-1);

	// Bot lower subarr and upper need subtraction of nLow for different reasons
	return ownIndex + scanIndex - nLow;
}

// CANNOT HANDLE DUPLICATES
__global__ void mergeKernel(int* arr, int* aux, int low, int mid, int high) {
	
	int idx = blockIdx.x * blockDim.x + threadIdx.x;

	int nLow = mid - low + 1; // optimize
	int nHigh = high - mid;
	int nTot = nLow + nHigh;
	
	if(idx >= nTot)
		return;

	int arrIndex = getIndex(&aux[low], idx, nLow, nTot);
	arr[low + arrIndex] = aux[low + idx];
	
	//printf("idx %d assigns %d to %d\n", idx, aux[low + idx], low + arrIndex);
}

// Just a sequential merge instead of nested kernel
__device__ void merge(int* arr, int* aux, int low, int mid, int high) {
	int i = 0;
	int j = 0;
	int mergedIndex = low;

	int nLow = mid - low + 1;
	int nHigh = high - mid;

	while (i < nLow && j < nHigh) {
		if (aux[low + i] < aux[mid + 1 + j]) {
			arr[mergedIndex] = aux[low + i];
			i++;
		}
		else {
			arr[mergedIndex] = aux[mid + 1 + j];
			j++;
		}
		mergedIndex++;
	}

	while (i < nLow) {
		arr[mergedIndex] = aux[low + i];
		i++;
		mergedIndex++;
	}
	while (j < nHigh) {
		arr[mergedIndex] = aux[mid + 1 + j];
		j++;
		mergedIndex++;
	}
}

__global__ void mergeSort(int* arr, int* aux, int currentSize, int n, int width) {

	int idx = blockIdx.x * blockDim.x + threadIdx.x;

	int low = idx * width;

	if(low >= n - currentSize || low < 0) // careful for overflow, especially if aligned and duplicates computations
		return;
	int mid = low + currentSize - 1;
	int high = min(low + width - 1, n-1);
	
	int nTot = high - low + 1; // number of threads to spawn

	if(nTot > 16384) { // Don't launch a kernel if the merge is small
	// Since this kernel only is launched with high number of threads a high blockSize
	// should still utilize all SMs but also give less overhead and better cache behaviour
		int numThreadsPerBlock = 1024;
		int numBlocks = (nTot + numThreadsPerBlock - 1) / numThreadsPerBlock;
		
		mergeKernel<<<numBlocks, numThreadsPerBlock>>>(arr, aux, low, mid, high);
		cudaCheckErrorDev();
	} else {
		merge(arr, aux, low, mid, high);
	}
}

void mergeSortGPU(int* arr, int n) {

	int* deviceArr;
	int* auxArr;

		
	cudaSafeCall(cudaDeviceSetCacheConfig(cudaFuncCachePreferL1));
	cudaSafeCall(cudaMallocManaged(&deviceArr, n * sizeof(int)));
	cudaSafeCall(cudaMallocManaged(&auxArr, n * sizeof(int)));
	cudaSafeCall(cudaMemcpy(deviceArr, arr, n * sizeof(int), cudaMemcpyDefault)); // Move arr to cuda managed memory

	for (int currentSize = 1; currentSize < n; currentSize *= 2) {

		int width = currentSize*2;
		int numSorts = (n + width - 1) / width; // number of sorting threads to spawn

		// Low amount here allows for higher parallelism over SM's in some cases since we want at least as many blocks as the number of SMs
		// Could be dynamic for best performance?
		int numThreadsPerBlock = 32; 
		int numBlocks = (numSorts + numThreadsPerBlock - 1) / numThreadsPerBlock;
		
		// Streams might speed things up?
		cudaSafeCall(cudaMemcpy(auxArr, deviceArr, n * sizeof(int), cudaMemcpyDeviceToDevice));
		mergeSort<<<numBlocks, numThreadsPerBlock>>>(deviceArr, auxArr, currentSize, n, width);
		cudaCheckError();
	}

	cudaSafeCall(cudaMemcpy(arr, deviceArr, n * sizeof(int), cudaMemcpyDefault));

	cudaFree(deviceArr);
	cudaFree(auxArr);
}