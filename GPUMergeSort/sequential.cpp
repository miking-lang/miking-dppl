#include <iostream>
#include <algorithm>
#include "utils.h"

using namespace std;

// Bottom Up
void mergeSeq(int* arr, int* aux, int low, int mid, int high) {
	int i = 0;
	int j = 0;
	int mergedIndex = low;

	int nLow = mid - low + 1;
	int nHigh = high - mid;

	while (i < nLow && j < nHigh) {
		if (aux[low + i] <= aux[mid + 1 + j]) {
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

void sortSeq(int* arr, int n) {

	int* aux = new int[n];
	// bool swapped = false;

	for (int currentSize = 1; currentSize < n; currentSize *= 2) {
		// swapPointers(&arr, &aux);
		// swapped = !swapped;
		for (int i = 0; i < n; i++)
			aux[i] = arr[i];

		for (int low = 0; low < n - currentSize; low += 2*currentSize) {
			int mid = low + currentSize - 1;
			int high = min(low + 2 * currentSize - 1, n-1);

			mergeSeq(arr, aux, low, mid, high);
		}

	}
	/*
	if(swapped) {
		swapPointers(&arr, &aux);
		for (int i = 0; i < n; i++)
			arr[i] = aux[i];
	}
	*/
	delete[] aux;
}

void mergeSortSeq(int* arr, int length) {
	
	sortSeq(arr, length);
}


/*
// Recursive
//void mergeSeq(int* arr, int low, int mid, int high) {
void mergeSeq(int* arrLow, int* arrHigh, int nLow, int nHigh) {
	int i = 0;
	int j = 0;

	int* auxArrLow = new int[nLow];
	int* auxArrHigh = new int[nHigh];

	for (i = 0; i < nLow; i++)
		auxArrLow[i] = arrLow[i];

	for (j = 0; j < nHigh; j++)
		auxArrHigh[j] = arrHigh[j];

	i = j = 0;
	int mergedIndex = 0;

	while (i < nLow && j < nHigh) {
		if (auxArrLow[i] <= auxArrHigh[j])
			arrLow[mergedIndex++] = auxArrLow[i++];
		else
			arrLow[mergedIndex++] = auxArrHigh[j++];
	}

	while (i < nLow)
		arrLow[mergedIndex++] = auxArrLow[i++];
	while (j < nHigh)
		arrLow[mergedIndex++] = auxArrHigh[j++];

}

void mergeSortSeq(int* arr, int low, int high) {
	if (low < high) {
		int mid = low + ((high - low) / 2);

		mergeSortSeq(arr, low, mid);
		mergeSortSeq(arr, mid + 1, high);

		int n1 = mid - low + 1;
		int n2 = high - mid;
		//mergeSeq(arr, low, mid, high);
		mergeSeq(&arr[low], &arr[mid+1], n1, n2);
	}
}
*/
