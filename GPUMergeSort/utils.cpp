#include <iostream>
#include <time.h>
//#include <thrust/sort.h>
#include <algorithm>

using namespace std;

struct timespec start, finish;
double elapsed;

void printArray(int* arr, int length) {
	cout << "[ ";
	for (int i = 0; i < length; i++)
		cout << arr[i] << " ";

	cout << "]" << endl;
}

bool checkSorted(int* arr, int length) {
	for(int i = 0; i < length; i++)
		if(arr[i] != i)
			return false;
	/*
	for (int i = 1; i < length; i++)
		if (arr[i] < arr[i - 1])
			return false;
	*/

	return true;
}

void randomizeArray(int* arr, int length) {
	for (int i = 0; i < length; i++)
		arr[i] = i;

	for (int i = 0; i < length; i++) {
		int swapIndex = rand() % (i + 1);
		arr[i] = arr[swapIndex];
		arr[swapIndex] = i;
	}
}

void swapPointers(int** a, int** b) {
	int* temp = *a;
	*a = *b;
	*b = temp;
}

bool arraysEqual(int* a, int* b, int n) {
	for (int i = 0; i < n; i++)
		if(a[i] != b[i])
			return false;
	
	return true;
}

int* getSortedCopy(int* a, int n) {
	int* copy = new int[n];
	for(int i = 0; i < n; i++)
		copy[i] = a[i];

	//thrust::sort(copy, copy + n);
	std::sort(copy, copy + n);

	return copy;
}

void startTimer() {
	clock_gettime(CLOCK_MONOTONIC, &start);
}

double getTimeElapsed() {
	clock_gettime(CLOCK_MONOTONIC, &finish);

	elapsed = (finish.tv_sec - start.tv_sec);
	elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;

	return elapsed;
}
