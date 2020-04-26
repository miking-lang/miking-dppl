#ifndef UTILS_INCLUDED
#define UTILS_INCLUDED

void printArray(int* arr, int length);
bool checkSorted(int* arr, int length);
void randomizeArray(int* arr, int length);
void swapPointers(int** a, int** b);
bool arraysEqual(int* a, int* b, int n);
int* getSortedCopy(int* a, int n);
void startTimer();
double getTimeElapsed();

#endif