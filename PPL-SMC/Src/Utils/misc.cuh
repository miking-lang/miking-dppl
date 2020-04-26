#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <iostream>
#include <vector>
#include <cstring>

#include "../Inference/Smc/smc.cuh"
#include "list.cuh"

using namespace std;

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

void printList(list_t<bool> l, string title="") {
    if(title.length() > 0)
        cout << title << ": ";
    cout << "[ ";
    for(int i = 0; i < l.size(); i++)
        cout << l[i] << " ";
    /*
    for(bool b : l) {
        cout << b << " ";
    }
    */
    cout << "]\n" << endl;
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