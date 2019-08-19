#ifndef LIST_INCLUDED
#define LIST_INCLUDED


#ifdef GPU
#include "../cudaErrorUtils.cu"
#endif

#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"

// Not yet supported for GPU!

template <typename T>
struct list_t {

    T* arr;
    int last = -1;

    HOST DEV T operator [] (int i) const {return arr[i];}
    HOST DEV T& operator [] (int i) {return arr[i];}

    HOST DEV bool operator==(list_t& other) const {
        for(int i = 0; i <= last; i++)
            if(other.arr[i] != arr[i])
                return false;

        return true;
    }

    HOST DEV bool operator==(T* other) const {
        for(int i = 0; i <= last; i++)
            if(other[i] != arr[i])
                return false;

        return true;
    }

    /*
    __device__ list_t(){
        
    }
    __device__ ~list_t(){
        
    }
    */

    /*list_t(int capacity) : arr[capacity] {
        
    }*/

    HOST DEV void initList(int capacity) {
        allocateMemory<T>(&arr, capacity);
    }

    HOST DEV void destroyList() {
        //freeMemory<T>(arr);
    }

    HOST DEV void push_back(T element) {
        last++;
        arr[last] = element;
    }

    HOST DEV int size() {
        return last + 1;
    }

    HOST DEV T back() {
        return arr[last];
    }
};


#endif