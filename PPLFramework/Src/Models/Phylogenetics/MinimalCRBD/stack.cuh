#ifndef STACK_INCLUDED
#define STACK_INCLUDED


const int MAX_STACK_DEPTH = 100;

#ifdef WORDS

struct stack_t {
    int stackPointer = 0;
    int arr[MAX_STACK_DEPTH];
    // void* arr = arrInt;

    HOST DEV void push(int element) {
        arr[stackPointer] = element;
        stackPointer++;
    }

    HOST DEV int pop() {
        stackPointer--;
        return arr[stackPointer];
    }

    HOST DEV int peek() {
        return arr[stackPointer - 1];
    }

    HOST DEV bool empty() {
        return stackPointer == 0;
    }

    template <typename T1>
    HOST DEV void pushType(T1 element) {
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4
        void* pStateVoid = static_cast<void*>(&element);
        int* pStateInt = static_cast<int*>(pStateVoid);
        for (int w = 0; w < numWords; w++) { 
            push(pStateInt[w]);
        }
    }

    template <typename T1>
    HOST DEV T1 popType() {
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4
        int pStateInt[numWords]; 

        for (int w = numWords-1; w >= 0; w--) {
            pStateInt[w] = pop();
        }

        void* pStateVoid = static_cast<void*>(pStateInt);
        T1 pState = *static_cast<T1*>(pStateVoid);
        return pState;
    }
};

#else

template <typename T>
struct stack_t {
    int stackPointer = 0;
    T arr[MAX_STACK_DEPTH];

    HOST DEV void push(T element) {
        arr[stackPointer] = element;
        stackPointer++;
    }

    HOST DEV T pop() {
        stackPointer--;
        return arr[stackPointer];
    }

    HOST DEV T peek() {
        return arr[stackPointer - 1];
    }

    HOST DEV bool empty() {
        return stackPointer == 0;
    }
};

#endif

#endif
