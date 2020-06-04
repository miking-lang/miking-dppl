#ifndef STACK_INCLUDED
#define STACK_INCLUDED


const int MAX_STACK_DEPTH = 100;

#ifdef WORDS

struct stack_t {
    int stackPointer = 0;
    int arr[MAX_STACK_DEPTH];
    // void* arr = arrInt;
    int numProgStates = 0;
    int numPC = 0;

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
        if(numWords == 1)
            numPC++;
        else
            numProgStates++;
        // printf("Pushed %d ints, SP=%d\n", numWords, stackPointer);
        void* pStateVoid = static_cast<void*>(&element);
        int* pStateInt = static_cast<int*>(pStateVoid);
        for (int w = 0; w < numWords; w++) { 
            push(pStateInt[w]);
        }
    }

    template <typename T1>
    HOST DEV void popType(T1* ret) {
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4
        if(numWords == 1)
            numPC--;
        else
            numProgStates--;
        // printf("Popped %d ints, numPC=%d, numProgStates=%d\n", numWords, numPC, numProgStates);
        // int pStateInt[numWords]; 
        void* voidRet = static_cast<void*>(ret);
        int* intRet = static_cast<int*>(voidRet);

        for (int w = numWords-1; w >= 0; w--) {
            intRet[w] = pop();
        }

        // void* pStateVoid = static_cast<void*>(pStateInt);
        // T1 pState = *static_cast<T1*>(pStateVoid);
        // return pState;
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
