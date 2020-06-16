#ifndef STACK_INCLUDED
#define STACK_INCLUDED


const int MAX_STACK_DEPTH = 100;

#define WORDS
#ifdef WORDS

struct stack_t {
    int stackPointer = 0;
    int arr[MAX_STACK_DEPTH];

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
        if(sizeof(T1) % 4 != 0)
            printf("Pushing type of size non multiple of 4: %lu\n", sizeof(T1));
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4
        
        void* pStateVoid = static_cast<void*>(&element);
        int* pStateInt = static_cast<int*>(pStateVoid);
        for (int w = 0; w < numWords; w++) { 
            push(pStateInt[w]);
        }
    }

    template <typename T1>
    HOST DEV void popType(T1* ret) {
        if(sizeof(T1) % 4 != 0)
            printf("Popping type of size non multiple of 4: %lu\n", sizeof(T1));
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4

        void* voidRet = static_cast<void*>(ret);
        int* intRet = static_cast<int*>(voidRet);

        for (int w = numWords-1; w >= 0; w--) {
            intRet[w] = pop();
        }
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
