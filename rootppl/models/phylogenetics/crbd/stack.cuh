#ifndef STACK_INCLUDED
#define STACK_INCLUDED


const int MAX_STACK_DEPTH = 100;

#define WORDS
#ifdef WORDS
const bool LARGE_COPIES = true;

// const int INCR_LEN = NUM_PARTICLES;

struct pStack_t {
    int stackPointer = 0;
    int arr[MAX_STACK_DEPTH];
    // int* arr;

    // Overloading of Assignment Operator
    HOST DEV void operator=(const pStack_t &S ) { 
        // printf("overload\n");
        stackPointer = S.stackPointer;
        for(int i = 0; i < stackPointer; i++)
            arr[i] = S.arr[i];

        // memcpy(arr, S.arr, stackPointer * sizeof(int));
        // copy(S.arr, S.arr + stackPointer, arr);
        // memcpy(this, &S, sizeof(int) + S.stackPointer * sizeof(int));
        // arr = S.arr;
    }

    HOST DEV void push(int element) {
        arr[stackPointer] = element;
        stackPointer++;
        //stackPointer += INCR_LEN;
        //printf("SP: %d, max: %d\n", stackPointer, MAX_STACK_DEPTH * NUM_PARTICLES);
    }

    HOST DEV int pop() {
        stackPointer--;
        // stackPointer -= INCR_LEN;
        return arr[stackPointer];
    }

    /*HOST DEV int peek() {
        // return arr[stackPointer - 1];
        return arr[stackPointer - INCR_LEN];
    }*/

    /*HOST DEV bool empty() {
        return stackPointer == 0;
    }*/

    template <typename T1>
    HOST DEV void pushType(T1 element) {
        if(sizeof(T1) % 4 != 0)
            printf("Pushing type of size non multiple of 4: %lu\n", sizeof(T1));
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4
        
        if(LARGE_COPIES) {
            memcpy(&arr[stackPointer], &element, sizeof(T1));
            stackPointer += numWords;
        } else {

            void* pStateVoid = static_cast<void*>(&element);
            int* pStateInt = static_cast<int*>(pStateVoid);
            
            for (int w = 0; w < numWords; w++) { 
                push(pStateInt[w]);
            }
        }
    }

    template <typename T1>
    HOST DEV void popType(T1* ret) {
        if(sizeof(T1) % 4 != 0)
            printf("Popping type of size non multiple of 4: %lu\n", sizeof(T1));
        int numWords = sizeof(T1) / 4; // Fix case with non multiple of 4

        if(LARGE_COPIES) {
            stackPointer -= numWords;
            memcpy(ret, &arr[stackPointer], sizeof(T1));
        } else {

            void* voidRet = static_cast<void*>(ret);
            int* intRet = static_cast<int*>(voidRet);

            for (int w = numWords-1; w >= 0; w--) {
                intRet[w] = pop();
            }
        }
    }
};

#else

template <typename T>
struct pStack_t {
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
