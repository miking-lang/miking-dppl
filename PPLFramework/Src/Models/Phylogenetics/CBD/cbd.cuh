#ifndef CBD_INCLUDED
#define CBD_INCLUDED

/*
#include "../Smc/smc.cuh"
#include "trees.cuh"

typedef short treeIdx_t;

// #define USE_STACK

#ifdef USE_STACK

// Both structs and static array members are deeply copied
struct stack_t {
    int stackPointer = 0;
    treeIdx_t treeIdx[MAX_DEPTH];

    HOST DEV void push(treeIdx_t idx) {
        treeIdx[stackPointer] = idx;
        stackPointer++;
    }

    HOST DEV treeIdx_t pop() {
        stackPointer--;
        return treeIdx[stackPointer];
    }

    HOST DEV treeIdx_t peek() {
        return treeIdx[stackPointer - 1];
    }
};

struct progState_t {
    stack_t stack;
};

#else

struct progState_t {
    treeIdx_t treeIdx;
};

#endif
*/
#endif