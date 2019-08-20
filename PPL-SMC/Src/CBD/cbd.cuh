#ifndef CBD_INCLUDED
#define CBD_INCLUDED

#include "../Smc/smc.cuh"

const int NUM_NODES = 7;
const int ROOT_IDX = 0;

struct treeLoc_t {
    int treeIdx;
    int parentIdx;
};

// Both structs and static array members are deeply copied
struct stack_t {
    int stackPointer = 0;
    treeLoc_t treeLocs[NUM_NODES]; // Could be as low as DEPTH?

    HOST DEV void push(treeLoc_t treeLoc) {
        treeLocs[stackPointer] = treeLoc;
        stackPointer++;
    }

    HOST DEV treeLoc_t pop() {
        stackPointer--;
        return treeLocs[stackPointer];
    }

    HOST DEV treeLoc_t peek() {
        return treeLocs[stackPointer - 1];
    }
};

struct progState_t {
    stack_t stack;
};

struct tree_t {
                                    //   0, 1, 2, 3, 4, 5, 6  parent could possibly be found with formula?
    const floating_t ages[NUM_NODES] = {10, 6, 4, 0, 0, 0, 0};
    const int idxLeft[NUM_NODES] = {1, 3, 5, -1, -1, -1, -1};
    const int idxRight[NUM_NODES] = {2, 4, 6, -1, -1, -1, -1};
    //const bool visited[NUM_NODES] = {false};
};


#endif