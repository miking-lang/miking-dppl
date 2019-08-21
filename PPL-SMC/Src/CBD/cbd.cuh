#ifndef CBD_INCLUDED
#define CBD_INCLUDED

#include "../Smc/smc.cuh"


const int ROOT_IDX = 0;

/* TREES */

/*
// simple toy tree, nodes: 7, maxDepth: 3
const int NUM_NODES = 7;
const int MAX_DEPTH = 3;
struct tree_t {
                                    //   0, 1, 2, 3, 4, 5, 6  parent could possibly be found with formula?
    const floating_t ages[NUM_NODES] =  {10, 6, 4,  0,  0,  0,  0};
    const int idxLeft[NUM_NODES] =      {1,  3, 5, -1, -1, -1, -1};
    const int idxRight[NUM_NODES] =     {2,  4, 6, -1, -1, -1, -1};
};
*/


/*
// TimeTree, pitheciidae monkeys (source: http://timetree.org/), nodes: 55, maxDepth: 9
const int NUM_NODES = 55;
const int MAX_DEPTH = 9;
struct tree_t {
    const floating_t ages[NUM_NODES] =  {17.993216 , 11.330548 , 11.016198 , 5.4853 , 7.31 , 10.237967 , 4.4329 , 0 , 2.9571 , 3.19694 , 7.11 , 6.1354 , 8.34 , 0 , 2.6 , 0 , 0 , 0 , 1.1078 , 0 , 5.9 , 3.04517 , 5.69232 , 0 , 4.88882 , 0 , 0 , 0.02283 , 0 , 3.2838 , 0 , 2.2398 , 0 , 4.3404 , 0 , 2.0967 , 0 , 0 , 0 , 2.9942 , 0 , 1.72 , 0 , 2.81 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1.33 , 0 , 0 , 0};
    const int idxLeft[NUM_NODES] =      {1 , 3 , 5 , 7 , 9 , 11 , 13 , -1 , 15 , 17 , 19 , 21 , 23 , -1 , 25 , -1 , -1 , -1 , 27 , -1 , 29 , 31 , 33 , -1 , 35 , -1 , -1 , 37 , -1 , 39 , -1 , 41 , -1 , 43 , -1 , 45 , -1 , -1 , -1 , 47 , -1 , 49 , -1 , 51 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 53 , -1 , -1 , -1};
    const int idxRight[NUM_NODES] =     {2 , 4 , 6 , 8 , 10 , 12 , 14 , -1 , 16 , 18 , 20 , 22 , 24 , -1 , 26 , -1 , -1 , -1 , 28 , -1 , 30 , 32 , 34 , -1 , 36 , -1 , -1 , 38 , -1 , 40 , -1 , 42 , -1 , 44 , -1 , 46 , -1 , -1 , -1 , 48 , -1 , 50 , -1 , 52 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 54 , -1 , -1 , -1};
};
*/


// Bisse tree, nodes: 63, maxDepth: 9
const int NUM_NODES = 63;
const int MAX_DEPTH = 9;
struct tree_t {
    const floating_t ages[NUM_NODES] =  {13.016 , 10.626 , 8.958 , 8.352 , 10.536 , 3.748 , 7.775 , 7.679 , 7.361 , 8.291 , 8.192 , 0 , 0.0033 , 0.0584 , 1.589 , 5.187 , 5.196 , 3.818 , 1.868 , 1.396 , 0 , 0.056 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 4.871 , 1.143 , 1.813 , 0.0866 , 1.06 , 0.0215 , 0 , 0 , 0 , 2.601 , 0 , 0.0829 , 0 , 0.0452 , 0 , 0 , 0.0001 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0.0203 , 0 , 0 , 0 , 0 , 0};
    const int idxLeft[NUM_NODES] =      {1 , 3 , 5 , 7 , 9 , 11 , 13 , 15 , 17 , 19 , 21 , -1 , 23 , 25 , 27 , 29 , 31 , 33 , 35 , 37 , -1 , 39 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 41 , 43 , 45 , 47 , 49 , 51 , -1 , -1 , -1 , 53 , -1 , 55 , -1 , 57 , -1 , -1 , 59 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 61 , -1 , -1 , -1 , -1 , -1};
    const int idxRight[NUM_NODES] =     {2 , 4 , 6 , 8 , 10 , 12 , 14 , 16 , 18 , 20 , 22 , -1 , 24 , 26 , 28 , 30 , 32 , 34 , 36 , 38 , -1 , 40 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 42 , 44 , 46 , 48 , 50 , 52 , -1 , -1 , -1 , 54 , -1 , 56 , -1 , 58 , -1 , -1 , 60 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , 62 , -1 , -1 , -1 , -1 , -1};
};




struct treeLoc_t {
    int treeIdx;
    int parentIdx;
};

// Both structs and static array members are deeply copied
struct stack_t {
    int stackPointer = 0;
    treeLoc_t treeLocs[MAX_DEPTH]; // Could be as low as DEPTH?

    HOST DEV void push(treeLoc_t treeLoc) {
        treeLocs[stackPointer] = treeLoc;
        stackPointer++;
        if(stackPointer >= MAX_DEPTH)
            printf("SP=%d\n", stackPointer);
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


#endif