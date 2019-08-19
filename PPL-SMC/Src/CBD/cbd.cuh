#ifndef CBD_INCLUDED
#define CBD_INCLUDED

#include "../Smc/smc.cuh"

const int NUM_NODES = 7;

struct progState_t {
    bool res;
};

struct tree_t {
                                    //   0, 1, 2, 3, 4, 5, 6
    const floating_t ages[NUM_NODES] = {10, 6, 4, 0, 0, 0, 0};
    const int idxLeft[NUM_NODES] = {1, 3, 5, -1, -1, -1, -1};
    const int idxRight[NUM_NODES] = {2, 4, 6, -1, -1, -1, -1};
};


#endif