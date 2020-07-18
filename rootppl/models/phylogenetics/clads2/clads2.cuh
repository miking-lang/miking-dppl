
/*
 * File clads2.cuh defines constants and type definitions used in the clads2 model clads2.cu. 
 */

const int MAX_DEPTH = 20;
const int STACK_LIMIT = MAX_DEPTH * 2 + 1;

typedef short treeIdx_t;
struct progState_t {
    pStack_t stack;
    floating_t alpha;
    floating_t sigma;
    floating_t epsilon;
    floating_t rho;
    treeIdx_t treeIdx;
};

struct simBranchRet_t {
    floating_t r0;
    floating_t r1;
    floating_t r2;

    DEV simBranchRet_t(){};

    DEV simBranchRet_t(floating_t r0_, floating_t r1_, floating_t r2_) {
        r0 = r0_;
        r1 = r1_;
        r2 = r2_;
    }
};

struct bblockArgs_t {
    floating_t lambda1;
    floating_t lambda2;

    DEV bblockArgs_t(){};
    DEV bblockArgs_t(floating_t lambda1_, floating_t lambda2_){
        lambda1 = lambda1_;
        lambda2 = lambda2_;
    };
};

struct pStack_t {
    int stackPointer = 0;
    bblockArgs_t args[STACK_LIMIT];

    DEV void push(bblockArgs_t element) {
        if(stackPointer >= STACK_LIMIT || stackPointer < 0)
            printf("Illegal stack push with sp=%d\n", stackPointer);
        args[stackPointer] = element;
        stackPointer++;
    }

    DEV bblockArgs_t pop() {
        stackPointer--;
        if(stackPointer < 0)
            printf("SP < 0!\n");
        return args[stackPointer];
    }

    DEV bblockArgs_t peek() {
        if(stackPointer-1 < 0)
            printf("SP < 0!\n");
        return args[stackPointer - 1];
    }
};
