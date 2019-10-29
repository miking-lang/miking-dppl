const int STACK_LIMIT = MAX_DEPTH * 2 + 1;

struct lambdaFun_t {
    floating_t lambda;
    floating_t z;
    floating_t t1;
};

struct bblockArgs_t {
    lambdaFun_t lf;
    floating_t mu;
    floating_t sigma;
};

struct stack_t {
    int stackPointer = 0;
    bblockArgs_t args[STACK_LIMIT];

    HOST DEV void push(bblockArgs_t element) {
        if(stackPointer >= STACK_LIMIT || stackPointer < 0)
            printf("Illegal stack push with sp=%d\n", stackPointer);
        args[stackPointer] = element;
        stackPointer++;
    }

    HOST DEV bblockArgs_t pop() {
        stackPointer--;
        if(stackPointer < 0)
            printf("SP < 0!\n");
        return args[stackPointer];
    }

    HOST DEV bblockArgs_t peek() {
        if(stackPointer-1 < 0)
            printf("SP < 0!\n");
        return args[stackPointer - 1];
    }
};


typedef short treeIdx_t;
struct progState_t {
    stack_t stack;
    treeIdx_t treeIdx;
};

struct nestedProgState_t {
    bool extinct;
};
typedef double return_t;

struct simBranchRet_t {
    lambdaFun_t lf;
    floating_t r1;
    floating_t r2;
    floating_t r3;
    floating_t r4;
    floating_t r5;
    int r6;
    floating_t r7;

    simBranchRet_t(){};

    simBranchRet_t(lambdaFun_t ilf, floating_t ir1, floating_t ir2, floating_t ir3, floating_t ir4, floating_t ir5, int ir6, floating_t ir7) {
        lf = ilf;
        r1 = ir1;
        r2 = ir2;
        r3 = ir3;
        r4 = ir4;
        r5 = ir5;
        r6 = ir6;
        r7 = ir7;
    }
};