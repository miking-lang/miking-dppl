/*
 * File bamm.cuh defines constants and type definitions used in the bamm model bamm.cu. 
 */

// #include "../tree-utils/tree_utils.cuh"

const int MAX_DEPTH = tree_t::MAX_DEPTH;
const int STACK_LIMIT = MAX_DEPTH * 2 + 1;

struct lambdaFun_t {
    floating_t lambda;
    floating_t z;
    floating_t t1;

    DEV lambdaFun_t(){};
    DEV lambdaFun_t(floating_t lambda_, floating_t z_, floating_t t1_){
        lambda = lambda_;
        z = z_;
        t1 = t1_;
    };
};

struct bblockArgs_t {
    lambdaFun_t lf;
    floating_t mu;
    floating_t eta;
    floating_t rho;

    DEV bblockArgs_t(){};
    DEV bblockArgs_t(lambdaFun_t lf_, floating_t mu_, floating_t eta_, floating_t rho_){
        lf = lf_;
        mu = mu_;
        eta = eta_;
        rho = rho_;
    };
};

struct pStack_t {
    int stackPointer = 0;
    bblockArgs_t args[STACK_LIMIT];

    DEV void push(bblockArgs_t element) {
        //if(stackPointer >= STACK_LIMIT || stackPointer < 0)
            //printf("Illegal stack push with sp=%d\n", stackPointer);
        args[stackPointer] = element;
        stackPointer++;
    }

    DEV bblockArgs_t pop() {
        stackPointer--;
        //if(stackPointer < 0)
            //printf("SP < 0!\n");
        return args[stackPointer];
    }

    DEV bblockArgs_t peek() {
        //if(stackPointer-1 < 0)
            //printf("SP < 0!\n");
        return args[stackPointer - 1];
    }
};


typedef short treeIdx_t;
struct alignas(8) progState_t {
    pStack_t stack;
    treeIdx_t treeIdx;
};

/*
struct nestedProgState_t {
    bool extinct;
};
typedef double return_t;
*/

struct simBranchRet_t {
    lambdaFun_t lf;
    floating_t r1;
    floating_t r2;
    floating_t r3;
    floating_t r4;
    floating_t r5;
    int r6;
    floating_t r7;

    DEV simBranchRet_t(){};

    DEV simBranchRet_t(lambdaFun_t lf_, floating_t r1_, floating_t r2_, floating_t r3_, floating_t r4_, floating_t r5_, int r6_, floating_t r7_) {
        lf = lf_;
        r1 = r1_;
        r2 = r2_;
        r3 = r3_;
        r4 = r4_;
        r5 = r5_;
        r6 = r6_;
        r7 = r7_;
    }
};