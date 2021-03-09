
/*
 * File clads2.cuh defines constants and type definitions used in the clads2 model clads2.cu. 
 */

const int MAX_DEPTH = tree_t::MAX_DEPTH;
//const int STACK_LIMIT = MAX_DEPTH * 2 + 1;
const int STACK_LIMIT = 8096;

struct pStack_t {
    int stackPointer = 0;
    // bblockArgs_t args[STACK_LIMIT];
    floating_t args[STACK_LIMIT];

    DEV void push(floating_t element) {
        if(stackPointer >= STACK_LIMIT || stackPointer < 0)
            printf("Illegal stack push with sp=%d\n", stackPointer);
        args[stackPointer] = element;
        stackPointer++;
    }

    DEV floating_t pop() {
        stackPointer--;
        if(stackPointer < 0)
            printf("SP < 0!\n");
        return args[stackPointer];
    }

    DEV floating_t peek() {
        if(stackPointer-1 < 0)
            printf("SP < 0!\n");
        return args[stackPointer - 1];
    }
};

typedef short treeIdx_t;
struct progState_t {
    pStack_t stack;
    floating_t alpha;
    floating_t sigma;
    floating_t epsilon;
    floating_t rho;
    floating_t lambda_0;
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




const int MAX_DIV = 10000;
const int MAX_LAM = 10000;
const floating_t MIN_LAM = 1e-5;
// const bool PANDA = false;

// Since PANDA is false
#define LAMBDA_CHOOSER(lambda, lambdaEnd, alpha, sigma) exp(SAMPLE(normal, log(alpha*lambdaEnd), sigma))
 

BBLOCK_HELPER(clads2GoesUndetected, {
 

    floating_t mu = epsilon * lambda;

    // extreme values patch 1/2
    if (lambda - mu > MAX_DIV)
		return false;

    if (lambda < MIN_LAM)
        return ! SAMPLE(bernoulli, rho);
    // end extreme values patch 1/2

    floating_t t = SAMPLE(exponential, lambda + mu);

    floating_t currentTime = startTime - t;
    if(currentTime < 0)
        return ! SAMPLE(bernoulli, rho); 


    bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    if(exctinction)
        return true;

    floating_t lambda1 = exp(SAMPLE(normal, log(alpha * lambda), sigma));
    floating_t lambda2 = exp(SAMPLE(normal, log(alpha * lambda), sigma));

    return BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda1, alpha, sigma, epsilon, rho)
        && BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda2, alpha, sigma, epsilon, rho);
 
}, bool, floating_t startTime, floating_t lambda, floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
 
 

/*
 * Simulates Hidden Speciation Events Along Branch
 */ 
BBLOCK_HELPER(simBranch, {

    floating_t t1 = startTime - stopTime;
    floating_t mu = epsilon * lambda;

    // extreme values patch 2/2
    if(lambda > MAX_LAM) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }

    if(lambda < MIN_LAM) {
        simBranchRet_t ret(lambda, lambda*t1, -mu*t1);
        return ret;
    }
    // extreme values patch 2/2

    floating_t t = SAMPLE(exponential, lambda);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
        simBranchRet_t ret(lambda, lambda*t1, -mu*t1);
        return ret;
    }

 
    floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);
    floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambda, alpha, sigma);

    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetected, currentTime, lambda2, alpha, sigma, epsilon, rho);

    if(! sideUndetected) {
        simBranchRet_t ret(0.0, 0.0, -INFINITY);
        return ret;
    }
    simBranchRet_t ret = BBLOCK_CALL(simBranch, currentTime, stopTime, lambda1, alpha, sigma, epsilon, rho);
    simBranchRet_t rt(ret.r0, ret.r1 + lambda*t, ret.r2 + log(2.0) - mu*t);
    return rt;

}, simBranchRet_t, floating_t startTime, floating_t stopTime, floating_t lambda, 
    floating_t alpha, floating_t sigma, floating_t epsilon, floating_t rho)
