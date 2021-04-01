
/*
 * Immediate sampling.
 * File clads2.cuh defines constants and type definitions used in the template. 
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


#define NUM_BBLOCKS 3
INIT_MODEL(progState_t, NUM_BBLOCKS)
 
BBLOCK_HELPER_DECLARE(clads2GoesUndetected, bool, floating_t, floating_t, floating_t, floating_t, floating_t, floating_t);

BBLOCK_DATA(tree, tree_t, 1);

 
 
// Not called on root as in WebPPL, instead root is handled in simClaDS2 bblock
BBLOCK(simTree, {

    // Fetch tree data
    tree_t* treeP = DATA_POINTER(tree);
    treeIdx_t treeIdx = PSTATE.treeIdx;

    // Terminate if tree is fully traversed
    if(treeIdx == -1) {
        PC++;
        BBLOCK_CALL(DATA_POINTER(bblocksArr)[PC], NULL);
        return;
    }

    PSTATE.treeIdx = treeP->idxNext[treeIdx];

    int indexParent = treeP->idxParent[treeIdx];

    floating_t lambda = PSTATE.stack.pop();

    simBranchRet_t ret = BBLOCK_CALL(simBranch, treeP->ages[indexParent], treeP->ages[treeIdx], lambda, PSTATE.alpha, PSTATE.sigma, PSTATE.epsilon, PSTATE.rho);

    floating_t lambdaEnd = ret.r0;

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnTerminalProb = interiorNode ? log(lambdaEnd) : log(PSTATE.rho);
    WEIGHT(ret.r2 + lnTerminalProb);

    // Collect node and branch info, todo?

    if(interiorNode) {
        floating_t lambda1 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);
        floating_t lambda2 = LAMBDA_CHOOSER(lambda, lambdaEnd, PSTATE.alpha, PSTATE.sigma);

        PSTATE.stack.push(lambda1);
        PSTATE.stack.push(lambda2);
    }

})
 
 
BBLOCK(simClaDS2, {
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
    #ifndef PRIORS
    #define PRIORS
    floating_t lambda_0 = SAMPLE(gamma, kLambda, thetaLambda);
    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, a / b);
    floating_t sigma = sqrt(sigmaSquared);
    floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));
    floating_t epsilon = SAMPLE(uniform, epsMin, epsMax);
    #endif
    
    PSTATE.lambda_0 = lambda_0;
    PSTATE.alpha = alpha;
    PSTATE.sigma = sigma;
    PSTATE.epsilon = epsilon;
    PSTATE.rho = rho;
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);

    floating_t lambda1 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);
    floating_t lambda2 = LAMBDA_CHOOSER(lambda_0, lambda_0, alpha, sigma);

    // bblockArgs_t args(lambda1, lambda2);
    // one for each child, and one for Survivorship Bias after tree simulations
    PSTATE.stack.push(lambda1);
    PSTATE.stack.push(lambda2);
    PSTATE.stack.push(lambda_0);

    PC++;
    BBLOCK_CALL(simTree);

    // Condition on detection (clads2GoesUndetected simulations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
})

// Should be equivalent to forward sampling
BBLOCK(conditionOnDetection, {

    tree_t* treeP = DATA_POINTER(tree);
    floating_t treeAge = treeP->ages[ROOT_IDX];

    floating_t lambda0 = PSTATE.stack.pop();
    floating_t alpha = PSTATE.alpha;
    floating_t sigma = PSTATE.sigma;
    floating_t epsilon = PSTATE.epsilon;
    floating_t rho = PSTATE.rho;

    int numSamples = 100;
    int numDetected = 0;
    for(int i = 0; i < numSamples; i++) {
        bool undetected = BBLOCK_CALL(clads2GoesUndetected, treeAge, lambda0, alpha, sigma, epsilon, rho);
        if(! undetected)
            numDetected++;
    }
    WEIGHT(-2.0 * log(numDetected / static_cast<floating_t>(numSamples)));

    PC++;

})
 
// Write particle data to file
CALLBACK(saveResults, {
    // results/treename-model-logz
    // results/treename-model-pdf_log
    
    std::string fileName = "results/clads2.csv";

    std::ofstream resultFile (fileName);
    resultFile << "lambda0 sigma alpha epsilon weight\n";
    if(resultFile.is_open()) {

        for(int i = 0; i < N; i++)
            resultFile << 
                PSTATES[i].lambda_0 << " " << PSTATES[i].sigma << " " << PSTATES[i].alpha << " " << PSTATES[i].epsilon << " " << 
                exp(WEIGHTS[i]) << "\n";

        resultFile.close();
    } else {
        printf("Could not open file %s\n", fileName.c_str());
    }

})

