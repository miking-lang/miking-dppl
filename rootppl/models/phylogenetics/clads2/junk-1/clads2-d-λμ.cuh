
/**
 * The delayed program state now has the latest k
 * and theta, as well as a stack of factors;
 */
typedef short treeIdx_t;
struct progStateDelayed_t {
  pStack_t stack;
  gamma_t lambda_0;
  gamma_t mu;
  floating_t lambda0;
  floating_t mu0;
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



/**
 * This function simulates the side-branches and returns 
 * true if the side branch does not make it to our sapmle.
 *
 * - start_time: when did the lineage speciate
 * - lambda0  (as reference to be updated)
 * - factor: the accummulated lambda factor at start_time
 * - alpha
 * - sigma
 * - epsilon
 * - rho
 * 
 * Returns: TRUE/FALSE
 * also it has side effect that the proposal for lambda0 is updated
 */
BBLOCK_HELPER(clads2GoesUndetectedDelayed, {
    
    // extreme values patch
    if (factor > 1e5) {
      return false; // detected for sure with insane div. rate
    }
    
    if (factor < 1e-5) {
      // lambda is very small, so nothing will happen to the lineage in terms of speciation
      // it will hit present and then we see
      bool undetected = !SAMPLE(bernoulli, rho);  
      return undetected;
    }
    // end extreme values patch 1
    
    // t is the waiting time until the next event (speciation or extinction)
    floating_t tSp  = sample_GammaExponential(lambda0, factor);
    floating_t tExt = sample_GammaExponential(mu, factor);
    floating_t t    = std::min(tSp, tExt);
    
    floating_t currentTime = startTime - t;
    
    if(currentTime < 0) { // we are in the future, rho is the detection probability
        bool undetected = !SAMPLE(bernoulli, rho);  
        return undetected;
    }
            
    //  bool exctinction = SAMPLE(bernoulli, mu / (mu + lambda));
    bool speciation = (tSp < tExt) ? true : false;
    bool extinction = !speciation;
    
    if(extinction) {
      return true;
    }
    
    // Realizes the new factor by which the current lambda (= lambda_0 x old factors)
    // and mu (in order to keep the epsilon ration)
    // are going to be multiplied. One for left and right.
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);

    bool ret1 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu, factor*exp(f1), alpha, sigma, rho);
    
    bool leftDetection = !ret1;
    if (leftDetection) return ret1; // no need to descend to the right side of the tree

    bool ret2 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu, factor*exp(f2), alpha, sigma, rho);
    return ret2;
    
  }, bool, floating_t startTime, gamma_t& lambda0, gamma_t& mu, floating_t factor, floating_t alpha, floating_t sigma, floating_t rho)




// Simulates the hidden speciation events along a branch
// Returns
//   - the accumulated probability along the branch
//   - the accumulated factors along the branch
// has side-effect that the proposal for lambda0 is updated
BBLOCK_HELPER(simBranchDelayed, {
    floating_t t1 = startTime - stopTime;
    
    // Extreme values patch 2/2
    if (factor > 1e5 ) {
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }
    if (factor < 1e-5) {
      //floating_t ret0 = score_GammaPoisson(0, t1, mu, factor);
        floating_t ret0 = 0.0;
      simBranchRet_t ret(factor, 0.0, ret0);
      return ret;
    }
    // End extreme values patch 2/2

    floating_t t = sample_GammaExponential(lambda0, factor);
    floating_t currentTime = startTime - t;

    if(currentTime <= stopTime) {
      floating_t ret1 = score_GammaPoisson(0, t1, mu, factor);
      simBranchRet_t ret(factor, 0.0, ret1);
      return ret;
    }
    
    // sample factors for left and right subtrees
    floating_t f1 = SAMPLE(normal, log(alpha), sigma); // left factor
    floating_t f2 = SAMPLE(normal, log(alpha), sigma); // right factor
    
    // we need to check if the side was undetected
    // w.l.o.g. we choose the right side to die
    //rate_t rightRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f2));
    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu, factor*exp(f2), alpha, sigma, rho);

    if(! sideUndetected) {   
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }
    
    // Now we will enter into the recursion to process the rest of the branch
    // and accummulate the factor
    //    rate_t leftRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f1));
    simBranchRet_t ret7 = BBLOCK_CALL(simBranchDelayed, currentTime, stopTime, lambda0, mu, factor*exp(f1), alpha, sigma, rho);

    floating_t extinctionProb = score_GammaPoisson(0, t, mu, factor);  // branch didn't go extinct
    
    // Now gather all weights and add 2 for the end of the branch
    // 1 and 2 are probs, 3 is a bool, 4 is a prob again
  simBranchRet_t rt(ret7.r0, 0.0, ret7.r2 + log(2.0) + extinctionProb);
  return rt;
  }, simBranchRet_t, floating_t startTime, floating_t stopTime, gamma_t& lambda0, gamma_t& mu, floating_t factor, floating_t alpha, floating_t sigma, floating_t rho); 

#define NUM_BBLOCKS 4
INIT_MODEL(progStateDelayed_t, NUM_BBLOCKS)

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

    floating_t factor = PSTATE.stack.pop();

    simBranchRet_t ret = BBLOCK_CALL(simBranchDelayed, treeP->ages[indexParent], treeP->ages[treeIdx],  PSTATE.lambda_0, PSTATE.mu, factor, PSTATE.alpha, PSTATE.sigma, PSTATE.rho);

    floating_t factorEnd = ret.r0;

    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnTerminalProb = interiorNode ? score_GammaExponential(0, PSTATE.lambda_0, factorEnd) : log(PSTATE.rho);
 
    WEIGHT(ret.r2 + lnTerminalProb);
    // TODO Collect node and branch info
    if(interiorNode) {
        floating_t f1 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);
        floating_t f2 = SAMPLE(normal, log(PSTATE.alpha), PSTATE.sigma);

        floating_t leftf = factorEnd*exp(f1);
        floating_t rightf = factorEnd*exp(f2);

        PSTATE.stack.push(rightf);
        PSTATE.stack.push(leftf);
    }
})
 
 
BBLOCK(simClaDS2, {
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
 
    // Test settings

    gamma_t lambda_0(kLambda, thetaLambda);
    gamma_t mu(kMu, thetaMu);

    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, a / b);
    floating_t sigma = sqrt(sigmaSquared);
    floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));
    
    floating_t factor = 1.0;
    
    PSTATE.lambda_0 = lambda_0;
    PSTATE.mu = mu;
    PSTATE.alpha = alpha;
    PSTATE.sigma = sigma;
    PSTATE.rho = rho;
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);
    
    floating_t f1 = SAMPLE(normal, log(alpha), sigma);
    floating_t f2 = SAMPLE(normal, log(alpha), sigma);
    
    floating_t leftf = factor*exp(f1);
    floating_t rightf = factor*exp(f2);
    
    // one for each child, and one for Survivorship Bias after tree simulations
    PSTATE.stack.push(rightf);
    PSTATE.stack.push(leftf);
    PSTATE.stack.push(factor);

    PC++;
    BBLOCK_CALL(simTree);

    // Condition on detection (clads2GoesUndetected simulations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
})

// Should be equivalent to forward sampling
BBLOCK(conditionOnDetection, {

    tree_t* treeP = DATA_POINTER(tree);
    floating_t treeAge = treeP->ages[ROOT_IDX];

    //gamma_t lambda0 = PSTATE.lambda_0;
    //gamma_t mu = PSTATE.mu;
    
    floating_t factor = PSTATE.stack.pop();
    floating_t alpha = PSTATE.alpha;
    floating_t sigma = PSTATE.sigma;
    floating_t rho = PSTATE.rho;

    int numSamples = 100;
    int numDetected = 0;
    for(int i = 0; i < numSamples; i++) {
      // TODO testing the direct application of PSTATE here
      bool undetected = BBLOCK_CALL(clads2GoesUndetectedDelayed, treeAge, PSTATE.lambda_0, PSTATE.mu, factor, alpha, sigma, rho);
        if(! undetected)
            numDetected++;
    }
    //printf("condition weihght: %f", -2.0 * log(numDetected / static_cast<floating_t>(numSamples)) );
    WEIGHT(-2.0 * log(numDetected / static_cast<floating_t>(numSamples)));

    PC++;

})

BBLOCK(sampleFinalLambda, {
    PSTATE.lambda0 = SAMPLE(gamma, PSTATE.lambda_0.k, PSTATE.lambda_0.theta);
    PSTATE.mu0 = SAMPLE(gamma, PSTATE.lambda_0.k, PSTATE.lambda_0.theta);
    PSTATE.epsilon = PSTATE.lambda0/PSTATE.mu0;
    PC++;
})
 
// Write particle data to file
CALLBACK(saveResults, {
      std::string fileName = "results/clads2-delayed.csv";
      std::ofstream resultFile (fileName);
      resultFile << "lambda0 k theta mu0 sigma alpha epsilon weight\n";
      if(resultFile.is_open()) {

          for(int i = 0; i < N; i++)
              resultFile << 
		PSTATES[i].lambda0 << " " << PSTATES[i].lambda_0.k << " " << PSTATES[i].lambda_0.theta << " " << PSTATES[i].mu0 << " " << PSTATES[i].sigma << " " << PSTATES[i].alpha << " " << PSTATES[i].epsilon << " " << 
                  exp(WEIGHTS[i]) << "\n";

          resultFile.close();
      } else {
          printf("Could not open file %s\n", fileName.c_str());
      }

  })

