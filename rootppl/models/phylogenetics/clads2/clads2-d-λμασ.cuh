BBLOCK_DATA(tree, tree_t, 1);

/**
 * The delayed program state now has the latest k
 * and theta, as well as a stack of factors;
 */
typedef short treeIdx_t;
struct progStateDelayed_t {
 
  floating_t factorArr[(tree->NUM_NODES)];
  // This array stores for each node
  // the factor at the beginning of its parent node.
  // TODO Technically we don't need a factor for the root (it is assumed to be 1)
  // so we can do one less.
  // But for now we are going to waste one posistion for easier debuggin

  floating_t factorEndArr[(tree->NUM_NODES)];
  // the factors at the end of the branches
  
  gamma_t lambda_0;
  gamma_t mu_0;
  normalInverseGamma_t alphaSigma;

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
    
    floating_t tExt = sample_GammaExponential(mu_0, factor);
    floating_t t    = std::min(tSp, tExt);
   
    floating_t currentTime = startTime - t;
    
    if(currentTime < 0) { // we are in the future, rho is the detection probability
        bool undetected = !SAMPLE(bernoulli, rho);  
        return undetected;
    }
   
    bool speciation =  (tSp < tExt) ? true : false;
    bool extinction = !speciation;
    
    if(extinction) {
      return true;
    }
    
    // Realizes the new factor by which the current lambda (= lambda_0 x old factors)
    // is going to be multiplied. One for left and right.
    floating_t f1 = sample_NormalInverseGammaNormal(alphaSigma);
    floating_t f2 = sample_NormalInverseGammaNormal(alphaSigma);

    bool ret1 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu_0, factor*exp(f1), alphaSigma, rho);
    
    bool leftDetection = !ret1;
    if (leftDetection) return ret1; // no need to descend to the right side of the tree
    
    bool ret2 = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu_0, factor*exp(f2), alphaSigma, rho);
    return ret2;
    
  }, bool, floating_t startTime, gamma_t& lambda0, gamma_t& mu_0, floating_t factor, normalInverseGamma_t& alphaSigma, floating_t rho)




// Simulates the hidden speciation events along a branch
// Returns
//   - the accumulated probability along the branch
//   - the accumulated factors along the branch
// has side-effect that the proposal for lambda0 is updated
BBLOCK_HELPER(simBranchDelayed, {

    floating_t t1 = startTime - stopTime;
    assert(0.0 <= t1);
    
    // extreme values patch 2/2
    if (factor > 1e5 ) {
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }

    if (factor < 1e-5) {
      //floating_t ret0 = score_GammaPoisson(0, t1, mu, factor);
      //floating_t ret0 = 0.0;
      simBranchRet_t ret(factor, 0.0, -INFINITY);
      return ret;
    }
    // end extreme values patch 2/2

    floating_t t = sample_GammaExponential(lambda0, factor);
    floating_t currentTime = startTime - t;
    

    if(currentTime <= stopTime) {
      floating_t ret1 = score_GammaPoisson(0, t1, mu_0, factor);
      simBranchRet_t ret(factor, 0.0, ret1);
      return ret;
    }
    
    // sample factors for left and right subtrees
    floating_t f1 = sample_NormalInverseGammaNormal(alphaSigma);
    floating_t f2 = sample_NormalInverseGammaNormal(alphaSigma);
    
    // we need to check if the side was undetected
    // w.l.o.g. we choose the right side to die
    //rate_t rightRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f2));
    bool sideUndetected = BBLOCK_CALL(clads2GoesUndetectedDelayed, currentTime, lambda0, mu_0, factor*exp(f2), alphaSigma, rho);

    if(! sideUndetected) {
      // this particle needs to die
      simBranchRet_t ret(0.0, 0.0, -INFINITY);
      return ret;
    }
    
    
    // Now we will enter into the recursion to process the rest of the branch
    // and accummulate the factor
    //    rate_t leftRate(lambdaRate.k, lambdaRate.theta, lambdaRate.factor*exp(f1));
    
    simBranchRet_t ret7 = BBLOCK_CALL(simBranchDelayed, currentTime, stopTime, lambda0, mu_0, factor*exp(f1), alphaSigma,  rho);

    floating_t extinctionProb = score_GammaPoisson(0, t, mu_0, factor);  // branch didn't go extinct
    
    // Now gather all weights and add 2 for the end of the branch
    // 1 and 2 are probs, 3 is a bool, 4 is a prob again
    //simBranchRetDelayed_t rt(ret7.prob + extinctionProb + log(2.0), ret7.factor);
  simBranchRet_t rt(ret7.r0, 0.0, ret7.r2 + log(2.0) + extinctionProb);

  return rt;
    
  }, simBranchRet_t, floating_t startTime, floating_t stopTime, gamma_t& lambda0, gamma_t& mu_0, floating_t factor, normalInverseGamma_t& alphaSigma, floating_t rho);



 

#define NUM_BBLOCKS 5
INIT_MODEL(progStateDelayed_t, NUM_BBLOCKS)



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

    
    
    //floating_t factor = PSTATE.stack.pop();

     floating_t factor = PSTATE.factorArr[treeIdx];
     //std::cout << "Simulating from: " << indexParent << "to" << treeIdx << ", ";
     // std::cout << "Reading factor[" << treeIdx << "] = " << PSTATE.factorArr[treeIdx]<< "\n";
     // std::cout << "popped: " << factor << "\n";
 
     // if (std::abs(factor - PSTATE.factorArr[treeIdx]) > 0.1) {
     //   std::cout << "ERROR CONDITION!";
          
     // }
     
     
    
    simBranchRet_t ret = BBLOCK_CALL(simBranchDelayed, treeP->ages[indexParent], treeP->ages[treeIdx],  PSTATE.lambda_0, PSTATE.mu_0, factor, PSTATE.alphaSigma, PSTATE.rho);

    floating_t factorEnd = ret.r0;
    PSTATE.factorEndArr[treeIdx] = factorEnd;

    
    bool interiorNode = treeP->idxLeft[treeIdx] != -1 || treeP->idxRight[treeIdx] != -1;
    floating_t lnTerminalProb = interiorNode ? score_GammaExponential(0, PSTATE.lambda_0, factorEnd) : log(PSTATE.rho);
    WEIGHT(ret.r2 + lnTerminalProb);

    // TODO Collect node and branch info, todo?    
    if(interiorNode) {
      floating_t f1 = sample_NormalInverseGammaNormal(PSTATE.alphaSigma);
      floating_t f2 = sample_NormalInverseGammaNormal(PSTATE.alphaSigma);

      floating_t leftf = factorEnd*exp(f1);
      floating_t rightf = factorEnd*exp(f2);
      
      //      PSTATE.stack.push(rightf);
      //PSTATE.stack.push(leftf);
      //std::cout << "Creating factor[" << (treeP->idxLeft[treeIdx]  ) << "] =" << leftf <<"\n";
      //std::cout << "and factor[" << (treeP->idxRight[treeIdx] ) << "]=" << rightf << "\n";
      PSTATE.factorArr[treeP->idxLeft[treeIdx]] = leftf;
      PSTATE.factorArr[treeP->idxRight[treeIdx]] = rightf;
//      PSTATE.factorArr[treeIdx - 1] = rightf;
//      PSTATE.factorArr[treeP->idxNext[treeIdx] - 1] = leftf;
    }
    
})
 
 
BBLOCK(simClaDS2, {
    tree_t* treeP = DATA_POINTER(tree);

    // Make sure this is the correct starting point
    PSTATE.treeIdx = treeP->idxLeft[ROOT_IDX];
    //std::cout << "At ROOT " << PSTATE.treeIdx << "\n";


   // floating_t lambda_0 = SAMPLE(gamma, k, theta);
    gamma_t lambda_0(k, theta);
    gamma_t mu_0(kMu, thetaMu);

    //floating_t sigmaSquared = 1.0 / SAMPLE(gamma, 1.0, 1.0 / 0.2);
    //floating_t sigma = sqrt(sigmaSquared);
    //floating_t alpha = exp(SAMPLE(normal, 0.0, sigma));
    
// * σ^2 | a,b ~ InverseGamma(a, b)
// * m ~ N(m0, v σ^2)
    normalInverseGamma_t alphaSigma(m0, v, a, b);
    
    floating_t epsilon = SAMPLE(uniform, 0.0, 1.0);
    floating_t factor = 1.0;
    
    PSTATE.lambda_0 = lambda_0;
    PSTATE.mu_0 = mu_0;
    PSTATE.alphaSigma = alphaSigma;
    PSTATE.epsilon = epsilon;
    PSTATE.rho = rho;
 
    // Correction Factor
    int numLeaves = countLeaves(treeP->idxLeft, treeP->idxRight, treeP->NUM_NODES);
    floating_t corrFactor = (numLeaves - 1) * log(2.0) - lnFactorial(numLeaves);
    WEIGHT(corrFactor);
    
    // TODO the following with a sample MACRO
    floating_t f1 = sample_NormalInverseGammaNormal(PSTATE.alphaSigma);
    floating_t f2 = sample_NormalInverseGammaNormal(PSTATE.alphaSigma);

    floating_t leftf = factor*exp(f1);
    floating_t rightf = factor*exp(f2);


    // bblockArgs_t args(lambda1, lambda2);
    // one for each child, and one for Survivorship Bias after tree simulations
    //    PSTATE.stack.push(rightf);
    //PSTATE.stack.push(leftf);
    //PSTATE.stack.push(factor);
    //std::cout << "Creating factor[" << (PSTATE.treeIdx) << "] =" << leftf << "\n";
    //std::cout << "and factor[" << (treeP->idxRight[ROOT_IDX] ) << "] = " << rightf << "\n";
    PSTATE.factorArr[PSTATE.treeIdx] = leftf;
    PSTATE.factorArr[treeP->idxRight[ROOT_IDX]] = rightf;

    PC++;
    BBLOCK_CALL(simTree);


    // Condition on detection (clads2GoesUndetected simulations)
    // Nested inference with "forward" method here, just simulation with WEIGHT( -2.0 * log(number of false))?
})


// Should be equivalent to forward sampling
BBLOCK(conditionOnDetection, {

    tree_t* treeP = DATA_POINTER(tree);
    floating_t treeAge = treeP->ages[ROOT_IDX];
    //floating_t factor = PSTATE.stack.pop();
    floating_t factor = 1.0;

    
    floating_t epsilon = PSTATE.epsilon;
    floating_t rho = PSTATE.rho;

    int numSamples = 100;
    int numDetected = 0;
    for(int i = 0; i < numSamples; i++) {
      bool undetected = BBLOCK_CALL(clads2GoesUndetectedDelayed, treeAge, PSTATE.lambda_0, PSTATE.mu_0, factor, PSTATE.alphaSigma, rho);
        if(! undetected)
            numDetected++;
    }
    //printf("condition weihght: %f", -2.0 * log(numDetected / static_cast<floating_t>(numSamples)) );
    WEIGHT(-2.0 * log(numDetected / static_cast<floating_t>(numSamples)));

    PC++;

})

BBLOCK(justResample, {
    //std::cout << "Resampling...\r\r\r\r\r\r\r\r\r\r\r\r";
    PC++;
})


BBLOCK(sampleFinalLambda, {
    PSTATE.lambda0 = SAMPLE(gamma, PSTATE.lambda_0.k, PSTATE.lambda_0.theta);
    PSTATE.mu0 = SAMPLE(gamma, PSTATE.mu_0.k, PSTATE.mu_0.theta);
    PSTATE.epsilon = PSTATE.mu0/PSTATE.lambda0;
    
    floating_t sigmaSquared = 1.0 / SAMPLE(gamma, PSTATE.alphaSigma.a, 1.0 / PSTATE.alphaSigma.b);
    PSTATE.sigma = sigmaSquared;
    PSTATE.alpha = SAMPLE(normal, PSTATE.alphaSigma.m0, 1/PSTATE.alphaSigma.v * PSTATE.sigma);
    PC++;
})
 
// Write particle data to file
CALLBACK(saveResults, {
    int M = std::min(1000, N);
    
    std::string fileName = "results/EXP-" + analysisName + ".csv";
    std::ofstream resultFile (fileName, std::ios_base::app);
    //resultFile << "lambda0 mu0 alpha log-alpha sigma2 epsilon log-weight\n";
    // lambda0 mu0 epsilon alpha sigma 
    if(resultFile.is_open()) {
      
      for(int i = 0; i < M; i++)
              resultFile << 
		PSTATES[i].lambda0 << ", " <<
		PSTATES[i].mu0 << ", " <<
		PSTATES[i].epsilon << ", " <<
		PSTATES[i].alpha << ", " <<
		PSTATES[i].sigma << ", " <<
		WEIGHTS[i] << "\n";

          resultFile.close();
      } else {
          printf("Could not open file %s\n", fileName.c_str());
      }

      std::string fileName2 = "results/EXP-" + analysisName + "-factors.csv";
      std::ofstream resultFile2 (fileName2, std::ios_base::app);
      if(resultFile2.is_open()) {

	for(int i = 0; i < M; i++) {
	  for (int j = 0; j < (tree->NUM_NODES); j++) {
	    resultFile2 << PSTATES[i].factorArr[j] << ", ";
	  }
	  resultFile2 << WEIGHTS[i] << "\n";
	}
	
	resultFile2.close();
      } else {
	printf("Could not open file %s\n", fileName2.c_str());
      }

      std::string fileName3 = "results/EXP-" + analysisName + "-end-factors.csv";
      std::ofstream resultFile3 (fileName3, std::ios_base::app);
      if(resultFile3.is_open()) {

	for(int i = 0; i < M; i++) {
	  for (int j = 0; j < (tree->NUM_NODES); j++) {
	    resultFile3 << PSTATES[i].factorEndArr[j] << ", ";
	  }
	  resultFile3 << WEIGHTS[i] << "\n";
	}
	
	resultFile3.close();
      } else {
	printf("Could not open file %s\n", fileName3.c_str());
      }

      // new lambdas at beginning of branches
      std::string fileName4 = "results/EXP-" + analysisName + "-new-lambdas.csv";
      std::ofstream resultFile4 (fileName4, std::ios_base::app);
      if(resultFile4.is_open()) {
	
	for(int i = 0; i < M; i++) {
	  resultFile4 << PSTATES[i].lambda0 << ", ";
	  for (int j = 1; j < (tree->NUM_NODES); j++) {
	    resultFile4 << SAMPLE(gamma, PSTATES[i].lambda_0.k, PSTATES[i].lambda_0.theta*PSTATES[i].factorArr[j]) <<  ", ";
	  }
	  resultFile4 << WEIGHTS[i] << "\n";
	}
	
	resultFile4.close();
      } else {
	printf("Could not open file %s\n", fileName4.c_str());
      }


      // new lambdas at the end of branches
      std::string fileName5 = "results/EXP-" + analysisName + "-new-lambdas-end.csv";
      std::ofstream resultFile5 (fileName5, std::ios_base::app);
      if(resultFile5.is_open()) {
	
	for(int i = 0; i < M; i++) {
	  for (int j = 0; j < (tree->NUM_NODES); j++) {
	    resultFile5 << SAMPLE(gamma, PSTATES[i].lambda_0.k, PSTATES[i].lambda_0.theta*PSTATES[i].factorEndArr[j]) <<  ", ";
	  }
	  resultFile5 << WEIGHTS[i] << "\n";
	}
	
	resultFile5.close();
      } else {
	printf("Could not open file %s\n", fileName4.c_str());
      }


  })

