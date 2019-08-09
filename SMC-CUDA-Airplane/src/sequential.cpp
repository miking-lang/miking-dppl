#include <iostream>
#include <random>

#include "utils.h"

using namespace std;

double MSESeq;
double MSEAvgSeq = 0;
int counterSeq = 0;

default_random_engine generator;


void calcMSESeq(floating_t* w, int* offspring) {
    double weightSum = 0;
    for(int i = 0; i < NUM_PARTICLES; i++)
        weightSum += w[i];
    
    for(int i = 0; i < NUM_PARTICLES; i++) {
        double expectedOffspring = w[i] * NUM_PARTICLES / weightSum;
        MSESeq += pow(offspring[i] - expectedOffspring, 2);
    }
}

// Numerically instable! Presort or use thrust lib
void calcInclusivePrefixSum(floating_t* w, floating_t* prefixSum) {
    prefixSum[0] = w[0];
    for(int i = 1; i < NUM_PARTICLES; i++) {
        prefixSum[i] = prefixSum[i-1] + w[i];
    }
}

void systematicOffspring(floating_t* prefixSum, int* cumulativeOffspring, floating_t u) {

   floating_t totalSum = prefixSum[NUM_PARTICLES-1];
   for(int i = 0; i < NUM_PARTICLES; i++) {
        floating_t expectedCumulativeOffspring = NUM_PARTICLES * prefixSum[i] / totalSum;
        cumulativeOffspring[i] = min(NUM_PARTICLES, static_cast<int>(floor(expectedCumulativeOffspring + u)));
   }
}

void offspringToAncestor(int* cumulativeOffspring, floating_t* ancestorX, floating_t* x) {
    for(int i = 0; i < NUM_PARTICLES; i++) {
        int start = i == 0 ? 0 : cumulativeOffspring[i - 1];
        int numCurrentOffspring = cumulativeOffspring[i] - start;
        floating_t xVal = x[i];
        for(int j = 0; j < numCurrentOffspring; j++)
            ancestorX[start+j] = xVal;
    }
}

void updateWeights(floating_t* x, floating_t* w, floating_t* planeObs, int t) {
    floating_t weightSum = 0;

    if(LOG_WEIGHTS) {
        for (int i = 0; i < NUM_PARTICLES; i++)
            w[i] = logNormalPDFObs(planeObs[t+1], mapLookupApprox(x[i]));

        floating_t m = maxValue(w, NUM_PARTICLES); // scale weights with max(log w)
        if(RESAMPLING_STRATEGY == MULTINOMIAL) {
            for (int i = 0; i < NUM_PARTICLES; i++) {
                w[i] -= m;
                w[i] = exp(w[i]);
                weightSum += w[i];
            }
            for(int i = 0; i < NUM_PARTICLES; i++) // Now division by zero is avoided, but numerical instability remains?
                w[i] /= weightSum;
        } else if(RESAMPLING_STRATEGY == SYSTEMATIC) {
            for (int i = 0; i < NUM_PARTICLES; i++) {
                w[i] -= m;
                w[i] = exp(w[i]);
            }
        }
        
    } else {
        if(RESAMPLING_STRATEGY == MULTINOMIAL) {
            for(int i = 0; i < NUM_PARTICLES; i++) {
                w[i] = normalPDFObs(planeObs[t+1], mapLookupApprox(x[i]));
                weightSum += w[i];
            }
            for (int i = 0; i < NUM_PARTICLES; i++)
                w[i] /= weightSum;
        } else if(RESAMPLING_STRATEGY == SYSTEMATIC) {
            for(int i = 0; i < NUM_PARTICLES; i++) {
                w[i] = normalPDFObs(planeObs[t+1], mapLookupApprox(x[i]));
            }
        }
    }
}

void smcSeq(floating_t* planeX, floating_t* planeObs) {
    floating_t* x = new floating_t[NUM_PARTICLES];
    floating_t* w = new floating_t[NUM_PARTICLES];
    floating_t* ancestorX = new floating_t[NUM_PARTICLES];
    floating_t* prefixSum = new floating_t[NUM_PARTICLES];
    int* cumulativeOffspring = new int[NUM_PARTICLES];
    int* offspring = new int[NUM_PARTICLES];

    // MSESeq = 0;
    
    generator.seed(time(NULL));
    uniform_real_distribution<floating_t> uniDist(0.0, MAP_SIZE);
    uniform_real_distribution<floating_t> uDist(0.0, 1.0);

    for(int i = 0; i < NUM_PARTICLES; i++) {
        x[i] = uniDist(generator);
        // w[i] = 1.0 / NUM_PARTICLES; // redundant
    }

    // Initial weight update
    updateWeights(x, w, planeObs, -1);
    

    // Time Loop, weighting lies one step ahead
    for(int t = 0; t < TIME_STEPS - 1; t++) {

        //for(int i = 0; i < NUM_PARTICLES; i++)
            //offspring[i] = 0;

        /** Resample*/
        if(RESAMPLING_STRATEGY == MULTINOMIAL) {
            discrete_distribution<int> catDist (w, w + NUM_PARTICLES);
            for (int i = 0; i < NUM_PARTICLES; i++) {
                int index = catDist(generator);
                //offspring[index] += 1;
                ancestorX[i] = x[index];
            }
        } else if(RESAMPLING_STRATEGY == SYSTEMATIC) {
            calcInclusivePrefixSum(w, prefixSum);
            floating_t u = uDist(generator);
            systematicOffspring(prefixSum, cumulativeOffspring, u);
            offspringToAncestor(cumulativeOffspring, ancestorX, x);
        }
        // calcMSESeq(w, offspring);

        // Assign and propagate
        
        for (int i = 0; i < NUM_PARTICLES; i++) {
            normal_distribution<floating_t> transitionDist (ancestorX[i] + VELOCITY, TRANSITION_STD);
            x[i] = transitionDist(generator);
        }
        
        // Weight
        //if(t < TIME_STEPS - 1) {
        updateWeights(x, w, planeObs, t); // can be optimized to include weight in previous loop, nvm...
        //}
        
        printStatus(x, w, planeX, TIME_STEPS-1);
    }
    // printStatus(x, w, planeX, TIME_STEPS-1);

    /*
    MSESeq /= TIME_STEPS;
    printf("MSE: %f, MSE/N: %f\n", MSESeq, MSESeq/NUM_PARTICLES);
    MSEAvgSeq += MSESeq / NUM_PARTICLES;
    counterSeq++;
    if(counterSeq >= 20) {
        printf("AvgMSE/N: %f\n", MSEAvgSeq / counterSeq);
        printStatus(x, w, planeX, TIME_STEPS-1);
    }
    */
   
    delete[] x;
    delete[] w;
    delete[] ancestorX;
    delete[] prefixSum;
    delete[] cumulativeOffspring;
    delete[] offspring;
}
