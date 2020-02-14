
// #include <bits/stdc++.h> 
#include <algorithm>

#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "../Utils/array.cuh"
#include "lda.cuh"

// g++ -x c++ Src/LDA/lda.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

// nvcc -arch=sm_75 -rdc=true Src/LDA/lda.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU


BBLOCK_DATA_2D(corpus, int, D, MAX_DOC_LENGTH); // DOCUMENTS
BBLOCK_DATA(docLength, int, D); // length of each document

const int NUM_BBLOCKS = 3;

// Set up eta (could be done globally, but this is only setup cost)
// Sample alpha and beta
BBLOCK(init, progState_t, {
    PSTATE.orgParticleIdx = i;
    floating_t eta[VOCAB_SIZE];
    for(int v = 0; v < VOCAB_SIZE; v++) 
        eta[v] = 1;

    floating_t alphaPrior[K];
    for(int k = 0; k < K; k++) 
        alphaPrior[k] = 1;

    BBLOCK_CALL(sampleDirichlet, alphaPrior, PSTATE.alpha, K); // eta is originally prior of beta, not alpha
    for(int k = 0; k < K; k++)
        BBLOCK_CALL(sampleDirichlet, eta, PSTATE.beta[k], VOCAB_SIZE);

    PSTATE.docIdx = -1;

    /* 
    // Categorical test
    floating_t dist[3];
    dist[0] = 0.1;
    dist[1] = 0.3;
    dist[2] = 0.6;
    int sampleFreq[3] = {0};

    for(int j = 0; j < 10000; j++) {
        int sample = BBLOCK_CALL(sampleCategorical, dist, 3);
        sampleFreq[sample]++;
    }

    printf("freqList: [");
    for(int j = 0; j < 3; j++)
        printf("%d, ", sampleFreq[j]);
    printf("]\n");
    */

    PC++;
    RESAMPLE = false;
})

// Sample theta and control document index and termination condition
BBLOCK(newDocument, progState_t, {
    PSTATE.docIdx++;
    PSTATE.wordIdx = 0;

    if(PSTATE.docIdx >= D) { // Terminate
        PC = NUM_BBLOCKS;

    } else { // Process next document
        PC++;
        BBLOCK_CALL(sampleDirichlet, PSTATE.alpha, PSTATE.theta[PSTATE.docIdx], K);
    }

    RESAMPLE = false;
})

BBLOCK(newWord, progState_t, {

    // Get particle state and relevant data
    int currDocIdx = PSTATE.docIdx;
    int currWordIdx = PSTATE.wordIdx;
    int currWord = DATA_POINTER(corpus)[currDocIdx][currWordIdx];
    int* docLengths = DATA_POINTER(docLength);

    // Check if end of document
    if(currWordIdx == docLengths[currDocIdx]) {
        PC--;
        RESAMPLE = false;
        return;
    }

    // Check if word does not exist, or end of doc is nearing
    if(currWord == -1) {
        RESAMPLE = false;
        int* doc = DATA_POINTER(corpus)[currDocIdx];
        int currDocLength = docLengths[currDocIdx];
        do { // Keep incerementing until word exists in vocab
            currWordIdx++;
            if(currWordIdx == currDocLength) {
                PC--;
                return;
            }
        } while(doc[currWordIdx] == -1);

    } else {
        int sampledTopic = BBLOCK_CALL(sampleCategorical, PSTATE.theta[currDocIdx], K); // z
        WEIGHT(PSTATE.beta[sampledTopic][currWord]); // Weight with likelihood of w: p(w | z, a, b ...)

        // if(currWordIdx > 182)
            // printf("currwordidx: %d, currdocidx: %d\n", currWordIdx, currDocIdx);
        RESAMPLE = currWordIdx % WORDS_PER_RESAMPLE == 0;
    }

    PSTATE.wordIdx = currWordIdx + 1;
})

struct tuple_t {
    floating_t prob;
    int idx;
};

bool compareTuple(tuple_t const& t1, tuple_t const& t2) { 
    return (t1.prob < t2.prob); 
} 

STATUSFUNC({
    
    int freqList[NUM_PARTICLES] = {0};
    for(int i = 0; i < NUM_PARTICLES; i++) {

        freqList[PSTATE.orgParticleIdx]++;
        
        /*
        printf("alpha: [");
        for(int k = 0; k < K; k++)
            printf("%f, ", PSTATE.alpha[k]);
        printf("]\n");
        
        for(int k = 0; k < K; k++) {
            printf("beta[%d]: [", k);
            for(int v = 0; v < VOCAB_SIZE; v++)
                printf("%f, ", PSTATE.beta[k][v]);
            printf("]\n");
        }
        
        for(int d = 0; d < D; d++) {
            printf("theta[%d]: [", d);
            for(int k = 0; k < K; k++)
            printf("%f, ", PSTATE.theta[d][k]);
            printf("]\n");
        }
        */

        // printf("Weight[%d]: %f\n", i, PWEIGHT);
    }

    int maxIdx = 0;
    for(int i = 1; i < NUM_PARTICLES; i++)
        if(freqList[i] > freqList[maxIdx])
            maxIdx = i;
    printf("MaxIdx: %d\n", maxIdx);
    floating_t inverseK = 1.0 / K;
    printf("inv K: %f\n", inverseK);

    int i = maxIdx;
    for(int k = 0; k < K; k++) {
        floating_t* currDist = PSTATE.beta[k];
        tuple_t tuples[VOCAB_SIZE];
        for(int j = 0; j < VOCAB_SIZE; j++) {
            floating_t productOverTopics = 1;
            for(int kInner = 0; kInner < K; kInner++) { // Switch iteration order for performance :)
                productOverTopics *= PSTATE.beta[kInner][j];
            }
            tuples[j].prob = PSTATE.beta[k][j] * log(PSTATE.beta[k][j] / (pow(productOverTopics, (1.0 / K))));
            // tuples[j].prob = currDist[j];
            tuples[j].idx = j;
        }
        int n = sizeof(tuples) / sizeof(tuples[0]); 
        std::sort(tuples, tuples + n, &compareTuple);
        printf("Topic[%d]: [", k);
        for (int j = VOCAB_SIZE-1; j >= VOCAB_SIZE-10; j--)
            printf("%d, ", tuples[j].idx);
        printf("]\n");
    }

})

void setup() {
    initGen();
    readCorpus(corpus);

    /*
    for(int d = 0; d < D; d++) {
        printf("doc[%d]: [", d);
        for(int w = 0; w < MAX_DOC_LENGTH; w++) {
            printf("%d, ", corpus[d][w]);
        }
        printf("]\n");
    }
    */
}

int main() {
    setup();

    COPY_DATA_GPU(corpus, int, D * MAX_DOC_LENGTH);
    COPY_DATA_GPU(docLength, int, D);

    SMCSTART(progState_t); // allokera array

    INITBBLOCK(init, progState_t); // create func pointer and add to array
    INITBBLOCK(newDocument, progState_t);
    INITBBLOCK(newWord, progState_t);

    SMCEND(progState_t);
}
