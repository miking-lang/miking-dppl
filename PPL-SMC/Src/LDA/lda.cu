
#include "../Smc/smc.cuh"
#include "../Smc/smcImpl.cuh"
#include "../Utils/distributions.cuh"
#include "../Utils/array.cuh"
#include "lda.cuh"

// g++ -x c++ Src/LDA/lda.cu Src/Utils/*.cpp -o smc.exe -std=c++11 -O3

// nvcc -arch=sm_75 -rdc=true Src/LDA/lda.cu Src/Utils/*.cpp -o smc.exe -lcudadevrt -std=c++11 -O3 -D GPU


BBLOCK_DATA_2D(corpus, int, D, MAX_DOC_LENGTH); // DOCUMENTS

const int NUM_BBLOCKS = 3;

// Set up eta (could be done globally, but this is only setup cost)
// Sample alpha and beta
BBLOCK(init, progState_t, {
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
    // sample z
    // weight w
    // check if end of document and change PC

    int currWord = DATA_POINTER(corpus)[PSTATE.docIdx][PSTATE.wordIdx];

    if (currWord == -1) {
        PC--;
        RESAMPLE = false;
    } else {
        RESAMPLE = PSTATE.wordIdx % WORDS_PER_RESAMPLE == 0;
    }

    PSTATE.wordIdx++;
})

STATUSFUNC({
    
    for(int i = 0; i < NUM_PARTICLES; i++) {
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

    SMCSTART(progState_t); // allokera array

    INITBBLOCK(init, progState_t); // create func pointer and add to array
    INITBBLOCK(newDocument, progState_t);
    INITBBLOCK(newWord, progState_t);

    SMCEND(progState_t);
}
