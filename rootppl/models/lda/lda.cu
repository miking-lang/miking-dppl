/*
 * WORK IN PROGRESS, NOT CURRENTLY WORKING
 */

#include <algorithm>

#include "inference/smc/smc.cuh"
#include "utils/misc.cuh"
#include "lda.cuh"

BBLOCK_DATA_2D(corpus, int, D, MAX_DOC_LENGTH); // DOCUMENTS
BBLOCK_DATA(docLength, int, D); // length of each document
BBLOCK_DATA(eta, floating_t, VOCAB_SIZE);

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

    BBLOCK_CALL(sampleDirichlet, alphaPrior, PSTATE.alpha, K);

    // for (int k = 0; k < K; k++)
    //     PSTATE.alpha[k] = 1;

    for(int k = 0; k < K; k++)
        BBLOCK_CALL(sampleDirichlet, eta, PSTATE.beta[k], VOCAB_SIZE);

    PSTATE.docIdx = -1;


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

    // printf("docLen: %d\n", docLengths[currDocIdx]);
    // printf("currWordIdx: %d\n", currWordIdx);

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
        // floating_t scalar = PSTATE.theta[currDocIdx][sampledTopic]; // Scale mutation by how likely this topic is
        // PSTATE.beta[sampledTopic][currWord] += BBLOCK_CALL(sampleUniform, 0, scalar / (D * MAX_DOC_LENGTH)); // mutate
        // normalizeArray(PSTATE.beta[sampledTopic], VOCAB_SIZE);
        
        // cannot seem to find performance increase by doing this...
        // Sample params again if noone resampled to you
        /*
        if ((currWordIdx+1) % WORDS_PER_RESAMPLE == 0 && currDocIdx < D / 2 && PSTATE.orgParticleIdx != i) {
            PSTATE.orgParticleIdx = i;
            floating_t* etaP = DATA_POINTER(eta);

            // int sampledIdx = BBLOCK_CALL(sampleCategoricalStandard, K);
            //for (int k = 0; k < K; k++)
            //    BBLOCK_CALL(sampleDirichlet, etaP, PSTATE.beta[k], VOCAB_SIZE);
            PSTATE.beta[sampledTopic][currWord] += 1.0 / VOCAB_SIZE;
        }
        */
        
        WEIGHT(log(PSTATE.beta[sampledTopic][currWord])); // Weight with likelihood of w: p(w | z, a, b ...)

        //for (int k = 0; k < K; k++) {
        //  PSTATE.beta[sampledTopic][currWord] += BBLOCK_CALL(sampleUniform, 0, 0.5 / (D * MAX_DOC_LENGTH)); // fit to data?
        // }

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

const int NUM_WORDS_PER_TOPIC = VOCAB_SIZE / K;
floating_t correctRatio(int bestWordsTopics[K][NUM_WORDS_PER_TOPIC]) {

    int numCorrectPerTopic[K] = {0};

    for (int k = 0; k < K; k++) {
        int currTopic = bestWordsTopics[k][0] / NUM_WORDS_PER_TOPIC;
        numCorrectPerTopic[currTopic] = 0;
        int minWordTopic = currTopic * NUM_WORDS_PER_TOPIC;
        int maxWordTopic = (currTopic+1) * NUM_WORDS_PER_TOPIC;
        // printf("currTopic: %d\n", currTopic);
        // printf("First word: %d\n", bestWordsTopics[k][0]);
        // printf("minWordTopic: %d\n", minWordTopic);
        // printf("maxWordTopic: %d\n", maxWordTopic);
        for (int j = 0; j < NUM_WORDS_PER_TOPIC; j++) {
            if (bestWordsTopics[k][j] >= minWordTopic && bestWordsTopics[k][j] < maxWordTopic) {
                numCorrectPerTopic[currTopic]++;
            }
        }
        // printf("NumCorrect topic[%d]: %d\n", currTopic, numCorrectPerTopic[currTopic]);
    }

    floating_t numCorrectTotal = 0;
    for (int k = 0; k < K; k++)
        numCorrectTotal += numCorrectPerTopic[k];
    // printf("NumCorrTot: %d\n", numCorrectTotal);
    // printf("%.4f, ", numCorrectTotal / VOCAB_SIZE);
    return numCorrectTotal / VOCAB_SIZE;
}

floating_t ratioSum = 0;
STATUSFUNC({
    
    int freqList[NUM_PARTICLES] = {0};
    for(int i = 0; i < NUM_PARTICLES; i++)
        freqList[PSTATE.orgParticleIdx]++;

    int maxIdx = 0;
    for(int i = 1; i < NUM_PARTICLES; i++)
        if(freqList[i] > freqList[maxIdx])
            maxIdx = i;

    
    // scan and find one of the correct particles
    // sample entire beta again on particles with some duplicate particles
    int i = maxIdx;
    // printf("MaxFreq: %d\n", freqList[maxIdx]);
    if(PSTATE.orgParticleIdx != maxIdx) {
        // printf("ERRRORRROR!\n");
        for (i = 0; i < NUM_PARTICLES; i++) {
            if(PSTATE.orgParticleIdx == maxIdx) {
                maxIdx = i;
                break;
            }
        }
    }

    i = maxIdx;
    // printf("MaxIdx: %d, OrgIdx: %d\n", maxIdx, PSTATE.orgParticleIdx);

    for(int k = 0; k < K; k++)
        normalizeArray<floating_t>(PSTATE.beta[k], VOCAB_SIZE);

    floating_t topicProducts[VOCAB_SIZE];
    for(int j = 0; j < VOCAB_SIZE; j++) {
        topicProducts[j] = 1;
        for(int k = 0; k < K; k++)
            topicProducts[j] *= PSTATE.beta[k][j];
    }
    
    int bestWordsTopics[K][NUM_WORDS_PER_TOPIC];
    for(int k = 0; k < K; k++) {

        tuple_t tuples[VOCAB_SIZE];
        for(int j = 0; j < VOCAB_SIZE; j++) {
            tuples[j].prob = PSTATE.beta[k][j] * log(PSTATE.beta[k][j] / (pow(topicProducts[j], (1.0 / K))));
            // tuples[j].prob = PSTATE.beta[k][j];
            tuples[j].idx = j;
        }
        int n = sizeof(tuples) / sizeof(tuples[0]); 
        std::sort(tuples, tuples + n, &compareTuple);
        // printf("Topic[%d]: [", k);
        // printf("[");
        int x = 0;
        for (int j = VOCAB_SIZE-1; j >= VOCAB_SIZE - NUM_WORDS_PER_TOPIC; j--) {
            bestWordsTopics[k][x] = tuples[j].idx;
            x++;
            // printf("%d", tuples[j].idx);
        }
    }

    ratioSum += correctRatio(bestWordsTopics);

})


void setup() {
    initGen();
    readCorpus(corpus);
    for (int d = 0; d < D; d++)
        docLength[d] = DOC_LENGTH[d];

    for(int v = 0; v < VOCAB_SIZE; v++)
            eta[v] = 1;
}


int main() {
    setup();


    COPY_DATA_GPU(corpus, int, D * MAX_DOC_LENGTH);
    COPY_DATA_GPU(docLength, int, D);
    COPY_DATA_GPU(eta, floating_t, VOCAB_SIZE);

    SMCSTART(progState_t); // allokera array

    ADD_BBLOCK(init, progState_t); // create func pointer and add to array
    ADD_BBLOCK(newDocument, progState_t);
    ADD_BBLOCK(newWord, progState_t);

    SMCEND(progState_t);
    floating_t meanRatio = ratioSum / 1000.0;
    printf("MeanRatio: %.5f\n", meanRatio);
}
