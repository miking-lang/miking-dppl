#include <algorithm>

#include "inference/smc/smc_impl.cuh"
#include "utils/distributions.cuh"
#include "utils/misc.cuh"
#include "lda.cuh"


/*
WORK IN PROGRESS, NOT CURRENTLY WORKING
*/

// THIS MODEL MIGHT HAVE AN ERROR, CHECK FOR SEGFAULT WITH LARGER NUMBER OF PARTICLES

BBLOCK_DATA_2D(corpus, int, D, MAX_DOC_LENGTH); // DOCUMENTS
BBLOCK_DATA(docLength, int, D); // length of each document
BBLOCK_DATA(alpha, floating_t, K);
BBLOCK_DATA(eta, floating_t, VOCAB_SIZE);

const int NUM_BBLOCKS = 1;
const int GIBBS_ITERATIONS = 100;


//HOST DEV void estimateBeta(floating_t beta[][VOCAB_SIZE], int cTilde[][VOCAB_SIZE], floating_t* etaP) {
HOST DEV void estimateBeta(floating_t beta[][VOCAB_SIZE], int** cTilde, floating_t* etaP) {

    floating_t etaSum = sumArray<floating_t>(etaP, VOCAB_SIZE);

    if(etaSum != VOCAB_SIZE)
        printf("ERROR 1\n");

    for(int k = 0; k < K; k++) {
        floating_t cTildeSum = sumArray<int>(cTilde[k], VOCAB_SIZE);
        for(int w = 0; w < VOCAB_SIZE; w++) {
            // if (cTilde[k][w] < 0)
                // printf("cTilde negative!\n");
            beta[k][w] = (etaP[w] + cTilde[k][w]) / (etaSum + cTildeSum);
        }
    }
}


//HOST DEV void estimateTheta(floating_t theta[][K], int c[][K], floating_t* alphaP) {
HOST DEV void estimateTheta(floating_t theta[][K], int** c, floating_t* alphaP) {
    
    floating_t alphaSum = sumArray<floating_t>(alphaP, K);

    if(alphaSum != K)
        printf("ERROR 2\n");

    for(int d = 0; d < D; d++) {
        floating_t cSum = sumArray<int>(c[d], K);
        for(int k = 0; k < K; k++)
            theta[d][k] = (alphaP[k] + c[d][k]) / (alphaSum + cSum);
    }
}


BBLOCK(gibbs, progState_t, {

    printf("Inside gibbs\n");
    // int z[D][MAX_DOC_LENGTH];
    int** z = new int*[D];
    for(int j = 0; j < D; j++)
        z[j] = new int[MAX_DOC_LENGTH];

    // int c[D][K] = {0};
    int** c = new int*[D];
    for(int j = 0; j < D; j++) {
        c[j] = new int[K];
        for (int k = 0; k < K; k++)
            c[j][k] = 0;
    }

    // int cTilde[K][VOCAB_SIZE] = {0};

    int** cTilde = new int*[K];
    for(int k = 0; k < K; k++) {
        cTilde[k] = new int[VOCAB_SIZE];
        for (int v = 0; v < VOCAB_SIZE; v++)
            cTilde[k][v] = 0;
    }
    
    floating_t* alphaP = DATA_POINTER(alpha);
    floating_t* etaP = DATA_POINTER(eta);
    floating_t alphaSum = sumArray<floating_t>(alphaP, K);
    floating_t etaSum = sumArray<floating_t>(etaP, VOCAB_SIZE);


    if(alphaSum != K)
        printf("ERROR 3\n");

    if(etaSum != VOCAB_SIZE)
        printf("ERROR 4\n");

    // Initialize z, c, cTilde
    for(int d = 0; d < D; d++) {
        int* doc = DATA_POINTER(corpus)[d];
        for(int n = 0; n < MAX_DOC_LENGTH; n++) {
            int w = doc[n];
            if (w == -1) continue;
            int k = BBLOCK_CALL(sampleCategoricalStandard, K);
            z[d][n] = k;
            c[d][k] += 1;
            cTilde[k][w] += 1;
        }
    }
    

    // Gibbs
    int k;
    for(int it = 0; it < GIBBS_ITERATIONS; it++) {
        if(it % 10 == 0)
            printf("Gibbs iteration: %d\n", it);

        for(int d = 0; d < D; d++) {
            int* doc = DATA_POINTER(corpus)[d];

            for(int n = 0; n < MAX_DOC_LENGTH; n++) {
                int w = doc[n];
                if (w == -1) continue;

                k = z[d][n];
                // if(c[d][k] <= 0)
                //     printf("Error c (%d, %d)\n", d , k);
                c[d][k] -= 1;
                // if (cTilde[k][w] <= 0)
                //     printf("Error cTilde (%d, %d)\n", k , w);
                cTilde[k][w] -= 1;

                floating_t cSum = sumArray<int>(c[d], K);

                floating_t distr[K];
                for (int kDist = 0; kDist < K; kDist++) {
                    floating_t cTildeSum = sumArray<int>(cTilde[kDist], VOCAB_SIZE);
                    floating_t factor1 = (alphaP[kDist] + c[d][kDist]) / (alphaSum + cSum);
                    floating_t factor2 = (etaP[kDist] + cTilde[kDist][w]) / (etaSum + cTildeSum);
                    distr[kDist] = factor1 * factor2;
                }

                normalizeArray<floating_t>(distr, K);
                k = BBLOCK_CALL(sampleCategorical, distr, K);
                if(k < 0 || k >= K)
                    printf("ERROR, k=%d\n", k);
                z[d][n] = k;
                c[d][k] += 1;
                cTilde[k][w] += 1;

            }

        }

    }

    estimateBeta(PSTATE.beta, cTilde, etaP);
    estimateTheta(PSTATE.theta, c, alphaP);

    for(int j = 0; j < D; j++)
        delete [] z[j];
    delete [] z;

    for(int j = 0; j < D; j++)
        delete [] c[j];

    delete [] c;

    for(int k = 0; k < K; k++)
        delete [] cTilde[k];
    delete[] cTilde;
    
    PC++;
    RESAMPLE = false;
    
})

struct tuple_t {
    floating_t prob;
    int idx;
};

bool compareTuple(tuple_t const& t1, tuple_t const& t2) { 
    return (t1.prob < t2.prob); 
} 

STATUSFUNC({

    int i = 0;
    // int bestWordsTopics[K][VOCAB_SIZE];

    for(int k = 0; k < K; k++)
        normalizeArray<floating_t>(PSTATE.beta[k], VOCAB_SIZE);

    floating_t topicProducts[VOCAB_SIZE];
    for(int j = 0; j < VOCAB_SIZE; j++) {
        topicProducts[j] = 1;
        for(int k = 0; k < K; k++)
            topicProducts[j] *= PSTATE.beta[k][j];
    }

    for(int k = 0; k < K; k++) {
        tuple_t tuples[VOCAB_SIZE];
        for(int j = 0; j < VOCAB_SIZE; j++) {
            
            tuples[j].prob = PSTATE.beta[k][j] * log(PSTATE.beta[k][j] / (pow(topicProducts[j], (1.0 / K))));
            // tuples[j].prob = PSTATE.beta[k][j];
            tuples[j].idx = j;
        }
        int n = sizeof(tuples) / sizeof(tuples[0]); 
        std::sort(tuples, tuples + n, &compareTuple);

        printf("[");
        // int x = 0;
        for (int j = VOCAB_SIZE-1; j >= VOCAB_SIZE-5; j--) {
            // bestWordsTopics[k][x] = tuples[j].idx;
            // x++;
            printf("%d", tuples[j].idx);
            if(j > VOCAB_SIZE-5)
                printf(", ");
        }
        printf("],\n");
    }

})

void setup() {
    initGen();
    readCorpus(corpus);
    for (int d = 0; d < D; d++)
        docLength[d] = DOC_LENGTH[d];

    for (int k = 0; k < K; k++)
        alpha[k] = 1;

    for(int v = 0; v < VOCAB_SIZE; v++)
        eta[v] = 1;
}

int main() {
    setup();

    COPY_DATA_GPU(corpus, int, D * MAX_DOC_LENGTH);
    COPY_DATA_GPU(docLength, int, D);
    COPY_DATA_GPU(eta, floating_t, VOCAB_SIZE);
    COPY_DATA_GPU(alpha, floating_t, K);

    SMCSTART(progState_t);

    INIT_BBLOCK(gibbs, progState_t);

    SMCEND(progState_t);
}
