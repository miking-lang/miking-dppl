#ifndef LDA_INCLUDED
#define LDA_INCLUDED

/*
 * File lda.cuh contains contants, type definitions and helper functions for LDA models. 
 */

#include "inference/smc/smc.cuh"
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>

using namespace std;

/** 
 * Uses alot of corpus specific information 
 */

// const int D = 679; // Number of documents
// const int VOCAB_SIZE = 856; 
// const int K = 10; // Number of topics


const int D = 10; // Number of documents 
const int K = 3; // Number of topics
const int VOCAB_SIZE = 3 * K;


// const floating_t ETA[VOCAB_SIZE] = {1};
const int WORDS_PER_RESAMPLE = 10;

// Gibbs
/*
struct progState_t {
    floating_t beta[K][VOCAB_SIZE]; // For each topic: a categorical distribution over words 
    floating_t theta[D][K]; // For each document: a categorical distrbution over topics
};
*/


// SMC
struct progState_t {
    floating_t alpha[K]; // Dirichlet parameter in theta: theta_d ~ Dir(alpha)
    floating_t beta[K][VOCAB_SIZE]; // For each topic: a categorical distribution over words 
    floating_t theta[D][K]; // For each document: a categorical distrbution over topics
    int docIdx;
    int wordIdx;
    int orgParticleIdx;
};

const int NUM_PARAMS = K + K * VOCAB_SIZE + D * K;
const int NUM_PARAMS_INIT = K + K * VOCAB_SIZE;


// const int MAX_DOC_LENGTH = 182;
// const int DOC_LENGTH[D] = {79, 54, 93, 85, 28, 94, 153, 102, 80, 163, 80, 96, 97, 165, 95, 68, 111, 109, 120, 65, 58, 109, 108, 78, 95, 93, 77, 99, 63, 155, 68, 98, 96, 95, 67, 81, 96, 118, 77, 131, 53, 71, 119, 124, 93, 95, 122, 96, 78, 80, 126, 114, 94, 67, 95, 75, 129, 94, 84, 87, 60, 71, 86, 54, 124, 85, 94, 107, 63, 96, 125, 71, 61, 92, 101, 111, 61, 73, 62, 108, 77, 71, 142, 90, 99, 65, 98, 56, 99, 79, 79, 65, 182, 83, 96, 64, 69, 94, 139, 83, 98, 58, 123, 156, 127, 51, 51, 77, 46, 81, 80, 122, 88, 91, 85, 38, 71, 64, 69, 109, 146, 78, 132, 71, 75, 147, 97, 63, 107, 98, 64, 78, 114, 61, 101, 84, 91, 81, 56, 78, 89, 69, 95, 80, 126, 65, 112, 79, 93, 78, 52, 91, 76, 70, 62, 91, 75, 73, 73, 67, 137, 92, 127, 74, 133, 112, 79, 79, 119, 116, 59, 76, 44, 63, 103, 72, 51, 108, 72, 73, 54, 73, 118, 76, 67, 122, 64, 76, 83, 110, 84, 74, 96, 86, 88, 115, 82, 148, 125, 116, 112, 72, 62, 71, 91, 81, 54, 92, 93, 114, 65, 55, 57, 90, 71, 72, 68, 70, 57, 85, 70, 116, 63, 72, 70, 135, 112, 59, 83, 102, 73, 78, 100, 69, 85, 79, 92, 106, 84, 79, 54, 110, 116, 155, 141, 91, 71, 62, 92, 78, 79, 131, 132, 119, 145, 136, 67, 69, 106, 66, 95, 69, 143, 67, 93, 49, 128, 106, 60, 98, 69, 70, 88, 149, 34, 69, 139, 108, 94, 58, 60, 68, 126, 95, 78, 59, 80, 103, 61, 109, 72, 85, 93, 110, 79, 107, 94, 153, 68, 82, 116, 143, 97, 102, 138, 71, 103, 97, 58, 72, 106, 74, 57, 94, 108, 64, 81, 88, 126, 87, 104, 84, 117, 51, 95, 83, 144, 94, 118, 114, 88, 98, 113, 62, 132, 152, 111, 107, 118, 81, 87, 80, 122, 85, 70, 73, 116, 83, 73, 141, 133, 70, 104, 86, 97, 84, 128, 79, 53, 94, 85, 91, 160, 102, 99, 85, 83, 66, 114, 86, 139, 51, 89, 138, 99, 115, 61, 107, 120, 88, 92, 63, 58, 149, 66, 129, 69, 73, 93, 75, 76, 114, 66, 125, 114, 89, 71, 89, 83, 74, 59, 105, 72, 112, 56, 47, 53, 82, 99, 127, 69, 81, 77, 88, 77, 90, 59, 77, 128, 90, 176, 109, 52, 65, 111, 48, 35, 132, 93, 67, 120, 88, 53, 88, 125, 94, 77, 81, 76, 57, 110, 78, 120, 134, 66, 77, 111, 106, 152, 88, 64, 117, 88, 54, 52, 120, 107, 43, 131, 111, 74, 58, 54, 101, 84, 118, 69, 74, 105, 76, 82, 144, 115, 54, 67, 98, 99, 78, 96, 64, 62, 39, 88, 94, 85, 69, 75, 88, 139, 101, 73, 63, 80, 105, 105, 85, 75, 102, 52, 76, 72, 60, 151, 98, 60, 108, 100, 63, 55, 90, 108, 107, 91, 81, 91, 91, 85, 87, 66, 97, 79, 89, 56, 90, 71, 81, 71, 58, 151, 110, 103, 84, 95, 140, 58, 91, 86, 68, 70, 115, 110, 103, 131, 75, 44, 59, 53, 109, 65, 100, 66, 82, 78, 76, 82, 151, 83, 93, 84, 54, 143, 55, 109, 81, 107, 50, 106, 115, 92, 94, 36, 120, 92, 99, 87, 74, 76, 108, 69, 82, 103, 75, 60, 86, 56, 96, 82, 51, 133, 31, 68, 83, 108, 60, 126, 44, 85, 131, 61, 50, 86, 133, 128, 82, 86, 83, 71, 75, 94, 78, 84, 77, 92, 106, 107, 103, 108, 86, 98, 74, 84, 96, 112, 61, 66, 66, 67, 125, 87, 74, 59, 86, 161, 164, 106, 119, 96, 83, 94, 105, 63, 57, 119, 64, 64, 107, 86, 98, 114, 136, 73, 47, 78, 65, 99, 102, 117, 52, 48, 121, 83, 129, 50, 102, 47, 70, 66, 68, 38, 91, 73, 85, 122, 73, 55, 41, 81, 124, 86};
const int N = 100;
const int MAX_DOC_LENGTH = N;
const int DOC_LENGTH[D] = {N, N, N, N, N, N, N, N, N, N};

// int CORPUS[D][MAX_DOC_LENGTH]; // COULD BE OPTIMIZED BY USING SHORT DATA TYPE

void readCorpus(int corpus[][MAX_DOC_LENGTH]) {
    printf("Number of parameters: %d\n", NUM_PARAMS);
    printf("Number of parameters sampled initially (a+b): %d\n", NUM_PARAMS_INIT);
    printf("Corpus size: %liKB\n", sizeof(int) * D * MAX_DOC_LENGTH / 1000);
    // ifstream infile("Src/LDA/processedDocumentsIdx.txt");
    ifstream infile("Src/LDA/simpleDataIdx.txt");

    int docIdx = 0;
    string line;
    string comma = ",";
    string currChar;
    while (getline(infile, line)) {
        int currWordIdx = 0;
        string number;
        for (int d = 0; d < line.length(); d++) {
            currChar = line[d];
            if (comma.compare(currChar) == 0) { // equal
                int parsedNumber = stoi(number);
                corpus[docIdx][currWordIdx] = parsedNumber;
                number = "";
                currWordIdx++;
            } else {
                number.append(currChar);
            }
        }
        docIdx++;
    }

    infile.close();
}

#endif
