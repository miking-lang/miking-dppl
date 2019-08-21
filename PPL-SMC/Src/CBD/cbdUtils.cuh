#ifndef CBDUTILS_INCLUDED
#define CBDUTILS_INCLUDED


int countLeaves(const int* leftIdx, const int* rightIdx, int size) {
    int numLeaves = 0;
    for(int i = 0; i < size; i++) {
        if(leftIdx[i] == -1 && rightIdx[i] == -1)
            numLeaves++;
    }
    return numLeaves;
}

// make tail-recursive?
double lnFactorial(int n) {
    if(n == 1) {
        return 0.0;
    } else {
        return log(n) + lnFactorial(n-1);
    }
}


#endif


