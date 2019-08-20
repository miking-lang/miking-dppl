#ifndef CBDUTILS_INCLUDED
#define CBDUTILS_INCLUDED


int countLeaves(const floating_t* ages, int size) {
    int numLeaves = 0;
    for(int i = 0; i < size; i++) {
        if(ages[i] == 0) // careful!
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


