#ifndef EXAMPLES_INCLUDED
#define EXAMPLES_INCLUDED

#include <vector>
#include <iostream>
#include "../../Utils/misc.cuh"

using namespace std;

struct progState_t {
    vector<bool> states;
};

struct hmmState_t {
    vector<bool> states;
    vector<bool> observations;
};

#endif