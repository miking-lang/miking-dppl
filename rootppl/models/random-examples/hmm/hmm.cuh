#ifndef EXAMPLES_INCLUDED
#define EXAMPLES_INCLUDED

// #include <vector>
#include <iostream>
#include "../../../Utils/list.cuh"
#include "../../../Utils/misc.cuh"

using namespace std;

struct progState_t {
    //vector<bool> states;
    list_t<bool> states;

    HOST DEV progState_t() {
        states.initList(5);
    }
};

struct hmmState_t {
    //vector<bool> states;
    //vector<bool> observations;

    list_t<bool> states;
    list_t<bool> observations;

    HOST DEV hmmState_t() {
        states.initList(5);
        observations.initList(5);
    }
};

#endif