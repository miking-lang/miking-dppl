#ifndef EXAMPLES_INCLUDED
#define EXAMPLES_INCLUDED

#include <list>
#include <iostream>
#include "../Utils/misc.cuh"

using namespace std;

struct progState_t {
    list<bool> states;
    int res;

    progState_t& operator=(const progState_t& p) { 
        printf("starting eq overloaded func...\n");
        res = p.res;
        printf("copied res!\n");
        //cout << p.states << endl;
        p.states; // works, but cannot do anything with it?
        printf("lol\n");
        states = static_cast<list<bool>>(p.states);
        printf("lol\n");
        printList(p.states);
        printf("copied states!\n");
        return *this;
    }
};

struct hmmState_t {
    list<bool> states;
    list<bool> observations;
};

typedef progState_t stateType;

#endif