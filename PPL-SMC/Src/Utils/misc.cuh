#ifndef MISC_INCLUDED
#define MISC_INCLUDED

#include <iostream>
#include <vector>
#include <cstring>

#include "../Smc/smc.cuh"

using namespace std;


void printList(vector<bool> l, string title="") {
    if(title.length() > 0)
        cout << title << ": ";
    cout << "[ ";
    for(bool b : l) {
        cout << b << " ";
    }
    cout << "]\n" << endl;
}

#endif