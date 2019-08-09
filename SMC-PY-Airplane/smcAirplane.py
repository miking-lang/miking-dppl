import timeit

import math

import numpy as np
from scipy import stats

import utils

OBSERVATION_STD = 1
TRANSITION_STD = 1
VELOCITY = 5
MAP_SIZE = 200
ALTITUDE = 10
TIME_STEPS = 100
N = 200

# mapApprox = np.empty(MAP_SIZE)
mapApprox = np.array([ALTITUDE - utils.mapLookup(i) for i in range(MAP_SIZE)])

planeX = []
planeObs = []


def mapLookupApprox(x):
    f = math.floor(x)
    c = math.ceil(x)
    if(c >= MAP_SIZE or f < 0):
        return 99999
    return (mapApprox[f] + mapApprox[c]) / 2


mapLookupApproxVec = np.vectorize(mapLookupApprox)


def smc():
    for t in range(TIME_STEPS):
        planeX.append(5 if t == 0 else np.random.normal(loc=planeX[t - 1] + VELOCITY, scale=TRANSITION_STD))
        planeObs.append(np.random.normal(mapLookupApprox(planeX[t]), OBSERVATION_STD))

    printSteps = True

    # init
    x = np.random.uniform(0, MAP_SIZE, N)
    w = [1 / N] * N
    utils.drawMap(planeX[0], x, w, "init")

    # weigh
    w = np.array(stats.norm.logpdf(planeObs[0], loc=mapLookupApproxVec(x), scale=OBSERVATION_STD))
    # w -= np.amax(w)
    w = np.exp(w)
    w /= np.sum(w)
    if (printSteps):
        utils.drawMap(planeX[0], x, w, "weigh", observedY=planeObs[0])

    for t in range(TIME_STEPS):
        # if(t > 29):
        if (not printSteps):
            utils.drawMap(planeX[t], x, w, "resample, propagate, weigh", observedY=planeObs[t])

        # resample and reset weights
        x = np.random.choice(x, size=N, p=w)
        w.fill(1 / N)
        if(printSteps):
            utils.drawMap(planeX[t], x, w, "resample", observedY=planeObs[t])

        # propagate
        x = np.random.normal(loc=x + VELOCITY, scale=TRANSITION_STD)
        if (printSteps):
            utils.drawMap(planeX[t], x, w, "propagate", observedY=planeObs[t])

        # weigh
        if(t < TIME_STEPS-1):
            w = np.array(stats.norm.logpdf(planeObs[t+1], loc=mapLookupApproxVec(x), scale=OBSERVATION_STD))
            # w -= np.amax(w)
            w = np.exp(w)
            w /= np.sum(w)

        '''
        numClose = 0
        for i in range(N):
            if(np.abs(x[i] - planeX[t]) < 15):
                numClose += 1
        print("NumClose:", numClose, ", t:", t, ", minX:", min(x), ", maxX:", max(x))
        '''

        if (printSteps):
            utils.drawMap(planeX[t+1], x, w, "weigh", observedY=planeObs[t])


if (__name__ == "__main__"):
    # smc()
    numTrials = 1
    print(timeit.timeit(smc, number=numTrials) / numTrials, "seconds (avg)")
