import numpy as np
import matplotlib
import matplotlib.pyplot as plt


# def drawMap(plane, particles, titleAppend=None, observedY=None):
# from airplaneClean import ALTITUDE
ALTITUDE = 10
matplotlib.use('tkagg')


# Returns height corresponding to x coordinate
def mapLookup(x):
    if (35 < x < 80):
        return 0
    c1 = 0.5 if (x < 20 or x > 60) else 1.5
    c2 = 1 if (x < 45 or x > 90) else 2
    return _weirdMapFunc(x, c1, c2)


def _weirdMapFunc(x, c1, c2):
    return np.sin(x / 4) / c1 + x / (7 * c2) - (x ** 2) / 400 + (x ** 3) / 100000


vec_map = np.vectorize(mapLookup)


def drawMap(planeX, x, w, titleAppend=None, observedY=None):
    numParticles = len(x)
    fig = plt.figure()
    subPlot = fig.add_subplot(111)
    subPlot.axhline(y=mapLookup(planeX), color="red", linestyle="--")
    if (observedY is not None):
        subPlot.axhline(y=ALTITUDE - observedY, color="grey", linestyle="--")
    xMap = []
    yMap = []
    for i in np.linspace(0, 200, 1001):
        xMap.append(i)
        yMap.append(mapLookup(i))

    subPlot.plot(planeX, 10, marker='*', markersize=10, color='red')
    subPlot.plot(xMap, yMap)

    for i in range(numParticles):
        subPlot.plot(x[i], 12, marker='o', markersize=min(w[i] * numParticles * 5, 50), color='blue')

    # maxIndex = np.argmax(w)
    # subPlot.plot(x[maxIndex], 12, marker='o', markersize=min(w[maxIndex] * numParticles * 5, 50), color='green')

    plt.xlabel("x")
    plt.ylabel("y")
    if titleAppend is not None:
        plt.title("Map - " + titleAppend)
    else:
        plt.title("Map")

    mng = plt.get_current_fig_manager()
    # mng.window.state('zoomed')
    # mng.window.showMaximized()
    mng.full_screen_toggle()
    # mng.frame.Maximize(True)
    plt.show()
    '''
    plt.show(block=False)
    plt.pause(1)
    plt.close()
    '''
