#ifndef UTILS_INCLUDED
#define UTILS_INCLUDED

// #define SINGLE_PRECISION

#ifdef SINGLE_PRECISION
typedef float floating_t;
#else
typedef double floating_t;
#endif

// Parameters
const int MULTINOMIAL = 0, SYSTEMATIC = 1, REJECTION = 2;
const int RESAMPLING_STRATEGY = SYSTEMATIC;

const bool LOG_WEIGHTS = false;
const int OBSERVATION_STD = 1;
const int TRANSITION_STD = 1;
const int VELOCITY = 1;
const int MAP_SIZE = 200;
const int ALTITUDE = 10;
const int TIME_STEPS = 25;
const int NUM_PARTICLES = 1 << 8;
const floating_t STARTING_POINT = 5.0;
const floating_t SQRT_TWO_PI = 2.506628274631000502415765284811045253006986740609938316629;
// const float SQRT_TWO_PI = sqrt(2 * M_PI);
const floating_t SQRT_TWO_PI_OBS_STD = SQRT_TWO_PI * OBSERVATION_STD;
const floating_t TWO_OBS_STD_SQUARED = OBSERVATION_STD * OBSERVATION_STD * 2;
// const floating_t LOG_SQRT_TWO_PI_OBS_STD = log(SQRT_TWO_PI_OBS_STD);

void initMap();
void destMap();
floating_t mapLookupApprox(floating_t x);
void printArray(floating_t* arr, int length);
// void printArray(double* arr, int length);
floating_t normalPDFObs(floating_t x, floating_t mean);
floating_t logNormalPDFObs(floating_t x, floating_t mean);
floating_t maxPDFObs();
void startTimer();
double getTimeElapsed();
floating_t* getMapApproxArr();
void printStatus(floating_t* x, floating_t* w, floating_t* planeX, int t);
floating_t maxValue(floating_t* arr, int length);

#endif