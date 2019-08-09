#include <time.h>

struct timespec start, finish;

void startTimer() {
	clock_gettime(CLOCK_MONOTONIC, &start);
}

double getTimeElapsed() {
	clock_gettime(CLOCK_MONOTONIC, &finish);

	double elapsed = (finish.tv_sec - start.tv_sec);
	elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;

	return elapsed;
}