#include <time.h>
#include <stdio.h>

struct timespec start, finish;

void startTimer() {
	#if !(defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__))
	clock_gettime(CLOCK_MONOTONIC, &start);
	#endif
}

double getTimeElapsed() {
	#if !(defined(WIN32) || defined(_WIN32) || defined(__WIN32) && !defined(__CYGWIN__))
	clock_gettime(CLOCK_MONOTONIC, &finish);

	double elapsed = (finish.tv_sec - start.tv_sec);
	elapsed += (finish.tv_nsec - start.tv_nsec) / 1000000000.0;

	return elapsed;
	#endif
	printf("Timer not working on Windows!\n");
	return -1;
}