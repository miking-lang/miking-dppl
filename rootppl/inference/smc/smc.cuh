#ifndef SMC_INCLUDED
#define SMC_INCLUDED

#include <cstddef>
#include "macros/macros.cuh"
#ifdef GPU
#include "utils/cuda_error_utils.cu"
#include <curand_kernel.h>
#endif

using namespace std;

// Kernel launch settings
#define NUM_THREADS_PER_BLOCK 128
#define NUM_THREADS_PER_BLOCK_NESTED 32


// Particle structure, allocated at start of inference
template <typename T>
struct particles_t {

    T* progStates;
    int* pcs;
    floating_t* weights;
};

// BBLOCK function
template <typename T>
using pplFunc_t = void (*)(
    #ifdef GPU 
    curandState*, 
    #endif
    particles_t<T>*, 
    int, 
    void*);


// Callback function, like bblock function but without index. As all particles are usually used here
template <typename T>
using callbackFunc_t = void (*)(particles_t<T>*, int, void*);


#endif






/* 
Measured with linux tool perf: perf stat -r numTimes prog progArgs
~0.9 sec avg over 6 runs with only some separate num_particles in nested and not
~0.915 sec avg over 6 runs with separate num_particles in nested and not


Primate tree execution time: avg +- std over 3 runs, for N particles
No stack precompute

CPU:
N=100:       0,01336  +- 0,00224 seconds time elapsed  ( +- 16,76% )
N=1000:      0,084143 +- 0,000972 seconds time elapsed ( +-  1,15% )
N=10 000:    0,8350   +- 0,0253 seconds time elapsed   ( +-  3,02% )
N=100 000:   8,0278   +- 0,0282 seconds time elapsed   ( +-  0,35% )
N=300 000:   24,1590  +- 0,0736 seconds time elapsed   ( +-  0,30% )
N=1 000 000: 80,6065  +- 0,0148 seconds time elapsed   ( +-  0,02% )

GPU:
N=100:       0,32938  +- 0,00919 seconds time elapsed  ( +-  2,79% )
N=1000:      0,3470   +- 0,0122 seconds time elapsed   ( +-  3,53% )
N=10 000:    0,38759  +- 0,00768 seconds time elapsed  ( +-  1,98% ) 
N=100 000:   0,62697  +- 0,00486 seconds time elapsed  ( +-  0,78% )
N=300 000:   1,4377   +- 0,0193 seconds time elapsed   ( +-  1,34% )
N=1 000 000: 5,6439   +- 0,0202 seconds time elapsed   ( +-  0,36% )


(Seems to take ~0.2sec before even initialization (initRandStates) can begin)
N=100K  => 1.7%  initRandStates, 78.1% execFuncs
N=300k  => 11.5% initRandStates, 76.9% execFuncs
N=1M    => 39.6% initRandStates, 55.1% execFuncs

N=100K: ~19 speedup CPU -> GPU, without init costs   (~13 with costs)
N=1M: ~30 speedup without init costs                 (~15 with costs)






NUM_PARTICLES: [10000, 100000] 
phylogenetics/crbd/crbd.cu

With max weight scaling
CPU: [0.946182988, 9.417857985]
GPU: [0.5766381133333334, 1.087258693] 

GPU Speedup: [[1.64086099 8.66202133]]

With max weight scaling removed on GPU
phylogenetics/crbd/crbd.cu
CPU: [0.9422143126666667, 9.426604939333332]
GPU: [0.44755016266666664, 0.94003224] 

GPU Speedup: [[ 2.10527085 10.02795919]]

*/

/*
25 runs
GPU no max:
Mean: 0.9295795017200001 , Std:  0.030816434479334492

GPU with max:
Mean: 1.0998520308400002 , Std:  0.03606856282216499
*/


/* 
*********** 25 runs with compile constant NUM_PARTICLES

NUM_PARTICLES: [10000, 50000] 

airplane/airplane.cu
CPU: [0.11122169364000001, 0.532083091]
GPU: [0.25338271252, 0.26756725428] 

phylogenetics/crbd/condbd.cu
CPU: [1.2366959077600002, 7.54062331904]
GPU: [0.7804601606, 1.6089079755600002] 

phylogenetics/crbd/crbd.cu
CPU: [0.92629217732, 4.609530027080001]
GPU: [0.5813464418400001, 0.81274080452] 

phylogenetics/bamm/bamm.cu
CPU: [0.77797657724, 3.89442340208]
GPU: [0.44647128028, 1.0050353062000001] 

GPU Speedup:
 [[0.43894744 1.9885957 ]
 [1.58457276 4.68679591]
 [1.59335658 5.67158681]
 [1.74250083 3.87491203]]


*********** 25 runs with program input NUM_PARTICLES

NUM_PARTICLES: [10000, 50000] 

airplane/airplane.cu
CPU: [0.10831851243999999, 0.52815956272]
GPU: [0.24631308676, 0.25710640492000003] 

phylogenetics/crbd/condbd.cu
CPU: [1.24617575236, 7.56731632812]
GPU: [0.73282718444, 1.56183501552] 

phylogenetics/crbd/crbd.cu
CPU: [0.9353200202399999, 4.6345830359599995]
GPU: [0.54944511172, 0.78363611832] 

phylogenetics/bamm/bamm.cu
CPU: [0.77110793224, 3.89562813756]
GPU: [0.43441566996, 0.9865827383200001] 

GPU Speedup:
 [[0.43975947 2.05424506]
 [1.70050426 4.8451445 ]
 [1.70229928 5.91420294]
 [1.77504631 3.94860764]]

*/