# PPL-SMC

An SMC inference framework. SMC related code lies in Src/SMC, problem specific programs lies in the different problem specific folder. All these folders contain one or more .cu files which contain a main function. Multiple files with main, means they are different versions and only one should be compiled. 
<br>
<br>
**How to compile**
<br>
All files containing a main function also contains a comment at the top of the file, with a command for compiling the program. These commands uses nvcc (Nvidias CUDA C compiler), make sure to change "-arch=sm_75" to be compatible with your GPU architecture (preferably the newest possible). Furthermore, the program can be compiled using g++ instead, if one does not want to use a GPU, and settles for single core CPU. <br>
Note: To compile .cu files with g++, use "-x c++". 
<br>
<br>
**Some notes on the structure**
<br>
The problem specific folders contain source code that is entirely detached from any CUDA related code. This is done by using macros (currently defined in smc.cuh). Basic blocks (BBLOCKs), functions that are called from the SMC engine, are declared with the BBLOCK macro. They will get the index according to the order they initialized in main, using the macro INITBBLOCK. All particles in SMC will correspond to a program execution (where the instructions correspond to the BBLOCKS). Each BBLOCK alters the particles program counter (macro PC), weight (macro WEIGHT) and whether it should resample (macro RESAMPLE). 
<br>
<br>
The SMC inference engine will run the BBLOCK pointed to by the PC for all particles, and then if specified, resample using the weights. This repeats until the PC points to a NULL function (the last function index + 1), then terminates. Currently, the SMC engine assumes that all particles resample and terminate at the same time. The GPU version has parallel resampling and executes the BBLOCKs in parallel. The CPU version does everything sequentially on one CPU core. 
<br>
<br>
Many options such as kernel launch options and number of particles are compile time constants specified in smc.cuh. 
