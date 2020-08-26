Some flaws/TODO:s to be aware of:

- The SAMPLE macro is not available from host functions when the program is compiled for GPU. This is due to the fact that distributions need a curand state then, which are not present outside inference. This could possibly be fixed, but I see no way to do this without much duplicated code. Use C++ distributions in these cases: http://www.cplusplus.com/reference/random/


- Distributions still without score functions:
    - diagCovNormal
    - Dirichlet
    - multinomial
    - multiVariateNormal

- The stack size of the CUDA threads can be insufficient for complex models with complex trees, e.g. BAMM with large trees. Better hardware with more memory might be useful, while sometimes the CPU performs better because of this. GPU memory can be tweaked in the configureMemSizeGPU function inside smc.cu, which is called before SMC starts. CUDA supports recursions but too complex recursions can increase compile times, result in poor memory behavior and even crashes. The nvlink warnings when compiling complex recursive models are harmless, but indicates that this problem can arise. 


- Using commas within macros such as BBLOCK, when not enclosed by parentheses as done in function calls or other macros can lead to problems. For example, when trying to initialize arrays like this “int arr[3] = {1, 2, 3};”. This gives errors when BBLOCK is expanded. Some of these cases can be fixed by using the CMA or COMMA macro instead of “,”. But fails in for example BBLOCK:s when there are multiple macros expansions of the BBLOCK. 

- Program States in particles are laid out in memory as an array of structs, as the program state struct is a custom struct for each model. This however, is sub-optimal for GPU:s, and the optimal way would be a struct of arrays. This is the case for other particle members such as the weights and program counters. If the program states could be set up in this way as well, GPU performance could be significantly improved. 

- No automated tests of the inference exists. It has only been manually verified by comparing model results to analytical results or inference results from other systems. 

- Fix build for Mac OS. Currently, the Makefile assumes g++. Clang should be supported for Mac and used by default as it is the default host compiler used by CUDA on Mac. Instructions for how to use Open MP on Mac should be added to the README. 

- Fix build for Windows. Currently the only verified solution for windows is thorugh Cygwin with gcc and adding a static library to the Makefile. This worked for Open MP as well. But Nvidia recommends Visual Studio for CUDA development. However, build settings should be set up to handle the different main files as well as different compile options (vanilla CPU, Open MP, CUDA). Perhaps a Makefile solution could work here as well, but it should use cl.exe as compiler then (which is the default host compiler used by NVCC on Windows).
