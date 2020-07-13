# miking-ppl

# PPLCore

To be written...


# RootPPL

RootPPL can be seen as an intermediate language for representing probabilistic models and comes with a framework that performs inference on the GPU in these models. These models are currently hardcoded, see examples in the folder rootppl/models. The idea is that high level Miking probabilistic programming languages should be compiled to this intermediate language. 

## Getting Started
The instructions below are tested on Ubuntu 18.04 but should work for other Linux distributions, Windows, and Mac. 

### Install
Before building RootPPL programs, a C++/CUDA compiler is required. RootPPL works on CPU and Nvidia GPU:s. For the CPU version, a C++ compiler should suffice, e.g. g++ (gcc is also required to install CUDA).  In order to build for GPU, [CUDA](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/ "CUDA Installation Guide") must be installed. The distribution-specific installation is recommended if possible (as opposed to the runfile), to avoid problems with the video drivers. 

### Build
To build the program, only the model needs to be compiled. For example, to compile the Airplane model to GPU, being in the rootppl directory:
```
nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/airplane/airplane.cu -o model -std=c++14 -O3
```
`-arch=sm_75` defines that it is being compiled for a GPU of minimum compute capability 7.5. This should be set to match the GPU being used. 

The program can also be built for CPU (currently only sequential single-core) using `g++` like so:

```
g++ -x c++ -I . models/airplane/airplane.cu -o model -std=c++14 -O3
```

### Building a simple model
Models are divided into fragments to enable pausing the execution within models. 
These fragments are functions referred to as basic blocks (```BBLOCK```). 
To control the program execution flow, a program counter (```PC```) can be modified. 
If it remains unchanged in when the basic block returns, resampling will be done usual, and then
the same block will be executed again. The program counter corresponds to the index of the basic block 
to be executed. So, incrementing it means that the next block will be executed after resampling. The 
```BBLOCK``` below does this. However, if no following blocks are defined, 
the inference will terminate as the model program has been executed. 

```
BBLOCK(useless, {
    PC++;
})
```

Any interesting model will contain random choices, i.e. sampling from distributions. 
Below is a coin flip example which flips a biased coin.

```
BBLOCK(coinFlip, {
    int coinSample = SAMPLE(bernoulli, 0.6);
    PC++;
})
```

In the coin flip example above, there is one problem, however. The sample is only stored in a local
variable and is never used. To store data that remains when the next block is executed, the program
state (```PSTATE```) should be used. Before defining the blocks, the model must be
initialized with the macro ```INIT_MODEL``` that takes two arguments. First the type of the program
state (this could be any type, e.g. ```int``` or a structure), then the number of basic blocks in
the program. So changing the above example:

```
INIT_MODEL(int, 1)
  
BBLOCK(coinFlip, {
    PSTATE = SAMPLE(bernoulli, 0.6);
    PC++;
})
```

Now to run this program, only one thing remains to be defined, the main function.
This is done with the ```MAIN``` macro, taking a code block as argument.
The code below shows what we need in order to run the program and perform SMC. 

```
MAIN({
    ADD_BBLOCK(coinFlip);
 
    SMC(NULL);
})
```

First, the block must be added to the array of blocks to be executed. The order in which
blocks are added with ```ADD_BLOCK```, defines their order in the array and thus
defines the order of execution together with the program counter. Secondly, the
```SMC``` macro (its parameter is explained below) starts the inference. 

Now the model can be compiled and executed! However, the result of the coin flips is
stored, but never used. To aggregate the results of all the particles' samples, 
a callback function (```CALLBACK```) can be used. The callback will be called after
inference, but before clean up of the particles. This way, the results can be used to
generate desired distributions or values before particles are deleted. 
Within the ```CALLBACK``` macro, the array of program states is accessed 
with ```PSTATES``` and the number of particles is accessed with the 
parameter ```N``` (which is hidden within the macro). 
Here is an example callback, called "sampleMean" that calculates and prints the mean 
of the samples. 

```
CALLBACK(sampleMean, {
    double sum = 0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i];
    double mean = sum / N;
    printf("Sample mean: %f\n", mean);
})
```

For this to be used, the ```SMC``` macro call in main must be 
changed to: ```SMC(sampleMean);```

This example can be found in
[rootppl/models/simple-examples/coin_flip_mean.cu](rootppl/models/simple-examples/coin_flip_mean.cu)
and, being in the rootppl directory, compiled with either:
```
g++ -x c++ -I . models/simple-examples/coin_flip_mean.cu -o model -std=c++14 -O3
```
or: 
```
nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/simple-examples/coin_flip_mean.cu -o model -std=c++14 -O3
```

Then it can be executed with the executable followed by the
number of particles, for example: ```./model 1000```. 

An example output is then:
```
Sample mean: 0.608000
0.000000
```

First we see our callback function's output. Then on the next line, is the logged
normalization constant from the inference. This is simply 0 here, since the model contains
no statements that alter the weights of the particles. 

### Building a more interesting model
TODO
