# miking-ppl

# PPLCore

To be written...


# RootPPL

RootPPL can be seen as an intermediate language for representing probabilistic models and comes with a framework that performs inference on the GPU in these models. These models are currently hardcoded, see examples in the folder rootppl/models. The idea is that high-level Miking probabilistic programming languages should be compiled to this intermediate language. 

## Getting Started
The instructions below are tested on Ubuntu 18.04 but should work for other Linux distributions, Windows, and Mac. 

### Install
Before building RootPPL programs, a C++/CUDA compiler is required. RootPPL works on CPU and Nvidia GPU:s. For the CPU version, a C++ compiler should suffice, e.g. g++ (gcc is also required to install CUDA).  In 
order to build for GPU, CUDA must be installed. See
CUDA installation guides: [Linux](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/ "CUDA Installation Guide Linux"), 
[Windows](https://docs.nvidia.com/cuda/cuda-installation-guide-microsoft-windows/index.html "CUDA Installation Guide Windows"), 
[Mac](https://docs.nvidia.com/cuda/cuda-installation-guide-mac-os-x/index.html "CUDA Installation Guide Mac") 

### Build
To build the program, clone this repository and change directory to the rootppl folder. Then to compile the model:
```
make MODEL=path/to/model.cu
```
This will compile the model along with the inference framework for CPU. To compile it for GPU, add your GPU:s compute capability to the arch variable.
You can find your GPU:s compute capability in the [Wikipedia table](https://en.wikipedia.org/wiki/CUDA#GPUs_supported).
Here is an example that will compile the airplane example for a GPU with a minimum compute capability of 7.5
(simply remove `arch=75` to compile for CPU):
```
make MODEL=models/airplane/airplane.cu arch=75 -j5
```

The first `make` will compile the entire inference framework and can take 20 seconds or so when building for GPU. (Run make with the `-j numThreads` to use multiple threads when building). 
__Note that if the inference framework is compiled for GPU, and then the model is compiled for CPU, there will be errors. So always perform a `make clean` before switching between CPU and GPU.__

This should generate an executable named `program`. Execute it with either `make run N=num_particles` or `./program num_particles`. For example:
```
make run N=1000
```

An example out of this:
```
./program 1000
Num particles close to target: 96.5%, MinX: 63.1558, MaxX: 84.4023
-119.270143
```
First is the command that is executed from the Makefile, it executes the executable `program` with program argument 1000.
The second row comes from a print statement within the model. Lastly, the log normalization constant approximated by the inference is printed. 

### Building a simple model
Models are divided into fragments to enable pausing the execution within models. 
These fragments are functions referred to as basic blocks (`BBLOCK`). 
To control the program execution flow, a program counter (`PC`) can be modified. 
If it remains unchanged in when the basic block returns, resampling will be done, and then
the same block will be executed again. The program counter corresponds to the index of the basic block 
to be executed. So, incrementing it means that the next block will be executed after resampling. An 
example of this can be seen in the example below. However, if no following blocks are defined, 
the inference will terminate as the model program has been executed. 

Any interesting model will contain random choices, i.e. sampling from distributions. 
Below is a coin flip example which flips a biased coin.

```CUDA
BBLOCK(coinFlip, {
    int coinSample = SAMPLE(bernoulli, 0.6);
    PC++;
})
```

In the coin flip example above, there is one problem, however. The sample is only stored in a local
variable and is never used. To store data that remains when the next block is executed, the program
state (`PSTATE`) should be used. Before defining the blocks, the model must be
initialized with the macro `INIT_MODEL` that takes two arguments. First the type of the program
state (this could be any type, e.g. `int` or a structure), then the number of basic blocks in
the program. So, adding it to the above example:

```CUDA
INIT_MODEL(int, 1)
  
BBLOCK(coinFlip, {
    PSTATE = SAMPLE(bernoulli, 0.6);
    PC++;
})
```

Now to run this program, only one thing remains to be defined, the main function.
This is done with the `MAIN` macro, taking a code block as argument.
The code below shows what we need in order to run the program and perform SMC. 

```CUDA
MAIN({
    ADD_BBLOCK(coinFlip);
 
    SMC(NULL);
})
```

First, the block must be added to the array of blocks to be executed. The order in which
blocks are added with `ADD_BLOCK`, defines their order in the array and thus
defines the order of execution together with the program counter. Secondly, the
`SMC` macro (its parameter is explained below) starts the inference. 

Now the model can be compiled and executed! However, the result of the coin flips is
stored, but never used. To aggregate the results of all the particles' samples, 
a callback function (`CALLBACK`) can be used. The callback will be called after
inference, but before clean up of the particles. This way, the results can be used to
generate desired distributions or values before particles are deleted. 
Within the `CALLBACK` macro, the array of program states is accessed 
with `PSTATES` and the number of particles is accessed with the 
parameter `N` (which is hidden within the macro). 
Here is an example callback, called "sampleMean" that calculates and prints the mean 
of the samples. 

```CUDA
CALLBACK(sampleMean, {
    double sum = 0;
    for(int i = 0; i < N; i++)
        sum += PSTATES[i];
    double mean = sum / N;
    printf("Sample mean: %f\n", mean);
})
```

For this to be used, the `SMC` macro call in main must be 
changed to: `SMC(sampleMean);`

This example can be found in
[rootppl/models/simple-examples/coin_flip_mean.cu](rootppl/models/simple-examples/coin_flip_mean.cu)
and, being in the rootppl directory, compiled with:
```
make MODEL=models/simple-examples/coin_flip_mean.cu
```

Then it can be executed with the executable followed by the
number of particles, for example: `./program 1000`. 

An example output is then:
```
Sample mean: 0.608000
0.000000
```

First we see our callback function's output. Then on the next line, is the logarithm of the
normalization constant approximated by the inference. This is simply 0 here, since the model contains
no statements that alter the weights of the particles. 

### Supplementary Examples

Below follows some examples, these are all models that are defined within one single block. 

#### Coin Flip Posterior
Full example: [rootppl/models/simple-examples/coin_flip.cu](rootppl/models/simple-examples/coin_flip.cu)

In this model, a bias for a coin is sampled from the prior beta distribution. Then we observe that the coin flip is true. This model thus infers the
posterior distribution of the bias, conditioned on the observation. 
```CUDA
BBLOCK(coinFlip, {
    double x = SAMPLE(beta, 2, 2);
    OBSERVE(bernoulli, x, true);

    PSTATE = x;
    PC++;
})
```

#### Gaussian Mixture Model
Full example: [rootppl/models/simple-examples/mixture.cu](rootppl/models/simple-examples/mixture.cu)

This model demonstrates an example of *stochastic branching*, meaning that different code is executed depending on the outcome of the sample. 
```CUDA
BBLOCK(mixture, {
    double x;
    if(SAMPLE(bernoulli, 0.7))
        x = SAMPLE(normal, -2, 1);
    else
        x = SAMPLE(normal, 3, 1);
 
    PSTATE = x;
    PC++;
})
```

#### Geometric Distribution
Full example: [rootppl/models/simple-examples/geometric.cu](rootppl/models/simple-examples/geometric.cu)

This model combines stochastic branching with recursion. Basic blocks do not fully support recursion themselves, as they take no custom arguments or return values. Instead, a helper function is used to express the recursive model:

```CUDA
BBLOCK_HELPER(geometricRecursive, {
    if(SAMPLE(bernoulli, p))
        return 1;
    else
        return BBLOCK_CALL(geometricRecursive, p) + 1;

}, int, double p)
```
```CUDA
BBLOCK(geometric, {
    PSTATE = BBLOCK_CALL(geometricRecursive, 0.6);
    PC++;
})
```

Note that the helper function takes its return value and parameters comma-separated after the function body. 

While recursive functions is supported by CUDA, iterative solutions are encouraged. Here is the same model, implemented with a loop instead:
```CUDA
BBLOCK(geometric, {
    int numFlips = 1;
    while(! SAMPLE(bernoulli, 0.6))
        numFlips++;
    PSTATE = numFlips;
    PC++;
})
```

### Building a more interesting model
TODO, stuff not yet demonstrated: 
- WEIGHT macro
- Multiple BBLOCK models
- Global data accessible by bblocks on GPU
- Program States containing more than a primitive datatype, e.g. structs. 
