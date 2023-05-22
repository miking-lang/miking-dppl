# Miking DPPL
Miking DPPL is a framework for developing probabilistic programming languages (PPLs) using [Miking](https://github.com/miking-lang/miking).
Currently, the framework includes the PPLs [CorePPL](#coreppl) and [RootPPL](#rootppl).

## Building and Installing
The main binary for development in Miking DPPL is called `cppl`. Currently, you build this binary by running `make` in the project root, and install it to `$HOME/.local/bin` with `make install` (uninstall is also possible through `make uninstall`). You must have [Miking](https://github.com/miking-lang/miking) installed (the `mi` command must be globally available) to build `cppl`.
The command `make test` executes a set of tests for various components in the Miking DPPL framework.

## CorePPL
CorePPL extends MExpr (see [Miking](https://github.com/miking-lang/miking)) with probabilistic constructs for defining random variables and for likelihood updating. For example, the program
```
mexpr
let x = assume (Beta 10.0 5.0) in
observe true (Bernoulli x);
x
```
encodes a simple distribution for the bias of a coin, by setting a Beta prior for the probability of observing heads (i.e., `true`) for a single flip of the coin.

You define random variables in CorePPL by providing a probability distribution to the `assume` construct.
Currently, there is no generated documentation for available distributions (you have to look at the source code).

You can update the likelihood through the `weight` and `observe` constructs.
With `weight`, you update the logarithm of the likelihood directly (e.g., `weight (log 2)` multiplies the likelihood with 2).
With `observe`, you update the likelihood with the value of the pmf or pdf for the given distribution at the given observation.
For example `observe true (Bernoulli 0.5)` updates the likelihood with a factor of 0.5.

## Compiling CorePPL to MExpr
The default option for inferring the distribution encoded by a CorePPL program is to compile it to MExpr (which then compiles to OCaml).
You compile a CorePPL program `cpplprog.mc` using the command `cppl -m <method> cpplprog.mc`, where `<method>` is an inference algorithm (run the command `cppl` to see the current list of available algorithms).
For example, `cppl -m is-lw cpplprog.mc` compiles `cpplprog.mc` to a binary file `out` which you can subsequently run to produce likelihood-weighted samples from the distribution encoded by `cpplprog.mc`:
```
$ ./out 10
-0.290110454733
0.80022937428 -0.222856874559
0.843424730606 -0.170284615588
0.80463734988 -0.217363600111
0.884065007231 -0.123224681456
0.660101541887 -0.415361604451
0.83498403869 -0.180342669655
0.430312010842 -0.84324472681
0.721170725777 -0.326879379468
0.675965445901 -0.391613319777
0.826919003807 -0.190048528529
```
The argument is the number of samples.
The first row prints the log of the normalizing constant, and the subsequent rows the samples.
The first column is the sample, and the second its log-weight.
For more help and options, run the `cppl` command without any arguments.

### Implementing New Inference Algorithms
The CorePPL to MExpr compiler is organized to make it easy to implement new inference algorithms.
Currently, the best way to understand the framework is to look at the source code.
There are also some slides available at `coreppl/docs/coreppl-to-mexpr.pdf`

## Compiling CorePPL to RootPPL
Another option to run inference over a CorePPL program is to compile it to RootPPL.
This option potentially results in more efficient inference (due to the efficiency of RootPPL), but also has quite a lot of limitations (for example, RootPPL does not support automatic memory management or higher-order functions).
A CorePPL program in a file `cpplprog.mc` can be compiled to a RootPPL file `out.cu` using the command `cppl -t rootppl cpplprog.mc`.
Currently, the only supported inference algorith in RootPPL is `smc-bpf` (the SMC bootstrap particle filter).
The file `out.cu` can then be compiled using, e.g., `rootppl out.cu --stack_size 10000`.
The stack size option is mandatory for compiling RootPPL programs compiled from CorePPL.
More information about RootPPL can be found at [RootPPL](#rootppl).

## RootPPL
RootPPL is an intermediate language for representing probabilistic models and comes with a framework that performs inference on the GPU in these models. See examples in the folder `rootppl/models`. The idea is that high-level Miking probabilistic programming languages should be compiled to this intermediate language.

### Getting Started
The instructions below are tested on Ubuntu 18.04 but should work for other Unix-like systems.

#### Prerequisites
Before building RootPPL programs, a C++/CUDA compiler is required. RootPPL works on CPU and Nvidia GPU:s. For the CPU version, a C++ compiler should suffice, e.g. g++.  In
order to build for GPU, CUDA must be installed. See
CUDA installation guides: [Linux](https://docs.nvidia.com/cuda/cuda-installation-guide-linux/ "CUDA Installation Guide Linux"),
[Mac](https://docs.nvidia.com/cuda/cuda-installation-guide-mac-os-x/index.html "CUDA Installation Guide Mac").

To run the CPU version in parallel, [OpenMP](https://www.openmp.org/resources/openmp-compilers-tools/) must be installed.
OpenMP comes with recent gcc versions. However, OpenMP is not necessary if one does not want to execute programs in parallel on the CPU.

#### Install `rootppl`
To install `rootppl` for the current user, first clone this repository and change directory to the `rootppl` folder. Then run:
```
make install
```
This will install `rootppl` to `$HOME/.local/bin` and inference engine sources to `$HOME/.local/src/rootppl`. Some systems, e.g. Mac OS, will require manually adding `$HOME/.local/bin` to `$PATH`.

#### Build
To compile a model and build an executable:
```
rootppl path/to/model.cu
```
This will compile the model along with the inference framework for CPU. The inference framework build products are placed under `build/`.
The `rootppl` command wraps `g++`/`nvcc`, and you may supply options used for `g++` and/or `nvcc` to `rootppl`.
These options will be used when compiling the supplied models, but not when compiling the inference framework.
One limitation is that it is currently only possible to supply source file(s) to `rootppl`.
Object/library files are not supported (for this, you must write your own custom build script).

To compile for GPU, add your GPU:s compute capability to the `--target` option.
You can find your GPU:s compute capability in the [Wikipedia table](https://en.wikipedia.org/wiki/CUDA#GPUs_supported).
Here is an example that will compile the airplane example for a GPU with a minimum compute capability of 7.5
(simply remove `--target sm_75` to compile for CPU):
```
rootppl models/airplane/airplane.cu --target sm_75 -j 5
```
The optional argument `-j x` speeds up the compilation process by spawning `x` jobs, allowing for parallel compilation.
The corresponding parallel CPU (OpenMP) example is:
```
rootppl models/airplane/airplane.cu --target omp -j 5
```
Alternatively, the c++ compiler can be specified with CXX. This is often required on Mac OS to enable OpenMP, by using g++ instead of the default clang. On Mac OS, g++ can be installed with e.g. `brew install gcc`. Then, assuming the version installed was `gcc-10`:
```
rootppl models/airplane/airplane.cu --target omp -j 5 --cxx g++-10
```

The first build will compile the entire inference framework and can take 20 seconds or so when building for GPU. (Run `rootppl` with the `-j num_threads` to use multiple threads and speed up the build). The inference framework is recompiled whenever options such as `--target` are modified between runs of `rootppl`.

Any of the above commands should generate an executable named `a.out` (add `-o <exec_name>` to name it differently). Execute it with `./a.out num_particles`. For example:
```
./a.out 1000
```

An example output of this:
```
./a.out 1000
-119.422143
Num particles close to target: 98.9%, MinX: 56.2349, MaxX: 91.1572
```
First is the command that is executed, it executes the executable `a.out` with argument `1000`.
The first row is the normalizing constant. The second row is a print statement within the model.

You can supply a second argument to `a.out`, which is the number of sweeps:
```
./a.out 1000 3
-115.482724
Num particles close to target: 89.7%, MinX: 51.5005, MaxX: 88.2039
-115.472101
Num particles close to target: 90.5%, MinX: 53.0577, MaxX: 90.0298
-115.496258
Num particles close to target: 88.7%, MinX: 48.1773, MaxX: 89.0957
```

#### Creating a simple model
Models are divided into fragments to enable pausing the execution within models.
These fragments are functions referred to as basic blocks (`BBLOCK`).
To control the program execution flow, a program counter (`PC`) can be modified.
If it remains unchanged in when the basic block returns, resampling will be done, and then
the same block will be executed again. The program counter corresponds to the index of the basic block
to be executed. So, incrementing it means that the next block will be executed after resampling. An
example of this can be seen in the example below. However, if no following blocks are defined,
the inference will terminate as the model program has been executed.

Any interesting model will contain random choices, i.e. sampling from distributions.
Below is a coin flip example which flips a biased coin. `SAMPLE` is a macro that can take variable number of arguments.
First is the distribution name, followed by the distribution specific arguments.

Sampling is done differently on the CPU and the GPU,but these differences are hidden in the model with the help
of this `SAMPLE` macro. All of these RootPPL keywords are macros similar to `SAMPLE`, they provide higher-level
constructs that can be used in the same way regardless of if the model will be executed on the CPU or the GPU.

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
parameter `N` (which is hidden within the macro). Also, the log-weights array can be accessed with
the `WEIGHTS` macro. These weights are normalised (so that the exponent of these log-weights sum to 1).
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
[`rootppl/models/simple-examples/coin_flip_mean.cu`](rootppl/models/simple-examples/coin_flip_mean.cu)
and, being in the `rootppl` directory, compiled with:
```
rootppl models/simple-examples/coin_flip_mean.cu
```

Then it can be executed with the executable followed by the
number of particles, for example: `./a.out 1000`.

An example output is then:
```
Sample mean: 0.608000
log normalization constant = 0.000000
```

First we see our callback function's output. Then on the next line, is the logarithm of the
normalization constant approximated by the inference. This is simply 0 here, since the model contains
no statements that alter the weights of the particles.

#### Supplementary Examples

Below follows some examples, these are all models that are defined within one single block.

##### Coin Flip Posterior
Full example: [`rootppl/models/simple-examples/coin_flip.cu`](rootppl/models/simple-examples/coin_flip.cu)

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

##### Gaussian Mixture Model
Full example: [`rootppl/models/simple-examples/mixture.cu`](rootppl/models/simple-examples/mixture.cu)

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

##### Geometric Distribution (Recursive)
Full example: [`rootppl/models/simple-examples/geometric_recursive.cu`](rootppl/models/simple-examples/geometric_recursive.cu)

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

While recursive functions is supported by CUDA, iterative solutions are encouraged. Below is the same model, implemented with a loop instead.
##### Geometric Distribution (Iterative)
Full example: [`rootppl/models/simple-examples/geometric_iterative.cu`](rootppl/models/simple-examples/geometric_iterative.cu)
```CUDA
BBLOCK(geometric, {
    int numFlips = 1;
    while(! SAMPLE(bernoulli, 0.6))
        numFlips++;
    PSTATE = numFlips;
    PC++;
})
```

#### Phylogenetic Models

More sophisticated models can be found in the [phylogenetics directory](rootppl/models/phylogenetics). These probabilistic
models to inference on observed phylogenetic trees. These observed trees can be found in [phylogenetics/tree-utils/trees.cuh](rootppl/models/phylogenetics/tree-utils/trees.cuh).
The correct phylogenetic models contain a link in the top of the file to the WebPPL source code used as reference when implementing them.
These models contain a number of new things, e.g.:
- Multiple basic blocks
- Structures as program states
- Recursive simulations
- Global data used by the model
- Resampling throughout the traversal of the observed tree

##### Constant-rate birth-death
In [phylogenetics/crbd](rootppl/models/phylogenetics/crbd), the constant-rate birth-death models can be found.
The most interesting file here is [crbd.cu](rootppl/models/phylogenetics/crbd/crbd.cu) as it is a correct model
used by evolutionary biologists. This model uses a pre-processed DFS traversal path over the observed tree, rather than using a call stack.

##### Further phylogenetic models
Further phylogenetic models are found in the package
[`phyrppl`](https://github.com/vsenderov/phyrppl).

#### The RootPPL Architecture

The architecture's main goal is to decouple the inference from the probabilistic models, this is the case for probabilistic
programming languages.
All inference engine code is located under `src/`.
In the `inference` directory, the code for inference methods can be found. Currently, only SMC is supported.
Within the `smc` directory, the `resampling` directory contain resampling strategies. Currently only Systematic Resampling is supported. The resampling has
a parallel implementation for the GPU, and a sequential and parallel implementation for the CPU.

- The `dists` directory contains distributions and scoring functions. Both models and inference can rely on these.
- The `macros` directory contains all the macros/constructs that are mostly used by models but also in inference code.
- The `utils` directory contains code that RootPPL uses, but can be used by models as well.
- The `models` directory contains example models that use the inference.

#### Compile with the built-in stack
To compile with the `progStateStack_t` program state, the macro `INIT_MODEL_STACK(num_bblocks)` should be used instead of `INIT_MODEL(progState_t, num_bblocks)`.
Then the stack size is specified in bytes with `--stack_size num_bytes`, e.g. `rootppl my_stack_model.cu --stack_size 10000`.

#### Building a more interesting model
TODO, stuff not yet demonstrated explicitly in README:
- WEIGHT macro
- Multiple BBLOCK models
- Global data accessible by bblocks on GPU
- Program States containing more than a primitive datatype, e.g. structs.
