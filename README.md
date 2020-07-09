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
nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/airplane/airplane.cu -o model.exe -std=c++14 -O3
```
`-arch=sm_75` defines that it is being compiled for a GPU of minimum compute capability 7.5. This should be set to match the GPU being used. 

The program can also be built for CPU (currently only sequential single-core) using `g++` like so:

```
g++ -x c++ -I . models/airplane/airplane.cu -o model.exe -std=c++11 -O3
```

## Probabilistic/Basic Blocks
TODO

## Important Macros
TODO

## Simple Example Model
TODO

