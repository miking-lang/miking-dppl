# miking-ppl

# PPLCore

To be written...

# RootPPL

RootPPL can be seen as an intermediate language for representing probabilistic models and comes with a framework that performs inference on the GPU in these models. These models are currently hardcoded, see examples in the folder rootppl/models. The idea is that high level Miking probabilistic programming languages should be compiled to this intermediate language. 

## Getting Started
To build the program, only the model needs to be compiled. For example, to compile the Airplane model to GPU, being in the rootppl directory:
```
nvcc -arch=sm_75 -rdc=true -lcudadevrt -I . models/airplane/airplane.cu -o model.exe -std=c++11 -O3
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

