```
title:  'TreePPL Compiler Hacker's Handbook'
author:
- Viktor Senderov
- Viktor Palmkvist
keywords: [miking, compiler]
abstract: |
  This document explains how the TreePPL compiler (part of Miking) works and can be used as a reference or for on-boarding new compiler developers to the TreePPL project.
```

# TreePPL Compiler Hacker's Handbook

Hi and welcome to the TreePPL compiler! In this guide you will find information on the intricacies of the TreePPL compiler, how to setup your development environment, and hopefully, after reading it, you will be well-equiped to start contributing to the project!
This is also a reference for some of the old developers such as us.
For this reason it will start with the very basics but also cover the advanced aspects of compiler development.

## What is TreePPL?

[TreePPL](https://treeppl.org) is a high level probabilistic programming language (PPL) build within the [Miking](https://miking.org) platform.
Its primary domain is phylogenetics and evolutionary biology and it can be thought of as a statistical modeling language with a focus on tree data structures.

## Getting started with TreePPL development

### Installation and prerequisites

In order to get started with TreePPL development you need to install both the `miking` and the `miking-dppl` packages from GitHub.
To install `miking`, follow the instructions in the [Miking README](https://github.com/miking-lang/miking), but in a nutshell, if you have all the necessary prerequisties such as OPAM, O'CAML, etc., you checkout the `develop` branch and issue from the Miking directory:

```
make clean
make install
cd stdlib; export MCORE_STDLIB=`pwd`; cd ..;
```

TreePPL itself is not a stand-alone package, but rather it is part of `miking-dppl`, and it is found under `treeppl` directory of that package.
The installation of `miking-dppl` has several steps.
First, you  install CorePPL, which is an intermediate language, to which all TreePPL programs compile to.
From the `miking-dppl` directory you issue:

```
make 
make install
```

Then, to install the treeppl compiler itself, `tppl`, you issue:

```
make install-treeppl
```

To unistall it

```
make uninstall-treeppl
```

This behavior is programmed in the `Makefile` in the root of `miking-dppl`.
There is another makefile, under `treeppl/Makefile`, which holds the recipe for building `tppl`.

### How does compilation work?

Let's take the simple "flip" example, found under `models/flip`.
Here, you will find three files: 

  - `flip.tppl`, which is the program,
  - `data.mc`, which is a data-file,
  - and `flip.mc`, which is a test file, containing the compiled instructions of the program in the intermediate language.

There are three ways to compile this program:

  1. Build the `tppl` compiler, and use it to compile the program
  
  ```
  make build/tppl
  build/tppl models/flip/flip.tppl models/flip/data.mc
  ```

  2. Interpret the `tppl` compiler using Miking
  
  ```
  mi run src/tppl.mc -- models/flip/flip.tppl models/flip/data.mc
  ```

  3. Bootstrap Miking using O'CAML/OPAM

  ```
  boot eval src/tppl.mc -- models/flip/flip.tppl models/flip/data.mc 
  ```

Method 1 is the fastest and is the one that the users will use.
However 2 and 3 might be used for debugging, more about this in the 
TODO Debugging chapter.

Regardless of which method you use, the actual compilation process is the same.
Here is the compilation pipeline:

```
  flip.tppl                                  data.mc
      |                                        |
     (1)                                       |
      |                                        |
  TreePPL abstract syntax tree (AST)          (3)
      |                                        |
     (2)                                       |
      |                                        |
  CorePPL program (AST)                +     Miking expression of the input 
                                      / \
                                (4)  /   \
                                    /     \
                        a CUDA RootPPL or a Miking program
                                    \     /
                                   (5)  (6)*
                                      \ /
                                  executable
```

In other words, the TreePPL program is parsed (1), then compiled to a CorePPL program (2), which is concatenated with the parsed data (3), then the CorePPL compiler (4) produces either a RootPPL C++ program (`.cu`), or a Miking MCore program (`.mc`). The RootPPL program can be compiled with `rootppl` (5) to get an executable. The default behavior, is, however, for the Miking compiler (6) to be executed as part of the TreePPL compilation, which produces the executable.

These steps will be discussed in detail later
TODO explain individual steps
but now let's look at the individual components of the compiler.

## Components of `tppl`

TODO