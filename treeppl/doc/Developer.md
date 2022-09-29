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
This is also a reference for experienced developers.
For this reason it will start with the very basics but also cover the advanced aspects of compiler development.

## What is TreePPL?

[TreePPL](https://treeppl.org) is a high level probabilistic programming language (PPL) build within the [Miking](https://miking.org) platform.
Its primary domain is phylogenetics and evolutionary biology and it can be thought of as a statistical modeling language with a focus on tree data structures.

## Getting started with TreePPL development

### Installation and prerequisites

In order to get started with TreePPL development you need to install both the `miking` and the `miking-dppl` packages from GitHub.
To install `miking`, follow the instructions in the [Miking README](https://github.com/miking-lang/miking), but in a nutshell, if you have all the necessary prerequisties such as OPAM, O'CAML, etc., you checkout the `develop` branch and issue from the `miking` directory:

```
make clean
make install
cd stdlib; export MCORE_STDLIB=`pwd`; cd ..;
```

TreePPL itself is not a stand-alone package, but rather it is a part of `miking-dppl`, and it is found under the `treeppl/` directory of that package.
The installation of `miking-dppl` has several steps.
First, you  install CorePPL, which is an intermediate language, to which all TreePPL programs compile to.
From the `miking-dppl` directory you issue:

```
make 
make install
```

Then, to install the TreePPL compiler itself, `tppl`, you issue:

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

To get started with the development of `tppl` you need to first understand how compilation works.
Let's take the simple "flip" example, found under `models/flip/`.
Here, you will find three files: 

  - `flip.tppl`, which is the TreePPL program,
  - `data.mc`, which is a data-file, expressed in the intermediate language CorePPL (temporary measure),
  - and `flip.mc`, which is a test file, containing the compiled instructions of the program in CorePPL, to which the tests will compare the compiled `flip.tppl` + `data.mc`.

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
                        a CUDA RootPPL or a Miking MCore program
                                    \     /
                                   (5)  (6)* - default behavior
                                      \ /
                                  executable
```

In other words, the TreePPL program is parsed (1), then compiled to a CorePPL program (2), which is concatenated with the parsed data (3), then the CorePPL compiler (4) produces either a RootPPL C++ program (`.cu`), or a Miking MCore program (`.mc`). The RootPPL program can be compiled with `rppl` (`rootppl` previously) (5) to get an executable. The default behavior, is, however, for the Miking compiler (6) to be executed as part of the TreePPL compilation, which produces the executable.

These steps will be discussed in detail later
TODO explain individual steps
but now let's look at the individual components of the compiler.

### Components of `tppl`

TODO Here discuss the command line parsing and the actual compiler, and Viktor's tool
The compiler has the following components:

#### Compiler command-line `src/tppl.mc`

The command-line `tppl` takes care of understanding the command-line arguments (not shown on figure), parses the input program, which is in the moment (2022-09-29) the first argument (step 1 in the figure) and the input data, which is for now the second argument (3), and invokes the compiler(s) (TreePPL-to-CorePPL followed CorePPL-to-RootPPL/MCore) on the generated TreePPL AST (2,4) with the correct options to produce the needed output (CUDA RootPPL or Miking MCore).

Command line parsing is very rough in the moment (2022-09-29) and it needs a more generic solution, perhaps the one taken from the CorePPL compiler itself.

### Parser specification `treeppl.syn`

The parser relies on the framework Tool (`tool.mc`) that vipa developed. The parser itself `treeppl-ast.mc` is generated on the fly by Tool by the syntax specification `treeppl.syn`.  In additional to the parser we need some helpers that are in `tppl-lexer-extensions.mc`.

TODO the syntax stuff might be moved to the directory `syntax/`.

#### Library for compiling TreePPL to CorePPL

The essential part of the compiler is under `src/treeppl-to-coreppl/compile.mc`, which is included from `tppl.mc`. Right now (2022-09-29), the only target is CorePPL, but we have decided to use this directory tree-structure to follow Miking-DPPL conventions. 

### Setting up the programming environment and debugging

The easiest way to setup your development environment is as follows: let's say you are working on a feauture, e.g. "for loops for TreePPL".

1. Write a test-case: `models/for/for.tppl` and `models/for/data.mc`.  You would also want a `models/for/for.mc`, which will be the CorePPL program, which `for.tppl` should compile into.
2. You want your development loop to be:
  - edit the compiler library `src/compile.mc`, and when you do a change, attempt to compile the test-case;
  - by default `tppl.mc` will dump the generated CorePPL to the standard out as well;
  - if encounter an error, or the AST does not look right, go back.
3. Finally you want to write a test case for `utest`, which will be treated in a separate section

TODO Writing tests.

To monitor the compiler for changes you might use something like `entr`.
For example:

```
find src/ | entr 'mi run src/tppl.mc -- models/for/for.tppl models/for/data.mc'
```

For debugging, there are various printing functions in Miking, the most useful are:

- pretty printing `print`: this one is useful when Miking knows how to pretty print the object (TODO I feel like this has to be explained better)
- and `dprint`, which is useful when `print` doesn't work, for example to dump ASTs.  To `dprint`, however you would need to compile with `boot`.

## Syntax Parsing

## Compiler in-depth

### Data Input

TODO

### Handling of externals

This PR attempts to add externals support to TreePPL.

We introduce two new functions, which may need to be moved to externals.mc

sem constructExternalMap : Expr -> Map String Name
sem filterExternalMap: Set String -> Expr -> Expr
Then we parse a runtime, which only includes dist-ext.mc and math-ext.mc, we filter only the externals that we need, we run symbolize on the filtered externals, then we construct a map from the identifier strings to the Names for the externals, and finally we retrieve the Names of the externals that we need by searching this map. Finally, to use an external we utilize (app_ (nvar_ NAME)), whereas NAME is the name we just found and is passed around with a TpplCompileContext record.

Currently, there is an issue in mexprCompile. To reproduce the issue, attempt to compile any of the examples:

miking-dppl/treeppl$ mi run src/tppl.mc -- models/externals/externals.tppl models/externals/data.mc 
checkpoint 1

FILE "/home/viktor/Dropbox/Work/miking/stdlib/ext/math-ext.mc" 25:0-25:37 ERROR: Error in exprName for CFA
external externalLog : Float -> Float

miking-dppl/treeppl$ 

### More....

## Writing tests

## Community stuff
 
### How Miking PR's work, etc.