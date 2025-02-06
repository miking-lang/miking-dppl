# Miking DPPL
Miking DPPL is a framework for developing probabilistic programming languages (PPLs) using [Miking](https://github.com/miking-lang/miking).
Currently, the framework includes the [CorePPL](#coreppl) language.
There is also a convenience Python plotting script available named `dppl-plot`.

If you came here from a paper:
- [**ESOP 2022**](https://link.springer.com/chapter/10.1007/978-3-030-99336-8_2): The state of this repository and project has changed significantly since this paper was published. For the repository as it was at the time, see [here](https://github.com/miking-lang/miking-dppl/tree/0616d5774e16d2f4799bc4f9ef57219e5fa57d1b) or the [associated artifact](https://zenodo.org/records/6004987). The artifact in particular contains the correct version of both Miking and Miking-dppl, not just Miking-dppl.

## Dependencies
Miking DPPL currently depends on:
- [Miking](https://github.com/miking-lang/miking)
- [Owl](https://ocaml.xyz/). Essentially, `opam install owl`.

## Building, Installing, and Running Tests
To build the CorePPL compiler `cppl`, simply run `make` in the project root, which produces the binary in `build/cppl`.
To remove the binary, run `make clean`.

In addition to building `cppl`, there are three additional make targets:
#### `make install`
Installs `cppl` and scripts (e.g., `dppl-plot`) to `$HOME/.local/bin`, and also source dependencies to `$HOME/.local/src` (according to the [systemd file system hierarchy](https://www.freedesktop.org/software/systemd/man/file-hierarchy.html)).
Install individual components using the `install-coreppl` and `install-scripts` targets.

#### `make uninstall`
Uninstalls everything installed by `make install`.

#### `make test`
Runs a comprehensive test suite for CorePPL.

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

The default option for inferring the distribution encoded by a CorePPL program is to compile it to MExpr (which then compiles to OCaml).
You compile a CorePPL program `cpplprog.mc` using the command `cppl -m <method> cpplprog.mc`, where `<method>` is an inference algorithm (run the command `cppl` without any arguments to see the current list of available algorithms).
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
The argument to `out` is the number of samples.
The first row prints the log of the normalizing constant, and the subsequent rows the samples.
The first column is the sample, and the second its log-weight.
To visualize the distribution induced by the samples, pipe the output to `dppl-plot`: `./out 10 | dppl-plot` (currently only supports float samples).
Run `dppl-plot --help` for more advice.

For more help and options, run the `cppl` command without any arguments.

### Example Models
The directory `coreppl/models` contains a set of example CorePPL programs.
A brief overview:
- `coin.mc`: The "Hello, world!" of probabilistic programming (similar to the example above)
- `coin-iter.mc`: The same example implemented with the higher-order `iter` function.
- `sprinkler.mc`: The classical sprinkler model often used to illustrate Bayesian inference.
- `regression.mc`: Bayesian linear regression for a simple data set.
- `ssm.mc`: A fairly simple state-space positioning model for a single data set.
- `diversification-models/crbd*.mc`: Constant rate birth-death model from evolutionary biology for two data sets.
- `diversification-models/clads*.mc`: Cladogenetic diversification rate shift model from evolutionary biology for the same two data sets.
- `latent-dirichlet-allocation/lda*.mc`: Latent dirichlet allocation for some simple synthetic data sets.
- `vector-borne-disease/vbd.mc`: An SEIR model for a single data set.

You compile and run the above models with the `cppl` command, and the models are also the basis for the test suite run as part of `make test`. See the test files under `coreppl/test` for suitable inference algorithms and parameters.

### The `infer` Keyword and its Limitations
The experimental `infer` keyword in CorePPL allows users to apply inference algorithms within CorePPL programs.
For examples showing how to use `infer`, see `coreppl/models/infer-loop.mc` and `coreppl/models/infer-test.mc`
To see the available inference algorithms and their parameters for use with `infer`, you must currently consult the source code under `coreppl/src/inference`.

If a CorePPL program contains no applications of `infer`, the entire program encodes one single inference problem.
However, if the program contains one or more `infer` applications, the program may encode one or more inference problems simultaneously.

The `infer` keyword currently has a few limitations on how it can be used:

* The second argument to `infer` must be provided inline. That is, we cannot store the argument in a let-binding to reuse it. We have to write it out explicitly for each case:
    ```
    let d = infer (Importance {particles = 1000}) model in -- OK

    let args = Importance {particles = 1000} in
    let d = infer args model in -- ERROR
    ```

* The definition of the model function must be visible from the `infer`. We cannot use a higher-order function as the model, as the lambda variable hides the definition from our transformation.
    ```
    let f = lam. ... in
    let d = infer (Importance {particles = 1000}) f in -- OK

    let d = infer (Importance {particles = 1000}) (lam. ...) in -- OK

    let g = lam f.
      ...
      let d = infer (Importance {particles = 1000}) f in -- ERROR
      ...
    in
    ```

### Implementing New Inference Algorithms
The CorePPL to MExpr compiler is organized to make it easy to implement new inference algorithms.
Currently, the best way to understand the framework is to look at the source code.
There are also some slides available at `coreppl/docs/coreppl-to-mexpr.pdf`
