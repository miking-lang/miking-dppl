# PPLCore

Installation and running instructions:
1. Install [opam](https://opam.ocaml.org/).
2. Install the opam packages [dune](https://opam.ocaml.org/packages/dune/) and
   [gsl](https://opam.ocaml.org/packages/gsl/).
3. (Optional) In Ubuntu, install the package `python3-matplotlib` if you want
   `src/hist.py` to work.
3. Run `dune build` from the project root.
4. The executable is `_build/default/src/pplcore.exe`. You can also use `dune
   exec -- src/pplcore.exe`.
