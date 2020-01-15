# PPLCore

Installation and running instructions:
1. Install [opam](https://opam.ocaml.org/).
2. Install the opam packages [dune](https://opam.ocaml.org/packages/dune/) and
   [gsl](https://opam.ocaml.org/packages/gsl/).
3. (Optional) Install matplotlib if you want to use `src/hist.py` (In Ubuntu, install the package `python3-matplotlib`).
3. Run `dune build` from the project root.
4. The executable is `_build/default/src/pplcore.exe`. You can alternatively use `dune
   exec -- src/pplcore.exe`.
