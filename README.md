# PPLCore

Installation and running instructions:
1. Make sure you are on the `pplcore` branch in your local clone.
2. Install [opam](https://opam.ocaml.org/).
3. Install the opam packages [dune](https://opam.ocaml.org/packages/dune/) and
   [gsl](https://opam.ocaml.org/packages/gsl/).
4. (Optional) Install matplotlib if you want to use `src/hist.py` (In Ubuntu, install the package `python3-matplotlib`).
5. Run `dune build` from the project root.
6. The executable is `_build/default/src/pplcore.exe`. You can alternatively use `dune
   exec -- src/pplcore.exe`.
