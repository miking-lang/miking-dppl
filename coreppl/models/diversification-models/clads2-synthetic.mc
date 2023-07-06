include "tree-synthetic.mc"
include "clads2.mc"

let model: () -> Float = lam.
  clads2 tree rho

mexpr
model ()
