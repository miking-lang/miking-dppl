include "tree-synthetic.mc"
include "crbd.mc"

let model: () -> Float = lam.
  crbd tree rho

mexpr
model ()
