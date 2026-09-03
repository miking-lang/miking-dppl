include "tree-alcedinidae.mc"
include "clads2.cppl"

let model: () -> Float = lam.
  clads2 tree rho

mexpr
model ()
