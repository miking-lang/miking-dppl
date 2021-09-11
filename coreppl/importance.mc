

-- include "ocaml/compile.mc"
include "compile-mcore.mc"
include "seq.mc"




let importanceSamplingInference = lam options. lam ast.
  compileRunMCore ast
