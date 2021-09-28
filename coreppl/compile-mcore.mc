include "ocaml/mcore.mc"

let compileRunMCore = lam ast.
  let stdin = "" in
  let args = [] in
  let stdout = compileRunMCore stdin args ast in
  print (concat stdout "\n")
