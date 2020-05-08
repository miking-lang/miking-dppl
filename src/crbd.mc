include "pplcore.mc"

mexpr
use PPLCoreAst in

let leaf_ = lam age.
  app_ (var_ "Leaf") (record_ [{key = "age", value = float_ age}])
in

let node_ = lam age. lam left. lam right.
  app_ (var_ "Node") (record_ [{key = "age", value = float_ age},
                               {key = "l",   value = left},
                               {key = "r",   value = right}])
in

let crbdGoesUndetected = ulet_ "crbdGoesUndetected"
                               (ulams_ ["startTime", "lambda", "mu"]
                                 (str_ "TODO"))
in

let crbd =
  bindall_ [
    condef_ "Leaf" tydyn_,
    condef_ "Node" tydyn_,
    ulet_ "tree" (node_ 1.0 (leaf_ 0.0) (leaf_ 0.0)),
    ulet_ "lambda" (float_ 0.2),
    ulet_ "mu" (float_ 0.1),
    ulet_ "_" (weight_ (float_ 0.3)), -- weight(log(2))
    crbdGoesUndetected
  ]
in

printLn (pprintCode 0 crbd)
