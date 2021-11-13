include "coreppl/imp.mc"

let model = lam state:State.
  let x = assumeBeta 2.0 2.0 state in
--  observeBernoulli true x state;
  ([x], state)

let main = output (inferImp 50000 model) ["#", "x"]
