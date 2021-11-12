include "coreppl/imp.mc"

let model = lam state:State.
  let x = assumeBeta 10.0 8.0 state in
  observeBernoulli true x state;
  observeBernoulli true x state;
  ([x], state)

let main = printCSV (inferImp 20000 model) ["x", "#"]
