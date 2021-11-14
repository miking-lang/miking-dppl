include "coreppl/imp.mc"

let model = lam state:State.
  let x1 = assumeBeta 10.0 8.0 state in
  observeBernoulli true x1 state;
  let x2 = assumeBeta 2.0 2.0 state in
  observeBernoulli true x2 state;
  observeBernoulli false x2 state;
  observeBernoulli true x2 state;
  observeBernoulli true x2 state;
  ([x1, x2], state)

let main = output (inferImp 50000 model) ["#", "x1", "x2"]
