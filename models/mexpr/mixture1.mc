
include "coreppl/imp.mc"

let model = lam state:State.
  let x =
    if assumeBernoulli 0.7 state then
      addf (assumeBeta 3.0 3.0 state) 0.5
    else
      assumeBeta 2.0 2.0 state
  in
  ([x], state)

let main = output (infer (numarg ()) model) ["x"]
