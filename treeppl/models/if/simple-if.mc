include "math.mc"

mexpr

recursive let simpleif: Float -> () =
  lam p.
  let e = assume (Bernoulli p) in
  match e with true then () else ()
in

(simpleif 0.5)
