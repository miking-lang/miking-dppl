/-
Constant-rate birth-death model (CRBD) model
Manual translation from crbd-demo.tppl into CorePPL.


include "flip-io.mc" -- hard-coded input

mexpr 

recursive let flip: Float -> Float =
  lam p.
  let e = assume (Bernoulli p) in
  if e then 1.0 else 0.0
in

(flip 0.5)
-/
mexpr
recursive
  let flip =
    lam p: Float.
      let e =
        assume
          (Bernoulli
             p)
      in
      e
in
let p =
  0.5
in
flip
  p
