include "ext/math-ext.mc"

-- Implements the smoothdivf intrinsic
let bump = lam x. if leqf x 0. then 0. else exp (divf -1. x)
let mollifierStep = lam a. lam b. lam x.
  divf (bump (subf x a)) (addf (bump (subf x a)) (bump (subf b x)))

let smoothdivf = lam x. lam y.
  if eqf y 0. then 0. else divf (mulf x (mollifierStep 0.5 1. y)) y

