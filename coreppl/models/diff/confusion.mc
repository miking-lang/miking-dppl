-- This model tests that dual number pertubations are not confused.
let model = lam.
  -- scalar differentiation
  let diff1 : (Float -> Float) -> Float -> Float = lam f. lam x. diff f x 1. in

  -- Should be 2., pertubation confusion between the outer and inner derivative
  -- will instead result in 3.
  diff1 (lam x. mulf x (diff1 (lam y. mulf x y) 2.)) 1.

mexpr
model ()
