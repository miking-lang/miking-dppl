-- This model tests that dual number pertubations are not confused.
let model = lam.
  -- scalar differentiation
  let diff1 : (Float -> Float) -> Float -> Float = lam f. lam x. diff f x 1. in

  -- NOTE(vipa, 2026-06-10): We insert a resample to allow SMC
  -- algorithms to run this model, even though it doesn't matter at
  -- all for this particular model
  resample;

  -- Should be 2., pertubation confusion between the outer and inner derivative
  -- will instead result in 3.
  diff1 (lam x. mulf x (diff1 (lam y. mulf x y) 2.)) 1.

mexpr
model ()
