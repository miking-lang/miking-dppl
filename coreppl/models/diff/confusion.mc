-- This model tests that dual number pertubations are not confused.
let model = lam.
  -- scalar differentiation
  let d : (Float -> Float) -> (Float -> Float) =
    lam f. lam x. let t = diff f x in t 1.
  in

  -- Should be 2., pertubation confusion between the outer and inner derivative
  -- will instead result in 3.
  d (lam x. mulf x (d (lam y. mulf x y) 2.)) 1.

mexpr
model ()
