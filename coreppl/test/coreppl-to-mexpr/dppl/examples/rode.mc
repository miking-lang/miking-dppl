include "../lib.mc"

let solve = lam f : FloatN -> FloatA -> FloatA. lam y0 : FloatA. lam x : FloatN.
  solveode (EF { add = addf, smul = mulf, stepSize = 1e-3 }) f y0 x

let rode = lam n : Int. lam dx : FloatN. lam t : ().
  -- Time-steps
  let xs = create n (lam i : Int. mulf (int2float i) dx) in

  -- Process noise
  let w = assume (Wiener ()) in

  -- RODE model
  let f = lam x : FloatN. lam y : FloatA. subf (sin (w x)) y in

  -- RODE solution
  let ys = map (lam x : FloatN. solve f 0. x) xs in

  -- Output in a format suitable for the plotting script
  let ys = mapi (lam i : Int. lam y : FloatA. (get xs i, [y])) ys in
  let ws = map (lam x : FloatN. (x, [w x])) xs in
  [ys, ws]

mexpr

let dist = infer (Importance { particles = 5 }) (rode 500 0.01) in

-- Just make sure that inference completes
match distEmpiricalSamples dist with (_, weights) in
utest foldl addf 0. (map exp weights) with 1. using eqfApprox 1e-3 in

()
