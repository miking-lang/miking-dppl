/- This example programs illustrates Bayesian regression -/

include "../lib.mc"

let regressionModel =
   lam d : [Float]. lam z : Float -> ModR [FloatA]. lam t : ().
    match (assume (Gamma 1. 1.), assume (Gamma 1. 1.))
      with (#var"θ", #var"𝜎²") in
    iter
      (lam t : (Float, Float).
        match t with (y, z) in observe y (Gaussian z (sqrt #var"𝜎²")))
      (zip (z #var"θ") d);
    #var"θ"

mexpr

let m = lam t : (). () in
infer (Importance { particles = 2 }) m;
()
