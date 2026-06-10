include "./ode-and-data.mc"
include "./bayesian-regression-model.mc"

let z = lam #var"θ" : FloatP.
  map (lam x : FloatP. g (y #var"θ" (x0, y0) x)) times
-- let z = lam #var"θ" : FloatA. map g (trace (y #var"θ") (x0, y0) times) -- slightly faster
let #var"Dist_θ" = infer (APF { particles = 5000, resample = "likelihood" }) (regressionModel data z)

mexpr

utest expectation #var"Dist_θ" with #var"true_θ" using eqfApprox 0.1 in

printFloatDist #var"Dist_θ"

-- These settings assumes that cppl and dppl-plot are in PATH.
-- Local Variables:
-- compile-command: "cppl --test --seed 1 --cps partial --dppl-typecheck bayesian-parameter-estimation.mc && ./out | dppl-plot && rm -rf ./out"
-- End:
