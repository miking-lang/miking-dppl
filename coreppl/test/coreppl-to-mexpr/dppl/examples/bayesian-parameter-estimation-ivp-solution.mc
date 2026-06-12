include "./bayesian-parameter-estimation.mc"

/- Illustrates inferring the posterior of IVP solution -/

let _model = lam t : ().
  let #var"θ" = assume #var"Dist_θ" in
  g (y #var"θ" (x0, y0) future)

let #var"Dist_z(future)" = infer (Importance { particles = 1000 }) _model

mexpr

printFloatDist #var"Dist_z(future)"

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck bayesian-parameter-estimation-ivp-solution.mc && ./out | dppl-plot && rm ./out"
-- End:
