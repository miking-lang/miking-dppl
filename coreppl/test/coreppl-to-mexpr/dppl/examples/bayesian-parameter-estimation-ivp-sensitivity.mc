include "./bayesian-parameter-estimation.mc"

/- Illustrates inferring the posterior of IVP solution sensitivites -/

let _model = lam t : ().
  let #var"θ" = assume #var"Dist_θ" in
  diff (lam #var"θ" : FloatA. g (y #var"θ" (x0, y0) future)) #var"θ" 1.

let #var"Dist_z/dθ(future)" = infer (Importance { particles = 1000 }) _model

mexpr

printFloatDist #var"Dist_z/dθ(future)"

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck baysian-parameter-estimation-ivp-sensitivity.mc && ./out | dppl-plot && rm ./out"
-- End:
