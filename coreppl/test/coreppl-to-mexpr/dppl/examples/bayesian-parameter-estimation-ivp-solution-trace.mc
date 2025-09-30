include "./bayesian-parameter-estimation.mc"

/- Illustrates inferring the posterior of IVP solutions -/

let _n = 200
let _h = 0.05
let timesExt = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

let _model = lam t : ().
  let #var"θ" = assume #var"Dist_θ" in
  trace (y #var"θ") (x0, y0) timesExt
let #var"Dist_y_trace" = infer (Importance { particles = 100 }) _model

mexpr

match distEmpiricalSamples #var"Dist_y_trace" with (samples, weights) in
printWeightedTrace samples weights

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck baysian-parameter-estimation-ivp-solution-trace.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
