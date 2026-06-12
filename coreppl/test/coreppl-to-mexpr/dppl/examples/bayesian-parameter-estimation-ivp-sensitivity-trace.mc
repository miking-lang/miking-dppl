include "./bayesian-parameter-estimation.mc"

/- Illustrates inferring the posterior of IVP solution sensitivites -/

let _n = 200
let _h = 0.05
let timesExt = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

let _model = lam t : ().
  let #var"θ" = assume #var"Dist_θ" in
  diff (lam #var"θ" : FloatA. trace (y #var"θ") (x0, y0) timesExt) #var"θ" 1.

let #var"Dist_dy/dθ_trace" = infer (Importance { particles = 100 }) _model

mexpr

match distEmpiricalSamples #var"Dist_dy/dθ_trace" with (samples, weights) in
let samples =
  map
    (lam ts : [(Float, [Float])].
      mapi (lam i : Int. lam t : (Float, [Float]). (get timesExt i, t.1)) ts)
    samples in
printWeightedTrace samples weights

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck bayesian-parameter-estimation-ivp-sensitivity-trace.mc && ./out | dppl-plot-process && rm ./out"
-- End:
