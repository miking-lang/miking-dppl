include "../lib.mc"
include "./ode-and-data.mc"

let _n = 200
let _h = 0.05
let times = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

-- Initial values
let x0 = 0.
let y0 = [1., 1.]

-- ODE model
let ode = lam #var"θ" : FloatA. lam x : FloatA. lam y : [FloatA].
  let t = lotkaVolterra (#var"θ", 1., 1., 3.) (get y 0, get y 1) in
  [t.0, t.1]

-- IVP solution
let y = lam #var"θ" : FloatA. lam xy0 : (FloatA, [FloatA]). lam x : FloatPC.
  solve (ode #var"θ") xy0 x

let f = ode

let fS =
  lam f : FloatA -> FloatA -> ModA ([FloatA] -> ModA [FloatA]).
    lam #var"θ" : FloatA. lam x : FloatA. lam y : [FloatA].
      match splitAt y (divi (length y) 2) with (y, #var"dy/dθ") in
      let #var"df/dy⋅dy/dθ" = diff (f #var"θ" x) y #var"dy/dθ" in
      let #var"df/dθ" = diff (lam #var"θ" : FloatA. f #var"θ" x y) #var"θ" 1. in
      concat (f #var"θ" x y) (adds #var"df/dy⋅dy/dθ" #var"df/dθ")

let yS = lam #var"θ" : FloatA. lam xy0 : (FloatA, [FloatA]). lam x1 : FloatPC.
  solve (fS f #var"θ") xy0 x1

let _model = lam t : ().
  let #var"θ" = assume (Uniform 1.0 2.0) in
  [
    map
      (lam t : (FloatA, [FloatA]). (t.0, [get t.1 2, get t.1 3]))
      (trace (yS #var"θ") (x0, (concat y0 [0., 0.])) times),
    diff (lam #var"θ" : FloatA. trace (y #var"θ") (x0, y0) times) #var"θ" 1.
  ]

let #var"Dist_dy/dθ" = infer (Importance { particles = 100 }) _model

mexpr

match distEmpiricalSamples #var"Dist_dy/dθ" with (samples, weights) in
let samples =
  map
    (lam x : [[(Float, [Float])]].
      map
        (lam ts : [(Float, [Float])].
          mapi
            (lam i : Int. lam t : (Float, [Float]). (get times i, t.1))
            ts)
        x)
    samples in
printWeightedTraces samples weights

-- local variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck ode-sensitivites-two-methods.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
