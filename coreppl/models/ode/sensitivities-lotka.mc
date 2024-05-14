include "./ode-common.mc"
include "./lotka-model.mc"

-- Number of time-steps
let n = 100
-- Size time-step
let dt = 0.1
-- Time-steps
let dts = create n (lam. dt)
-- Times-points
let ts = reverse (foldl (lam ts. lam dt. cons (addf dt (head ts)) ts) [0.] dts)

-- This model is deterministic so you can run it with `cppl -p 1` becasue each
-- sample is identical.
let model = lam.
  -- This function allows us to differentiate a trace w.r.t. a scalar
  let d : (Float -> [[Float]]) -> Float -> [[Float]] = lam f. lam x.
    let t = diff f x in t 1.
  in

  -- We parametrize our ODE model on the prey per-capita growth rate.
  let lotkaVolterra = lam p. lam xs.
    match lotkaVolterra (p, 1., 1., 3.) (get xs 0, get xs 1) with (f1, f2) in
    [f1, f2]
  in

  -- We construct a parametrized solution trace from our ODE mode.
  let trace = lam p. odeTrace (lam. lam xs. lotkaVolterra p xs) [1., 1.] dts in

  -- Uniformly draw the model parameter
  let p = assume (Uniform 1.1 1.5) in

  -- Produce solution traces
  let tr = trace p in

  -- Trace the sensitivites w.r.t. the parameter by differentiating the solution
  -- trace.
  let sns = d trace p in

  -- Output on a format suitable for the `dppl-plot-process` script
  let output = map2 (lam t. lam xs. (t, xs)) ts in
  map output [tr, sns]

mexpr
model ()
