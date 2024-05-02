let map2 = lam f. lam seq1. lam seq2.
  create (length seq1) (lam i. f (get seq1 i) (get seq2 i))

-- Computes an ODE solution trace for the ODE `f`, inital values `xs0` and
-- time-steps `dts`.
let odeTrace : (Float -> [Float] -> [Float]) -> [Float] -> [Float] -> [[Float]] =
  lam f. lam xs0. lam dts.
    reverse
      (foldl
         (lam trace. lam dt.
           cons (solveode (RK4 { stepSize = 1e-3 }) f (head trace) dt) trace)
         [xs0] dts)

-- Lotka-Volterra's predator-pray model, where `x.0`, `x.1` are the prey,
-- predator densities, respectivly. The parameters `p = (p0, p1, p2, 03)`,
-- parametrizes:
-- - p0: prey per-capita growth rate
-- - p1: prey death rate from predators
-- - p2: predator per-capita growth rate
-- - p3: predator death rate from lack of prey
let lv = lam p : (Float, Float, Float, Float). lam x : (Float, Float).
  [
    subf (mulf p.0 x.0) (mulf p.1 (mulf x.0 x.1)),
    subf (mulf p.2 (mulf x.0 x.1)) (mulf p.3 x.1)
  ]

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

  -- We parametrize our solution trace on the prey per-capita growth rate.
  let trace = lam p.
    odeTrace (lam. lam xs. lv (p, 1., 1., 3.) (get xs 0, get xs 1)) [1., 1.] dts
  in

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
