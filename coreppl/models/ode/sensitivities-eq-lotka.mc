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

let model = lam.
  -- This function allows us to differentiate a trace w.r.t. a scalar
  let diffTr = lam f : Float -> [[Float]]. lam x : Float.
    let t = diff f x in t 1.
  in

  -- Jacbian-vector product.
  let jvp = lam f : [Float] -> [Float]. lam t : [Float]. diff f t in

  let lotkaVolterra = lam p. lotkaVolterra (p, 1., 1., 3.) in

  -- We parametrize our ODE model on the prey per-capita growth rate.
  let ode1 = lam p. lam xs.
    match lotkaVolterra p (get xs 0, get xs 1) with (f1, f2) in
    [f1, f2]
  in

  -- We construct a parametrized solution trace from our ODE mode.
  let trace1 = lam p. odeTrace (lam. lam xs. ode1 p xs) [1., 1.] dts in

  -- We also create a second ODE model that includes the sensitivities directly.
  let ode2 = lam p. lam xs.
    match xs with [x, y, dxdp, dydp] in
    concat
      -- original ODE model: f(x), in x' = f(x).
      (ode1 p [x, y])
      -- sensitivity model: (dx/dp)' = (df/dx)(dx/dp) + df/dp
      (map2 addf
         (jvp (ode1 p) [x, y] [dxdp, dydp])
         (jvp (lam p. ode1 (get p 0) [x, y]) [p] [1.]))
  in

  -- We construct a parametrized solution trace from our ODE mode.
  let trace2 = lam p.
    map (lam xs. [get xs 2, get xs 3])
      (odeTrace (lam. lam xs. ode2 p xs) [1., 1., 0., 0.] dts)
  in

  -- Uniformly draw the model parameter
  let p = assume (Uniform 1.1 1.5) in

  -- Trace the sensitivites w.r.t. the parameter by differentiating the solution
  -- trace.
  let sns1 = diffTr trace1 p in

  -- We also compute the forward continous sensitvities.
  let sns2 = trace2 p in

  let mse = lam xs. lam ys.
    divf
      (foldl addf 0. (map (lam err. pow err 2.) (map2 subf xs ys)))
      (int2float (length xs))
  in

  -- Output on a format suitable for the `dppl-plot-process` script
  let output = map2 (lam t. lam xs. (t, xs)) ts in
  map output [sns1, sns2]

  -- Or uncomment the line below to output a distribution over the MSE, suitable
  -- for the `dppl-plot` script of the methods for computing the sensitivities.
  -- mse (join sns1) (join sns2)


mexpr
model ()
