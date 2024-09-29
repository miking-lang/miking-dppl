include "./ode-common.mc"
include "./lotka-model.mc"

let model = lam n. lam dt. lam.
  -- Time-steps
  let dts = create n (lam. dt) in

  -- Times-points
  let ts = reverse (foldl (lam ts. lam dt. cons (addf dt (head ts)) ts) [0.] dts) in

  -- This function allows us to differentiate a trace w.r.t. to its parameters
  let diffTr = lam f : [Float] -> [[Float]]. lam x : [Float].
    diff f x
  in

  -- Jacbian-vector product.
  let jvp = lam f : [Float] -> [Float]. lam t : [Float]. diff f t in

  -- Function that takes a parametrized ODE model as input and
  -- outputs a ODE model augmented with sensitivity equations.
  let augmentSens = lam f : [Float] -> [Float] -> [Float]. lam nx : Int.
    lam ps : [Float]. lam xs : [Float].
      let np = length ps in
      -- Group the dependent variables:
      --  xs = [x_1, .., x_n,
      --        (dxdp_1)_1, .., (dxdp_1)_nx,
      --        ..,
      --        (dxdp_np)_1, .., (dxdp_np)_nx]
      -- into:
      --   [[x_1, .., x_nx],
      --    [(dxdp_1)_1, .., (dxdp_1)_nx],
      --    ..,
      --    [(dxdp_np)_1, .., (dxdp_np)_nx]]
      match divide xs nx with [xs] ++ dxsdps in
      concat
        -- original ODE model: f(x), in x' = f(x).
        (f ps xs)
        (join
           (mapi
              (lam i. lam dxdp.
                -- sensitivity model: (dx/dp_i)' = (df/dx)(dx/dp_i) + df/dp_i
                map2 addf
                  (jvp (f ps) xs dxdp)
                  (jvp (lam ps. f ps xs) ps (onehot np i)))
              dxsdps))
  in

  let lotkaVolterra = lam p. lotkaVolterra (p, 1., 1., 3.) in

  -- We parametrize our ODE model on the prey per-capita growth rate.
  let ode1 = lam ps. lam xs.
    match lotkaVolterra (head ps) (get xs 0, get xs 1) with (f1, f2) in
    [f1, f2]
  in

  -- Initial values
  let xs0 = [1., 1.] in

  -- The number of dependent variables
  let nx = length xs0 in

  -- We construct a parametrized solution trace from our ODE mode.
  let trace1 = lam ps. odeTrace (lam. lam xs. ode1 ps xs) xs0 dts in

  -- We also create a second ODE model that includes the sensitivities directly.
  let ode2 = augmentSens ode1 nx in

  -- We construct a parametrized solution trace from our ODE mode.
  let trace2 = lam ps.
    let xs0 = concat xs0 (join (create (length ps) (lam. create nx (lam. 0.)))) in
    map (lam xs. [get xs 2, get xs 3])
      (odeTrace (lam. lam xs. ode2 ps xs) xs0 dts)
  in

  -- Uniformly draw the model parameter
  let p = assume (Uniform 1.1 1.5) in

  -- Trace the sensitivites w.r.t. the parameter by differentiating the solution
  -- trace.
  let sns1 = diffTr trace1 [p] [1.] in

  -- We also compute the forward continous sensitvities.
  let sns2 = trace2 [p] in

  -- Output on a format suitable for the `dppl-plot-process` script
  let output = map2 (lam t. lam xs. (t, xs)) ts in
  map output [sns1, sns2]

mexpr

-- Number of time-steps
let n = 100 in
-- Size time-step
let dt = 0.1 in

-- infer posterior of sensitivites
let sns = infer (Importance { particles = 100 }) (model n dt) in

-- computes the distribution of the MSE between the two methods for computing
-- sensitivites.
let err = lam sns.
  infer (Importance { particles = 100 })
    (lam. let sns = map (map (lam t. t.1)) (assume sns) in -- throw away the time-stamp
        mse (join (get sns 0)) (join (get sns 1)))
in

-- plot output with `dppl-plot-process` script
printDist odePrintTraces sns

-- Or comment the previous line and uncomment the line below to output a
-- distribution over the MSE, suitable for the `dppl-plot` script.
-- printDist printFloat (err sns)
