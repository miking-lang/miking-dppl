include "math.mc"

type ODEFnArg = {
  t : Float,                    -- free variable t
  xs : [Float],                 -- states x(t) ∈ ℝ^nx
  us : [Float],                 -- inputs u(t) ∈ ℝ^nu
  ps : [Float]                  -- parameters p ∈ ℝ^np
}

let _checkFinalTimeAndStepSize = lam tf. lam stepSize.
  if ltf tf 0. then
    error "odeSolver: negative final time"
  else
    if ltf stepSize 0. then
      error "odeSolver: negative step size"
    else ()

let _map2 = lam f. lam seq1. lam seq2.
  create (length seq1) (lam i. f (get seq1 i) (get seq2 i))

recursive let _solveFixedH = lam step. lam stepSize. lam arg.
  if gtf (absf arg.t) 0. then
    let h = minf arg.t stepSize in
    let xs = step h arg in
    _solveFixedH step stepSize { arg with t = subf arg.t h, xs = xs }
  else arg.xs
end

let _solveFixed = lam step. lam stepSize. lam arg.
  if eqf arg.t 0. then arg.xs else _solveFixedH step stepSize arg


-- ┌─────────┐
-- │ Solvers │
-- └─────────┘

type ODESolve = (Float -> [Float] -> [Float]) -> [Float] -> Float -> [Float]


-- ┌───────────────┐
-- │ Euler-Forward │
-- └───────────────┘

let odeSolverEFSolve : { stepSize : Float } -> ODESolve =
  lam opt. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf opt.stepSize;
    let integrate =
      lam h. lam arg. _map2 addf arg.xs (map (mulf opt.stepSize) (f arg.t arg.xs))
    in
    _solveFixed integrate opt.stepSize { t = tf, xs = x0, us = [], ps = [] }


-- ┌──────────────────────────┐
-- │ Runge-Kutta Fourth Order │
-- └──────────────────────────┘

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `f`    : function that computes x'(t) ∈ ℝ^nx
-- `h`    : step-size of the integration
-- `arg`  : Argument to `f`.
let odeSolverRK4Integrate : (ODEFnArg -> [Float]) -> Float -> ODEFnArg -> [Float] =
  lam f. lam h. lam arg.
    match arg with { t = t, xs = xs } in
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f arg in
    let tmps = _map2 (lam x. lam k. addf x (mulf h2 k)) xs k1s in
    let k2s = f { arg with t = t2, xs = tmps } in
    let tmps = _map2 (lam x. lam k. addf x (mulf h2 k)) xs k2s in
    let k3s = f { arg with t = t2, xs = tmps } in
    let tmps = _map2 (lam x. lam k. addf x (mulf h k)) xs k3s in
    let k4s = f { arg with t = th, xs = tmps } in
    mapi
      (lam i. lam x.
        let k1 = get k1s i in
        let k2 = mulf 2. (get k2s i) in
        let k3 = mulf 2. (get k3s i) in
        let k4 = get k4s i in
        addf x (mulf h (divf (addf k1 (addf k2 (addf k3 k4))) 6.)))
      xs

let odeSolverRK4Solve : { stepSize : Float } -> ODESolve =
  lam opt. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf opt.stepSize;
    _solveFixed
      (odeSolverRK4Integrate (lam arg. f arg.t arg.xs))
      opt.stepSize
      { t = tf, xs = x0, us = [], ps = [] }

mexpr

recursive let eqSeq = lam eq. lam seq1. lam seq2.
  switch (seq1, seq2)
    case ([],[]) then true
    case ([a] ++ seq1,[b] ++ seq2) then
      if eq a b then eqSeq eq seq1 seq2
      else false
    case _ then false
  end
in

-- Harmonic oscillator
let f = lam t. lam xs.
  let x = get xs 0 in
  let v = get xs 1 in
  [
    v,
    (negf x)
  ]
in

let x0 = [1., 0.] in

-- Analytical solution
let xs = lam t. [cos t, negf (sin t)] in

let eq = eqSeq (eqfApprox 1e-7) in
utest xs 0. with x0 using eq in

-- ┌───────────────────────────┐
-- │ Test Euler Forward Solver │
-- └───────────────────────────┘

let eq = eqSeq (eqfApprox 1e-3) in
utest xs 0. with x0 using eq in

let opt = { stepSize = 1e-4 } in

let xsHat = odeSolverEFSolve opt f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in


-- ┌──────────────────────────────────────┐
-- │ Test Runge-Kutta Fourth Order Solver │
-- └──────────────────────────────────────┘

let eq = eqSeq (eqfApprox 1e-7) in
utest xs 0. with x0 using eq in

let opt = { stepSize = 1e-4 } in

let xsHat = odeSolverRK4Solve opt f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in

()
