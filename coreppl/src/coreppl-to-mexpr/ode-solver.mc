include "tuple.mc"
include "math.mc"

let _checkFinalTimeAndStepSize = lam tf. lam stepSize.
  if ltf tf 0. then
    error "odeSolver: negative final time"
  else
    if ltf stepSize 0. then
      error "odeSolver: negative step size"
    else ()

recursive let _solveFixedH = lam step. lam stepSize. lam arg.
  if gtf (absf arg.t) 0. then
    let h = minf arg.t stepSize in
    let xs = step h arg in
    _solveFixedH step stepSize { arg with t = subf arg.t h, xs = xs }
  else arg.xs
end

let _solveFixed = lam step. lam stepSize. lam arg.
  if eqf arg.t 0. then arg.xs else _solveFixedH step stepSize arg


-- ┌───────────────┐
-- │ Euler-Forward │
-- └───────────────┘

let odeSolverEFSolve =
  lam opt. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf opt.stepSize;
    let integrate =
      lam h. lam arg. opt.add arg.xs (opt.smul opt.stepSize (f arg.t arg.xs))
    in
    _solveFixed integrate opt.stepSize { t = tf, xs = x0 }


-- ┌──────────────────────────┐
-- │ Runge-Kutta Fourth Order │
-- └──────────────────────────┘

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `f`    : function that computes x'(t) ∈ ℝ^nx
-- `h`    : step-size of the integration
-- `arg`  : Argument to `f`.
let odeSolverRK4Integrate =
  lam opt. lam f. lam h. lam arg.
    match arg with { t = t, xs = xs } in
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f arg in
    let tmps = opt.add xs (opt.smul h2 k1s) in
    let k2s = f { arg with t = t2, xs = tmps } in
    let tmps = opt.add xs (opt.smul h2 k2s) in
    let k3s = f { arg with t = t2, xs = tmps } in
    let tmps = opt.add xs (opt.smul h k3s) in
    let k4s = f { arg with t = th, xs = tmps } in
      opt.add
        xs
        (opt.smul
           (divf h 6.)
           (opt.add
              k1s
              (opt.add
                 (opt.smul 2. k2s)
                 (opt.add
                    (opt.smul 2. k3s)
                    k4s))))


let odeSolverRK4Solve =
  lam opt. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf opt.stepSize;
    _solveFixed
      (odeSolverRK4Integrate opt (lam arg. f arg.t arg.xs))
      opt.stepSize
      { t = tf, xs = x0 }

mexpr

let eq2 = lam eq. tupleEq2 eq eq in

-- Harmonic oscillator
let f = lam t. lam xs. (xs.1, (negf xs.0)) in

let x0 = (1., 0.) in

-- Analytical solution
let xs = lam t. (cos t, negf (sin t)) in

let eq = eq2 (eqfApprox 1e-7) in
utest xs 0. with x0 using eq in

let opt = {
  stepSize = 1e-4,
  add = lam a. lam b. (addf a.0 b.0, addf a.1 b.1),
  smul = lam s. lam a. (mulf s a.0, mulf s a.1)
} in

-- ┌───────────────────────────┐
-- │ Test Euler Forward Solver │
-- └───────────────────────────┘

let eq = eq2 (eqfApprox 1e-3) in
utest xs 0. with x0 using eq in

let xsHat = odeSolverEFSolve opt f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in


-- ┌──────────────────────────────────────┐
-- │ Test Runge-Kutta Fourth Order Solver │
-- └──────────────────────────────────────┘

let eq = eq2 (eqfApprox 1e-7) in
utest xs 0. with x0 using eq in

let xsHat = odeSolverRK4Solve opt f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in

()
