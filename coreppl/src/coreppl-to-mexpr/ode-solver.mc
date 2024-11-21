include "tuple.mc"
include "math.mc"

recursive let _repeat = lam f. lam n. lam acc.
  if leqi n 0 then acc
  else _repeat f (subi n 1) (f acc)
end

let _checkFinalTimeAndStepSize = lam tf. lam h.
  if ltf tf 0. then
    error "odeSolver: negative final time"
  else
    if ltf h 0. then
      error "odeSolver: negative step size"
    else ()

recursive let _solveFixedH
  : all a. (Float -> Float -> a -> a) -> Float -> Float -> a -> a
  = lam step. lam h. lam t. lam xs.
    if gtf (absf t) 0. then
      let h = minf t h in
      let xs = step h t xs in
      _solveFixedH step h (subf t h) xs
    else xs
end

let _solveFixed
  : all a. (Float -> Float -> a -> a) -> Float -> Float -> a -> a
  = lam step. lam h. lam t. lam xs.
  if eqf t 0. then xs else _solveFixedH step h t xs


-- ┌───────────────┐
-- │ Euler-Forward │
-- └───────────────┘

-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `h`    : Step-size of the integration
-- `f`    : Function that computes x'(t)
-- `x0`   : Initial value x(0)
-- `tf`   : Final time
-- returns the solution x(tf) at the time tf
let odeSolverEFSolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> (Float -> a -> a) -> a -> Float -> a
  = lam add. lam smul. lam h. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf h;
    let integrate =
      lam h. lam t. lam x. add x (smul h (f t x))
    in
    _solveFixed integrate h tf x0

-- ┌────────────────────────┐
-- │ Euler-Forward Averaged │
-- └────────────────────────┘

-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `h`    : Step-size of the integration
-- `n`    : The number if intervals to split `h` when averaging over t
-- `f`    : Function that computes x'(t)
-- `x0`   : Initial value x(0)
-- `tf`   : Final time
-- returns the solution x(tf) at the time tf
let odeSolverEFASolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> Int -> (Float -> a -> a) -> a -> Float -> a
  = lam add. lam smul. lam h. lam n. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf h;
    let dt = divf h (int2float n) in
    let integrate =
      lam h. lam t. lam x.
        let f =
          _repeat
            (lam acc.
              match acc with (t, sum) in
              let t = addf t dt in
              (t, add sum (smul h (f t x))))
            (subi n 1)
            (t, smul h (f t x))
        in
        add x (smul (divf 1. (int2float n)) f.1)
    in
    _solveFixed integrate h tf x0

-- ┌──────────────────────────┐
-- │ Runge-Kutta Fourth Order │
-- └──────────────────────────┘

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `f`    : Function that computes x'(t)
-- `h`    : Step-size of the integration
-- `t`    : Time
-- `x`   : x(t)
-- returns x(t+h)
let odeSolverRK4Integrate
  : all a. (a -> a -> a) -> (Float -> a -> a) -> (Float -> a -> a) -> Float -> Float -> a -> a
  = lam add. lam smul. lam f. lam h. lam t. lam x.
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f t x in
    let tmps = add x (smul h2 k1s) in
    let k2s = f t2 tmps in
    let tmps = add x (smul h2 k2s) in
    let k3s = f t2 tmps in
    let tmps = add x (smul h k3s) in
    let k4s = f th tmps in
      add
        x
        (smul
           (divf h 6.)
           (add
              k1s
              (add
                 (smul 2. k2s)
                 (add (smul 2. k3s) k4s))))


-- Fourth order explicit Runge Kutta (RK4) ODEs solver.
-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `h`    : Step-size of the integration
-- `f`    : Function that computes x'(t)
-- `x0`   : Initial value x(0)
-- `tf`   : Final time
-- returns the solution x(tf) at the time tf
let odeSolverRK4Solve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> (Float -> a -> a) -> a -> Float -> a
  = lam add. lam smul. lam h. lam f. lam x0. lam tf.
    _checkFinalTimeAndStepSize tf h;
    _solveFixed
      (odeSolverRK4Integrate add smul f)
      h
      tf
      x0

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

let xsHat = odeSolverEFSolve opt.add opt.smul opt.stepSize f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in


-- ┌──────────────────────────────────────┐
-- │ Test Runge-Kutta Fourth Order Solver │
-- └──────────────────────────────────────┘

let eq = eq2 (eqfApprox 1e-7) in
utest xs 0. with x0 using eq in

let xsHat = odeSolverRK4Solve opt.add opt.smul opt.stepSize f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in

()
