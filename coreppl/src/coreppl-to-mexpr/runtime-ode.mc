include "tuple.mc"
include "math.mc"

recursive let _repeat = lam f. lam n. lam acc.
  if leqi n 0 then acc
  else _repeat f (subi n 1) (f acc)
end

let _checkStepSize = lam h.
  if ltf h 0. then
    error "odeSolver: non positive step-size"
  else ()

let _solveFixed
  : all a. (a -> Float -> Float -> a) -> (Float, a) -> Float -> Float -> (Float, a)
  = lam step. lam tx0. lam h. lam t1.
    if eqf tx0.0 t1 then tx0 else
      match tx0 with (t0, x0) in
      let sgn = if leqf t0 t1 then 1. else -1. in
      let h = mulf sgn h in
      recursive let recur = lam t. lam x.
        if leqf (mulf sgn (subf t1 (addf t h))) 0. then
          let h = subf t1 t in
          (addf t h, step x t h)
        else
          recur (addf t h) (step x t h)
      in recur t0 x0

let _solveFixedHalfStepErrControl
  : all a. (a -> Float -> Float -> a) ->
          (a -> a -> Bool) ->
           (Float, a) -> Float -> Float -> (Float, a)
  = lam step. lam ok.
    lam tx0. lam h. lam t1.
    if eqf tx0.0 t1 then tx0
    else
      match tx0 with (t0, x0) in
      let sgn = if leqf t0 t1 then 1. else -1. in
      let h = mulf sgn h in
      let trystep = lam x. lam t. lam h.
        let h2 = divf h 2. in
        let xh2 = step x t h2 in
        let x2h2 = step xh2 (addf t h2) h2 in
        let xh = step x t h in
        if ok x2h2 xh then (addf t h, x2h2)
        else (t, x)
      in
      recursive let recur = lam t. lam x.
        if leqf (mulf sgn (subf t1 (addf t h))) 0. then
          let h = subf t1 t in
          trystep x t h
        else
          match trystep x t h with (tnext, xnext) in
          if eqf t tnext then (t, x)
          else recur tnext xnext
      in recur t0 x0

-- ┌───────────────┐
-- │ Euler-Forward │
-- └───────────────┘

-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `h`    : Step-size of the integration
-- `ok`   : Optional error control function, where `ok xh2 xh` compares the
--          solution at half the step-size (xh2) to the solution at the given
--          step-size (xh).
-- `f`    : Right-hand side in x'(t) = f(t, x(t))
-- `tx0`  : Initial value (t0, x(t0))
-- `t1`   : Final time
--
-- returns the solution x(t1) at the time t1 and t1.
let odeSolverEFSolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float ->
    (Float -> a -> a) -> (Float, a) -> Float -> (Float, a)
  = lam add. lam smul. lam h.
    _checkStepSize h;
    lam f. lam tx0. lam t1.
      let integrate = lam x. lam t. lam h. add x (smul h (f t x)) in
      _solveFixed integrate tx0 h t1

-- See `odeSolverEFSolve`.
--
-- `ok`   : Error control function, where `ok xh2 xh` compares the
--          solution at half the step-size (xh2) to the solution at the given
--          step-size (xh).
--
-- returns the solution x(t1) at the time t1 and t1, or at the first time where
-- the error control fails.
let odeSolverEFHalfStepErrControlSolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> (a -> a -> Bool) ->
    (Float -> a -> a) -> (Float, a) -> Float -> (Float, a)
  = lam add. lam smul. lam h. lam ok.
    _checkStepSize h;
    lam f. lam tx0. lam t1.
      let integrate = lam x. lam t. lam h. add x (smul h (f t x)) in
      _solveFixedHalfStepErrControl integrate ok tx0 h t1

-- ┌────────────────────────┐
-- │ Euler-Forward Averaged │
-- └────────────────────────┘

-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `h`    : Step-size of the integration
-- `n`    : The number if intervals to split `h` when averaging over t
-- `f`    : Right-hand side in x'(t) = f(t, x(t))
-- `tx0`  : Initial value (t0, x(t0))
-- `t1`   : Final time
-- returns the solution x(t1) at the time t1 and t1
let odeSolverEFASolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> Int ->
    (Float -> a -> a) -> (Float, a) -> Float -> (Float, a)
  = lam add. lam smul. lam h. lam n.
    lam f. lam tx0. lam t1.
      _checkStepSize h;
      let dt = divf h (int2float n) in
      let integrate =
        lam x. lam t. lam h.
          let f =
            _repeat
              (lam acc.
                match acc with (t, sum) in
                let t = addf t dt in
                (t, add sum (smul h (f t x))))
              (subi n 1)
              (t, smul h (f t x)) in
          add x (smul (divf 1. (int2float n)) f.1) in
      _solveFixed integrate tx0 h t1

-- ┌──────────────────────────┐
-- │ Runge-Kutta Fourth Order │
-- └──────────────────────────┘

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `add`  : Implements vector addition
-- `smul` : Implements scalar multiplication
-- `f`    : Right-hand side in x'(t) = f(t, x(t))
-- `x`    : x(t)
-- `t`    : Time
-- `h`    : Step-size of the integration
-- returns x(t+h)
let odeSolverRK4Integrate
  : all a. (a -> a -> a) -> (Float -> a -> a) -> (Float -> a -> a) -> a -> Float -> Float -> a
  = lam add. lam smul. lam f. lam x. lam t. lam h.
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f t x in
    let x1 = add x (smul h2 k1s) in
    let k2s = f t2 x1 in
    let x2 = add x (smul h2 k2s) in
    let k3s = f t2 x2 in
    let x3 = add x (smul h k3s) in
    let k4s = f th x3 in
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
-- See `odeSolveEFSolve`.
let odeSolverRK4Solve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float ->
    (Float -> a -> a) -> (Float, a) -> Float -> (Float, a)
  = lam add. lam smul. lam h.
    _checkStepSize h;
    lam f. lam tx0. lam t1.
      _solveFixed (odeSolverRK4Integrate add smul f) tx0 h t1

-- See `odeSolveEFHalfStepErrControlSolve`.
let odeSolverRK4HalfStepErrControlSolve
  : all a. (a -> a -> a) -> (Float -> a -> a) -> Float -> (a -> a -> Bool) ->
    (Float -> a -> a) -> (Float, a) -> Float -> (Float, a)
  = lam add. lam smul. lam h. lam ok.
    _checkStepSize h;
    lam f. lam tx0. lam t1.
      _solveFixedHalfStepErrControl
        (odeSolverRK4Integrate add smul f) ok tx0 h t1
mexpr

let eq2 = lam eq. tupleEq2 eq eq in

-- ┌───────────────────────┐
-- │ WITHOUT ERROR CONTROL │
-- └───────────────────────┘

-- Harmonic oscillator
let f = lam t. lam x. (x.1, (negf x.0)) in

-- Analytic solution
let x = lam t. (t, (sin t, cos t)) in

let opt = {
  stepSize = 1e-5,
  add = lam a. lam b. (addf a.0 b.0, addf a.1 b.1),
  smul = lam s. lam a. (mulf s a.0, mulf s a.1) } in

let eq = lam tol. tupleEq2 (eqfApprox 1e-16) (eq2 (eqfApprox tol)) in

-- ┌───────────────────────────┐
-- │ Test Euler Forward Solver │
-- └───────────────────────────┘


let solve = odeSolverEFSolve opt.add opt.smul opt.stepSize f in

let xHat = solve (x -1.) in
utest xHat -1. with x -1. in
utest xHat 1. with x 1. using eq 1e-5 in
utest xHat 2. with x 2. using eq 1e-4 in
utest xHat 3. with x 3. using eq 1e-4 in

let xHat = solve (x 1.) in
utest xHat 1. with x 1. in
utest xHat -1. with x -1. using eq 1e-5 in
utest xHat -2. with x -2. using eq 1e-4 in
utest xHat -3. with x -3. using eq 1e-4 in

-- ┌──────────────────────────────────────┐
-- │ Test Runge-Kutta Fourth Order Solver │
-- └──────────────────────────────────────┘

let solve = odeSolverRK4Solve opt.add opt.smul opt.stepSize f in

let xHat = solve (x -1.) in
utest xHat -1. with x -1. in
utest xHat 1. with x 1. using eq 1e-11 in
utest xHat 2. with x 2. using eq 1e-11 in
utest xHat 3. with x 3. using eq 1e-11 in

let xHat = solve (x 1.) in
utest xHat 1. with x 1. in
utest xHat -1. with x -1. using eq 1e-11 in
utest xHat -2. with x -2. using eq 1e-11 in
utest xHat -3. with x -3. using eq 1e-11 in

-- ┌────────────────────┐
-- │ WITH ERROR CONTROL │
-- └────────────────────┘

-- ODE RHS
let f = lam t. lam x. mulf x x in

-- Analytic solution (for x0 = 1)
let x = lam t. (t, divf 1. (subf 1. t)) in

let opt = {
  stepSize = 1e-5,
  add = addf,
  smul = mulf
} in

let ok = lam x2h2. lam xh. ltf (absf (subf xh x2h2)) 1e-4 in

let eq = lam tol. tupleEq2 (eqfApprox 1e-16) (eqfApprox tol) in

-- ┌───────────────────────────┐
-- │ Test Euler Forward Solver │
-- └───────────────────────────┘

let xHat =
  odeSolverEFHalfStepErrControlSolve opt.add opt.smul opt.stepSize ok f (x 0.)
in

utest xHat 0. with x 0. in
utest xHat 0.1 with x 0.1 using eq 1e-6 in
utest xHat 0.2 with x 0.2 using eq 1e-5 in
utest xHat 0.3 with x 0.3 using eq 1e-5 in
utest xHat 0.4 with x 0.4 using eq 1e-5 in
utest xHat 0.5 with x 0.5 using eq 1e-4 in
utest xHat 0.6 with x 0.6 using eq 1e-4 in
utest xHat 0.7 with x 0.7 using eq 1e-4 in
utest xHat 0.8 with x 0.8 using eq 1e-3 in
utest xHat 0.9 with x 0.9 using eq 1e-2 in
utest xHat 0.99 with x 0.99 using eq 1e-0 in
utest
  let t1 = (xHat 0.999).0 in
  and (ltf 0.99 t1) (ltf t1 0.999)
  with true in
utest xHat 0.999 with x (xHat 0.999).0 using eq 1e-0 in

utest xHat -1. with x -1. using eq 1e-6 in
utest xHat -2. with x -2. using eq 1e-6 in
utest xHat -3. with x -3. using eq 1e-6 in

-- ┌──────────────────────────────────────┐
-- │ Test Runge-Kutta Fourth Order Solver │
-- └──────────────────────────────────────┘

let xHat =
  odeSolverRK4HalfStepErrControlSolve opt.add opt.smul opt.stepSize ok f (x 0.)
in

utest xHat 0. with x 0. in
utest xHat 0.1 with x 0.1 using eq 1e-13 in
utest xHat 0.2 with x 0.2 using eq 1e-13 in
utest xHat 0.3 with x 0.3 using eq 1e-12 in
utest xHat 0.4 with x 0.4 using eq 1e-12 in
utest xHat 0.5 with x 0.5 using eq 1e-11 in
utest xHat 0.6 with x 0.6 using eq 1e-11 in
utest xHat 0.7 with x 0.7 using eq 1e-11 in
utest xHat 0.8 with x 0.8 using eq 1e-10 in
utest xHat 0.9 with x 0.9 using eq 1e-9 in
utest xHat 0.999 with x 0.999 using eq 1e-5 in
utest
  let t1 = (xHat 0.9999).0 in
  and (ltf 0.999 t1) (ltf t1 0.9999)
  with true in
utest xHat 0.9999 with x (xHat 0.9999).0 using eq 1e-4 in

utest xHat -1. with x -1. using eq 1e-12 in
utest xHat -2. with x -2. using eq 1e-12 in
utest xHat -3. with x -3. using eq 1e-12 in

()
