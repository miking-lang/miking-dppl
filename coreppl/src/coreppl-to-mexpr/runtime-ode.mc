include "math.mc"

type ODEFnArg = {
  t : Float,                    -- free variable t
  xs : [Float],                 -- states x(t) ∈ ℝ^nx
  us : [Float],                 -- inputs u(t) ∈ ℝ^nu
  ps : [Float]                  -- parameters p ∈ ℝ^np
}

-- ** INTEGRATION STEP FUNCTION
-- Fourth order explicit Runge Kutta (RK4) ODEs integration step.
-- `f`    : function that computes x'(t) ∈ ℝ^nx
-- `xidxs` : index vector of size nx
-- `h`    : step-size of the integration
-- `t`    : value of t
-- `xs`   : values of the states x(t) ∈ ℝ^nx
-- `us`   : inputs u ∈ ℝ^nu
-- `ps`   : parameters p ∈ ℝ^np
let odeSolverRK4Step :
  (ODEFnArg -> [Float])
   -> [Int]
     -> Float
       -> Float
         -> [Float]
           -> [Float]
             -> [Float]
               -> [Float] =
  lam f. lam xidxs. lam h. lam t. lam xs. lam us. lam ps.
    let _map2 = lam f. lam seq1. lam seq2.
      create (length seq1) (lam i. f (get seq1 i) (get seq2 i))
    in
    let h2 = divf h 2. in
    let t2 = addf t h2 in
    let th = addf t h in
    let k1s = f { t = t, xs = xs, us = us, ps = ps } in
    let tmps = _map2 (lam x. lam k. addf x (mulf h2 k)) xs k1s in
    let k2s = f { t = t2, xs = tmps, us = us, ps = ps} in
    let tmps = _map2 (lam x. lam k. addf x (mulf h2 k)) xs k2s in
    let k3s = f { t = t2, xs = tmps, us = us, ps = ps } in
    let tmps = _map2 (lam x. lam k. addf x (mulf h k)) xs k3s in
    let k4s = f { t = th, xs = tmps, us = us, ps = ps } in
    map
      (lam i.
        let k1 = get k1s i in
        let k2 = mulf 2. (get k2s i) in
        let k3 = mulf 2. (get k3s i) in
        let k4 = get k4s i in
        let x = get xs i in
          addf x (mulf h (divf (addf k1 (addf k2 (addf k3 k4))) 6.)))
      xidxs

let odeSolverRK4Solve :
  { stepSize : Float }
    -> (Float -> [Float] -> [Float])
       -> [Float]
         -> Float
           -> [Float] =
  lam opt. lam f. lam x0. lam tf.
    if ltf tf 0. then
      error "odeSolverRK4Solve: negative final time"
    else
      if ltf opt.stepSize 0. then
        error "odeSolverRK4Solve: negative step size"
      else
        if eqf tf 0. then x0
        else
          let xidxs = mapi (lam i. lam. i) x0 in
          recursive let recur = lam i. lam xs.
            if gtf (absf i) 0. then
              let h = minf i opt.stepSize in
              let xs =
                odeSolverRK4Step
                  (lam arg. f arg.t arg.xs) xidxs h (subf tf i) xs [] []
              in
              recur (subf i h) xs
            else xs
          in
          recur tf x0

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


-- ┌─────────────────┐
-- │ Test RK4 Solver │
-- └─────────────────┘

utest xs 0. with x0 using eq in

let opt = { stepSize = 1e-4 } in

let xsHat = odeSolverRK4Solve opt f x0 in
utest xsHat 1. with xs 1. using eq in
utest xsHat 2. with xs 2. using eq in
utest xsHat 3. with xs 3. using eq in

()
