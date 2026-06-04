include "../lib.mc"

-- Specialize solver
let solve =
  lam f :
    FloatC -> ModS ((FloatS, FloatS, FloatS) -> (ModS (FloatS, FloatS, FloatS))).
    lam xy0 : (FloatC, (FloatS, FloatS, FloatS)).
      lam x1 : FloatC.
        solveode (EFEC
          { add = addt
          , smul = smult
          , stepSize = 1e-4,
            ok =
              lam yh : (FloatP, FloatP, FloatP).
                lam y2h2 : (FloatP, FloatP, FloatP).
                  ltf
                    (l2normt (subt yh y2h2))
                    (mulf 3. 1e-2)
          })
          f xy0 x1

-- Creates a solution trace from a IVP solution
let trace =
  lam y : (Float, (FloatS, FloatS, FloatS)) -> ModS (Float ->
    (ModS (FloatA, (FloatS, FloatS, FloatS)))).
    lam xy0 : (Float, (Float, Float, Float)).
      lam xs : [Float].
        tail
          (reverse
             (foldl
                (lam xys : [(Float, (FloatS, FloatS, FloatS))]. lam x1 : Float.
                  match y (head xys) x1 with (x1t, y1) in
                  -- Assert that we reach the expected solution time.
                  utest x1t with x1 using eqfApprox 1.e-15 in
                  cons (x1, y1) xys)
                [xy0] xs))

let _h = 0.2
let _n = 300
let times = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

-- Parameters
let z0 = 4.
let nu = 1.
let r = 1.                      -- Cancer cell growth rate
let e = 0.34                    -- Cancer cell growth saturation parameter
let mu = 0.1                    -- Cancer cell death rate
let aP = 4.5                    -- Promotor production rate by cancer cells
let bP = 0.11                   -- Promotor decay rate by cancer cells
let aI = 0.2                    -- Inhibitor production rate by cancer cells
let bI = 0.01                   -- Inhibitor decay rate by cancer cells

-- Initial Values
let t0 = 0.

let c0 = 35.
let p0 = mulf (divf aP bP) c0
let i0 = divf (addf z0 (mulf aI c0)) bI

let rode = lam t : ().
  -- Process noise
  let w = assume (Wiener ()) in

  -- IVP solution
  let sol =
    lam #var"θ" : (FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS).
      match #var"θ" with (z0, nu, r, e, mu, aP, bP, aI, bI) in
      lam tcpi0 : (Float, (FloatS, FloatS, FloatS)).
        lam t : Float.
          -- Stochastic Process. express inhibitor production from normal tissue.
          let z = lam w : FloatS.
            mulf z0
              (subf 1.
                 (mulf
                    (mulf 2. nu)
                    (smoothdivf w (addf 1. (mulf w w))))) in

          -- ODE model
          let f1 = lam cpi : (FloatS, FloatS, FloatS).
            match cpi with (c, p, i) in
            subf
              (mulf
                 (smoothdivf
                    (mulf r c)
                    (addf 1. (mulf e c)))
                 (smoothdivf p (addf 1. i)))
              (mulf mu c) in
          let f2 = lam cpi : (FloatS, FloatS, FloatS).
            match cpi with (c, p, i) in subf (mulf aP c) (mulf bP p) in
          let f3 = lam cpi : (FloatS, FloatS, FloatS). lam w : FloatS.
            match cpi with (c, p, i) in
            subf (addf (z w) (mulf aI c)) (mulf bI i) in
          let f =
            lam t : FloatC.
              let wt : FloatS = w t in
              lam cpi : (FloatS, FloatS, FloatS).
                (f1 cpi, f2 cpi, f3 cpi wt) in

          -- Solution
          solve f tcpi0 t in

  -- Trace solution, its sensitivity, and the wiener realization
  let #var"θ" = (z0, nu, r,  e,  mu, aP, bP, aI, bI) in
  let ej      = (0., 0., 0., 0., 0., 0., 0., 1., 0.) in
  [ trace (sol #var"θ") (t0, (c0, p0, i0)) times
  , diff
      (lam #var"θ" : (FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS, FloatS).
        trace (sol #var"θ") (t0, (c0, p0, i0)) times)
      #var"θ"
      ej
  , map (lam t : Float. (t, (w t, 0., 0.))) times
  ]

let #var"Dist_RODE" = infer (Importance { particles = 5 }) rode

mexpr

match distEmpiricalSamples #var"Dist_RODE" with (samples, weights) in
let samples =
  map
    (lam x : [[(Float, (Float, Float, Float))]].
      map
        (lam ts : [(Float, (Float, Float, Float))].
          mapi (lam i : Int. lam t : (Float, (Float, Float, Float)).
            (get times i, [(t.1).0, (t.1).1, (t.1).2]))
            ts)
        x)
    samples in
printWeightedTraces samples weights

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck tumor-inhibitor-rode.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
