include "../lib.mc"

let solve =
  lam f : FloatC -> FloatS -> ModS FloatS.
    lam xy0 : (FloatC, FloatS).
      lam x1 : FloatC.
        solveode (EFEC
          { add = lam x : FloatA. lam y : FloatA. addf x y
          , smul = lam x : FloatA. lam y : FloatA. mulf x y
          , stepSize = 1e-4,
          ok = lam yh : FloatP. lam y2h2 : FloatP.
            let err = subf yh y2h2 in ltf (mulf err err) 1e-4 })
          f xy0 x1

let trace =
  lam y : (Float, Float) -> Float -> (Float, Float).
    lam xy0 : (Float, Float).
      lam xs : [Float].
        tail
          (reverse
             (foldl
                (lam xys : [(Float, Float)]. lam x1 : Float.
                  cons (y (head xys) x1) xys)
                [xy0] xs))

let _h = 0.01
let _n = 500
let times = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

let x0 = 0.
let y0 = 0.

let rode = lam t : ().
  -- Process noise
  let w = assume (Wiener ()) in

  -- ODE model
  let f = lam x : FloatC. let wx : FloatS = w x in lam y : FloatS. subf (sin wx) y in

  -- IVP solution
  let y = lam xy0 : (Float, Float). lam x : Float. solve f xy0 x in

  -- Trace solution and Wiener realization
  [
    trace y (x0, y0) times,
    map (lam x : Float. (x, w x)) times
  ]

let #var"Dist_RODE" = infer (Importance { particles = 5 }) rode

mexpr

match distEmpiricalSamples #var"Dist_RODE" with (samples, weights) in
let samples = map
                (lam xs : [[(Float, Float)]].
                  map
                    (lam ts : [(Float, Float)].
                      map (lam t : (Float, Float). (t.0, [t.1]))
                        ts)
                    xs)
                samples in
printWeightedTraces samples weights

-- Local Variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck rode.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
