include "../lib.mc"

let solve =
  lam f : FloatA -> ModC (ModA (FloatA -> ModC (ModA (FloatA)))).
    lam xy0 : (FloatA, FloatA).
      lam x : Float.
        solveode
          (RK4EC
            { add = lam x : FloatA. lam y : FloatA. addf x y
            , smul = lam x : FloatA. lam y : FloatA. mulf x y
            , stepSize = 1e-2
            , ok =
              lam yh : Float. lam y2h2 : Float.
                let err = subf yh y2h2 in ltf (mulf err err) 1e-2
            })
          f xy0 x

let solve2 =
  lam f : FloatA -> ModC (ModA ((FloatA, FloatA) -> ModC (ModA (FloatA, FloatA)))).
    lam xy0 : (FloatA, (FloatA, FloatA)).
      lam x : Float.
        solveode
          (RK4EC
            { add = addp
            , smul = smulp
            , stepSize = 1e-2
            , ok =
              lam yh : (Float, Float). lam y2h2 : (Float, Float).
                ltf (l2normp (subp yh y2h2)) 1e-2 })
          f xy0 x

let _n = 200
let _h = 0.05
let times = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

let x0 = 0.
let y0 = 1.

-- Models nuclear half life for example
let f = lam #var"θ" : FloatA. lam x : FloatA. lam y : FloatA.
  mulf (negf #var"θ") y

-- Sensitivity with diff of solve
let s1 = lam #var"θ" : FloatA. lam x1 : Float.
  diff (lam #var"θ" : FloatA. solve (f #var"θ") (x0, y0) x1) #var"θ" 1.

-- Sensitivity with solve with diff
let s2 = lam #var"θ" : FloatA. lam x1 : Float.
  let g = lam x : FloatA. lam y : (FloatA, FloatA).
    match y with (y, s) in
    let f = lam #var"θ" : FloatA. lam y : FloatA. f #var"θ" x y in
    ( f #var"θ" y
    , addf
        (diff (f #var"θ") y s)
        (diff (lam #var"θ" : FloatA. f #var"θ" y) #var"θ" 1.) ) in
  match solve2 g (x0, (y0, 0.)) x1 with (_, (_, s)) in (0., s)

let _model = lam t : ().
  let #var"θ" = assume (Uniform 0.1 0.5) in
  [ map (lam x : Float. match s1 #var"θ" x with (_, s) in (x, [s])) times
  , map (lam x : Float. match s2 #var"θ" x with (_, s) in (x, [s])) times
  ]

let #var"Dist_sθ" = infer (Importance { particles = 10 }) _model

mexpr

match distEmpiricalSamples #var"Dist_sθ" with (samples, weights) in
let samples =
  map
    (lam x : [[(Float, [Float])]].
      map
        (lam ts : [(Float, [Float])].
          mapi (lam i : Int. lam t : (Float, [Float]). (get times i, t.1)) ts)
        x)
    samples in
printWeightedTraces samples weights

-- local variables:
-- compile-command: "cppl --seed 1 --cps partial --dppl-typecheck ode-sensitivites-two-methods-scalar.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
