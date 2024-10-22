include "../lib.mc"

-- Specialize solveode
let solve =
  lam f : FloatN -> [FloatN] -> [FloatN]. lam y0 : [FloatN]. lam n : Int. lam dx : FloatN.
  let s = lam f : FloatN -> [FloatN] -> [FloatN]. lam y0 : [FloatN]. lam x : FloatN.
    solveode (RK4 { add = adds, smul = smuls, stepSize = 1e-3 }) f y0 x
  in
  reverse
    (foldl (lam ys : [[FloatN]]. lam dx : FloatN. cons (s f (head ys) dx) ys)
       [y0] (create n (lam i : Int. dx)))

let rode = lam n : Int. lam dx : FloatN. lam t : ().

  -- Process noise
  let w = assume (Wiener ()) in

  -- RODE model
  let f = lam x : FloatN. lam y : [FloatN].
    match y with [x, y] in [1., subf (sin (w x)) y]
  in

  -- RODE solution
  let ys = solve f [0., 0.] n dx in

  -- Output in a format suitable for the plotting script
  let ys =
    mapi (lam i : Int. lam y : [FloatN]. (mulf (int2float i) dx, [get y 1])) ys
  in
  let ws = create n (lam i : Int. let x = mulf (int2float i) dx in (x, [w x])) in
  [ys, ws]

mexpr

-- Print the distribution of RODE solutions and process noise.
-- plot output with `dppl-plot-process` script. I.e., from the repository root
-- ./build/cppl --effect-frontend coreppl/test/coreppl-to-mexpr/dppl/examples/rode.mc && ./out | ./scripts/dppl-plot-process && rm ./out
let printDist = false in

let dist = infer (Importance { particles = 5 }) (rode 500 0.01) in

-- Just make sure that inference completes
match distEmpiricalSamples dist with (_, weights) in
utest foldl addf 0. (map exp weights) with 1. in

(if printDist then printODETracesDist dist else ())
