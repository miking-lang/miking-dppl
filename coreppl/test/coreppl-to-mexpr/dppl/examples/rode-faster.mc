include "../lib.mc"

-- let solve =
--   lam f : FloatN -> (FloatN, FloatA) -> (FloatN, FloatA).
--     lam y0 : (FloatN, FloatA).
--       lam n : Int.
--         lam h : FloatN.
--           let s = lam y0 : (FloatN, FloatA). lam x : FloatN.
--             solveode (EF {add = addp, smul = smulp, stepSize = 1e-3}) f y0 x
--           in
--           reverse
--             (foldl
--                (lam ys : [(FloatN, FloatA)]. lam i : Int.
--                  cons (s (head ys) h) ys)
--                [y0]
--                (create n (lam i : Int. i)))

let solve =
  lam f : FloatN -> (FloatN, FloatA) -> (FloatN, FloatA).
    lam y0 : (FloatN, FloatA).
      lam x : FloatN.
        solveode (EF {add = addp, smul = smulp, stepSize = 1e-4}) f y0 x

let solve2 =
  lam f : FloatN -> (FloatN, FloatA) -> (FloatN, FloatA).
    lam y0 : (FloatN, FloatA).
      lam x : FloatN.
        solveode (EFA {n = 10, add = addp, smul = smulp, stepSize = 1e-4}) f y0 x


-- Time-steps
let h = 0.01
let n  = 500
let hs = create n (lam i : Int. h)
let xs = create (addi n 1) (lam i : Int. mulf (int2float i) h)

let rode = lam t : ().
  -- Process noise
  let w = assume (Wiener ()) in

  -- RODE model
  let f = lam x : FloatN. lam y : (FloatN, FloatA). (1., subf (sin (w y.0)) y.1) in

  -- RODE solution
  let ys =
    reverse
      (foldl
         (lam ys : [(FloatN, FloatA)]. lam h : FloatN.
           cons (solve f (head ys) h) ys)
         [(0., 0.)]
         hs)
  in

  -- Output in a format suitable for the plotting script
  let ys = mapi (lam i : Int. lam y : (FloatN, FloatA). (get xs i, [y.1])) ys in
  let ws = map (lam x : FloatN. (x, [w x])) xs in
  [ys, ws]

mexpr

let dist = infer (Importance { particles = 5 }) rode in

match distEmpiricalSamples dist with (samples, weights) in
writeFile "rode-faster.json"
  (let write = lam j : Int.
    seqToJson
      (create (length samples)
         (lam i : Int.
           seqToJson
             (map (lam s : (FloatN, [FloatN]). floatToJson (head s.1))
                (get (get samples i) j))))
   in
   jsonObject [
     ("xs", (seqToJson (map floatToJson xs))),
     ("ys", write 0),
     ("ws", write 1)
   ]);

-- Just make sure that inference completes
match distEmpiricalSamples dist with (_, weights) in
utest foldl addf 0. (map exp weights) with 1. using eqfApprox 1e-3 in

()
