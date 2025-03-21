include "./baysian-parameter-estimation.mc"

let zip = lam a : [(FloatA, FloatA)]. lam b : [FloatA].
  mapi (lam i : Int. lam a : (FloatA, FloatA). (a, get b i)) a

let diff1P = lam f : FloatP -> [(FloatA, FloatA)]. lam x : FloatP.
  diffP f x 1.

let solve =
  lam f : FloatA -> (FloatA, FloatA) -> (FloatA, FloatA).
    lam y0 : (FloatA, FloatA).
      lam n : Int.
        lam h : FloatP.
          reverse
            (foldl (lam ys : [(FloatA, FloatA)]. lam i : Int.
              cons (solve f (head ys) h) ys)
               [y0] (create n (lam i : Int. i)))

mexpr

let nData = subi (length data) 1 in
let n = 200 in
let h = 0.05 in
let xs = create (addi n 1) (lam i : Int. mulf h (int2float i)) in

let trueSens =
  diff1P (lam x : FloatP. solve (ode x) y0 n h) #var"trueθ"
in

let model = lam t : ().
  -- Unknown parameter
  let #var"θ" = assume (Gaussian 1. 1.) in
  -- Measurement noise
  let #var"σ" = assume (Beta 2. 2.) in
  -- Solve IVP
  let ys = solve (ode #var"θ") y0 nData hData in
  -- Update the likelihood on observations
    iter
      (lam t : ((FloatN, FloatN), FloatN).
        match t with (y, o) in observe o (Gaussian (output y) #var"σ"))
  (zip ys data);
  -- Compute sensitivites w.r.t. θ
  let f = lam x : FloatP. solve (ode x) y0 n h in
  diffP f #var"θ" 1.
in
let dist = infer (APF { particles = 1000 }) model in

match distEmpiricalSamples dist with (samples, weights) in
writeFile "baysian-parameter-estimation-sensitivities.json"
  (jsonObject [
    ("weights", (seqToJson (map floatToJson weights))),
    ("xs", (seqToJson (map floatToJson xs))),
    ("trueSens",
     seqToJson (map (lam x : (FloatN, FloatN). floatToJson x.0) trueSens)),
    ("samples",
     let samples : [[String]] =
       (map (map (lam x : (FloatN, FloatN). floatToJson x.0)) samples)
     in
     seqToJson (map seqToJson samples))
  ]);

()
