include "math.mc"

let model1 = lam.
  let a = assume (Gamma 2. 3.) in
  let b = assume (Exponential 2.) in
  let c = assume (Beta 2. 2.) in
  let d = assume (Gaussian 1. 2.) in
  let e = assume (Uniform 2. 3.) in
  foldl addf 0. [a, b, c, d, e]

mexpr
let initialAcc = (1000, 0) in
let out = ref initialAcc in

let resampleBehavior: (Int, Int) -> Int -> ((Int, Int),([Bool], Int)) = 
  lam acc. lam length.
    let valu = assume (UniformDiscrete 0 (subi length 1)) in
    let vec = create length (lam. true) in
    let acc = (acc.0, addi acc.1 1) in
    modref out acc;
    (acc,(vec, valu))
in

let res = infer (LightweightMCMC { continue = (lam. initialAcc, lam x. lam. lam. ((subi x.0 1, x.1), neqi x.0 0)), resampleBehavior = resampleBehavior }) model1 in
utest (deref out) with (0, 1000) in

()