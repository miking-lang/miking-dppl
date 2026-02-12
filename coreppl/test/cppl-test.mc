include "test.mc"
--include "../src/coreppl-to-mexpr/runtime-dists.mc"

let cpplResOfDist: all a. (a -> String) -> Int -> Dist a -> CpplRes =
  lam f. lam burn. lam dist.
    match distEmpiricalSamples dist with (vs,ws) in
    let nvs = length vs in
    let samples = subsequence vs (mini nvs burn) nvs in
    let nws = length ws in
    let lweights =  subsequence ws (mini nws burn) nws in
    let nc = distEmpiricalNormConst dist in
    { samples = map f samples, lweights = lweights, extra = Some nc }

let resampleBehavior: all a. Float -> a -> Int -> (a,([Bool], Int)) = 
  lam globalProb. lam acc. lam length.
    let valu = if (assume (Bernoulli globalProb)) then
      negi 2
    else
      assume (UniformDiscrete 0 (subi length 1))
    in
    let vec = create length (lam. true) in
    (acc,(vec, valu))
