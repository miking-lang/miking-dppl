include "test.mc"

let cpplResOfDist: all a. (a -> String) -> Int -> use RuntimeDistBase in Dist a -> CpplRes =
  lam f. lam burn. lam dist.
    match distEmpiricalSamples dist with (vs,ws) in
    let samples = subsequence vs burn (length vs) in
    let lweights =  subsequence ws burn (length ws) in
    let nc = distEmpiricalNormConst dist in
    { samples = map f samples, lweights = lweights, extra = Some nc }
