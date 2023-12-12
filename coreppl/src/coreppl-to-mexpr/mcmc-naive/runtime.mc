include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

-- In naive MCMC, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam v. lam state. modref state (addf (deref state) v)

-- General inference algorithm for naive MCMC
let run : all a. Unknown -> (State -> a) -> use RuntimeDistBase in Dist a =
  lam config. lam model.
  use RuntimeDist in

  recursive let mh : [Float] -> [a] -> Int -> ([Float], [a]) =
    lam weights: [Float]. lam samples: [a]. lam iter: Int.
      if leqi iter 0 then (weights, samples)
      else
        let state: State = ref 0. in
        let sample: a = model state in
        let weight: Float = deref state in
        let prevWeight: Float = head weights in
        let prevSample: a = head samples in
        let logMhAcceptProb: Float = minf 0. (subf weight prevWeight) in
        let iter: Int = subi iter 1 in
        if bernoulliSample (exp logMhAcceptProb) then
          mcmcAccept ();
          mh (cons weight weights) (cons sample samples) iter
        else
          mh (cons prevWeight weights) (cons prevSample samples) iter
  in

  let runs = config.iterations in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit runs;

  -- Draw an initial sample first
  let state = ref 0. in
  let sample = model state in
  let weight = deref state in
  let iter: Int = subi runs 1 in

  -- Draw remaining samples
  let res = mh [weight] [sample] iter in

  -- Reverse to get the correct order
  let res = match res with (weights,samples) in
    (reverse weights, reverse samples)
  in

  -- Return
  constructDistEmpirical res.1 (create runs (lam. 1.))
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
