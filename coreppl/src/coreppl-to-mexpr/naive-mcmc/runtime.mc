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
let run : all a. (State -> a) -> (Res a -> ()) -> () = lam model. lam printResFun.

  -- Read number of runs and sweeps
  match monteCarloArgs () with (runs, sweeps) in

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
          mh (cons weight weights) (cons sample samples) iter
        else
          mh (cons prevWeight weights) (cons prevSample samples) iter
  in

  -- Repeat once for each sweep
  repeat (lam.

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

      -- Print
      printResFun res

    ) sweeps

let printRes : all a. (a -> String) -> Res a -> () = lam printFun. lam res.
  -- NOTE(dlunde,2022-05-23): I don't think printing the norm. const makes
  -- sense for MCMC
  -- printLn (float2string (normConstant res.0));
  printSamples printFun res.0 res.1
