include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"
include "option.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

-- Any-type, used for traces
type Any = ()


-- In aligned MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State = {

  -- The weight of the current execution
  weight: Ref Float,

  -- The weight of reused values in the current execution
  weightReused: Ref Float,

  -- The aligned trace for this execution
  alignedTrace: Ref [Any],

  -- Number of encountered assumes (unaligned and aligned)
  traceLength: Ref Int,

  -- The previous aligned trace, with potentially invalidated samples
  oldAlignedTrace: Ref [Option Any],

  -- Aligned trace length (a constant, determined at the first run)
  alignedTraceLength: Ref Int

}

-- NOTE(dlunde,2022-05-23): The below implementation does not
-- work with ropes for some reason (segfaults). We must therefore use lists.
let emptyList = toList []

-- State (reused throughout inference)
let state: State = {
  weight = ref 0.,
  weightReused = ref 0.,
  alignedTrace = ref emptyList,
  traceLength = ref 0,
  oldAlignedTrace = ref emptyList,
  alignedTraceLength = ref (negi 1)
}


let updateWeight = lam v.
  let weight = state.weight in
  modref weight (addf (deref weight) v)

-- Procedure at aligned samples
let sampleAligned: all a. Dist a -> a = lam dist.
  let oldAlignedTrace: [Option Any] = deref state.oldAlignedTrace in
  let newSample: () -> Any = lam. unsafeCoerce (dist.sample ()) in
  let sample: Any =
    match oldAlignedTrace with [sample] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      match sample with Some sample then
        let s: a = unsafeCoerce sample in
        modref state.weightReused
          (addf (deref state.weightReused) (dist.logObserve s));
        sample
      else newSample ()

    -- This case should only happen in the first run when there is no previous
    -- aligned trace
    else newSample ()

  in
  modref state.alignedTrace (cons sample (deref state.alignedTrace));
  unsafeCoerce sample

-- Function to propose aligned trace changes between MH iterations.
let modTrace: () -> () = lam.

  let alignedTraceLength: Int = deref state.alignedTraceLength in

  -- One index must always change
  let invalidIndex: Int = uniformDiscreteSample 0 (subi alignedTraceLength 1) in

  -- Enable global modifications in 20% (TODO: Replace with X) of cases
  let modGlobal: Bool = bernoulliSample 0.2 in

  recursive let rec: Int -> [Any] -> [Option Any] -> [Option Any] =
    lam i. lam samples. lam acc.
      match samples with [sample] ++ samples then
        -- Invalidate sample if it has the invalid index or with probability
        -- 0.5 (TODO: Replace with Y) if global modification is enabled.
        let mod =
          if eqi i 0 then true else
            if modGlobal then bernoulliSample 0.5 else false in

        let acc: [Option Any] =
          cons (if mod then None () else Some sample) acc in
        rec (subi i 1) samples acc

      else acc
  in
  modref state.oldAlignedTrace
    (rec invalidIndex (deref state.alignedTrace) emptyList)


-- General inference algorithm for aligned MCMC
let run : all a. (State -> a) -> (Res a -> ()) -> () = lam model. lam printResFun.

  -- Read number of runs and sweeps
  match monteCarloArgs () with (runs, sweeps) in

  recursive let mh : [Float] -> [a] -> Int -> ([Float], [a]) =
    lam weights. lam samples. lam iter.
      if leqi iter 0 then (weights, samples)
      else
        let prevTraceLength = deref state.traceLength in
        let prevWeight = deref state.weight in
        let prevWeightReused = deref state.weightReused in
        modTrace ();
        modref state.weight 0.;
        modref state.weightReused 0.;
        modref state.traceLength 0;
        modref state.alignedTrace emptyList;
        let sample = model state in
        let weight = deref state.weight in
        let weightReused = deref state.weightReused in
        let traceLength = deref state.traceLength in
        let prevSample = head samples in
        let logMhAcceptProb =
          minf 0. (addf (addf
                    (subf weight prevWeight)
                    (subf weightReused prevWeightReused))
                    (subf (log (int2float prevTraceLength))
                              (log (int2float traceLength))))
        in
        let iter = subi iter 1 in
        -- NOTE(dlunde,2022-08-22): Are the weights really meaningful beyond
        -- computing the MH acceptance ratio?
        if bernoulliSample (exp logMhAcceptProb) then
          mh (cons weight weights) (cons sample samples) iter
        else
          mh (cons prevWeight weights) (cons prevSample samples) iter
  in

  -- Repeat once for each sweep
  repeat (lam.

      -- First sample
      let sample = model state in
      -- NOTE(dlunde,2022-08-22): Are the weights really meaningful beyond
      -- computing the MH acceptance ratio?
      let weight = deref state.weight in
      let iter = subi runs 1 in

      -- Set aligned trace length (constant, only modified here)
      modref state.alignedTraceLength (length (deref state.alignedTrace));

      -- Sample the rest
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
