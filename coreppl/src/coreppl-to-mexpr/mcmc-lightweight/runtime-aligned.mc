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

  -- The weight of reused values in the previous and current executions
  prevWeightReused: Ref Float,
  weightReused: Ref Float,

  -- The aligned trace for this execution.
  alignedTrace: Ref [(Any, Float)],
  -- The unaligned traces in between the aligned traces, including their
  -- syntactic ID for matching.
  unalignedTraces: Ref [[(Any, Float, Int)]],

  -- Whether or not to reuse local unaligned samples
  reuseUnaligned: Ref Bool,

  -- The previous aligned trace, with potentially invalidated samples
  oldAlignedTrace: Ref [Option (Any, Float)],
  -- The previous unaligned traces in between the aligned traces, including
  -- their syntactic ID for matching.
  oldUnalignedTraces: Ref [[(Any, Float, Int)]],

  -- Aligned trace length (a constant, determined at the first run)
  alignedTraceLength: Ref Int

}

-- NOTE(dlunde,2022-05-23): The below implementation does not
-- work with ropes for some reason (segfaults). We must therefore use lists.
let emptyList = toList []

-- State (reused throughout inference)
let state: State = {
  weight = ref 0.,
  prevWeightReused = ref 0.,
  weightReused = ref 0.,
  alignedTrace = ref emptyList,
  unalignedTraces = ref (toList [emptyList]),
  reuseUnaligned = ref true,
  oldAlignedTrace = ref emptyList,
  oldUnalignedTraces = ref emptyList,
  alignedTraceLength = ref (negi 1)
}

let updateWeight = lam v.
  modref state.weight (addf (deref state.weight) v)

let newSample: all a. Dist a -> (Any,Float) = lam dist.
  let s = use RuntimeDist in sample dist in
  let w = use RuntimeDist in logObserve dist s in
  (unsafeCoerce s, w)

let reuseSample: all a. Dist a -> Any -> Float -> (Any, Float) =
  lam dist. lam sample. lam w.
    let s: a = unsafeCoerce sample in
    let wNew = use RuntimeDist in logObserve dist s in
    modref state.weightReused (addf (deref state.weightReused) wNew);
    modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
    (sample, wNew)

-- Procedure at aligned samples
let sampleAligned: all a. Dist a -> a = lam dist.
  let oldAlignedTrace: [Option (Any,Float)] = deref state.oldAlignedTrace in
  let sample: (Any, Float) =
    match oldAlignedTrace with [sample] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      match sample with Some (sample,w) then
        reuseSample dist sample w
      else
        newSample dist
    else
      -- This case should only happen in the first run when there is no
      -- previous aligned trace, or when we take a global step
      newSample dist
  in

  -- Reset reuseUnaligned
  modref state.reuseUnaligned true;

  -- Add new empty unaligned trace for next segment.
  let unalignedTraces: [[(Any, Float, Int)]] = deref state.unalignedTraces in
  modref state.unalignedTraces (cons emptyList unalignedTraces);

  -- Remove head of oldUnalignedTraces
  (match deref state.oldUnalignedTraces with [] then () else
    modref state.oldUnalignedTraces (tail (deref state.oldUnalignedTraces)));

  modref state.alignedTrace (cons sample (deref state.alignedTrace));
  unsafeCoerce (sample.0)

let sampleUnaligned: all a. Int -> Dist a -> a = lam i. lam dist.
  let sample: (Any, Float) =
    if deref state.reuseUnaligned then
      let oldUnalignedTraces = deref state.oldUnalignedTraces in
      match oldUnalignedTraces with [[(sample,w,iOld)] ++ samples] ++ rest then
        if eqi i iOld then
          modref state.oldUnalignedTraces (cons samples rest);
          reuseSample dist sample w
        else
          modref state.reuseUnaligned false; newSample dist
      else
        newSample dist
    else
      newSample dist
  in
  match deref state.unalignedTraces with [current] ++ rest in
  match sample with (sample,w) in
  modref state.unalignedTraces (cons (cons (sample,w,i) current) rest);
  unsafeCoerce sample

-- Function to propose aligned trace changes between MH iterations.
let modTrace: () -> () = lam.

  let alignedTraceLength: Int = deref state.alignedTraceLength in

  recursive let rec: Int -> [(Any,Float)] -> [Option (Any,Float)]
                       -> [Option (Any,Float)] =
    lam i. lam samples. lam acc.
      match samples with [sample] ++ samples then
        -- Invalidate sample if it has the invalid index
        let acc: [Option (Any, Float)] =
          cons (if eqi i 0 then None () else Some sample) acc in
        rec (subi i 1) samples acc

      else acc
  in

  -- Enable global modifications with probability gProb
  let gProb = compileOptions.mcmcLightweightGlobalProb in
  let modGlobal: Bool = bernoulliSample gProb in

  if modGlobal then
    modref state.oldAlignedTrace emptyList;
    modref state.oldUnalignedTraces emptyList
  else
    -- One index must always change
    let invalidIndex: Int = uniformDiscreteSample 0 (subi alignedTraceLength 1) in
    modref state.oldAlignedTrace
      (rec invalidIndex (deref state.alignedTrace) emptyList);

    -- Also set correct old unaligned traces (always reused if possible, no
    -- invalidation)
    modref state.oldUnalignedTraces (deref state.unalignedTraces)

let runModel = lam model.
  let sample = model state in

  -- Reverse all unaligned traces _and_ the list of unaligned traces
  modref state.unalignedTraces (mapReverse (lam trace.
    reverse trace
  ) (deref state.unalignedTraces));

  sample

-- General inference algorithm for aligned MCMC
let run : all a. Unknown -> (State -> a) -> Dist a =
  lam config. lam model.

  recursive let mh : [Float] -> [a] -> Int -> ([Float], [a]) =
    lam weights. lam samples. lam iter.
      if leqi iter 0 then (weights, samples)
      else
        let prevAlignedTrace = deref state.alignedTrace in
        let prevUnalignedTraces = deref state.unalignedTraces in
        let prevSample = head samples in
        let prevWeight = head weights in
        modTrace ();
        modref state.weight 0.;
        modref state.prevWeightReused 0.;
        modref state.weightReused 0.;
        modref state.reuseUnaligned true;
        modref state.alignedTrace emptyList;
        modref state.unalignedTraces (toList [emptyList]);
        let sample = runModel model in
        let weight = deref state.weight in
        let weightReused = deref state.weightReused in
        let prevWeightReused = deref state.prevWeightReused in
        let logMhAcceptProb =
          minf 0. (addf
                    (subf weight prevWeight)
                    (subf weightReused prevWeightReused))
        in
        -- print "logMhAcceptProb: "; printLn (float2string logMhAcceptProb);
        -- print "weight: "; printLn (float2string weight);
        -- print "prevWeight: "; printLn (float2string prevWeight);
        -- print "weightReused: "; printLn (float2string weightReused);
        -- print "prevWeightReused: "; printLn (float2string prevWeightReused);
        -- print "prevTraceLength: "; printLn (float2string (int2float prevTraceLength));
        let iter = subi iter 1 in
        if bernoulliSample (exp logMhAcceptProb) then
          mcmcAccept ();
          mh
            (cons weight weights)
            (cons sample samples)
            iter
        else
          -- NOTE(dlunde,2022-10-06): VERY IMPORTANT: Restore previous traces
          -- as we reject and reuse the old sample.
          modref state.alignedTrace prevAlignedTrace;
          modref state.unalignedTraces prevUnalignedTraces;
          mh
            (cons prevWeight weights)
            (cons prevSample samples)
            iter
  in

  let runs = config.iterations in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit runs;

  -- First sample
  let sample = runModel model in
  -- NOTE(dlunde,2022-08-22): Are the weights really meaningful beyond
  -- computing the MH acceptance ratio?
  let weight = deref state.weight in
  let iter = subi runs 1 in

  -- Set aligned trace length (a constant, only modified here)
  modref state.alignedTraceLength (length (deref state.alignedTrace));

  -- Sample the rest
  let res = mh [weight] [sample] iter in

  -- Reverse to get the correct order
  let res = match res with (weights,samples) in
    (reverse weights, reverse samples)
  in

  (if compileOptions.printAcceptanceRate then
    printLn (float2string (mcmcAcceptRate ()))
  else ());

  -- Return
  use RuntimeDist in
  constructDistEmpirical res.1 (create runs (lam. 1.))
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
