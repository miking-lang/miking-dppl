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

-- Type used to resume execution midway through a previous run using a
-- continuation.
type Cont a = {
  cont: Any -> a,
  weight: Float,
  prevWeightReused: Float,
  weightReused: Float,
  unalignedTraces: [[(Any, Float, Int)]],
  dist: Dist Any
}

-- In aligned MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State a = {

  -- The weight of the current execution
  weight: Ref Float,

  -- The weight of reused values in the previous and current executions
  prevWeightReused: Ref Float,
  weightReused: Ref Float,

  -- NOTE(dlunde,2022-11-03): Both the aligned and unaligned traces are stored
  -- in _reverse_ order (unlike oldAlignedTrace and oldUnalignedTraces that are
  -- stored in the actual order)
  -- The aligned trace
  alignedTrace: Ref [(Any, Float, Cont a)],
  -- The unaligned traces in between the aligned traces, including their
  -- syntactic ID for matching.
  unalignedTraces: Ref [[(Any, Float, Int)]],

  -- Whether or not to reuse local unaligned samples
  reuseUnaligned: Ref Bool,

  -- The previous aligned trace
  oldAlignedTrace: Ref [(Any, Float)],
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
let state: State Unknown = {
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
let sampleAligned: all a. Dist a -> (a -> Unknown) -> Unknown = lam dist. lam k.
  -- Snapshot that can later be used to resume execution from this sample.
  let cont: Cont Unknown = {
    cont = unsafeCoerce k,
    weight = deref state.weight,
    prevWeightReused = deref state.prevWeightReused,
    weightReused = deref state.weightReused,
    unalignedTraces = deref state.unalignedTraces,
    dist = unsafeCoerce dist
  } in


  let oldAlignedTrace: [(Any,Float)] = deref state.oldAlignedTrace in
  let sample: (Any, Float) =
    match oldAlignedTrace with [(sample,w)] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      reuseSample dist sample w
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

  modref state.alignedTrace
    (cons (sample.0, sample.1, cont) (deref state.alignedTrace));
  k (unsafeCoerce sample.0)

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

-- Function to run new MH iterations.
let runNext: all a. (State -> a) -> a = lam model.

  -- Enable global modifications with probability gProb
  let gProb = compileOptions.mcmcLightweightGlobalProb in
  let modGlobal: Bool = bernoulliSample gProb in

  if modGlobal then (
    modref state.oldAlignedTrace emptyList;
    modref state.oldUnalignedTraces emptyList;
    modref state.weight 0.;
    modref state.prevWeightReused 0.;
    modref state.weightReused 0.;
    modref state.reuseUnaligned true;
    modref state.alignedTrace emptyList;
    modref state.unalignedTraces (toList [emptyList]);
    model state
  ) else

    recursive let rec: Int -> [(Any,Float,Cont a)] -> [(Any,Float)] -> a =
      lam i. lam alignedTrace. lam oldAlignedTrace.
        match alignedTrace with [sample] ++ alignedTrace then
          if gti i 0 then
            -- printLn (join ["Continuing at: ", int2string i]);
            rec (subi i 1) alignedTrace
              (cons (sample.0, sample.1) oldAlignedTrace)
          else (
            -- printLn (join ["Stopping at: ", int2string i]);
            let cont = sample.2 in
            let s = newSample cont.dist in
            modref state.oldAlignedTrace (cons s oldAlignedTrace);
            modref state.oldUnalignedTraces (mapReverse (lam trace.
              reverse trace
            ) cont.unalignedTraces);
            modref state.weight cont.weight;
            modref state.prevWeightReused cont.prevWeightReused;
            modref state.weightReused cont.weightReused;
            modref state.alignedTrace alignedTrace;
            modref state.unalignedTraces cont.unalignedTraces;

            -- This is where we actually run the program
            -- printLn "A";
            let res = sampleAligned cont.dist cont.cont in
            -- printLn "B";
            res
          )
        else error "Impossible"
    in

    -- One index must always change
    let invalidIndex: Int =
      uniformDiscreteSample 0 (subi (deref state.alignedTraceLength) 1) in
    -- printLn (join ["The aligned trace length is: ", int2string (deref state.alignedTraceLength)]);
    -- printLn (join ["The aligned trace length is really: ", int2string (length (deref state.alignedTrace))]);
    -- printLn (join ["The invalid index is: ", int2string invalidIndex]);
    rec invalidIndex (deref state.alignedTrace) emptyList

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
        let sample = runNext model in
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
        -- printLn "-----";
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
  let sample = model state in
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

  -- Return
  use RuntimeDist in
  constructDistEmpirical res.1 (create runs (lam. 1.))
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
