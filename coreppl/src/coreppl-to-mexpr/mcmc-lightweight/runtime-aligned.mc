include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"
include "option.mc"

include "../runtime-common.mc"
include "kernel.mc"

-- Any-type, used for traces
type Any = ()


-- In aligned MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State = {

  -- The weight of the current execution
  weight: Ref Float,
  --
  driftPrevValue: Ref (Any, Float),
  -- The Hasting ratio for the driftKernel call
  driftHastingRatio: Ref Float,

  -- The weight of reused values in the previous and current executions
  prevWeightReused: Ref Float,
  weightReused: Ref Float,

  -- NOTE(dlunde,2022-11-03): Both the aligned and unaligned traces are stored
  -- in _reverse_ order (unlike oldAlignedTrace and oldUnalignedTraces that are
  -- stored in the actual order)
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
-- OPT(dlunde,2023-05-25): Wrap in lambda due to value restriction. Possible that the
-- type checker can handle this in the future though.
let emptyList = lam. toList []

-- TODO: Remove me
let countReuse = ref 0
let countReuseUnaligned = ref 0

-- State (reused throughout inference)
let state: State = {
  weight = ref 0.,
  driftPrevValue = ref ((), 0.),
  driftHastingRatio = ref 0.,
  prevWeightReused = ref 0.,
  weightReused = ref 0.,
  alignedTrace = ref (emptyList ()),
  unalignedTraces = ref (toList [(emptyList ())]),
  reuseUnaligned = ref true,
  oldAlignedTrace = ref (emptyList ()),
  oldUnalignedTraces = ref (emptyList ()),
  alignedTraceLength = ref (negi 1)
}

let updateWeight = lam v.
  -- print "Weight: "; print (float2string (exp v)); print " TotalWeight: "; printLn (float2string (exp (addf (deref state.weight) v)));
  modref state.weight (addf (deref state.weight) v)

let newSample: all a. use RuntimeDistBase in Dist a -> (Any,Float) = lam dist.
  -- printLn "Nsample";
  let s = use RuntimeDist in sample dist in
  let w = use RuntimeDist in logObserve dist s in
  (unsafeCoerce s, w)

-- Drift Kernel Function
-- - we have access here to the driftScale parameter compileOptions.driftScale
-- - modeled on reuseSample
-- Call one time per run
let moveSample: all a. use RuntimeDistBase in Dist a -> (Any, Float) =
  lam dist.
  use RuntimeDistElementary in

  let prevSample = deref state.driftPrevValue in
  let prev = prevSample.0 in
  let drift = compileOptions.driftScale in

  let kernel = choseKernel dist (unsafeCoerce prev) drift in

  let proposal = sample kernel in

  let proposalPriorProb = logObserve dist (unsafeCoerce proposal) in
  let reverseKernel = choseKernel dist (unsafeCoerce proposal) drift in

  let prevToProposalProb = logObserve kernel (unsafeCoerce proposal) in
  let proposalToPrevProb = logObserve reverseKernel (unsafeCoerce prev) in

  modref state.driftHastingRatio (subf proposalToPrevProb prevToProposalProb);
  modref state.weightReused (addf (deref state.weightReused) proposalPriorProb);
  modref state.prevWeightReused (addf (deref state.prevWeightReused) prevSample.1);

  (unsafeCoerce proposal, proposalPriorProb)

let reuseSample: all a. use RuntimeDistBase in Dist a -> Any -> Float -> (Any, Float) =
  lam dist. lam sample. lam w.
    let s: a = unsafeCoerce sample in
    let wNew = use RuntimeDist in logObserve dist s in
    -- printLn (join ["Mod weightReused: ", float2string (exp wNew), "; "]);
    -- printLn (join ["Mod prevWeightReused: ", float2string w]);
    modref state.weightReused (addf (deref state.weightReused) wNew);
    modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
    (sample, wNew)

-- Procedure at aligned samples
let sampleAligned: all a. use RuntimeDistBase in Dist a -> a = lam dist.
  let oldAlignedTrace: [Option (Any,Float)] = deref state.oldAlignedTrace in
  let sample: (Any, Float) =
    match oldAlignedTrace with [sample] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      match sample with Some (sample,w) then
        modref countReuse (addi 1 (deref countReuse));
        -- print "Aligned ";
        reuseSample dist sample w
      else
        if compileOptions.driftKernel then
          -- printLn "Not reused!";
          let res = moveSample dist in
          modref state.driftPrevValue ((), 0.);
          res
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
  modref state.unalignedTraces (cons (emptyList ()) unalignedTraces);

  -- Remove head of oldUnalignedTraces
  (match deref state.oldUnalignedTraces with [] then () else
    modref state.oldUnalignedTraces (tail (deref state.oldUnalignedTraces)));

  modref state.alignedTrace (cons sample (deref state.alignedTrace));
  unsafeCoerce (sample.0)

let sampleUnaligned: all a. Int -> use RuntimeDistBase in Dist a -> a = lam i. lam dist.
  let sample: (Any, Float) =
    if deref state.reuseUnaligned then
      let oldUnalignedTraces = deref state.oldUnalignedTraces in
      match oldUnalignedTraces with [[(sample,w,iOld)] ++ samples] ++ rest then
        if eqi i iOld then
          modref state.oldUnalignedTraces (cons samples rest);
          modref countReuseUnaligned (addi 1 (deref countReuseUnaligned));
          -- print "Unaligned ";
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

type Config a acc =
  { continue : (acc, acc -> a -> (acc, Bool))
  , keepSample : Int -> Bool
  , globalProb : Float
  }

-- Function to propose aligned trace changes between MH iterations.
let modTrace: all a. all acc. Config a acc -> () = lam config.

  let alignedTraceLength: Int = deref state.alignedTraceLength in

  recursive let rec: Int -> [(Any,Float)] -> [Option (Any,Float)]
                       -> [Option (Any,Float)] =
    lam i. lam samples. lam acc.
      match samples with [sample] ++ samples then
        -- Invalidate sample if it has the invalid index
        let acc: [Option (Any, Float)] =
          cons (if eqi i 0 then modref state.driftPrevValue sample; None () else Some sample) acc in
        rec (subi i 1) samples acc

      else acc
  in

  -- Enable global modifications with probability gProb
  let gProb = config.globalProb in
  let modGlobal: Bool = bernoulliSample gProb in

  if modGlobal then
    modref state.oldAlignedTrace (emptyList ());
    modref state.oldUnalignedTraces (emptyList ())
  else
    -- One index must always change
    let invalidIndex: Int = uniformDiscreteSample 0 (subi alignedTraceLength 1) in
    modref state.oldAlignedTrace
      (rec invalidIndex (deref state.alignedTrace) (emptyList ()));

    -- Also set correct old unaligned traces (always reused if possible, no
    -- invalidation)
    modref state.oldUnalignedTraces (mapReverse (lam trace.
      reverse trace
    ) (deref state.unalignedTraces));

    ()

-- General inference algorithm for aligned MCMC
let run : all a. all acc. Config a acc -> (State -> a) -> use RuntimeDistBase in Dist a =
  lam config. lam model.

  recursive let mh : [a] -> Float -> a -> (acc, Bool) -> Int -> [a] =
    lam samples. lam prevWeight. lam prevSample. lam continueState. lam iter.
      match continueState with (continueState, true) then
        let prevAlignedTrace = deref state.alignedTrace in
        let prevUnalignedTraces = deref state.unalignedTraces in
        modTrace config;
        modref state.weight 0.;
        modref state.driftHastingRatio 0.;
        modref state.prevWeightReused 0.;
        modref state.weightReused 0.;
        modref state.reuseUnaligned true;
        modref state.alignedTrace (emptyList ());
        modref state.unalignedTraces (toList [(emptyList ())]);
        let sample = model state in
        -- print "prevAlignedTrace: ["; print (strJoin ", " (map (lam tup. float2string tup.1) prevAlignedTrace)); printLn "]";
        -- print "alignedTrace: ["; print (strJoin ", " (map (lam tup. float2string tup.1) (deref state.alignedTrace))); printLn "]";
        -- print "prevUnalignedTraces: ["; print (strJoin ", " (map (lam ls. join ["[", strJoin "," (map (lam tup. float2string tup.1) ls), "]"]) prevUnalignedTraces)); printLn "]";
        -- print "unalignedTraces: ["; print (strJoin ", " (map (lam ls. join ["[", strJoin "," (map (lam tup. float2string tup.1) ls), "]"]) (deref state.unalignedTraces))); printLn "]";
        let weight = deref state.weight in
        let driftHastingRatio = deref state.driftHastingRatio in
        let weightReused = deref state.weightReused in
        let prevWeightReused = deref state.prevWeightReused in
        let logMhAcceptProb =
          minf 0. (addf
                    (addf
                      (subf weight prevWeight)
                      (subf weightReused prevWeightReused))
                    driftHastingRatio)
        in
        -- print "logMhAcceptProb: "; printLn (float2string (exp logMhAcceptProb));
        -- print "weight: "; printLn (float2string (exp  weight));
        -- print "prevWeight: "; printLn (float2string (exp prevWeight));
        -- print "weightReused: "; printLn (float2string (exp weightReused));
        -- print "prevWeightReused: "; printLn (float2string (exp prevWeightReused));
        -- printLn "-----";
        match
          if bernoulliSample (exp logMhAcceptProb) then
            mcmcAccept ();
            (weight, sample)
          else
            -- NOTE(dlunde,2022-10-06): VERY IMPORTANT: Restore previous traces
            -- as we reject and reuse the old sample.
            modref state.alignedTrace prevAlignedTrace;
            modref state.unalignedTraces prevUnalignedTraces;
            (prevWeight, prevSample)
        with (weight, sample) in
        let samples = if config.keepSample iter then snoc samples sample else samples in
        mh samples weight sample (config.continue.1 continueState sample) (addi iter 1)
      else samples
  in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit ();

  -- First sample
  let sample = model state in
  let weight = deref state.weight in

  -- Set aligned trace length (a constant, only modified here)
  modref state.alignedTraceLength (length (deref state.alignedTrace));

  let iter = 0 in
  let samples = if config.keepSample iter then [sample] else [] in

  -- Sample the rest
  let samples = mh samples weight sample (config.continue.1 config.continue.0 sample) (addi iter 1) in

  -- printLn (join ["Number of reused aligned samples:", int2string (deref countReuse)]);
  -- printLn (join ["Number of reused unaligned samples:", int2string (deref countReuseUnaligned)]);
  -- Return
  let numSamples = length samples in
  use RuntimeDist in
  constructDistEmpirical samples (make numSamples 1.0)
    (EmpMCMC {acceptRate = mcmcAcceptRate numSamples})
