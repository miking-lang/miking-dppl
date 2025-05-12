include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"
include "option.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

include "./config.mc"

-- Any-type, used for traces
type Any

-- Type used to resume execution midway through a previous run using a
-- continuation.
type Cont a = {
  cont: Any -> a,
  weight: Float,
  dist: use RuntimeDistBase in Dist Any,
  drift: Any -> use RuntimeDistBase in Dist Any
}

-- In aligned MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State a = {

  -- The weight of the current execution
  weight: Ref Float,

  -- The Hasting ratio for the driftKernel call
  driftHastingRatio: Ref Float,

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
-- OPT(dlunde,2023-05-25): Wrap in lambda due to value restriction. Possible that the
-- type checker can handle this in the future though.
let emptyList = lam. toList []

-- TODO: Remove me
let countReuse = ref 0
let countReuseUnaligned = ref 0

-- NOTE(dlunde,2023-05-25): Fixed result type of the model. Assumes there is only _one_ model, problems with extract and infer?
type Result = Unknown

-- State (reused throughout inference)
let state: State Result = {
  weight = ref 0.,
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
  modref state.weight (addf (deref state.weight) v)

let newSample: all a. use RuntimeDistBase in Dist a -> (Any,Float) = lam dist.
  let s = use RuntimeDist in sample dist in
  let w = use RuntimeDist in logObserve dist s in
  (unsafeCoerce s, w)

-- Drift Kernel Function
-- - modeled on reuseSample
-- Call one time per run
let moveSample: all a. a -> Float -> (a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (Any, Float) =
  lam prev. lam w. lam drift. lam dist.
    use RuntimeDistElementary in

    let kernel = drift prev in

    let proposal = sample kernel in

    let proposalPriorProb = logObserve dist proposal in
    let reverseKernel = drift proposal in

    let prevToProposalProb = logObserve kernel proposal in
    let proposalToPrevProb = logObserve reverseKernel prev in

    modref state.driftHastingRatio (subf proposalToPrevProb prevToProposalProb);
    modref state.weightReused (addf (deref state.weightReused) proposalPriorProb);
    modref state.prevWeightReused (addf (deref state.prevWeightReused) w);

    (unsafeCoerce proposal, proposalPriorProb)

let reuseSample: all a. use RuntimeDistBase in Dist a -> Any -> Float -> (Any, Float) =
  lam dist. lam sample. lam w.
    let s: a = unsafeCoerce sample in
    let wNew = use RuntimeDist in logObserve dist s in
    -- print (join ["Mod weightReused: ", float2string wNew, "; "]);
    -- printLn (join ["Mod prevWeightReused: ", float2string w]);
    if eqfApprox 1e-14 wNew 0.0 then
      newSample dist
    else
      modref state.weightReused (addf (deref state.weightReused) wNew);
      modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
      (sample, wNew)

-- Procedure at aligned samples
let sampleAlignedBase: all a. ((a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (Any, Float)) -> (a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (a -> Result)
                         -> Result =
  lam f. lam drift. lam dist. lam k.

    -- Snapshot that can later be used to resume execution from this sample.
    let cont: Cont Result = {
      cont = unsafeCoerce k,
      weight = deref state.weight,
      dist = unsafeCoerce dist,
      drift = unsafeCoerce drift
    } in

    let sample: (Any, Float) = f drift dist in

    -- Reset reuseUnaligned
    modref state.reuseUnaligned true;

    -- Add new empty unaligned trace for next segment.
    let unalignedTraces: [[(Any, Float, Int)]] = deref state.unalignedTraces in
    modref state.unalignedTraces (cons (emptyList ()) unalignedTraces);

    -- Remove head of oldUnalignedTraces
    (match deref state.oldUnalignedTraces with [] then () else
      modref state.oldUnalignedTraces (tail (deref state.oldUnalignedTraces)));

    modref state.alignedTrace
      (cons (sample.0, sample.1, cont) (deref state.alignedTrace));
    k (unsafeCoerce sample.0)

let sampleAligned: all a. (a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (a -> Result) -> Result =
  lam x. sampleAlignedBase (lam drift. lam dist.
    let oldAlignedTrace: [(Any,Float)] = deref state.oldAlignedTrace in
    match oldAlignedTrace with [(sample,w)] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      modref countReuse (addi 1 (deref countReuse));
      -- print "Aligned ";
      reuseSample dist sample w
    else
      -- print "Aligned : NewSample";
      newSample dist
  ) x

let sampleAlignedForceNew: all a. (a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (a -> Result) -> Result =
  lam x. sampleAlignedBase (lam. newSample) x

let sampleAlignedKernel: all a. a -> Float -> (a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (a -> Result) -> Result =
  lam prev. lam w. sampleAlignedBase (moveSample prev w)

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

-- Function to run new MH iterations.
let runNext: all acc. all dAcc. Config Result acc dAcc -> (State Result -> Result) -> Result =
  lam config. lam model.

  -- Enable global modifications with probability gProb
  let gProb = config.globalProb in
  let modGlobal: Bool = bernoulliSample gProb in

  if modGlobal then (
    modref state.oldAlignedTrace (emptyList ());
    modref state.oldUnalignedTraces (emptyList ());
    modref state.weight 0.;
    modref state.driftHastingRatio 0.;
    modref state.prevWeightReused 0.;
    modref state.weightReused 0.;
    modref state.reuseUnaligned true;
    modref state.alignedTrace (emptyList ());
    modref state.unalignedTraces (toList [(emptyList ())]);
    model state
  ) else

    recursive let rec: Int -> [(Any,Float,Cont Result)] -> [[(Any, Float, Int)]]
                           -> [(Any,Float)]        -> [[(Any, Float, Int)]]
                           -> Result =
      lam i. lam alignedTrace. lam unalignedTraces.
      lam oldAlignedTrace. lam oldUnalignedTraces.
        match (alignedTrace,unalignedTraces)
        with ([s1] ++ alignedTrace, [s2] ++ unalignedTraces) then
          if gti i 0 then
            rec (subi i 1) alignedTrace unalignedTraces
              (cons (s1.0, s1.1) oldAlignedTrace)
              (cons (reverse s2) oldUnalignedTraces)
          else (
            let cont = s1.2 in
            modref state.oldAlignedTrace oldAlignedTrace;
            modref state.oldUnalignedTraces (cons (emptyList ()) (cons (reverse s2) oldUnalignedTraces));
            modref state.weight cont.weight;
            modref state.prevWeightReused 0.;
            modref state.weightReused 0.;
            modref state.alignedTrace alignedTrace;
            modref state.unalignedTraces unalignedTraces;
            -- printLn (join ["New aligned trace length: ", int2string (length (deref state.alignedTrace))]);
            -- printLn (join ["Old aligned trace length: ", int2string (length (deref state.oldAlignedTrace))]);
            -- printLn (join ["New unaligned traces length: ", int2string (length (deref state.unalignedTraces))]);
            -- printLn (join ["Old unaligned traces length: ", int2string (length (deref state.oldUnalignedTraces))]);
            -- printLn "---";

            -- This is where we actually run the program
            -- printLn "A";
            if config.driftKernel then
              sampleAlignedKernel s1.0 s1.1 cont.drift cont.dist cont.cont
            else
              sampleAlignedForceNew cont.drift cont.dist cont.cont
          )
        else error "Impossible"
    in

    -- One index must always change
    let invalidIndex: Int =
      uniformDiscreteSample 0 (subi (deref state.alignedTraceLength) 1) in
    -- printLn (join ["Aligned trace length: ", int2string (length (deref state.alignedTrace))]);
    -- printLn (join ["Unaligned traces length: ", int2string (length (deref state.unalignedTraces))]);
    -- printLn (join ["The invalid index is: ", int2string invalidIndex]);
    rec invalidIndex (deref state.alignedTrace) (deref state.unalignedTraces)
      (emptyList ()) (emptyList ())

-- General inference algorithm for aligned MCMC
let run : all acc. all dAcc. Config Result acc dAcc -> (State Result -> Result) -> use RuntimeDistBase in Dist Result =
  lam config. lam model.

  recursive let mh : [Result] -> Float -> Result -> dAcc -> (acc, Bool) -> Int -> [Result] =
    lam keptSamples. lam prevWeight. lam prevSample. lam debugState. lam continueState. lam iter.
      match continueState with (continueState, true) then
        let prevAlignedTrace = deref state.alignedTrace in
        let prevUnalignedTraces = deref state.unalignedTraces in
        let sample = runNext config model in
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
        -- print "weight: "; printLn (float2string weight);
        -- print "prevWeight: "; printLn (float2string prevWeight);
        -- print "weightReused: "; printLn (float2string weightReused);
        -- print "prevWeightReused: "; printLn (float2string prevWeightReused);
        -- printLn "-----";
        match
          if bernoulliSample (exp logMhAcceptProb) then
            mcmcAccept ();
            (true, weight, sample)
          else
          -- NOTE(dlunde,2022-10-06): VERY IMPORTANT: Restore previous traces
          -- as we reject and reuse the old sample.
            modref state.alignedTrace prevAlignedTrace;
            modref state.unalignedTraces prevUnalignedTraces;
            (false, prevWeight, prevSample)
        with (accepted, weight, sample) in
        let keptSamples = if config.keepSample iter then snoc keptSamples sample else keptSamples in
        let debugInfo =
          { accepted = accepted
          } in
        mh keptSamples weight sample (config.debug.1 debugState debugInfo) (config.continue.1 continueState sample) (addi iter 1)
      else keptSamples
  in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit ();

  -- First sample
  let sample = model state in
  let weight = deref state.weight in

  -- Set aligned trace length (a constant, only modified here)
  modref state.alignedTraceLength (length (deref state.alignedTrace));

  -- If the program contains no samples at all, then `alignedTrace` is
  -- empty, and `mh` will crash. However, such a program requires no
  -- inference at all, thus we exit early here.
  if null (deref state.alignedTrace) then
    -- NOTE(vipa, 2025-02-14): It's important that this line is
    -- *before* the use, otherwise `sample` will refer to the `sem`
    -- from `RuntimeDist`. Because we don't type-check the return
    -- value of this function in practice, and the `Dist` type places
    -- no restrictions on the type of its samples this compiles, but
    -- fails at run time
    let res = [sample] in
    use RuntimeDist in
    constructDistEmpirical res [1.]
      (EmpMCMC { acceptRate = mcmcAcceptRate 1 })
  else

  let iter = 0 in
  let samples = if config.keepSample iter then [sample] else [] in

  let debugInfo =
    { accepted = true
    } in

  -- Sample the rest
  let samples = mh samples weight sample (config.debug.1 config.debug.0 debugInfo) (config.continue.1 config.continue.0 sample) (addi iter 1) in

  -- printLn (join ["Number of reused aligned samples:", int2string (deref countReuse)]);
  -- printLn (join ["Number of reused unaligned samples:", int2string (deref countReuseUnaligned)]);

  let numSamples = length samples in
  use RuntimeDist in
  constructDistEmpirical samples (make numSamples 1.0)
    (EmpMCMC {acceptRate = mcmcAcceptRate numSamples})
