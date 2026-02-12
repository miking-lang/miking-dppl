include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"
include "option.mc"
include "sys.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

include "./config.mc"

-- Any-type, used for traces
type Any

-- Type used to resume execution midway through a previous run using a
-- continuation.
type Cont a = {
  cont: Any -> a,
  priorWeight : Float,
  weight: Float,
  dist: use RuntimeDistBase in Dist Any,
  drift: Any -> use RuntimeDistBase in Dist Any
}

-- In aligned MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State a = {

  -- The weight of the current execution
  weight: Ref Float,
  -- The "prior" probability, i.e., the probability density of getting
  -- exact values we got from the `assume`s we've encountered
  priorWeight: Ref Float,

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
  alignedTraceLength: Ref Int,

  unalignedResamp: Ref [Bool]

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
  priorWeight = ref 0.,
  driftHastingRatio = ref 0.,
  prevWeightReused = ref 0.,
  weightReused = ref 0.,
  alignedTrace = ref (emptyList ()),
  unalignedTraces = ref (toList [(emptyList ())]),
  reuseUnaligned = ref true,
  oldAlignedTrace = ref (emptyList ()),
  oldUnalignedTraces = ref (emptyList ()),
  alignedTraceLength = ref (negi 1),
  unalignedResamp = ref (emptyList ())
}

-- Function to reset the state when doing a global update
let resetState : State Result -> () = lam state. (
  modref state.oldAlignedTrace (emptyList ());
  modref state.oldUnalignedTraces (emptyList ());
  modref state.weight 0.;
  modref state.priorWeight 0.;
  modref state.driftHastingRatio 0.;
  modref state.prevWeightReused 0.;
  modref state.weightReused 0.;
  modref state.reuseUnaligned true;
  modref state.alignedTrace (emptyList ());
  modref state.unalignedTraces (emptyList ());
  modref state.unalignedResamp (emptyList ());
  ()
)

-- Function to modify the state accordently to the need of the inference method
let modifyState : State Result -> [(Any,Float,Cont Result)] -> [[(Any, Float, Int)]]
                  -> [(Any,Float)] -> [[(Any, Float, Int)]] -> [(Any, Float, Int)]
                  -> [Bool] -> Cont Result -> () = 
                  lam state. lam alignedTrace. lam unalignedTraces. lam oldAlignedTrace. 
                  lam oldUnalignedTraces. lam s2. lam unalignedResamp. lam cont.
    modref state.oldAlignedTrace oldAlignedTrace;
    modref state.oldUnalignedTraces (cons (emptyList ()) (cons (reverse s2) oldUnalignedTraces));
    modref state.weight cont.weight;
    modref state.priorWeight cont.priorWeight;
    modref state.prevWeightReused 0.;
    modref state.weightReused 0.;
    modref state.alignedTrace alignedTrace;
    modref state.unalignedTraces unalignedTraces;
    modref state.unalignedResamp unalignedResamp;
  ()

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
    if eqf wNew (negf inf) then
      newSample dist
    else
      modref state.weightReused (addf (deref state.weightReused) wNew);
      modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
      (sample, wNew)

-- Procedure at aligned samples
let sampleAlignedBase
  : all a. ((a -> use RuntimeDistBase in Dist a) -> use RuntimeDistBase in Dist a -> (Any, Float))
  -> (a -> use RuntimeDistBase in Dist a)
  -> use RuntimeDistBase in Dist a
  -> (a -> Result)
  -> Result
  = lam f. lam drift. lam dist. lam k.

    -- Snapshot that can later be used to resume execution from this sample.
    let cont: Cont Result = {
      cont = unsafeCoerce k,
      weight = deref state.weight,
      priorWeight = deref state.priorWeight,
      dist = unsafeCoerce dist,
      drift = unsafeCoerce drift
    } in

    let sample: (Any, Float) = f drift dist in

    -- set reuseUnaligned
    (if eqi (length (deref state.unalignedResamp)) 0 then
      modref state.reuseUnaligned true
    else
      modref state.reuseUnaligned (head (deref state.unalignedResamp)));

    (match deref state.unalignedResamp with [] then () else
      modref state.unalignedResamp (tail (deref state.unalignedResamp)));

    -- Add new empty unaligned trace for next segment.
    let unalignedTraces: [[(Any, Float, Int)]] = deref state.unalignedTraces in
    modref state.unalignedTraces (cons (emptyList ()) unalignedTraces);

    -- Remove head of oldUnalignedTraces
    (match deref state.oldUnalignedTraces with [] then () else
      modref state.oldUnalignedTraces (tail (deref state.oldUnalignedTraces)));

    modref state.alignedTrace
      (cons (sample.0, sample.1, cont) (deref state.alignedTrace));
    modref state.priorWeight (addf (deref state.priorWeight) sample.1);
    k (unsafeCoerce sample.0)

let sampleAligned
  : all a. (a -> use RuntimeDistBase in Dist a)
  -> use RuntimeDistBase in Dist a
  -> (a -> Result)
  -> Result
  = lam x. sampleAlignedBase (lam drift. lam dist.
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

let sampleAlignedForceNew
  : all a. (a -> use RuntimeDistBase in Dist a)
  -> use RuntimeDistBase in Dist a
  -> (a -> Result)
  -> Result
  = lam x. sampleAlignedBase (lam. newSample) x

let sampleAlignedKernel
  : all a. a
  -> Float
  -> (a -> use RuntimeDistBase in Dist a)
  -> use RuntimeDistBase in Dist a
  -> (a -> Result)
  -> Result
  = lam prev. lam w. sampleAlignedBase (moveSample prev w)

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
  modref state.priorWeight (addf (deref state.priorWeight) w);
  unsafeCoerce sample

-- Function to run new MH iterations.
-- Pass the acc from resampleBehavior and return it
let runNext: all acc. all dAcc. Config Result acc dAcc -> (State Result -> Result) -> Int -> Result =
  lam config. lam model. lam iter.

  let resBehav = config.resampleBehavior (unsafeCoerce iter) (length (deref state.alignedTrace)) in

  match resBehav with (iter, (unalignedResamp, invalidIndex)) then
    if eqi invalidIndex (negi 2) then
      resetState state;
      model state
    else

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
              -- Load the resample bool seq for unaligned trace
              let cutP = subi (length unalignedResamp) (addi invalidIndex 1) in
              let resamp = (splitAt unalignedResamp cutP).1 in
              -- Here to modify what left from the trace
              (modifyState state alignedTrace unalignedTraces oldAlignedTrace oldUnalignedTraces s2 resamp cont);
              

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
    -- printLn (join ["Aligned trace length: ", int2string (length (deref state.alignedTrace))]);
    -- printLn (join ["Unaligned traces length: ", int2string (length (deref state.unalignedTraces))]);
    -- printLn (join ["The invalid index is: ", int2string invalidIndex]);
    rec invalidIndex (deref state.alignedTrace) (deref state.unalignedTraces)
      (emptyList ()) (emptyList ())
    
  else error "Impossible"


-- General inference algorithm for aligned MCMC
let run : all acc. all dAcc. Config Result acc dAcc -> (State Result -> Result) -> use RuntimeDistBase in Dist Result =
  lam config. lam model.

  recursive let mh : [Result] -> Float -> Float -> Result -> dAcc -> (acc, Bool) -> Int -> [Result] =
    lam keptSamples. lam prevWeight. lam prevPriorWeight. lam prevSample. lam debugState. lam continueState. lam iter.
      match continueState with (continueState, true) then
        let beta = config.temperature continueState in
        let prevAlignedTrace = deref state.alignedTrace in
        let prevUnalignedTraces = deref state.unalignedTraces in
        -- Calculate the global probability given the current state
        let sample = runNext config model iter in
      -- print "prevAlignedTrace: ["; print (strJoin ", " (map (lam tup. float2string tup.1) prevAlignedTrace)); printLn "]";
        -- print "alignedTrace: ["; print (strJoin ", " (map (lam tup. float2string tup.1) (deref state.alignedTrace))); printLn "]";
        -- print "prevUnalignedTraces: ["; print (strJoin ", " (map (lam ls. join ["[", strJoin "," (map (lam tup. float2string tup.1) ls), "]"]) prevUnalignedTraces)); printLn "]";
        -- print "unalignedTraces: ["; print (strJoin ", " (map (lam ls. join ["[", strJoin "," (map (lam tup. float2string tup.1) ls), "]"]) (deref state.unalignedTraces))); printLn "]";
        let weight = deref state.weight in
        let priorWeight = deref state.priorWeight in
        let driftHastingRatio = deref state.driftHastingRatio in
        let weightReused = deref state.weightReused in
        let prevWeightReused = deref state.prevWeightReused in
        -- Calculate the Hastings ratio.
        let logMhAcceptProb = minf 0. (addf
                    (addf 
                      (mulf beta (subf weight prevWeight))
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
            (true, weight, priorWeight, sample)
          else
          -- NOTE(dlunde,2022-10-06): VERY IMPORTANT: Restore previous traces
          -- as we reject and reuse the old sample.
            modref state.alignedTrace prevAlignedTrace;
            modref state.unalignedTraces prevUnalignedTraces;
            (false, prevWeight, prevPriorWeight, prevSample)
        with (accepted, weight, priorWeight, sample) in
        let keptSamples = if config.keepSample iter then snoc keptSamples sample else keptSamples in
        let debugInfo =
          { accepted = accepted
          } in
        let debugState = config.debug.1 debugState debugInfo in
        let sampleInfo =
          { weight = weight
          , priorWeight = priorWeight
          } in
        let continueState = config.continue.1 continueState sampleInfo sample in
        mh keptSamples weight priorWeight sample debugState continueState (addi iter 1)
      else keptSamples
  in

  -- First sample -- call the model until we get a non-zero weight
  recursive let firstSample : (State Result -> Result) -> State Result -> Int -> State Result =
    lam model. lam state. lam i.
      let sample = model state in 
      let weight = deref state.weight in
      let weightReused = deref state.weightReused in
      let priorWeight = deref state.priorWeight in
      let prevWeightReused = deref state.prevWeightReused in
      if or (leqf weight (log 0.0)) (isNaN weight) then
        resetState state;
        -- printLn (join ["Try ", int2string i, " at sampling positive prob. sample. Sample weight: ", float2string (weight)]);
        firstSample model state (addi i 1)
      else sample
    in 

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit ();
 
  let sample = firstSample model state 1 in
  let weight = deref state.weight in
  let priorWeight = deref state.priorWeight in

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

  -- Set up debug and continue states
  let debugInfo =
    { accepted = true
    } in
  let debugState = config.debug.1 config.debug.0 debugInfo in

  let continueState = config.continue.0 () in
  let sampleInfo =
    { weight = weight
    , priorWeight = priorWeight
    } in
  let continueState = config.continue.1 continueState sampleInfo sample in

  -- Sample the rest
  let samples = mh samples weight priorWeight sample debugState continueState (addi iter 1) in

  -- printLn (join ["Number of reused aligned samples:", int2string (deref countReuse)]);
  -- printLn (join ["Number of reused unaligned samples:", int2string (deref countReuseUnaligned)]);

  -- Return: We on only need to output the samples if they are not written to a file already
  let numSamples = length samples in
  use RuntimeDist in
  constructDistEmpirical samples (make numSamples 1.0)
    (EmpMCMC {acceptRate = mcmcAcceptRate numSamples})
