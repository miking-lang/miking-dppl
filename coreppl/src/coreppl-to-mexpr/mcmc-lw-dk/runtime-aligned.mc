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

  -- NOTE(dlunde,2022-11-03): Both the aligned and unaligned traces are stored
  -- in _reverse_ order (unlike oldAlignedTrace and oldUnalignedTraces that are
  -- stored in the actual order)
  -- The aligned trace for this execution.
  -- NOTE(vsenderov, 2024-06-06): We introduce the Bool to the aligned trace
  -- to indicate whether the particular sample should be moved with a drift kernel
  alignedTrace: Ref [(Any, Float, Bool)],
  -- The unaligned traces in between the aligned traces, including their
  -- syntactic ID for matching.
  unalignedTraces: Ref [[(Any, Float, Int)]],

  -- Whether or not to reuse local unaligned samples
  reuseUnaligned: Ref Bool,

  -- The previous aligned trace, with potentially invalidated samples
  oldAlignedTrace: Ref [Option (Any, Float, Bool)],
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

-- NOTE(vsenderov, 2024-06-06): Changed the type signature of the trace
let newSample: all a. use RuntimeDistBase in Dist a -> (Any,Float,Bool) = lam dist.
  let s = use RuntimeDist in sample dist in
  let w = use RuntimeDist in logObserve dist s in
  (unsafeCoerce s, w, false)



-- Drift Kernel Function
-- We have access here to the driftScale parameter compileOptions.driftScale
let moveSampleOld: all a. use RuntimeDistBase in Dist a -> Any -> (Any,Float,Bool) = 
  lam dist. lam prev.
  use RuntimeDistElementary in
  match dist with DistGamma d then 
    -- printLn "Gamma match";
    -- NOTE(vsenderov, 2024-Jun-10): 
    -- Possible alternatives here:
    --   - use a log-normal proposal (need to pay attention to Jacobian weight!)
    --   - use the Gamma dist itself by adjust its scale (or shape),
    --     but potentially this will lead to very local proposals if rate goes down
    
    -- 1. Log Transformation
    let x: Float = unsafeCoerce prev in
    let logX = log x in

    printLn (float2string x);

    -- 2. Sampling in Log-Space using Normal Distribution
    let kernel = DistGaussian {
      mu = logX,
      sigma = compileOptions.driftScale
    } in
    let logXp = use RuntimeDist in sample kernel in

    -- 3. Exponential Transformation to Original Space
    let xp = unsafeCoerce (exp logXp) in

    -- 4. Correcting the Weight with the Jacobian Term
    let kernelWeight = subf (use RuntimeDist in logObserve kernel logXp) logXp in -- correct
    -- let kernelWeight = (use RuntimeDist in logObserve kernel logXp) in  -- TEST ERROR, don't uncomment unless testing

    -- 5. Computing the prior weight of the proposal, not needed with the correct weight
    let priorWeight = use RuntimeDist in logObserve dist xp in

    -- 6. Putting it all together
    -- let w = subf priorWeight kernelWeight  in 
    -- let w = subf kernelWeight priorWeight in -
    let w = kernelWeight in 
    (unsafeCoerce xp, w,false)
    
  -- TODO match more distributions here, next attempt at Poisson:
  -- The shift on Poisson has no parameters yet, everything hard-coded
  --   else match dist with DistPoisson d then
  --     --printLn "Poisson match";
  -- --      --newSample dist   -- early abort
  --     if (bernoulliSample 0.5) then
  --       --printLn "Flipping";
  --       newSample dist
  --     else
    -- ? reuseSample dist sample w -- but we do not have w
  --       let s = unsafeCoerce prev in
  --       let w = use RuntimeDist in logObserve dist s in
  --       (unsafeCoerce s, w ,false)
  -- --   if eqi st 0 then 
  -- --     (unsafeCoerce 1,0.,false)
  --   else
  --     let kernelChoice = DistBernoulli { p = 0.5 } in
  --     let increase = use RuntimeDist in sample kernelChoice in
  --     let s = if increase then addi st 1 else subi st 1 in
  --     let w = negf 0.30102999566 in -- log 0.5
  --     (unsafeCoerce s,w,false)

  else 
  -- Finally we counldn't find a match, so we resample
    newSample dist



-- Drift Kernel Function
-- - we have access here to the driftScale parameter compileOptions.driftScale
-- - modeled on reuseSample
let moveSample: all a. use RuntimeDistBase in Dist a -> Any -> Float -> (Any, Float, Bool) = 
  lam dist. lam prev. lam w.
  use RuntimeDistElementary in
  match dist with DistGamma d then 
    -- printLn "Gamma match";
    -- NOTE(vsenderov, 2024-Jun-10): Possible alternatives here:
    -- - use a log-normal proposal (need to pay attention to Jacobian weight!)
    -- - use the Gamma dist itself by adjust its scale (or shape),  but potentially this will lead to very local proposals if rate goes down
    
    let s: Float = unsafeCoerce prev in
    -- printLn (float2string s);
    let logS: Float = log s in
    let kernel = DistGaussian {
      mu = logS,
      sigma = compileOptions.driftScale
    } in
    let logSNew = use RuntimeDist in sample kernel in
    let sNew = exp logSNew in 
    -- printLn "Propopsing ->";
    -- printLn (float2string sNew);
    
    let sNew = unsafeCoerce sNew in
    let priorWeight = use RuntimeDist in logObserve dist sNew in

    -- Correcting the Weight with the Jacobian Term, intention: logObserve LogNormal sNew
    --let forwardDensity = subf (use RuntimeDist in logObserve kernel logSNew) logSNew in -- correct
    --let backwardDensity = subf (use RuntimeDist in logObserve kernel logS) logS in
    --let hastingsRatio = subf backwardDensity forwardDensity in 
    -- Simplifies due to symmetric kernel:
    let hastingsRatio = subf logSNew logS in 
    let wNew = addf priorWeight hastingsRatio in

    -- printLn "wNew, wOld:";
    -- printLn (float2string wNew);
    -- printLn (float2string w);

    modref state.weightReused (addf (deref state.weightReused) wNew);
    modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
                    
    -- NOTE(vsenderov): Tim could you figure which of the next two is correct?
    -- (unsafeCoerce sNew, priorWeight, false) 
    (unsafeCoerce sNew, wNew, false) 
    
  -- TODO match more distributions here, next attempt at Poisson:
  -- The shift on Poisson has no parameters yet, everything hard-coded
  --   else match dist with DistPoisson d then
  --     --printLn "Poisson match";
  -- --      --newSample dist   -- early abort
  --     if (bernoulliSample 0.5) then
  --       --printLn "Flipping";
  --       newSample dist
  --     else
    -- ? reuseSample dist prev w -- but we do not have w
  --       let s = unsafeCoerce prev in
  --       let w = use RuntimeDist in logObserve dist s in
  --       (unsafeCoerce s, w ,false)
  -- --   if eqi st 0 then 
  -- --     (unsafeCoerce 1,0.,false)
  --   else
  --     let kernelChoice = DistBernoulli { p = 0.5 } in
  --     let increase = use RuntimeDist in sample kernelChoice in
  --     let s = if increase then addi st 1 else subi st 1 in
  --     let w = negf 0.30102999566 in -- log 0.5
  --     (unsafeCoerce s,w,false)

  else 
  -- Finally we counldn't find a match, so we resample
    newSample dist

let reuseSample: all a. use RuntimeDistBase in Dist a -> Any -> Float -> (Any, Float, Bool) =
  lam dist. lam sample. lam w.
    let s: a = unsafeCoerce sample in
    let wNew = use RuntimeDist in logObserve dist s in
    -- print (join ["Mod weightReused: ", float2string wNew, "; "]);
    -- printLn (join ["Mod prevWeightReused: ", float2string w]);
    modref state.weightReused (addf (deref state.weightReused) wNew);
    modref state.prevWeightReused (addf (deref state.prevWeightReused) w);
    (sample, wNew, false)

-- Procedure at aligned samples
let sampleAligned: all a. use RuntimeDistBase in Dist a -> a = lam dist.
  let oldAlignedTrace: [Option (Any,Float,Bool)] = deref state.oldAlignedTrace in
  let sample: (Any, Float, Bool) =
    match oldAlignedTrace with [sample] ++ oldAlignedTrace then
      modref state.oldAlignedTrace oldAlignedTrace;
      match sample with Some (sample,w,move) then
        --printLn (bool2string move);
        if (not move) then
          modref countReuse (addi 1 (deref countReuse));
          --print "Aligned ";
          reuseSample dist sample w
        else
          -- printLn "Moving invalidated aligned sample!";
          modref countReuse (addi 1 (deref countReuse)); -- ?
          moveSample dist sample w
          --moveSampleOld dist sample
      else
        never -- existing samples should always have the structure (Any,Float,Bool)
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
  let sample: (Any, Float, Bool) =
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
  match sample with (sample,w,m) in
  modref state.unalignedTraces (cons (cons (sample,w,i) current) rest);
  unsafeCoerce sample

-- Function to propose aligned trace changes between MH iterations.
let modTrace: Unknown -> () = lam config.

  let alignedTraceLength: Int = deref state.alignedTraceLength in

  recursive let rec: Int -> [(Any,Float,Bool)] -> [Option (Any,Float,Bool)]
                       -> [Option (Any,Float,Bool)] =
    lam i. lam samples. lam acc.
      match samples with [(s,w,m)] ++ samples then
        -- Invalidate sample if it has the invalid index
        let acc: [Option (Any, Float, Bool)] =
          cons (if eqi i 0 then Some (s,w,true) else Some (s,w,m)) acc in
        rec (subi i 1) samples acc

      else acc
  in

  recursive let invalidateAll: [(Any,Float,Bool)] -> [Option (Any,Float,Bool)]
                       -> [Option (Any,Float,Bool)] =
    lam samples. lam acc.
      match samples with [(s,w,m)] ++ samples then
        -- Invalidate samples
        let acc: [Option (Any, Float, Bool)] =
          cons (Some (s,w,true)) acc in
        invalidateAll samples acc
      else acc
  in

  -- Enable global modifications with probability gProb
  let gProb = config.globalProb in
  let modGlobal: Bool = bernoulliSample gProb in

  if modGlobal then
    -- In case of a truly global move, redraw everything
    modref state.oldAlignedTrace (emptyList ());
    modref state.oldUnalignedTraces (emptyList ())
  -- The following is probably correct (global local move),
  -- but I haven't tested it
  -- else if (bernoulliSample 0.5) then
  --   -- TODO command line argument here
  --   -- flip a coin and invalidate all samples 
  --   -- for a global move with drift kerneles
  --   --printLn "global move";
  --   -- TODO instead of emptying the list here for the aligned trace
  --   -- invalidate them with (_,_,false) so that they are _moved_
  --   -- and not redrawn
  --   modref state.oldAlignedTrace
  --     (invalidateAll (deref state.alignedTrace) (emptyList ()));
  --   modref state.oldUnalignedTraces (emptyList ())
  else
    -- One index must always change
    -- TODO hack: scan which state.oldAlignedTrace have Gamma distribution
    -- and then invalidate only amongst them with some probability
    -- and amongst the other guys with another probability
    -- let nuisance: [Bool] = whichNuissance state.alignedTrace in
    -- [False, False, False, True, True, True...] but permutations possible!
    -- sample one of the False indicies somehow
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
let run : all a. Unknown -> (State -> a) -> use RuntimeDistBase in Dist a =
  lam config. lam model.

  recursive let mh : [Float] -> [a] -> Int -> ([Float], [a]) =
    lam weights. lam samples. lam iter.
      if leqi iter 0 then (weights, samples)
      else
        let prevAlignedTrace = deref state.alignedTrace in
        let prevUnalignedTraces = deref state.unalignedTraces in
        let prevSample = head samples in
        let prevWeight = head weights in
        modTrace config;
        modref state.weight 0.;
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
          -- printLn "Accept";
          mcmcAccept ();
          mh
            (cons weight weights)
            (cons sample samples)
            iter
        else
          -- printLn "Reject";
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

  -- printLn (join ["Number of reused aligned samples:", int2string (deref countReuse)]);
  -- printLn (join ["Number of reused unaligned samples:", int2string (deref countReuseUnaligned)]);

  -- Return
  use RuntimeDist in
  constructDistEmpirical res.1 (create runs (lam. 1.))
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
