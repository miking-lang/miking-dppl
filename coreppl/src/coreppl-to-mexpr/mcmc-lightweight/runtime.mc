include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"
include "option.mc"
include "map.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

-- Any-type, used for samples
type Any = ()

type Address = [Int]

-- In lightweight MCMC, the state is the accumulated weight, a map of samples for the current run, and a map of samples from the previous run to potentially reuse.
type State = {

  -- The weight of the current execution
  weight: Ref Float,

  -- The weight of reused values in the current execution
  weightReused: Ref Float,

  -- The sample database for this execution
  db: Ref (Map Address Any),

  -- Number of encountered assumes
  traceLength: Ref Int,

  -- The previous database, with potentially invalidated samples
  oldDb: Ref (Map Address (Option Any))

}

let emptyList = toList []

-- Custom sequence comparison (the one in the standard library is optimized for
-- ropes, not lists)
let seqCmp : all a. (a -> a -> Int) -> [a] -> [a] -> Int = lam cmp. lam s1. lam s2.
  recursive let work = lam s1. lam s2.
    match (s1, s2) with ([h1] ++ t1, [h2] ++ t2) then
      let c = cmp h1 h2 in
      if eqi c 0 then work t1 t2
      else c
    else match (s1, s2) with (t1, []) then 1
    else match (s1, s2) with ([], t2) then negi 1
    else 0
  in
  work s1 s2

let emptyAddressMap = mapEmpty (seqCmp subi)

-- State (reused throughout inference)
let state: State = {
  weight = ref 0.,
  weightReused = ref 0.,
  db = ref emptyAddressMap,
  traceLength = ref 0,
  oldDb = ref emptyAddressMap
}

let updateWeight = lam v.
  modref state.weight (addf (deref state.weight) v)

let incrTraceLength: () -> () = lam.
  modref state.traceLength (addi (deref state.traceLength) 1)

-- Procedure at samples
let sample: all a. Address -> Dist a -> a = lam addr. lam dist.
  use RuntimeDist in
  let oldDb: Map Address (Option Any) = deref state.oldDb in
  let newSample: () -> Any = lam. unsafeCoerce (sample dist) in

  let sample: Any =
    match mapLookup addr oldDb with Some (Some sample) then
      let s: a = unsafeCoerce sample in
      modref state.weightReused
        (addf (deref state.weightReused) (logObserve dist s));
      sample
    else newSample ()
  in
  incrTraceLength ();
  modref state.db (mapInsert addr sample (deref state.db));
  unsafeCoerce sample

-- Function to propose db changes between MH iterations.
let modDb: () -> () = lam.

  let gProb = compileOptions.mcmcLightweightGlobalProb in
  let mProb = compileOptions.mcmcLightweightGlobalModProb in

  let db = deref state.db in

  -- Enable global modifications with probability gProb
  let modGlobal: Bool = bernoulliSample gProb in

  -- One item in the db (chosen at random) must always change
  let invalidIndex: Int = uniformDiscreteSample 0 (subi (mapSize db) 1) in
  let currentIndex: Ref Int = ref 0 in
  modref state.oldDb
    (mapMap (lam sample: Any.
       -- Invalidate sample if it has the invalid index or with probability
       -- mProb if global modification is enabled.
       let mod =
         if eqi invalidIndex (deref currentIndex) then true else
           if modGlobal then bernoulliSample mProb else false in
       let sample = if mod then None () else Some sample in
       modref currentIndex (addi (deref currentIndex) 1);
       sample
    ) db)

-- General inference algorithm for aligned MCMC

let run : all a. Unknown -> (State -> a) -> Dist a =
  lam config. lam model.
  use RuntimeDist in

  recursive let mh : [Float] -> [Float] -> [a] -> Int -> ([Float], [a]) =
    lam weights. lam weightsReused. lam samples. lam iter.
      if leqi iter 0 then (weights, samples)
      else
        let prevSample = head samples in
        let prevTraceLength = deref state.traceLength in
        let prevWeight = head weights in
        let prevWeightReused = head weightsReused in
        modDb ();
        modref state.weight 0.;
        modref state.weightReused 0.;
        modref state.db emptyAddressMap;
        modref state.traceLength 0;
        let sample = model state in
        let traceLength = deref state.traceLength in
        let weight = deref state.weight in
        let weightReused = deref state.weightReused in
        let logMhAcceptProb =
          minf 0. (addf (addf
                    (subf weight prevWeight)
                    (subf weightReused prevWeightReused))
                    (subf (log (int2float prevTraceLength))
                              (log (int2float traceLength))))
        in
        -- print "logMhAcceptProb: "; printLn (float2string logMhAcceptProb);
        -- print "weight: "; printLn (float2string weight);
        -- print "prevWeight: "; printLn (float2string prevWeight);
        -- print "weightReused: "; printLn (float2string weightReused);
        -- print "prevWeightReused: "; printLn (float2string prevWeightReused);
        -- print "prevTraceLength: "; printLn (float2string (int2float prevTraceLength));
        -- print "traceLength: "; printLn (float2string (int2float traceLength));
        let iter = subi iter 1 in
        if bernoulliSample (exp logMhAcceptProb) then
          mcmcAccept ();
          mh
            (cons weight weights)
            (cons weightReused weightsReused)
            (cons sample samples)
            iter
        else
          mh
            (cons prevWeight weights)
            (cons prevWeightReused weightsReused)
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
  let weightReused = deref state.weightReused in
  let iter = subi runs 1 in

  -- Sample the rest
  let res = mh [weight] [weightReused] [sample] iter in

  -- Reverse to get the correct order
  let res = match res with (weights,samples) in
    (reverse weights, reverse samples)
  in

  (if compileOptions.printAcceptanceRate then
    printLn (float2string (mcmcAcceptRate ()))
  else ());

  -- Return
  DistEmpirical {
    weights = weights,
    samples = samples,
    extra = EmpMCMC { acceptRate = mcmcAcceptRate () }
  }
