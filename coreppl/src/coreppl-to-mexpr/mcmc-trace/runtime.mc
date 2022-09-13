include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "math.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

-- Any-type, used for traces
type Any = ()

-- In trace MCMC, the state is the accumulated weight, samples, and samples to
-- reuse.
type State = {

  -- The weight of the current execution
  weight: Ref Float,

  -- The trace and trace length counter
  trace: Ref [Any],
  traceLength: Ref Int,

  -- The previous trace and when to cut
  oldTrace: Ref [Any],
  cut: Ref Int

}

-- NOTE(dlunde,2022-05-23): The below implementation does not
-- work with ropes for some reason (segfaults). We must therefore use lists.
let emptyList = createList 0 (lam. ())

-- State (reused throughout inference)
let state = {
  weight = ref 0.,
  trace = ref emptyList,
  traceLength = ref 0,
  oldTrace = ref emptyList,
  cut = ref 0
}

let updateWeight = lam v.
  let weight = state.weight in
  modref weight (addf (deref weight) v)

let sampleMH: all a. Dist a -> a = lam dist.
  let traceLength = deref state.traceLength in
  let cut = deref state.cut in
  let sample: Any =
    if lti traceLength cut then
      let r = deref state.oldTrace in
      let sample = head r in
      modref state.oldTrace (tail r);
      sample
    else unsafeCoerce (dist.sample ())
  in
  modref state.traceLength (addi (deref state.traceLength) 1);
  modref state.trace (cons sample (deref state.trace));
  unsafeCoerce sample

-- General inference algorithm for trace MCMC
let run : all a. (State -> a) -> (Res a -> ()) -> () =
  lam model. lam printResFun.

    -- Read number of runs and sweeps
    match monteCarloArgs () with (runs, sweeps) in

    recursive let mh : [Float] -> [a] -> Int -> ([Float], [a]) =
      lam weights. lam samples. lam iter.
        if leqi iter 0 then (weights, samples)
        else
          let prevTraceLength = deref state.traceLength in
          -- print "prev trace length: "; printLn (int2string prevTraceLength);
          modref state.weight 0.;
          modref state.traceLength 0;
          -- mapReverse (lam val. dprint val; printLn "") (deref state.trace);
          -- printLn (int2string (length (reverse (deref state.trace))));
          modref state.oldTrace (reverse (deref state.trace));
          modref state.trace emptyList;
          -- print "trace length: "; printLn (int2string (deref state.traceLength));
          modref state.cut (uniformDiscreteSample 0 (subi prevTraceLength 1));
          -- print "cut: "; printLn (int2string (deref state.cut));

          let sample = model state in
          let weight = deref state.weight in
          -- printResFun ([weight],[sample]); printLn "------";
          let prevWeight = head weights in
          let prevSample = head samples in
          let traceLength = deref state.traceLength in
          let logMhAcceptProb =
            minf 0. (addf (subf weight prevWeight)
                          (subf (log (int2float prevTraceLength))
                                (log (int2float traceLength)))) in
          let iter = subi iter 1 in
          if bernoulliSample (exp logMhAcceptProb) then
            mcmcAccept ();
            mh (cons weight weights) (cons sample samples) iter
          else
            mh (cons prevWeight weights) (cons prevSample samples) iter
    in

    -- Repeat once for each sweep
    repeat (lam.

        -- Used to keep track of acceptance ratio
        mcmcAcceptInit runs;

        -- First sample
        let sample = model state in
        let weight = deref state.weight in
        let iter = subi runs 1 in

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
  (if compileOptions.printAcceptanceRate then
    printLn (float2string (mcmcAcceptRate ()))
  else ());
  printSamples printFun (mapReverse (lam. 0.) res.0) res.1
