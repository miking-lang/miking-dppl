include "../runtime-common.mc"
include "../runtime-dists.mc"

include "./config.mc"
include "./pval-mut.mc"
include "./pval-debug.mc"

lang SimplePValGraphBase = PValInterface + RuntimeDist
  sem simpleStoreAssume : all a. [PSomeAssumeRef] -> PAssumeRef a -> [PSomeAssumeRef]
  sem simpleStoreAssume st = | r ->
    snoc st (asSomeAssume (None ()) r)

  sem simpleStoreSubmodel st = | _ -> st

  sem simpleStoreExport : all a. [PSomeAssumeRef] -> PExportRef a -> ([PSomeAssumeRef], PExportRef a)
  sem simpleStoreExport st = | x -> (st, x)

  sem simpleStoreWeight st = | _ -> st

  sem reexportMapAccumL = | f -> mapAccumL f
end

lang SimplePValGraph = SimplePValGraphBase + MutPVal
end

lang DebugSimplePValGraph = SimplePValGraphBase + PValVisiGraph
end

type State = ()

let run
  : use SimplePValGraph in all ret. SimplePValConfig ret
  -> (PValState [PSomeAssumeRef] -> PValState ([PSomeAssumeRef], PExportRef ret))
  -> Dist ret
  = lam config. lam f.
    use SimplePValGraph in

    if not (null config.debugOutput) then
      use DebugSimplePValGraph in
      let instance = instantiate f [] in
      let json = graphToJson instance in
      writeFile config.debugOutput (json2string json);
      constructDistEmpirical [] [] (EmpMCMC {acceptRate = 0.0})
    else

    let interface : SimplePValInterface (PValInstance Complete ([PSomeAssumeRef], PExportRef ret)) ret =
      { instantiate = lam.
        let instance = instantiate f [] in
        {instance = instance, numAligned = length (getSt instance).0}
      , getWeight = getWeight
      , getRet = lam instance.
        readPreviousExport (getSt instance).1 instance
      , step = lam predicate. lam toResample. lam instance.
        (if null toResample then error "Inference error: SimplePVal got a resample step with no resampling." else ());
        let assumes = (getSt instance).0 in
        let instance = startStep instance in
        let instance = foldl
          (lam instance. lam idx. resampleSomeAssume (get assumes idx) instance)
          instance toResample in
        match finalizeStep predicate instance with (accept, instance) in
        {accept = accept, instance = instance}
      } in

    let startTime = wallTimeMs () in
    let res = config.run interface in
    let endTime = wallTimeMs () in
    printLn (join ["Simple time took: ", float2string (subf endTime startTime), "ms"]);
    if null res.smc then
      -- mcmc mode
      match mapAccumL (lam acc. lam pair. (if pair.0 then addi acc 1 else acc, pair.1)) 0 res.mcmc
        with (accept, samples) in
      constructDistEmpirical samples (map (lam. 1.0) samples)
        (EmpMCMC {acceptRate = divf (int2float accept) (int2float (length samples))})
    else
      -- smc mode
      match unzip res.smc with (weights, samples) in
      constructDistEmpirical samples weights
        (EmpNorm {normConst = normConstant weights})
