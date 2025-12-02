include "pval-interface.mc"


-- === General implementation of MCMC ===

lang MCMCPVal = PValInterface
  type MCMCConfig st a =
    { getSample : PValInstance Complete st -> a
    , step : PValInstance Partial st -> PValInstance Partial st
    , iterations : Int  -- TODO(vipa, 2025-09-24): Make this something more general
    }

  type MCMCResult st a =
    { samples : [a]
    , acceptanceRatio : Float
    , finalInstance : PValInstance Complete st
    }

  sem mcmc : all st. all a. MCMCConfig st a -> PValInstance Complete st -> MCMCResult st a
  sem mcmc config = | instance ->
    let acceptPred = lam. lam prob. bernoulliSample (exp prob) in
    recursive let work = lam acc.
      if eqi acc.iterations 0 then acc else
      match finalizeStep acceptPred (config.step (startStep acc.instance)) with (accepted, instance) in
      let acc =
        { iterations = subi acc.iterations 1
        , accepted = addi acc.accepted (if accepted then 1 else 0)
        , samples = snoc acc.samples (config.getSample instance)
        , instance = instance
        } in
      work acc in
    let res = work {iterations = config.iterations, accepted = 0, samples = [], instance = instance} in
    { samples = res.samples
    , acceptanceRatio = divf (int2float res.accepted) (int2float config.iterations)
    , finalInstance = res.instance
    }
end


-- === A simple store of assumes, generic enough to work for any model ===

let _chooseUniform : all a. [a] -> a
  = lam l. get l (uniformDiscreteSample 0 (subi (length l) 1))

lang SimpleState = PValInterface
  syn SimpleState x =
  | SimpleState {here : [PSomeAssumeRef], below : [PSubmodelRef (SimpleState ())], export : x}

  sem simpleInit : all x. x -> SimpleState x
  sem simpleInit = | export ->
    SimpleState {here = [], below = [], export = export}

  sem simpleStoreAssume : all a. all x. SimpleState x -> PAssumeRef a -> SimpleState x
  sem simpleStoreAssume st = | ref ->
    match st with SimpleState st in
    SimpleState {st with here = snoc st.here (asSomeAssume (lam d. lam. d) ref)}

  sem simpleStoreExport : all x1. all x2. SimpleState x1 -> PExportRef x2 -> SimpleState (PExportRef x2)
  sem simpleStoreExport st = | ref ->
    match st with SimpleState st in
    SimpleState {here = st.here, below = st.below, export = ref}

  sem simpleStoreWeight : all x. SimpleState x -> PWeightRef -> SimpleState x
  sem simpleStoreWeight st = | ref -> st

  sem simpleStoreSubmodel : all x. SimpleState x -> PSubmodelRef (SimpleState ()) -> SimpleState x
  sem simpleStoreSubmodel st = | ref ->
    match st with SimpleState st in
    SimpleState {st with below = snoc st.below ref}

  sem simpleReadExport : all x. all complete. PValInstance complete (SimpleState (PExportRef x)) -> x
  sem simpleReadExport = | instance ->
    match getSt instance with SimpleState st in
    readPreviousExport st.export instance

  sem simpleResampleAligned : all x. Float -> PValInstance Partial (SimpleState x) -> PValInstance Partial (SimpleState x)
  sem simpleResampleAligned globalProb = | instance ->
    match getSt instance with SimpleState st in
    if bernoulliSample globalProb then
      -- NOTE(vipa, 2025-12-09): We assume that all submodels depend
      -- on at least one aligned assume, so that if the aligned assume
      -- is resampled then the submodel will be replaced. I believe
      -- the only cases where this does not hold are 1) we have a
      -- `pure` value fed into a `bind` or 2) there's a `cache` before
      -- feeding into `bind`. Neither should happen with the automatic
      -- generation.
      foldr resampleSomeAssume instance st.here
    else
      resampleSomeAssume (_chooseUniform st.here) instance
end

lang SimpleMCMCPVal = SimpleState + MCMCPVal
  sem mkMCMCConfig : all x. Int -> Float -> MCMCConfig (SimpleState (PExportRef x)) x
  sem mkMCMCConfig iterations = | globalProb ->
    { getSample = simpleReadExport
    , step = simpleResampleAligned globalProb
    , iterations = iterations
    }
end
