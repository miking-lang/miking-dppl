include "ext/dist-ext.mc"

-- NOTE(vipa, 2025-04-15): The contents of this file must be kept in
-- sync with typeCheckInferMethod and inferMethodConfig of
-- "inference/pval-graph.mc"
type SimplePValInterface instance ret =
  { instantiate : () -> {numAligned : Int, instance : instance}
  , getWeight : instance -> Float
  , getRet : instance -> ret
  -- NOTE(vipa, 2026-05-25): step gets a log-probability
  , step : (Float -> Bool) -> [Int] -> instance -> {accept : Bool, instance : instance}
  }

-- Only one of the returned sequences should be non-empty
type SimplePValRun ret = all instance. SimplePValInterface instance ret -> {smc : [(Float, ret)], mcmc : [(Bool, ret)]}

type SimplePValConfig ret =
  { run : SimplePValRun ret
  , debugOutput : String
  }

type SimpleMCMCConfig =
  { globalProb : Float
  , iterations : Int
  }
let simplePValGraphMCMC : all ret. SimpleMCMCConfig -> SimplePValRun ret = lam config.
  let acceptPred = lam prob. bernoulliSample (exp prob) in
  let f : SimplePValRun ret = lam interface.
    recursive let init = lam.
      let res = interface.instantiate () in
      if eqf (interface.getWeight res.instance) (negf inf)
      then init ()
      else res in
    match init () with {instance = instance, numAligned = numAligned} in
    if eqi numAligned 0 then
      {mcmc = [(true, interface.getRet instance)], smc = []}
    else
    recursive let work = lam acc.
      if eqi acc.iterations 0 then acc.samples else
      let toResample =
        if bernoulliSample config.globalProb
        then create numAligned (lam i. i)
        else [uniformDiscreteSample 0 (subi numAligned 1)] in
      let res = interface.step acceptPred toResample acc.instance in
      let acc =
        { acc with iterations = subi acc.iterations 1
        , instance = res.instance
        , samples = snoc acc.samples (res.accept, interface.getRet res.instance)
        } in
      work acc in
    { mcmc = work {iterations = subi config.iterations 1, instance = instance, samples = [(true, interface.getRet instance)]}
    , smc = []
    }
  in #frozen"f"
