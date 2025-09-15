include "ext/mat-ext.mc"
include "ext/dist-ext.mc"

include "json.mc"
include "set.mc"

let _cmpSym = lam a. lam b. subi (sym2hash a) (sym2hash b)
let eqb = lam a. lam b.
  if a then b else not b
type Never
type Never2


-- === Internals ===

type PDist a =
  { sample : () -> a
  , logObserve : a -> Float
  }
let p_sample : all a. PDist a -> a = lam dist. dist.sample ()
let p_logObserve : all a. PDist a -> a -> Float = lam dist. lam val. dist.logObserve val

type IterationID = Int


-- === "Shallow" representation of piece-wise static PVal models ===

type PVal x a
con PVal : all x. all a.
  { value : Ref a
  , changeId : Ref IterationID
  } -> PVal x a

type PState =
  { id : IterationID
  , permanentWeight : Ref Float
  , temporaryWeight : Ref Float
  , reset : Ref [() -> ()]
  }

type PValModel a =
  { update : PState -> (Bool, a)
  , read : () -> a
  }

type PValInterface x
con PVI : all x.
  { pure : all a. a -> PVal x a
  , identity : all a. all y. PVal y a -> PVal x a
  , map : all a. all b. all y. (a -> b) -> PVal y a -> PVal x b
  , mapWeight : all a. all b. all y. (a -> (Float, b)) -> PVal y a -> PVal x b
  , ap : all a. all b. all y. all z. PVal y (a -> b) -> PVal z a -> PVal x b
  , cache : all a. all y. (a -> a -> Bool) -> PVal y a -> PVal x a
  , bind : all a. all b. all y. (all z. PValInterface z -> a -> PVal z b) -> PVal y a -> PVal x b
  , assume : all a. all y. PVal y (PDist a) -> (Ref (a -> PDist a), Ref IterationID, PVal x a)
  } -> PValInterface x

recursive
let buildPValModel : all a. all b. IterationID -> (all x. PValInterface x -> (b, PVal x a)) -> (b, Float, PValModel a)
  = lam startId. lam mk.
    let updates : Ref [PState -> ()] = ref [] in
    let addUpdateF : (PState -> ()) -> () = lam f.
      modref updates (snoc (deref updates) f) in
    let initWeight : Ref Float = ref 0.0 in
    let interface = PVI
      { pure =
        let f : all a. a -> PVal () a = lam v.
          PVal {value = ref v, changeId = ref startId}
        in #frozen"f"
      , identity =
        let f : all a. all y. PVal y a -> PVal () a = lam p.
          match p with PVal p in
          PVal p
        in #frozen"f"
      , cache =
        let f : all a. all y. (a -> a -> Bool) -> PVal y a -> PVal () a = lam eq. lam a.
          match a with PVal a in
          let value = ref (deref a.value) in
          let changeId = ref startId in
          let update = lam st.
            if eqi st.id (deref a.changeId) then
              let prevValue = deref value in
              if eq prevValue (deref a.value) then () else
              modref value (deref a.value);
              modref changeId st.id;
              modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
            else () in
          addUpdateF update;
          PVal {value = value, changeId = changeId}
        in #frozen"f"
      , map =
        let f : all a. all b. all y. (a -> b) -> PVal y a -> PVal () b = lam f. lam a.
          match a with PVal a in
          let value = ref (f (deref a.value)) in
          let changeId = ref startId in
          let update = lam st.
            if eqi st.id (deref a.changeId) then
              let prevValue = deref value in
              modref value (f (deref a.value));
              modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
              modref changeId st.id
            else () in
          addUpdateF update;
          PVal {value = value, changeId = changeId}
        in #frozen"f"
      , mapWeight =
        let f : all a. all b. all y. (a -> (Float, b)) -> PVal y a -> PVal () b = lam f. lam a.
          match a with PVal a in
          match f (deref a.value) with (weight, value) in
          modref initWeight (addf (deref initWeight) weight);
          let weight = ref weight in
          let value = ref value in
          let changeId = ref startId in
          let update = lam st.
            if eqi st.id (deref a.changeId) then
              match f (deref a.value) with (newWeight, newValue) in
              let prevValue = deref value in
              let prevWeight = deref weight in
              modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));
              modref value newValue;
              modref weight newWeight;
              let reset = lam.
                modref value prevValue;
                modref weight prevWeight in
              modref st.reset (snoc (deref st.reset) reset);
              modref changeId st.id
            else () in
          addUpdateF update;
          PVal {value = value, changeId = changeId}
        in #frozen"f"
      , ap =
        let f : all a. all b. all y. all z. PVal y (a -> b) -> PVal z a -> PVal () b = lam f. lam a.
          match f with PVal f in
          match a with PVal a in
          let value = ref ((deref f.value) (deref a.value)) in
          let changeId = ref startId in
          let update = lam st.
            if or (eqi st.id (deref f.changeId)) (eqi st.id (deref a.changeId)) then
              let prevValue = deref value in
              modref value ((deref f.value) (deref a.value));
              modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
              modref changeId st.id
            else () in
          addUpdateF update;
          PVal {value = value, changeId = changeId}
        in #frozen"f"
      , bind =
        let f : all a. all b. all y. (all z. PValInterface z -> a -> PVal z b) -> PVal y a -> PVal () b = lam f. lam a.
          match a with PVal a in
          let f : all z. PValInterface z -> (b, PVal z b) = lam pvi.
            let pval = f pvi (deref a.value) in
            (match pval with PVal res in deref res.value, pval) in
          match buildPValModel startId #frozen"f" with (value, weight, model) in
          modref initWeight (addf (deref initWeight) weight);
          let model = ref model in
          let value = ref value in
          let weight = ref weight in
          let changeId = ref startId in
          let update = lam st.
            if eqi st.id (deref a.changeId) then
              -- We've updated the argument, create a new sub-model
              let prevModel = deref model in
              let prevValue = deref value in
              let prevWeight = deref weight in
              match buildPValModel st.id #frozen"f" with (newValue, newWeight, newModel) in
              modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));
              modref model newModel;
              modref value newValue;
              modref weight newWeight;
              modref changeId st.id;
              let reset = lam.
                modref model prevModel;
                modref value prevValue;
                modref weight prevWeight in
              modref st.reset (snoc (deref st.reset) reset)
            else
              -- We've not updated the argument, update the sub-model
              let subSt = {st with permanentWeight = ref 0.0} in
              match (deref model).update subSt with (changed, newValue) in
              if changed then
                let prevValue = deref value in
                let prevWeight = deref weight in
                let newWeight = deref subSt.permanentWeight in
                modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));
                modref value newValue;
                modref weight newWeight;
                modref changeId st.id;
                let reset = lam.
                  modref value prevValue;
                  modref weight prevWeight in
                modref st.reset (snoc (deref st.reset) reset)
              else ()
            in
          addUpdateF update;
          PVal {value = value, changeId = changeId}
        in #frozen"f"
      , assume =
        let f : all a. all y. PVal y (PDist a) -> (Ref (a -> PDist a), Ref IterationID, PVal () a) = lam dist.
          match dist with PVal dist in
          let value = ref (p_sample (deref dist.value)) in
          let changeId = ref startId in
          let weight = ref (p_logObserve (deref dist.value) (deref value)) in
          let drift = ref (lam. (deref dist.value)) in
          let update = lam st.
            if eqi st.id (deref changeId) then
              -- Draw a new sample, i.e., value changes
              let prevValue = deref value in
              let prevWeight = deref weight in

              let kernel = (deref drift) prevValue in
              let proposal = p_sample kernel in
              let reverseKernel = (deref drift) proposal in

              let newWeight = p_logObserve (deref dist.value) proposal in

              let prevToProposalProb = p_logObserve kernel proposal in
              let proposalToPrevProb = p_logObserve reverseKernel prevValue in

              modref value proposal;
              modref weight newWeight;
              modref changeId st.id;
              let reset = lam.
                modref value prevValue;
                modref weight prevWeight in
              modref st.reset (snoc (deref st.reset) reset);
              modref st.temporaryWeight
                (addf
                  (addf
                    (deref st.temporaryWeight)
                    (subf newWeight prevWeight))
                  (subf proposalToPrevProb prevToProposalProb))
            else if eqi st.id (deref dist.changeId) then
              -- Reuse current sample, i.e., value doesn't change
              let prevWeight = deref weight in
              let newWeight = p_logObserve (deref dist.value) (deref value) in
              modref weight newWeight;
              modref st.reset (snoc (deref st.reset) (lam. modref weight prevWeight));
              modref st.temporaryWeight (addf (deref st.temporaryWeight) (subf newWeight prevWeight))
            else () in
          addUpdateF update;
          (drift, changeId, PVal {value = value, changeId = changeId})
        in #frozen"f"
      } in
    match mk interface with (b, pval) in
    let updates = deref updates in
    let mkModel : PVal () a -> PValModel a = lam p.
      match p with PVal p in
      { update = lam st.
        for_ updates (lam up. up st);
        (eqi st.id (deref p.changeId), deref p.value)
      , read = lam. deref p.value
      } in
    (b, deref initWeight, mkModel pval)
end


-- === MCMC ===

let p_init : Float -> {id : IterationID, permanentWeight : Float}
  = lam initWeight. {id = 0, permanentWeight = initWeight}

let p_step
  : all a. {id : IterationID, permanentWeight : Float}
  -> (IterationID -> ())
  -> PValModel a
  -> ({id : IterationID, permanentWeight : Float}, Bool, a)
  = lam prevSt. lam triggerResample. lam p.
    let st =
      { id = addi prevSt.id 1
      , reset = ref []
      , permanentWeight = ref prevSt.permanentWeight
      , temporaryWeight = ref 0.0
      } in
    triggerResample st.id;
    let newValue = (p.update st).1 in
    let acceptProb = minf 0.0
      (addf
        (subf (deref st.permanentWeight) prevSt.permanentWeight)
        (deref st.temporaryWeight)) in
    if bernoulliSample (exp acceptProb) then
      ({id = st.id, permanentWeight = deref st.permanentWeight}, true, newValue)
    else
      for_ (deref st.reset) (lam f. f ());
      ({prevSt with id = st.id}, false, p.read ())

let p_runInference : all a. (IterationID -> ()) -> PValModel a -> Float -> Int -> [a]
  = lam triggerResample. lam p. lam initWeight. lam totalIterations.
    recursive let work = lam st. lam accepts. lam list. lam iterations.
      if eqi iterations 0 then
        list
      else
      match p_step st triggerResample p with (st, accepted, value) in
      work st (if accepted then addi accepts 1 else accepts) (snoc list value) (subi iterations 1)
    in work (p_init initWeight) 0 [p.read ()] totalIterations


-- === Helpers for writing models, timing execution, and printing/summarizing results ===

let timeF : all a. (() -> a) -> (Float, a)
  = lam f.
    let before = wallTimeMs () in
    let res = f () in
    let after = wallTimeMs () in
    (subf after before, res)

let p_bernoulli : Float -> PDist Bool
  = lam p.
    { sample = lam. bernoulliSample p
    , logObserve = lam v. bernoulliLogPmf p v
    }

let p_uniformDiscrete : Int -> Int -> PDist Int
  = lam min. lam max.
    { sample = lam. uniformDiscreteSample min max
    , logObserve = lam x. uniformDiscreteLogPdf min max x
    }

let p_gaussian : Float -> Float -> PDist Float
  = lam mu. lam sigma.
    { sample = lam. gaussianSample mu sigma
    , logObserve = lam x. gaussianLogPdf mu sigma x
    }

let p_beta : Float -> Float -> PDist Float
  = lam a. lam b.
    { sample = lam. betaSample a b
    , logObserve = lam x. betaLogPdf a b x
    }

let _chooseUniform : all a. [a] -> a
  = lam l. get l (uniformDiscreteSample 0 (subi (length l) 1))

let interval2string : (Float, Float) -> String
  = lam pair.
    join [float2string pair.0, "-", float2string pair.1]

let histogram : all a. (a -> a -> Int) -> [a] -> [(a, Float)]
  = lam cmp. lam l.
    let hist = foldl (lam acc. lam a. mapInsertWith addi a 1 acc) (mapEmpty cmp) l in
    let count = int2float (mapFoldWithKey (lam total. lam. lam count. addi total count) 0 hist) in
    let hist = mapMap (lam v. divf (int2float v) count) hist in
    mapBindings hist

let bucket : all a. Int -> Float -> Float -> [Float] -> [((Float, Float), Float)]
  = lam numBuckets. lam min. lam max. lam l.
    let bucketSize = divf (subf max min) (int2float numBuckets) in
    let hist = mapFromSeq subi (create numBuckets (lam i. (i, 0))) in
    let f = lam acc. lam x. mapInsertWith addi (floorfi (divf (subf x min) bucketSize)) 1 acc in
    let hist = foldl f hist l in
    let count = int2float (mapFoldWithKey (lam total. lam. lam count. addi total count) 0 hist) in
    let convPair = lam pair.
      let base = addf min (mulf bucketSize (int2float pair.0)) in
      ( (base, addf bucketSize base)
      , divf (int2float pair.1) count
      ) in
    map convPair (mapBindings hist)

let progressBarNoPad : Int -> Float -> String
  = lam width. lam fraction.
    let filled = roundfi (mulf (int2float width) fraction) in
    make filled '=' -- (make (subi width filled) ' ')

let hist2string : all a. (a -> String) -> [(a, Float)] -> String
  = lam toStr. lam l.
    strJoin "\n" (map (lam pair. join [toStr pair.0, "\t", float2string pair.1, "\t", progressBarNoPad 100 pair.1]) l)

mexpr


-- === Smaller test models ===

let bern_and =
  let run = lam.
    let a = assume (Bernoulli 0.5) in
    let b = assume (Bernoulli 0.5) in
    and a b in
  let p_run =
    let f = lam pvi.
      match pvi with PVI pvi in
      match pvi.assume (pvi.pure (p_bernoulli 0.5)) with (_, drawA, a) in
      match pvi.assume (pvi.pure (p_bernoulli 0.5)) with (_, drawB, b) in
      ([drawA, drawB], pvi.ap (pvi.map and a) b) in
    match buildPValModel 0 #frozen"f" with (draws, initWeight, p) in
    let draws = snoc
      (map modref draws)
      (lam id. iter (lam r. modref r id) draws) in
    (lam id. _chooseUniform draws id, initWeight, p) in
  match timeF (lam. p_runInference p_run.0 p_run.2 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n bern_and";
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string bool2string (histogram cmpBool p_res));
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples res).0));
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples resP).0));
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples resA).0));
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples res2).0));
  ()
in

let simple_bind =
  let run = lam.
    if assume (Bernoulli 0.5)
    then assume (Bernoulli 0.9)
    else assume (Bernoulli 0.5) in
  let p_run =
    let f = lam pvi.
      match pvi with PVI pvi in
      match pvi.assume (pvi.pure (p_bernoulli 0.5)) with (_, drawC, c) in
      let f = lam pvi. lam c.
        match pvi with PVI pvi in
        if c
        then (pvi.assume (pvi.pure (p_bernoulli 0.9))).2
        else (pvi.assume (pvi.pure (p_bernoulli 0.5))).2 in
      (lam id. modref drawC id, pvi.bind #frozen"f" c) in
    buildPValModel 0 #frozen"f" in
  match timeF (lam. p_runInference p_run.0 p_run.2 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n simple_bind";
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string bool2string (histogram cmpBool p_res));
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples res).0));
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples resP).0));
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples resA).0));
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string bool2string (histogram cmpBool (distEmpiricalSamples res2).0));
  ()
in

let manual_geometric =
  let run = lam.
    recursive let work = lam acc.
      if assume (Bernoulli 0.5)
      then work (addi acc 1)
      else acc in
    let c = assume (Bernoulli 0.5) in
    if c
    then work 1
    else 0 in
  let p_run =
    let f = lam pvi.
      match pvi with PVI pvi in
      recursive let work : all z. Int -> PValInterface z -> Bool -> PVal z Int = lam acc. lam pvi. lam c.
        let recur = lam x. lam y. work (addi acc 1) x y in
        match pvi with PVI pvi in
        if c
        then pvi.bind #frozen"recur" (pvi.assume (pvi.pure (p_bernoulli 0.5))).2
        else pvi.pure acc in
      match pvi.assume (pvi.pure (p_bernoulli 0.5)) with (_, draw, c) in
      let start = lam x. work 0 x in
      (modref draw, pvi.bind #frozen"start" c) in
    buildPValModel 0 #frozen"f" in
  let p_runCache =
    let f = lam pvi.
      match pvi with PVI pvi in
      recursive let work : all z. Int -> PValInterface z -> Bool -> PVal z Int = lam acc. lam pvi. lam c.
        let recur = lam x. lam y. work (addi acc 1) x y in
        match pvi with PVI pvi in
        if c
        then pvi.bind #frozen"recur" (pvi.assume (pvi.pure (p_bernoulli 0.5))).2
        else pvi.pure acc in
      match pvi.assume (pvi.pure (p_bernoulli 0.5)) with (_, draw, c) in
      let start = lam x. work 0 x in
      (modref draw, pvi.bind #frozen"start" (pvi.cache eqb c)) in
    buildPValModel 0 #frozen"f" in
  match timeF (lam. p_runInference p_run.0 p_run.2 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. p_runInference p_runCache.0 p_runCache.2 p_runCache.1 100000) with (p_timeCache, p_resCache) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n manual_geometric";
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string int2string (histogram subi p_res));
  printLn (join [float2string p_timeCache, "ms (PVal cache)"]);
  printLn (hist2string int2string (histogram subi p_resCache));
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string int2string (histogram subi (distEmpiricalSamples res).0));
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string int2string (histogram subi (distEmpiricalSamples resP).0));
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string int2string (histogram subi (distEmpiricalSamples resA).0));
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string int2string (histogram subi (distEmpiricalSamples res2).0));
  ()
in

-- -- modref debug 1;

let coin =
  let observations = [true, true, true, false, true, true, false, true] in
  let run = lam.
    let p = assume (Beta 1.0 1.0) in
    for_ observations (lam o. observe o (Bernoulli p));
    p in
  let p_run =
    let f = lam pvi.
      match pvi with PVI pvi in
      match pvi.assume (pvi.pure (p_beta 1.0 1.0)) with (_, draw, p) in
      let f = lam p.
        let weights = map (p_logObserve (p_bernoulli p)) observations in
        let sum = foldl addf 0.0 weights in
        (sum, p) in
      (modref draw, pvi.mapWeight f p) in
    buildPValModel 0 #frozen"f" in
  match timeF (lam. p_runInference p_run.0 p_run.2 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n coin";
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string interval2string (bucket 10 0.0 1.0 p_res));
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string interval2string (bucket 10 0.0 1.0 (distEmpiricalSamples res).0));
  printLn (join ["Accept rate: ", float2string (distEmpiricalAcceptRate res)]);
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string interval2string (bucket 10 0.0 1.0 (distEmpiricalSamples resP).0));
  printLn (join ["Accept rate: ", float2string (distEmpiricalAcceptRate resP)]);
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string interval2string (bucket 10 0.0 1.0 (distEmpiricalSamples resA).0));
  printLn (join ["Accept rate: ", float2string (distEmpiricalAcceptRate resA)]);
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string interval2string (bucket 10 0.0 1.0 (distEmpiricalSamples res2).0));
  printLn (join ["Accept rate: ", float2string (distEmpiricalAcceptRate res2)]);
  ()
in

let tree_inference =
  type Tree in
  con Leaf : {id : Int, x : Float} -> Tree in
  con Node : {left : Tree, right : Tree, x : Float} -> Tree in

  recursive let asShape = lam t. switch t
    case Leaf x then int2string x.id
    case Node x then join ["(", asShape x.left, ", ", asShape x.right, ")"]
    end in

  let initTrees =
    [ Leaf {id = 0, x = 0.0}
    , Leaf {id = 1, x = 5.0}
    , Leaf {id = 2, x = 10.0}
    , Leaf {id = 3, x = 15.0}
    -- , Leaf {id = 4, x = 20.0}
    ] in

  recursive let minId = lam t. switch t
    case Leaf x then x.id
    case Node x then mini (minId x.left) (minId x.right)
    end in
  let getX = lam t. switch t
    case Leaf x then x.x
    case Node x then x.x
    end in
  let mkNode = lam x. lam l. lam r.
    if lti (minId l) (minId r)
    then Node {left = l, right = r, x = x}
    else Node {left = r, right = l, x = x} in

  let p_runTree =
    let f = lam pvi.
      match pvi with PVI pvi in
      let pickpair = lam resamples. lam n.
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
        let f = lam i. lam j.
          if lti j i then (j,i) else (i, addi j 1) in
        ( concat resamples [resA, resB]
        , pvi.ap (pvi.map f a) b
        ) in

      match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
      let deviateFromDist = lam x. p_gaussian x 10.0 in
      let rootDist = pvi.map deviateFromDist rootValue in

      recursive let cluster = lam resamples. lam trees.
        match trees with [tree] then (resamples, tree) else
        match pickpair resamples (length trees) with (resamples, pair) in
        match pvi.assume rootDist with (_, resHere, here) in
        let resamples = snoc resamples resHere in
        let carryOn = lam idx. lam pvi. lam pair.
          match pvi with PVI pvi in
          match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
          with (l, r) in
          pvi.identity (get trees (addi idx (if leqi l idx then if leqi r (addi idx 1) then 2 else 1 else 0))) in
        let carryOns = create (subi (length trees) 2) (lam i. let f = lam x. carryOn i x in pvi.bind #frozen"f" pair) in
        let treePair =
          let f = lam pvi. lam pair.
            match pvi with PVI pvi in
            pvi.ap (pvi.map (lam l. lam r. (l, r)) (get trees pair.0)) (get trees pair.1) in
          pvi.bind #frozen"f" pair in
        let f = lam rootDist. lam here. lam treePair.
          let l = treePair.0 in
          let r = treePair.1 in
          let calcWeight = lam t. addf
            (negf (p_logObserve rootDist (getX t)))
            (p_logObserve (deviateFromDist here) (getX t)) in
          (addf (calcWeight l) (calcWeight r), mkNode here l r) in
        cluster resamples (snoc carryOns (pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.map f rootDist) here) treePair))) in

      let addInitialObservation =
        let obsInit = lam rootDist.
          ( foldl addf 0.0
            (map (lam t. p_logObserve rootDist (getX t)) initTrees)
          , ()
          ) in
        pvi.ap (pvi.map (lam. lam x. x) (pvi.cache (lam. lam. true) (pvi.mapWeight obsInit rootDist))) in
      match cluster [resRoot] (map pvi.pure initTrees)
      with (resamples, tree) in
      ( lam id. _chooseUniform
        (snoc (map modref resamples)
          (lam id. for_ resamples (lam r. modref r id)))
        id
      , addInitialObservation tree
      ) in
    buildPValModel 0 #frozen"f" in

  let p_runChunky =
    let f = lam pvi.
      match pvi with PVI pvi in
      let pickpair = lam resamples. lam n.
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
        let f = lam i. lam j.
          if lti j i then (j,i) else (i, addi j 1) in
        ( concat resamples [resA, resB]
        , pvi.ap (pvi.map f a) b
        ) in

      match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
      let deviateFromDist = lam x. p_gaussian x 10.0 in
      let rootDist = pvi.map deviateFromDist rootValue in

      recursive let cluster = lam resamples. lam nTrees. lam trees.
        if eqi nTrees 1 then (resamples, pvi.map head trees) else
        match pickpair resamples nTrees with (resamples, pair) in
        match pvi.assume rootDist with (_, resHere, here) in
        let resamples = snoc resamples resHere in
        let f = lam rootDist. lam here. lam pair. lam trees.
          let l = get trees pair.0 in
          let r = get trees pair.1 in
          let trees = mapOption (lam x. x)
            (mapi (lam idx. lam v. if or (eqi idx pair.0) (eqi idx pair.1) then None () else Some v) trees) in
          let calcWeight = lam t. addf
            (negf (p_logObserve rootDist (getX t)))
            (p_logObserve (deviateFromDist here) (getX t)) in
          (addf (calcWeight l) (calcWeight r), snoc trees (mkNode here l r))
        in
        cluster resamples (subi nTrees 1) (pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map f rootDist) here) pair) trees)) in

      let f = lam rootDist.
        ( foldl addf 0.0
          (map (lam t. p_logObserve rootDist (getX t)) initTrees)
        , initTrees
        ) in
      match cluster [resRoot] (length initTrees) (pvi.cache (lam. lam. true) (pvi.mapWeight f rootDist))
      with (resamples, tree) in
      ( lam id. _chooseUniform
        (snoc (map modref resamples)
          (lam id. for_ resamples (lam r. modref r id)))
        id
      , tree
      ) in
    buildPValModel 0 #frozen"f" in

  let p_runDag =
    let f = lam pvi.
      match pvi with PVI pvi in
      let pickpair = lam resamples. lam n.
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
        match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
        let f = lam i. lam j.
          if lti j i then (j,i) else (i, addi j 1) in
        ( concat resamples [resA, resB]
        , pvi.ap (pvi.map f a) b
        ) in

      match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
      let deviateFromDist = lam x. p_gaussian x 10.0 in
      let rootDist = pvi.map deviateFromDist rootValue in

      recursive let cluster = lam resamples. lam nTrees. lam trees.
        if eqi nTrees 1 then (resamples, pvi.map head trees) else
        match pickpair resamples nTrees with (resamples, pair) in
        let i = pvi.map (lam p. p.0) pair in
        let j = pvi.map (lam p. p.1) pair in
        match pvi.assume rootDist with (_, resHere, here) in
        let resamples = snoc resamples resHere in
        let fetchTree = lam idx. lam rootDist. lam here. lam trees.
          let t = get trees idx in
          ( addf
            (negf (p_logObserve rootDist (getX t)))
            (p_logObserve (deviateFromDist here) (getX t))
          , t
          ) in
        let l = pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map fetchTree i) rootDist) here) trees) in
        let r = pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map fetchTree j) rootDist) here) trees) in
        let trees =
          let f = lam p. lam trees. mapOption (lam x. x) (mapi (lam idx. lam v. if or (eqi idx p.0) (eqi idx p.1) then None () else Some v) trees) in
          pvi.ap (pvi.map f pair) trees in
        let addMerged = lam l. lam r. lam here. lam trees.
          snoc trees (mkNode here l r) in
        let trees = pvi.ap (pvi.ap (pvi.ap (pvi.map addMerged l) r) here) trees in
        cluster resamples (subi nTrees 1) trees in

      let f = lam rootDist.
        ( foldl addf 0.0
          (map (lam t. p_logObserve rootDist (getX t)) initTrees)
        , initTrees
        ) in
      match cluster [resRoot] (length initTrees) (pvi.cache (lam. lam. true) (pvi.mapWeight f rootDist))
      with (resamples, tree) in
      ( lam id. _chooseUniform
        (snoc (map modref resamples)
          (lam id. for_ resamples (lam r. modref r id)))
        id
      , tree
      ) in
    buildPValModel 0 #frozen"f" in

  let run = lam.
    let pickpair = lam n.
      let i = assume (UniformDiscrete 0 (subi n 1)) in
      let j = assume (UniformDiscrete 0 (subi n 2)) in
      if lti j i then (i,j) else (i,addi j 1) in

    let rootValue = assume (Gaussian 0.0 10.0) in
    let deviateFromDist = lam x. Gaussian x 10.0 in
    let rootDist = deviateFromDist rootValue in
    let cancelRootDist = lam x.
      weight (negf (gaussianLogPdf rootValue 10.0 x)) in

    recursive let cluster = lam nTrees. lam trees.
      if eqi nTrees 1 then head trees else
      match pickpair nTrees with (i, j) in
      let l = get trees i in
      let r = get trees j in
      let trees = mapOption (lam x. x) (mapi (lam idx. lam v. if or (eqi idx i) (eqi idx j) then None () else Some v) trees) in
      let here = assume rootDist in
      cancelRootDist (getX l);
      cancelRootDist (getX r);
      observe (getX l) (deviateFromDist here);
      observe (getX r) (deviateFromDist here);
      cluster (subi nTrees 1) (snoc trees (mkNode here l r)) in

    for_ initTrees (lam t. observe (getX t) rootDist);
    cluster (length initTrees) initTrees in

  printLn "\n tree_inference";
  match timeF (lam. p_runInference p_runTree.0 p_runTree.2 p_runTree.1 1000000) with (p_time, p_res) in
  printLn (join [float2string p_time, "ms (PVal Tree)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runTree.1).0 .1);
  match timeF (lam. p_runInference p_runChunky.0 p_runChunky.2 p_runChunky.1 1000000) with (p_time, p_res) in
  printLn (join [float2string p_time, "ms (PVal Chunky)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runChunky.1).0 .1);
  match timeF (lam. p_runInference p_runDag.0 p_runDag.2 p_runDag.1 1000000) with (p_time, p_res) in
  printLn (join [float2string p_time, "ms (PVal DAG)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
  -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runDag.1).0 .1);
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (time, res) in
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res).0)));
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeA, resA) in
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resA).0)));
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeP, resP) in
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resP).0)));
  match timeF (lam. infer (Importance {particles = 1000000}) run) with (time2, res2) in
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res2).0)));
  ()
in

()
