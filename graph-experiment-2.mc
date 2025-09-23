include "ext/mat-ext.mc"
include "ext/dist-ext.mc"

include "json.mc"
include "set.mc"

let _cmpSym = lam a. lam b. subi (sym2hash a) (sym2hash b)
let eqb = lam a. lam b.
  if a then b else not b
type Never
type Never2


-- === Distribution types ===

type PDist a =
  { sample : () -> a
  , logObserve : a -> Float
  }
let p_sample : all a. PDist a -> a = lam dist. dist.sample ()
let p_logObserve : all a. PDist a -> a -> Float = lam dist. lam val. dist.logObserve val


-- === Finally tagless representation of piece-wise static PVal models ===

lang PValInterfaceBase
  syn PVal x a =
end

type SomePVal x
con SomePVal : all x. all a. use PValInterfaceBase in PVal x a -> SomePVal x

type HCons a as
type HConsL a as
type HNil

type PValHList x l
con PVHCons : all x. all a. all l. (use PValInterfaceBase in PVal x a, PValHList x l) -> PValHList x (HCons a l)
con PVHConsL : all x. all a. all l. (use PValInterfaceBase in [PVal x a], PValHList x l) -> PValHList x (HConsL a l)
con PVHNil : all x. () -> PValHList x HNil

type Complete
type Partial

lang PValInterface = PValInterfaceBase
  syn PValState st y =

  syn PWeightRef =
  syn PAssumeRef a =
  syn PExportRef a =
  syn PSubmodelRef st =


  syn PValInstance complete st =


  -- === Working with an instance of a model ===

  -- Create a new instance of a model (completely evaluated).
  sem instantiate : all st. (all y. PValState st y -> PValState st y) -> st -> PValInstance Complete st
  -- Get the state in the instance (that contains references to
  -- internals of the model).
  sem getSt : all complete. all st. PValInstance complete st -> st
  -- Start accumulating actions to perform in a single update step.
  sem startStep : all st. PValInstance Complete st -> PValInstance Partial st
  -- Takes a predicate, typically probabilistic, that decides whether
  -- to return the model instance as it was before the step (if false)
  -- or after (if true). The predicate is given the post-step instance
  -- and an mcmc acceptance (log-)ratio computed from the step itself.
  sem finalizeStep : all st. (PValInstance Complete st -> Float -> Bool) -> PValInstance Partial st -> (Bool, PValInstance Complete st)
  -- Mark a given assume to be resampled in the upcoming step.
  sem resampleAssume : all st. all a. PAssumeRef a -> PValInstance Partial st -> PValInstance Partial st
  -- Set the drift kernel to use for the given assume. Will be used in
  -- *all* upcoming steps, not just the current one.
  sem setAssumeDrift : all st. all a. (a -> PDist a) -> PAssumeRef a -> PValInstance Partial st -> PValInstance Partial st
  -- Read the current value produced by the given assume. In a
  -- `Partial` instance this returns the value before the step was
  -- initiated.
  sem readPreviousAssume : all complete. all st. all a. PAssumeRef a -> PValInstance complete st -> a
  -- Read the current value produced by the given export. In a
  -- `Partial` instance this returns the value before the step was
  -- initiated.
  sem readPreviousExport : all complete. all st. all a. PExportRef a -> PValInstance complete st -> a
  -- Read the current state of the given sub-model. In a `Partial`
  -- instance this returns the value before the step was initiated.
  sem readPreviousSubmodel : all complete. all st. all st2. PSubmodelRef st2 -> PValInstance complete st -> st2


  -- === Building a model ===

  -- Insert a dynamic check for if a given value has changed, removing
  -- the need to update later portions of the model if the newly
  -- produced value is the same as the previous.
  sem p_cache : all st. all x. all y. all a. PValState st y
    -> (a -> a -> Bool)
    -> PVal y a
    -> (PValState st y, PVal y a)
  -- Make a probabilistic value available to read outside the model,
  -- via a model instance.
  sem p_export : all st. all x. all y. all a. PValState st y
    -> (st -> PExportRef a -> st)
    -> PVal y a
    -> PValState st y

  -- Lift a deterministic value to a probabilistic one. (from the
  -- Pointed type class)
  sem p_pure : all st. all x. all y. all a. PValState st y
    -> a
    -> (PValState st y, PVal y a)
  -- Make a deterministic "update" to a probabilistic value. (from the
  -- Functor type class)
  sem p_map : all st. all x. all y. all a. all b. PValState st y
    -> (a -> b)
    -> PVal y a
    -> (PValState st y, PVal y b)
  -- Combine two probabilistic values. Often used in conjunction with
  -- `p_map`. (from the Applicative type class)
  sem p_apply : all st. all x. all y. all a. all b. PValState st y
    -> PVal y (a -> b)
    -> PVal y a
    -> (PValState st y, PVal y b)
  -- Create a sub-model whose shape depends on a probabilistic
  -- value. Note that the sub-model must explicitly list its inputs
  -- (the PValHList argument). (mostly equivalent with bind from
  -- Monad, but slightly weaker because of the explicit inputs)
  sem p_bind : all st. all st2. all y. all a. all as. all b. PValState st y
    -> (st -> PSubmodelRef st2 -> st)
    -> st2
    -> (all z. PValState st2 z -> a -> PValHList z as -> (PValState st2 z, PVal z b))
    -> PVal y a
    -> PValHList y as
    -> (PValState st y, PVal y b)
  -- Select which probabilistic value to propagate based on the value
  -- of another probabilistic value. Weaker version of bind that
  -- sometimes admits more efficient implementation. (mostly
  -- equivalent with select from Selective)
  sem p_select : all st. all y. all a. all as. all b. PValState st y
    -> (all z. a -> PValHList z as -> PVal z b)
    -> PVal y a
    -> PValHList y as
    -> (PValState st y, PVal y b)

  -- Introduce a weight.
  sem p_weight : all st. all x. all y. all a. PValState st y
    -> (st -> PWeightRef -> st)
    -> (a -> Float)
    -> PVal y a
    -> PValState st y
  -- Draw a value from a distribution.
  sem p_assume : all st. all x. all y. all a. PValState st y
    -> (st -> PAssumeRef a -> st)
    -> PVal y (PDist a)
    -> (PValState st y, PVal y a)

  -- Versions that don't record their reference in the state
  sem p_bind_ : all st. all x. all y. all a. all as. all b. PValState st y
    -> (all z. PValState () z -> a -> PValHList z as -> (PValState () z, PVal z b))
    -> PVal y a
    -> PValHList y as
    -> (PValState st y, PVal y b)
  sem p_bind_ st f a = | refs -> p_bind st (lam st. lam. st) () #frozen"f" a refs

  sem p_weight_ : all st. all x. all y. all a. PValState st y
    -> (a -> Float)
    -> PVal y a
    -> PValState st y
  sem p_weight_ st f = | a -> p_weight st (lam st. lam. st) f a

  sem p_assume_ : all st. all x. all y. all a. PValState st y
    -> PVal y (PDist a)
    -> (PValState st y, PVal y a)
  sem p_assume_ st = | dist -> p_assume st (lam st. lam. st) dist
end

lang PValDefaultSelect = PValInterface
  sem p_select st f a = | as ->
    let f : all z. PValState () z -> a -> PValHList z as -> Unknown = lam st. lam a. lam as. (st, f a as) in
    p_bind_ st #frozen"f" a as
end


-- === Mutable PVal model instances (should be used affinely) ===

lang MutPVal = PValInterface + PValDefaultSelect
  type IterationID = Int

  type PValRec a = {value : Ref a, changeId : Ref IterationID}
  syn PVal x a = | PVal (PValRec a)

  syn PValState st y = | PVS {initId : IterationID, updates : [PState -> ()], initWeight : Float, st : st}
  syn PWeightRef = | PWeightRef {} -- TODO(vipa, 2025-09-23): figure out what we want to be able to do here, and thus what we need to store
  syn PAssumeRef a = | PAssumeRef {drift : Ref (a -> PDist a), changeId : Ref IterationID, read : () -> a}
  syn PExportRef a = | PExportRef {read : () -> a}
  syn PSubmodelRef st = | PSubmodelRef {readSt : () -> st}

  type PState =
    { id : IterationID
    , permanentWeight : Ref Float
    , temporaryWeight : Ref Float
    , reset : Ref [() -> ()]
    }
  type UpdateFunction = PState -> ()

  syn PValInstance complete st =
  | PVI {st : st, update : UpdateFunction, permanentWeight : Float, id : IterationID}

  sem initModel : all st. all a. st -> (all y. PValState st y -> PValState st y) -> (st, Float, UpdateFunction)
  sem initModel initSt = | f -> _initModel 0 initSt f
  sem _initModel : all st. all st2. all a. all y. IterationID -> st -> (PValState st y -> PValState st2 y) -> (st2, Float, UpdateFunction)
  sem _initModel initId initSt = | f ->
    let st = PVS
      { updates = []
      , initWeight = 0.0
      , initId = initId
      , st = initSt
      } in
    match f st with PVS st in
    let updates = st.updates in
    let update = lam st.
      for_ updates (lam up. up st) in
    (st.st, st.initWeight, update)

  sem instantiate f = | st ->
    match _initModel 0 st f with (st, initWeight, update) in
    PVI {st = st, update = update, permanentWeight = initWeight, id = 0}

  sem getSt = | PVI x -> x.st

  sem startStep = | PVI x -> PVI {x with id = addi x.id 1}

  sem finalizeStep pred = | PVI x ->
    let st =
      { id = x.id
      , permanentWeight = ref x.permanentWeight
      , temporaryWeight = ref 0.0
      , reset = ref []
      } in
    x.update st;
    let acceptProb = minf 0.0
      (addf
        (subf (deref st.permanentWeight) x.permanentWeight)
        (deref st.temporaryWeight)) in
    let new = PVI {x with permanentWeight = deref st.permanentWeight} in
    if pred new acceptProb then (true, new) else
    for_ (deref st.reset) (lam f. f ());
    (false, PVI x)

  sem resampleAssume aref = | pvi & PVI p ->
    match aref with PAssumeRef x in
    modref x.changeId p.id;
    pvi

  sem setAssumeDrift drift aref = | pvi ->
    match aref with PAssumeRef x in
    modref x.drift drift;
    pvi

  sem readPreviousAssume aref = | _ ->
    match aref with PAssumeRef x in
    x.read ()

  sem readPreviousExport eref = | _ ->
    match eref with PExportRef x in
    x.read ()

  sem readPreviousSubmodel mref = | _ ->
    match mref with PSubmodelRef x in
    x.readSt ()

  sem p_cache st eq = | PVal a ->
    match st with PVS st in
    let value = ref (deref a.value) in
    let changeId = ref st.initId in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        let prevValue = deref value in
        if eq prevValue (deref a.value) then () else
        modref value (deref a.value);
        modref changeId st.id;
        modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
      else () in
    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_export st store = | PVal a ->
    match st with PVS st in
    PVS {st with st = store st.st (PExportRef {read = lam. deref a.value})}

  sem p_pure st = | a ->
    match st with PVS {initId = initId} in
    (st, PVal {value = ref a, changeId = ref initId})

  sem p_map st f = | PVal a ->
    match st with PVS st in
    let value = ref (f (deref a.value)) in
    let changeId = ref st.initId in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        let prevValue = deref value in
        modref value (f (deref a.value));
        modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
        modref changeId st.id
      else () in
    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_apply st f = | PVal a ->
    match st with PVS st in
    match f with PVal f in
    let value = ref ((deref f.value) (deref a.value)) in
    let changeId = ref st.initId in
    let update = lam st.
      if or (eqi st.id (deref f.changeId)) (eqi st.id (deref a.changeId)) then
        let prevValue = deref value in
        modref value ((deref f.value) (deref a.value));
        modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
        modref changeId st.id
      else () in
    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_bind st store initSt2 f a = | refs ->
    match st with PVS st in
    match a with PVal a in
    let f : PValState st2 y -> PValState (PValRec b, st2) y = lam st.
      match f st (deref a.value) refs with (PVS st, PVal pval) in
      PVS {st = (pval, st.st), updates = st.updates, initWeight = st.initWeight, initId = st.initId} in
    match _initModel st.initId initSt2 f with ((pval, st2), initWeight, model) in
    let initWeight = addf st.initWeight initWeight in

    let value = ref (deref pval.value) in
    let changeId = ref (deref pval.changeId) in
    let model = ref model in
    let st2 = ref st2 in
    let pval = ref pval in
    let localWeight = ref initWeight in

    let update = lam st.
      if eqi st.id (deref a.changeId) then
        -- We've updated the argument, create a new sub-model
        let prevValue = deref value in
        let prevModel = deref model in
        let prevSt2 = deref st2 in
        let prevWeight = deref localWeight in
        let prevPVal = deref pval in

        match _initModel st.id initSt2 f with ((newPVal, newSt2), newWeight, newModel) in
        let newValue = deref newPVal.value in
        modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));

        modref value newValue;
        modref changeId st.id;
        modref model newModel;
        modref st2 newSt2;
        modref pval newPVal;
        modref localWeight newWeight;

        let reset = lam.
          modref value prevValue;
          modref model prevModel;
          modref st2 prevSt2;
          modref pval prevPVal;
          modref localWeight prevWeight in
        modref st.reset (snoc (deref st.reset) reset)
      else
        -- We've not updated the argument, update the sub-model
        let prevWeight = deref localWeight in
        let innerSt = {st with permanentWeight = ref prevWeight} in
        (deref model) innerSt;
        let newWeight = deref innerSt.permanentWeight in
        modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));
        modref localWeight newWeight;
        modref st.reset (snoc (deref st.reset) (lam. modref localWeight prevWeight));
        if eqi st.id (deref (deref pval).changeId) then
          -- The sub-model has updated output, propagate
          let prevValue = deref value in
          let newValue = deref (deref pval).value in
          modref value newValue;
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
        else
          ()
    in
    (PVS {st with initWeight = initWeight, updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_weight st store f = | PVal a ->
    match st with PVS st in
    let w = f (deref a.value) in
    let initWeight = addf st.initWeight w in
    let w = ref w in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        let prevWeight = deref w in
        let newWeight = f (deref a.value) in
        modref st.permanentWeight (addf (deref st.permanentWeight) (subf newWeight prevWeight));
        modref w newWeight;
        modref st.reset (snoc (deref st.reset) (lam. modref w prevWeight))
      else () in
    PVS {st with updates = snoc st.updates update, initWeight = initWeight, st = store st.st (PWeightRef ())}

  sem p_assume st store = | PVal dist ->
    match st with PVS st in
    let value = ref (p_sample (deref dist.value)) in
    let changeId = ref st.initId in
    let w = ref (p_logObserve (deref dist.value) (deref value)) in
    let drift = ref (lam. (deref dist.value)) in
    let update = lam st.
      if eqi st.id (deref changeId) then
        -- Draw a new sample, i.e., value changes
        let prevValue = deref value in
        let prevWeight = deref w in

        let kernel = (deref drift) prevValue in
        let proposal = p_sample kernel in
        let reverseKernel = (deref drift) proposal in

        let newWeight = p_logObserve (deref dist.value) proposal in

        let prevToProposalProb = p_logObserve kernel proposal in
        let proposalToPrevProb = p_logObserve reverseKernel prevValue in

        modref value proposal;
        modref w newWeight;
        modref changeId st.id;
        let reset = lam.
          modref value prevValue;
          modref w prevWeight in
        modref st.reset (snoc (deref st.reset) reset);
        modref st.temporaryWeight
          (addf
            (addf
              (deref st.temporaryWeight)
              (subf newWeight prevWeight))
            (subf proposalToPrevProb prevToProposalProb))
      else if eqi st.id (deref dist.changeId) then
        -- Reuse current sample, i.e., value doesn't change
        let prevWeight = deref w in
        let newWeight = p_logObserve (deref dist.value) (deref value) in
        modref w newWeight;
        modref st.reset (snoc (deref st.reset) (lam. modref w prevWeight));
        modref st.temporaryWeight (addf (deref st.temporaryWeight) (subf newWeight prevWeight))
      else () in
    ( PVS
      { st with updates = snoc st.updates update
      , st = store st.st (PAssumeRef {drift = drift, changeId = changeId, read = lam. deref value})
      }
    , PVal {value = value, changeId = changeId}
    )
end


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

type SomePAssumeRef
con SomePAssumeRef : all x. use PValInterface in PAssumeRef x -> SomePAssumeRef

lang SimpleResample = PValInterface
  type SimpleState x = ([SomePAssumeRef], Option (PExportRef x))

  sem simpleStore : all a. all x. SimpleState x -> PAssumeRef a -> SimpleState x
  sem simpleStore rs = | r -> (snoc rs.0 (SomePAssumeRef r), rs.1)

  sem simpleExport : all x. SimpleState x -> PExportRef x -> SimpleState x
  sem simpleExport rs = | r -> (rs.0, Some r)

  sem simpleResample : all x. Float -> PValInstance Partial (SimpleState x) -> PValInstance Partial (SimpleState x)
  sem simpleResample globalProb = | instance ->
    let st = getSt instance in
    let doResample = lam instance. lam someAssume.
      match someAssume with SomePAssumeRef x in
      resampleAssume x instance in
    if bernoulliSample globalProb then
      foldl doResample instance st.0
    else
      doResample instance (_chooseUniform st.0)

  sem simpleRead : all x. all complete. PValInstance complete (SimpleState x) -> x
  sem simpleRead = | instance ->
    readPreviousExport (optionGetOrElse (lam. error "Impossible") (getSt instance).1) instance
end


-- === Bern and ==

let baseline = lam.
  let a = assume (Bernoulli 0.5) in
  let b = assume (Bernoulli 0.5) in
  and a b

lang BernAnd = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    match p_assume st simpleStore dist with (st, a) in
    match p_assume st simpleStore dist with (st, b) in
    match p_map st and a with (st, tmp) in
    match p_apply st tmp b with (st, res) in
    p_export st simpleExport res
end

lang RunBernAndMut = BernAnd + MCMCPVal + MutPVal
end

let result =
  printLn "\n=== bern_and ===";
  let globalProb = 0.1 in
  let iterations = 100000 in
  let toString = bool2string in
  let mkHisto = histogram cmpBool in
  let summarizePVal = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto res)) in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) in
  let run =
    use RunBernAndMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()


-- === Simple Bind ===

let baseline = lam.
  if assume (Bernoulli 0.5)
  then assume (Bernoulli 0.9)
  else assume (Bernoulli 0.5)

lang SimpleBind = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    match p_assume st simpleStore dist with (st, c) in
    let f : all z. PValState () z -> Unknown -> PValHList z HNil -> Unknown = lam st. lam c. lam.
      if c then
        match p_pure st (p_bernoulli 0.9) with (st, dist) in
        p_assume_ st dist
      else
        match p_pure st (p_bernoulli 0.5) with (st, dist) in
        p_assume_ st dist in
    match p_bind_ st #frozen"f" c (PVHNil ()) with (st, res) in
    p_export st simpleExport res
end

lang RunSimpleBindMut = SimpleBind + MCMCPVal + MutPVal
end

let result =
  printLn "\n=== simple_bind ===";
  let globalProb = 0.1 in
  let iterations = 100000 in
  let toString = bool2string in
  let mkHisto = histogram cmpBool in
  let summarizePVal = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto res)) in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) in
  let run =
    use RunSimpleBindMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()


-- === Manual Geometric ===

let baseline = lam.
  recursive let work = lam acc.
    if assume (Bernoulli 0.5)
    then work (addi acc 1)
    else acc in
  let c = assume (Bernoulli 0.5) in
  if c
  then work 1
  else 0

lang ManualGeometric = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    match p_assume st simpleStore dist with (st, c) in
    recursive let f : all z. Int -> PValState () z -> Unknown -> PValHList z Unknown -> (PValState () z, PVal z Unknown) = lam i. lam st. lam c. lam dist.
      match dist with PVHCons (dist, PVHNil ()) in
      let recur = lam x. lam y. lam z. f (addi i 1) x y z in
      if c then
        match p_assume_ st dist with (st, c) in
        p_bind_ st #frozen"recur" c (PVHCons (dist, PVHNil ()))
      else
        p_pure st i in
    let start = lam x. f 0 x in
    match p_bind_ st #frozen"start" c (PVHCons (dist, PVHNil ())) with (st, res) in
    p_export st simpleExport res
end

lang RunManualGeometricMut = ManualGeometric + MCMCPVal + MutPVal
end

let result =
  printLn "\n=== manual_geometric ===";
  let globalProb = 0.1 in
  let iterations = 100000 in
  let toString = int2string in
  let mkHisto = histogram subi in
  let summarizePVal = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto res)) in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) in
  let run =
    use RunManualGeometricMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()

-- === Coin ===

let observations = [true, true, true, false, true, true, false, true]

let baseline = lam.
  let p = assume (Beta 1.0 1.0) in
  for_ observations (lam o. observe o (Bernoulli p));
  p

lang CoinOneObserve = SimpleResample
  sem run = | st ->
    match p_pure st (p_beta 1.0 1.0) with (st, dist) in
    match p_assume st simpleStore dist with (st, p) in
    let st = p_weight_ st (lam p. foldl addf 0.0 (map (lam o. p_logObserve (p_bernoulli p) o) observations)) p in
    p_export st simpleExport p
end

lang CoinManyObserve = SimpleResample
  sem run = | st ->
    match p_pure st (p_beta 1.0 1.0) with (st, dist) in
    match p_assume st simpleStore dist with (st, p) in
    let f = lam st. lam o.
      p_weight_ st (lam p. p_logObserve (p_bernoulli p) o) p in
    let st = foldl f st observations in
    p_export st simpleExport p
end

lang RunCoinOneObserveMut = CoinOneObserve + MCMCPVal + MutPVal
end

lang RunCoinManyObserveMut = CoinManyObserve + MCMCPVal + MutPVal
end

let result =
  printLn "\n=== coin ===";
  let globalProb = 0.1 in
  let iterations = 100000 in
  let toString = interval2string in
  let mkHisto = bucket 10 0.0 1.0 in
  let summarizePVal = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto res)) in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) in
  let run =
    use RunCoinOneObserveMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut one observe" (timeF run);
  let run =
    use RunCoinManyObserveMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut many observe" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()


-- === Tree Inference ===

type Tree
con Leaf : {id : Int, x : Float} -> Tree
con Node : {left : Tree, right : Tree, x : Float} -> Tree

recursive let asShape = lam t. switch t
  case Leaf x then int2string x.id
  case Node x then join ["(", asShape x.left, ", ", asShape x.right, ")"]
  end
end

let initTrees =
  [ Leaf {id = 0, x = 0.0}
  , Leaf {id = 1, x = 5.0}
  , Leaf {id = 2, x = 10.0}
  , Leaf {id = 3, x = 15.0}
  -- , Leaf {id = 4, x = 20.0}
  ]

recursive let minId = lam t. switch t
  case Leaf x then x.id
  case Node x then mini (minId x.left) (minId x.right)
  end
end
let getX = lam t. switch t
  case Leaf x then x.x
  case Node x then x.x
  end
let mkNode = lam x. lam l. lam r.
  if lti (minId l) (minId r)
  then Node {left = l, right = r, x = x}
  else Node {left = r, right = l, x = x}

let baseline = lam.
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
  cluster (length initTrees) initTrees

lang TreeInferenceTreeBind = SimpleResample
  sem run = | st ->
    let pickpair = lam st. lam n.
      match p_pure st (p_uniformDiscrete 0 (subi n 1)) with (st, dist) in
      match p_assume st simpleStore dist with (st, i) in
      match p_pure st (p_uniformDiscrete 0 (subi n 2)) with (st, dist) in
      match p_assume st simpleStore dist with (st, j) in
      let f = lam i. lam j. if lti j i then (j,i) else (i, addi j 1) in
      match p_map st f i with (st, tmp) in
      p_apply st tmp j in

    match p_pure st (p_gaussian 0.0 10.0) with (st, dist) in
    match p_assume st simpleStore dist with (st, rootValue) in
    let deviateFromDist = lam x. p_gaussian x 10.0 in
    match p_map st deviateFromDist rootValue with (st, rootDist) in

    recursive let cluster = lam st. lam trees.
      match trees with [tree] then (st, tree) else
      match pickpair st (length trees) with (st, pair) in
      match p_assume st simpleStore rootDist with (st, here) in
      let mkCarryOn = lam idx. lam st.
        match splitAt trees idx with (_, [idx0, idx1, idx2] ++ _) in
        let f : all z. PValState () z -> Unknown -> PValHList z Unknown -> (Unknown, PVal z Tree)
          = lam st. lam pair. lam trip.
            match trip with PVHCons (idx0, PVHCons (idx1, PVHCons (idx2, PVHNil ()))) in
            match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
            with (l, r) in
            if leqi l idx then
              if leqi r (addi idx 1)
              then (st, idx2)
              else (st, idx1)
            else (st, idx0)
        in
        p_bind_ st #frozen"f" pair (PVHCons (idx0, PVHCons (idx1, PVHCons (idx2, PVHNil ())))) in
      match mapAccumL (lam st. lam f. f st) st (create (subi (length trees) 2) mkCarryOn) with (st, carryOns) in
      let f : all z. PValState () z -> Unknown -> PValHList z Unknown -> (Unknown, PVal z (Tree, Tree))
        = lam st. lam pair. lam trees.
          match trees with PVHConsL (trees, PVHNil ()) in
          match p_map st (lam l. lam r. (l, r)) (get trees pair.0) with (st, tmp) in
          p_apply st tmp (get trees pair.1) in
      match p_bind_ st #frozen"f" pair (PVHConsL (trees, PVHNil ())) with (st, treePair) in
      let f = lam rootDist. lam here. lam treePair.
        let l = treePair.0 in
        let r = treePair.1 in
        let calcWeight = lam t. addf
          (negf (p_logObserve rootDist (getX t)))
          (p_logObserve (deviateFromDist here) (getX t)) in
        addf (calcWeight l) (calcWeight r) in
      match p_map st f rootDist with (st, tmp) in
      match p_apply st tmp here with (st, tmp) in
      match p_apply st tmp treePair with (st, tmp) in
      let st = p_weight_ st (lam w. w) tmp in
      let f = lam here. lam treePair.
        mkNode here treePair.0 treePair.1 in
      match p_map st f here with (st, tmp) in
      match p_apply st tmp treePair with (st, merged) in
      cluster st (snoc carryOns merged)
    in

    let f = lam rootDist.
      foldl addf 0.0 (map (lam t. p_logObserve rootDist (getX t)) initTrees) in
    let st = p_weight_ st f rootDist in
    match mapAccumL p_pure st initTrees with (st, initTrees) in
    match cluster st initTrees with (st, tree) in
    p_export st simpleExport tree
end

lang RunTreeInferenceTreeBindMut = TreeInferenceTreeBind + MCMCPVal + MutPVal
end

lang TreeInferenceTreeSelect = SimpleResample
  sem run = | st ->
    let pickpair = lam st. lam n.
      match p_pure st (p_uniformDiscrete 0 (subi n 1)) with (st, dist) in
      match p_assume st simpleStore dist with (st, i) in
      match p_pure st (p_uniformDiscrete 0 (subi n 2)) with (st, dist) in
      match p_assume st simpleStore dist with (st, j) in
      let f = lam i. lam j. if lti j i then (j,i) else (i, addi j 1) in
      match p_map st f i with (st, tmp) in
      p_apply st tmp j in

    match p_pure st (p_gaussian 0.0 10.0) with (st, dist) in
    match p_assume st simpleStore dist with (st, rootValue) in
    let deviateFromDist = lam x. p_gaussian x 10.0 in
    match p_map st deviateFromDist rootValue with (st, rootDist) in

    recursive let cluster = lam st. lam trees.
      match trees with [tree] then (st, tree) else
      match pickpair st (length trees) with (st, pair) in
      match p_assume st simpleStore rootDist with (st, here) in
      let mkCarryOn = lam idx. lam st.
        match splitAt trees idx with (_, [idx0, idx1, idx2] ++ _) in
        let f : all z. Unknown -> PValHList z Unknown -> PVal z Tree
          = lam pair. lam trip.
            match trip with PVHCons (idx0, PVHCons (idx1, PVHCons (idx2, PVHNil ()))) in
            match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
            with (l, r) in
            if leqi l idx then
              if leqi r (addi idx 1)
              then idx2
              else idx1
            else idx0
        in
        p_select st #frozen"f" pair (PVHCons (idx0, PVHCons (idx1, PVHCons (idx2, PVHNil ())))) in
      match mapAccumL (lam st. lam f. f st) st (create (subi (length trees) 2) mkCarryOn) with (st, carryOns) in
      let f : all z. Unknown -> PValHList z Unknown -> PVal z Tree
        = lam pair. lam trees.
          match trees with PVHConsL (trees, PVHNil ()) in
          get trees pair.0 in
      match p_select st #frozen"f" pair (PVHConsL (trees, PVHNil ())) with (st, l) in
      let f : all z. Unknown -> PValHList z Unknown -> PVal z Tree
        = lam pair. lam trees.
          match trees with PVHConsL (trees, PVHNil ()) in
          get trees pair.1 in
      match p_select st #frozen"f" pair (PVHConsL (trees, PVHNil ())) with (st, r) in
      let f = lam rootDist. lam here. lam t. addf
        (negf (p_logObserve rootDist (getX t)))
        (p_logObserve (deviateFromDist here) (getX t)) in
      match p_map st f rootDist with (st, tmp) in
      match p_apply st tmp here with (st, tmp) in
      match p_apply st tmp l with (st, lWeight) in
      let st = p_weight_ st (lam x. x) lWeight in
      match p_apply st tmp r with (st, rWeight) in
      let st = p_weight_ st (lam x. x) rWeight in
      let f = lam here. lam l. lam r.
        mkNode here l r in
      match p_map st f here with (st, tmp) in
      match p_apply st tmp l with (st, tmp) in
      match p_apply st tmp r with (st, merged) in
      cluster st (snoc carryOns merged)
    in

    let f = lam rootDist.
      foldl addf 0.0 (map (lam t. p_logObserve rootDist (getX t)) initTrees) in
    let st = p_weight_ st f rootDist in
    match mapAccumL p_pure st initTrees with (st, initTrees) in
    match cluster st initTrees with (st, tree) in
    p_export st simpleExport tree
end

lang RunTreeInferenceTreeSelectMut = TreeInferenceTreeSelect + MCMCPVal + MutPVal
end

let result =
  printLn "\n=== tree_inference ===";
  let globalProb = 0.1 in
  let iterations = 100000 in
  let toString = lam x. x in
  let mkHisto = lam x. histogram cmpString (map asShape x) in
  let summarizePVal = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto res)) in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) in
  let run =
    use RunTreeInferenceTreeBindMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut tree bind" (timeF run);
  let run =
    use RunTreeInferenceTreeSelectMut in
    let instance = instantiate #frozen"run" ([], None ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut tree select" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 0))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()

-- let tree_inference =
--   type Tree in
--   con Leaf : {id : Int, x : Float} -> Tree in
--   con Node : {left : Tree, right : Tree, x : Float} -> Tree in

--   recursive let asShape = lam t. switch t
--     case Leaf x then int2string x.id
--     case Node x then join ["(", asShape x.left, ", ", asShape x.right, ")"]
--     end in

--   let initTrees =
--     [ Leaf {id = 0, x = 0.0}
--     , Leaf {id = 1, x = 5.0}
--     , Leaf {id = 2, x = 10.0}
--     , Leaf {id = 3, x = 15.0}
--     -- , Leaf {id = 4, x = 20.0}
--     ] in

--   recursive let minId = lam t. switch t
--     case Leaf x then x.id
--     case Node x then mini (minId x.left) (minId x.right)
--     end in
--   let getX = lam t. switch t
--     case Leaf x then x.x
--     case Node x then x.x
--     end in
--   let mkNode = lam x. lam l. lam r.
--     if lti (minId l) (minId r)
--     then Node {left = l, right = r, x = x}
--     else Node {left = r, right = l, x = x} in

--   let p_runTree =
--     let f = lam pvi.
--       match pvi with PVI pvi in
--       let pickpair = lam resamples. lam n.
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
--         let f = lam i. lam j.
--           if lti j i then (j,i) else (i, addi j 1) in
--         ( concat resamples [resA, resB]
--         , pvi.ap (pvi.map f a) b
--         ) in

--       match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
--       let deviateFromDist = lam x. p_gaussian x 10.0 in
--       let rootDist = pvi.map deviateFromDist rootValue in

--       recursive let cluster = lam resamples. lam trees.
--         match trees with [tree] then (resamples, tree) else
--         match pickpair resamples (length trees) with (resamples, pair) in
--         match pvi.assume rootDist with (_, resHere, here) in
--         let resamples = snoc resamples resHere in
--         let carryOn = lam idx. lam pvi. lam pair.
--           match pvi with PVI pvi in
--           match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
--           with (l, r) in
--           pvi.identity (get trees (addi idx (if leqi l idx then if leqi r (addi idx 1) then 2 else 1 else 0))) in
--         let carryOns = create (subi (length trees) 2) (lam i. let f = lam x. carryOn i x in pvi.bind #frozen"f" pair) in
--         let treePair =
--           let f = lam pvi. lam pair.
--             match pvi with PVI pvi in
--             pvi.ap (pvi.map (lam l. lam r. (l, r)) (get trees pair.0)) (get trees pair.1) in
--           pvi.bind #frozen"f" pair in
--         let f = lam rootDist. lam here. lam treePair.
--           let l = treePair.0 in
--           let r = treePair.1 in
--           let calcWeight = lam t. addf
--             (negf (p_logObserve rootDist (getX t)))
--             (p_logObserve (deviateFromDist here) (getX t)) in
--           (addf (calcWeight l) (calcWeight r), mkNode here l r) in
--         cluster resamples (snoc carryOns (pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.map f rootDist) here) treePair))) in

--       let addInitialObservation =
--         let obsInit = lam rootDist.
--           ( foldl addf 0.0
--             (map (lam t. p_logObserve rootDist (getX t)) initTrees)
--           , ()
--           ) in
--         pvi.ap (pvi.map (lam. lam x. x) (pvi.cache (lam. lam. true) (pvi.mapWeight obsInit rootDist))) in
--       match cluster [resRoot] (map pvi.pure initTrees)
--       with (resamples, tree) in
--       ( lam id. _chooseUniform
--         (snoc (map modref resamples)
--           (lam id. for_ resamples (lam r. modref r id)))
--         id
--       , addInitialObservation tree
--       ) in
--     buildPValModel 0 #frozen"f" in

--   let p_runChunky =
--     let f = lam pvi.
--       match pvi with PVI pvi in
--       let pickpair = lam resamples. lam n.
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
--         let f = lam i. lam j.
--           if lti j i then (j,i) else (i, addi j 1) in
--         ( concat resamples [resA, resB]
--         , pvi.ap (pvi.map f a) b
--         ) in

--       match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
--       let deviateFromDist = lam x. p_gaussian x 10.0 in
--       let rootDist = pvi.map deviateFromDist rootValue in

--       recursive let cluster = lam resamples. lam nTrees. lam trees.
--         if eqi nTrees 1 then (resamples, pvi.map head trees) else
--         match pickpair resamples nTrees with (resamples, pair) in
--         match pvi.assume rootDist with (_, resHere, here) in
--         let resamples = snoc resamples resHere in
--         let f = lam rootDist. lam here. lam pair. lam trees.
--           let l = get trees pair.0 in
--           let r = get trees pair.1 in
--           let trees = mapOption (lam x. x)
--             (mapi (lam idx. lam v. if or (eqi idx pair.0) (eqi idx pair.1) then None () else Some v) trees) in
--           let calcWeight = lam t. addf
--             (negf (p_logObserve rootDist (getX t)))
--             (p_logObserve (deviateFromDist here) (getX t)) in
--           (addf (calcWeight l) (calcWeight r), snoc trees (mkNode here l r))
--         in
--         cluster resamples (subi nTrees 1) (pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map f rootDist) here) pair) trees)) in

--       let f = lam rootDist.
--         ( foldl addf 0.0
--           (map (lam t. p_logObserve rootDist (getX t)) initTrees)
--         , initTrees
--         ) in
--       match cluster [resRoot] (length initTrees) (pvi.cache (lam. lam. true) (pvi.mapWeight f rootDist))
--       with (resamples, tree) in
--       ( lam id. _chooseUniform
--         (snoc (map modref resamples)
--           (lam id. for_ resamples (lam r. modref r id)))
--         id
--       , tree
--       ) in
--     buildPValModel 0 #frozen"f" in

--   let p_runDag =
--     let f = lam pvi.
--       match pvi with PVI pvi in
--       let pickpair = lam resamples. lam n.
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 1))) with (_, resA, a) in
--         match pvi.assume (pvi.pure (p_uniformDiscrete 0 (subi n 2))) with (_, resB, b) in
--         let f = lam i. lam j.
--           if lti j i then (j,i) else (i, addi j 1) in
--         ( concat resamples [resA, resB]
--         , pvi.ap (pvi.map f a) b
--         ) in

--       match pvi.assume (pvi.pure (p_gaussian 0.0 10.0)) with (_, resRoot, rootValue) in
--       let deviateFromDist = lam x. p_gaussian x 10.0 in
--       let rootDist = pvi.map deviateFromDist rootValue in

--       recursive let cluster = lam resamples. lam nTrees. lam trees.
--         if eqi nTrees 1 then (resamples, pvi.map head trees) else
--         match pickpair resamples nTrees with (resamples, pair) in
--         let i = pvi.map (lam p. p.0) pair in
--         let j = pvi.map (lam p. p.1) pair in
--         match pvi.assume rootDist with (_, resHere, here) in
--         let resamples = snoc resamples resHere in
--         let fetchTree = lam idx. lam rootDist. lam here. lam trees.
--           let t = get trees idx in
--           ( addf
--             (negf (p_logObserve rootDist (getX t)))
--             (p_logObserve (deviateFromDist here) (getX t))
--           , t
--           ) in
--         let l = pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map fetchTree i) rootDist) here) trees) in
--         let r = pvi.mapWeight (lam x. x) (pvi.ap (pvi.ap (pvi.ap (pvi.map fetchTree j) rootDist) here) trees) in
--         let trees =
--           let f = lam p. lam trees. mapOption (lam x. x) (mapi (lam idx. lam v. if or (eqi idx p.0) (eqi idx p.1) then None () else Some v) trees) in
--           pvi.ap (pvi.map f pair) trees in
--         let addMerged = lam l. lam r. lam here. lam trees.
--           snoc trees (mkNode here l r) in
--         let trees = pvi.ap (pvi.ap (pvi.ap (pvi.map addMerged l) r) here) trees in
--         cluster resamples (subi nTrees 1) trees in

--       let f = lam rootDist.
--         ( foldl addf 0.0
--           (map (lam t. p_logObserve rootDist (getX t)) initTrees)
--         , initTrees
--         ) in
--       match cluster [resRoot] (length initTrees) (pvi.cache (lam. lam. true) (pvi.mapWeight f rootDist))
--       with (resamples, tree) in
--       ( lam id. _chooseUniform
--         (snoc (map modref resamples)
--           (lam id. for_ resamples (lam r. modref r id)))
--         id
--       , tree
--       ) in
--     buildPValModel 0 #frozen"f" in

--   let run = lam.
--     let pickpair = lam n.
--       let i = assume (UniformDiscrete 0 (subi n 1)) in
--       let j = assume (UniformDiscrete 0 (subi n 2)) in
--       if lti j i then (i,j) else (i,addi j 1) in

--     let rootValue = assume (Gaussian 0.0 10.0) in
--     let deviateFromDist = lam x. Gaussian x 10.0 in
--     let rootDist = deviateFromDist rootValue in
--     let cancelRootDist = lam x.
--       weight (negf (gaussianLogPdf rootValue 10.0 x)) in

--     recursive let cluster = lam nTrees. lam trees.
--       if eqi nTrees 1 then head trees else
--       match pickpair nTrees with (i, j) in
--       let l = get trees i in
--       let r = get trees j in
--       let trees = mapOption (lam x. x) (mapi (lam idx. lam v. if or (eqi idx i) (eqi idx j) then None () else Some v) trees) in
--       let here = assume rootDist in
--       cancelRootDist (getX l);
--       cancelRootDist (getX r);
--       observe (getX l) (deviateFromDist here);
--       observe (getX r) (deviateFromDist here);
--       cluster (subi nTrees 1) (snoc trees (mkNode here l r)) in

--     for_ initTrees (lam t. observe (getX t) rootDist);
--     cluster (length initTrees) initTrees in

--   printLn "\n tree_inference";
--   match timeF (lam. p_runInference p_runTree.0 p_runTree.2 p_runTree.1 1000000) with (p_time, p_res) in
--   printLn (join [float2string p_time, "ms (PVal Tree)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
--   -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runTree.1).0 .1);
--   match timeF (lam. p_runInference p_runChunky.0 p_runChunky.2 p_runChunky.1 1000000) with (p_time, p_res) in
--   printLn (join [float2string p_time, "ms (PVal Chunky)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
--   -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runChunky.1).0 .1);
--   match timeF (lam. p_runInference p_runDag.0 p_runDag.2 p_runDag.1 1000000) with (p_time, p_res) in
--   printLn (join [float2string p_time, "ms (PVal DAG)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
--   -- printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_runDag.1).0 .1);
--   match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (time, res) in
--   printLn (join [float2string time, "ms (MCMC lightweight)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res).0)));
--   match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeA, resA) in
--   printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resA).0)));
--   match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeP, resP) in
--   printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resP).0)));
--   match timeF (lam. infer (Importance {particles = 1000000}) run) with (time2, res2) in
--   printLn (join [float2string time2, "ms (IS lightweight)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res2).0)));
--   ()
-- in

-- ()


mexpr
-- TODO(vipa, 2025-09-25): For whatever reason we end up with a Decl
-- without an info field if we have `infer` above but not here. I have
-- no idea why.
let x = infer (Default ()) (lam. ()) in
printLn "\n\nDone";
()
