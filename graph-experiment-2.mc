include "ext/arr-ext.mc"
include "ext/dist-ext.mc"
include "parray.mc"

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

type Complete
type Partial

lang PValInterface
  syn PVal a =
  syn PValState st =

  syn PWeightRef =
  syn PAssumeRef a =
  syn PExportRef a =
  syn PSubmodelRef st =


  syn PValInstance complete st =


  -- === Working with an instance of a model ===

  -- Create a new instance of a model (completely evaluated).
  sem instantiate : all st. all st2. (PValState st -> PValState st2) -> st -> PValInstance Complete st2
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
  -- Mark a given assume to be resampled in the upcoming step with the
  -- given drift kernel. The first argument is the distribution we
  -- *should* have drawn from, to make it easy to use no drift kernel.
  sem resampleAssume : all st. all a. (PDist a -> a -> PDist a) -> PAssumeRef a -> PValInstance Partial st -> PValInstance Partial st
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
  sem p_cache : all st. all a. PValState st
    -> (a -> a -> Bool)
    -> PVal a
    -> (PValState st, PVal a)
  -- Make a probabilistic value available to read outside the model,
  -- via a model instance.
  sem p_export : all st. all st2. all a. PValState st
    -> (st -> PExportRef a -> st2)
    -> PVal a
    -> PValState st2

  -- Lift a deterministic value to a probabilistic one. (from the
  -- Pointed type class)
  sem p_pure : all st. all a. PValState st
    -> a
    -> (PValState st, PVal a)
  -- Make a deterministic "update" to a probabilistic value. (from the
  -- Functor type class)
  sem p_map : all st. all a. all b. PValState st
    -> (a -> b)
    -> PVal a
    -> (PValState st, PVal b)
  -- Combine two probabilistic values. Often used in conjunction with
  -- `p_map`. (from the Applicative type class)
  sem p_apply : all st. all a. all b. PValState st
  -> PVal (a -> b)
    -> PVal a
    -> (PValState st, PVal b)
  -- Create a sub-model whose shape depends on a probabilistic
  -- value. (mostly equivalent with bind from Monad)
  sem p_bind : all st. all ist. all ist2. all st2. all a. all b. PValState st
    -> (st -> PSubmodelRef ist2 -> st2)
    -> ist
    -> (PValState ist -> a -> (PValState ist2, PVal b))
    -> PVal a
    -> (PValState st2, PVal b)
  -- Select which probabilistic value to propagate based on the value
  -- of another probabilistic value. Weaker version of bind that
  -- sometimes admits more efficient implementation. (mostly
  -- equivalent with select from Selective)
  sem p_select : all st. all a. all b. PValState st
    -> (a -> PVal b)
    -> PVal a
    -> (PValState st, PVal b)

  -- Introduce a weight.
  sem p_weight : all st. all st2. all a. PValState st
    -> (st -> PWeightRef -> st2)
    -> (a -> Float)
    -> PVal a
    -> PValState st2
  -- Draw a value from a distribution.
  sem p_assume : all st. all st2. all a. PValState st
    -> (st -> PAssumeRef a -> st2)
    -> PVal (PDist a)
    -> (PValState st2, PVal a)

  -- Versions that don't record their reference in the state
  sem p_bind_ : all st. all a. all as. all b. PValState st
    -> (PValState () -> a -> (PValState (), PVal b))
    -> PVal a
    -> (PValState st, PVal b)
  sem p_bind_ st f = | a -> p_bind st (lam st. lam. st) () f a

  sem p_weight_ : all st. all a. PValState st
    -> (a -> Float)
    -> PVal a
    -> PValState st
  sem p_weight_ st f = | a -> p_weight st (lam st. lam. st) f a

  sem p_assume_ : all st. all a. PValState st
    -> PVal (PDist a)
    -> (PValState st, PVal a)
  sem p_assume_ st = | dist -> p_assume st (lam st. lam. st) dist
end


-- === Mutable PVal model instances (should be used affinely) ===

-- This implementation is essentially a list of update functions,
-- closing over a bunch of references. Each node has two primary
-- references: the current value, and when it was last changed. Each
-- update function looks at the changed time of its inputs, then maybe
-- updates its value and changed time. More complicated nodes have
-- additional references storing other kinds of state, e.g., some
-- weight or a reference to a sub-model.

lang MutPVal = PValInterface
  type IterationID = Int

  type PValRec a = {value : Ref a, changeId : Ref IterationID}
  syn PVal a = | PVal (PValRec a)

  syn PValState st = | PVS {initId : IterationID, updates : [PState -> ()], initWeight : Float, st : st}
  syn PWeightRef = | PWeightRef {} -- TODO(vipa, 2025-09-23): figure out what we want to be able to do here, and thus what we need to store
  syn PAssumeRef a = | PAssumeRef {drift : Ref (PDist a -> a -> PDist a), changeId : Ref IterationID, read : () -> a}
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

  sem initModel : all st. all st2. all a. st -> (PValState st -> PValState st2) -> (st2, Float, UpdateFunction)
  sem initModel initSt = | f -> _initModel 0 initSt f
  sem _initModel : all st. all st2. all a. IterationID -> st -> (PValState st -> PValState st2) -> (st2, Float, UpdateFunction)
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
    (if eqf initWeight (negf inf) then
      printLn "WARNING: instantiate returned -inf weight model"
     else ());
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

  sem resampleAssume driftf aref = | pvi & PVI p ->
    match aref with PAssumeRef x in
    modref x.drift driftf;
    modref x.changeId p.id;
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
    let st =
      { st = store st.st (PExportRef {read = lam. deref a.value})
      , updates = st.updates
      , initWeight = st.initWeight
      , initId = st.initId
      } in
    PVS st

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

  sem p_bind st store initSt2 f = | PVal a ->
    match st with PVS st in
    let f : PValState ist -> PValState (PValRec b, ist2) = lam st.
      match f st (deref a.value) with (PVS st, PVal pval) in
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
    let st =
      { st = store st.st (PSubmodelRef {readSt = lam. deref st2})
      , initWeight = initWeight
      , updates = snoc st.updates update
      , initId = st.initId
      } in
    (PVS st, PVal {value = value, changeId = changeId})

  sem p_select st f = | PVal a ->
    match st with PVS st in
    let pval = f (deref a.value) in
    let value = ref (deref (match pval with PVal x in x.value)) in
    let pval = ref pval in
    let changeId = ref (deref a.changeId) in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        let prevPVal = deref pval in
        let prevValue = deref value in
        let newPVal = f (deref a.value) in
        modref pval newPVal;
        modref value (deref (match newPVal with PVal x in x.value));
        modref changeId st.id;
        let reset = lam.
          modref pval prevPVal;
          modref value prevValue in
        modref st.reset (snoc (deref st.reset) reset)
      else
        match deref pval with PVal ret in
        if eqi st.id (deref ret.changeId) then
          let prevValue = deref value in
          modref value (deref ret.value);
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
        else () in
    (PVS {st with updates = snoc st.updates update}, PVal {changeId = changeId, value = value})

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
    let st =
      { st = store st.st (PWeightRef ())
      , updates = snoc st.updates update
      , initWeight = initWeight
      , initId = st.initId
      } in
    PVS st

  sem p_assume st store = | PVal dist ->
    match st with PVS st in
    let value = ref (p_sample (deref dist.value)) in
    let changeId = ref st.initId in
    let w = ref (p_logObserve (deref dist.value) (deref value)) in
    let drift = ref (lam d. lam. d) in
    let update = lam st.
      if eqi st.id (deref changeId) then
        -- Draw a new sample, i.e., value changes
        let prevValue = deref value in
        let prevWeight = deref w in

        let kernel = (deref drift) (deref dist.value) prevValue in
        let proposal = p_sample kernel in
        let reverseKernel = (deref drift) (deref dist.value) proposal in

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
    let st =
      { st = store st.st (PAssumeRef {drift = drift, changeId = changeId, read = lam. deref value})
      , updates = snoc st.updates update
      , initWeight = st.initWeight
      , initId = st.initId
      } in
    (PVS st , PVal {value = value, changeId = changeId})
end


-- === Simple Persistent PVal (less copying) ===

-- This implementation is the same as the one above, except each
-- reference also knows how deep it is in sub-models, meaning we can
-- reference values in super-models without copying to sub-models.

type Dyn
let asDyn : all a. a -> Dyn = unsafeCoerce
let fromDyn : all a. Dyn -> a = unsafeCoerce

lang SimplePersistentPVal2 = PValInterface
  syn RefID a = | RefID { idx : Int, level : Int }
  type IterationID = Int

  type State =
    { valuesAbove : PA (PA Dyn)
    , values : PA Dyn
    , permanentWeight : Float
    , temporaryWeight : Float
    }

  syn PValState st = | PVS
    { nextId : Int
    , specId : IterationID
    , initState : State
    , level : Int
    , update : [IterationID -> State -> State]
    , readSubState : PA Dyn -> PA Dyn
    , mapSubState : (PA Dyn -> PA Dyn) -> PA Dyn -> PA Dyn
    , st : st
    }

  syn PWeightRef = | PWeightRef {} -- TODO(vipa, 2025-09-29):
  syn PAssumeRef a = | PAssumeRef
    { readValue : PA Dyn -> a
    , writeValue : (IterationID, a) -> PA Dyn -> PA Dyn
    , writeDrift : (PDist a -> a -> PDist a) -> PA Dyn -> PA Dyn
    }
  syn PExportRef a =
  | PExportRef {readValue : PA Dyn -> a}
  | PExportVal {value : a}
  syn PSubmodelRef st =
  | PSubmodelRef {readSt : PA Dyn -> st}
  | PSubmodelVal {st : st}

  type PVarRec a = {id : RefID (IterationID, a)}
  syn PVal a =
  | PVal {value : a}
  | PVar (PVarRec a)

  syn PValInstance complete st = | PVI
    { st : st
    , state : State
    , specId : IterationID
    , update : IterationID -> State -> State
    }

  sem addUpdate : all st. (IterationID -> State -> State) -> PValState st -> PValState st
  sem addUpdate f = | PVS st -> PVS {st with update = snoc st.update f}

  sem mapSt : all st. all st2. (st -> st2) -> PValState st -> PValState st2
  sem mapSt f = | PVS st -> PVS
    { st = f st.st
    , nextId = st.nextId
    , specId = st.specId
    , update = st.update
    , level = st.level
    , initState = st.initState
    , readSubState = st.readSubState
    , mapSubState = st.mapSubState
    }

  sem newNodeState : all st. all a. PValState st -> a -> (PValState st, PVarRec a)
  sem newNodeState st = | value ->
    match st with PVS {specId = specId} in
    match newState st (specId, value) with (st, refId) in
    (st, {id = refId})

  sem newState : all st. all a. PValState st -> a -> (PValState st, RefID a)
  sem newState st = | value ->
    match st with PVS st in
    let id = st.nextId in
    let st =
      { st with nextId = addi st.nextId 1
      , initState = {st.initState with values = addPA st.initState.values (asDyn value)}
      } in
    (PVS st, RefID {idx = id, level = st.level})

  sem mkGetState : all st. all a. PValState st -> RefID a -> State -> a
  sem mkGetState st = | ref ->
    match st with PVS st in
    match ref with RefID x in
    if eqi st.level x.level
    then lam st. fromDyn (getPA st.values x.idx)
    else lam st. fromDyn (getPA (getPA st.valuesAbove x.level) x.idx)

  sem mkPutState : all st. all a. PValState st -> RefID a -> a -> State -> State
  sem mkPutState st = | ref ->
    match st with PVS st in
    match ref with RefID x in
    if eqi st.level x.level
    then lam val. lam st. {st with values = setPA st.values x.idx (asDyn val)}
    else error "Tried to call mkPutState for reference at a different level"

  sem mkUpdateState : all st. all a. PValState st -> RefID a -> (a -> a) -> State -> State
  sem mkUpdateState st = | ref ->
    match st with PVS st in
    match ref with RefID x in
    if eqi st.level x.level
    then lam f. lam st. {st with values = updatePA st.values x.idx (lam val. asDyn (f (fromDyn val)))}
    else error "Tried to call mkUpdateState for reference at a different level"

  sem mkReadRef : all st. all a. PValState st -> RefID a -> PA Dyn -> a
  sem mkReadRef st = | RefID x ->
    match st with PVS {level = level, readSubState = readSubState} in
    if eqi level x.level
    then lam vals. fromDyn (getPA (readSubState vals) x.idx)
    else error "Tried to call mkReadRef for reference at a different level"

  sem mkWriteRef : all st. all a. PValState st -> RefID a -> a -> PA Dyn -> PA Dyn
  sem mkWriteRef st = | RefID x ->
    match st with PVS {level = level, mapSubState = mapSubState} in
    if eqi level x.level
    then lam val. mapSubState (lam vals. setPA vals x.idx (asDyn val))
    else error "Tried to call mkWriteRef for reference at a different level"

  sem mkMapRef : all st. all a. PValState st -> RefID a -> (a -> a) -> PA Dyn -> PA Dyn
  sem mkMapRef st = | RefID x ->
    match st with PVS {level = level, mapSubState = mapSubState} in
    if eqi level x.level
    then lam f. mapSubState (lam vals. updatePA vals x.idx (lam val. asDyn (f (fromDyn val))))
    else error "Tried to call mkUpdateRef for reference at a different level"

  sem onInitState : all st. all a. (State -> a) -> PValState st -> a
  sem onInitState f = | PVS st -> f st.initState

  sem withInitState : all st. (State -> State) -> PValState st -> PValState st
  sem withInitState f = | PVS st -> PVS {st with initState = f st.initState}

  sem instantiate f = | st ->
    let st = PVS
      { nextId = 0
      , specId = 0
      , initState =
        { valuesAbove = emptyPA
        , values = emptyPA
        , permanentWeight = 0.0
        , temporaryWeight = 0.0
        }
      , level = 0
      , update = []
      , st = st
      , readSubState = lam st. st
      , mapSubState = lam f. lam st. f st
      } in
    match f st with PVS st in
    let updates = st.update in
    (if eqf st.initState.permanentWeight (negf inf) then
      printLn "WARNING: instantiate returned -inf weight model"
     else ());
    PVI
    { st = st.st
    , state = st.initState
    , specId = addi st.specId 1
    , update = lam specId. lam st.
      foldl (lam st. lam f. f specId st) {st with temporaryWeight = 0.0} updates
    }

  sem getSt = | PVI x -> x.st

  sem startStep = | PVI x -> PVI x

  sem finalizeStep pred = | PVI x ->
    let state = x.update x.specId x.state in
    let acceptProb = minf 0.0
      (addf
        (subf state.permanentWeight x.state.permanentWeight)
        state.temporaryWeight) in
    let new = PVI {x with state = {state with temporaryWeight = 0.0}, specId = addi x.specId 1} in
    if pred new acceptProb then (true, new) else
    (false, PVI {x with specId = addi x.specId 1})

  sem resampleAssume driftf aref = | PVI x ->
    match aref with PAssumeRef a in
    let prevValue = a.readValue x.state.values in
    let state = {x.state with values = a.writeValue (x.specId, prevValue) x.state.values} in
    let state = {state with values = a.writeDrift driftf state.values} in
    PVI {x with state = state}

  sem readPreviousAssume aref = | PVI x ->
    match aref with PAssumeRef a in
    a.readValue x.state.values

  sem readPreviousExport eref = | PVI x ->
    match eref with PExportRef e in
    e.readValue x.state.values

  sem readPreviousSubmodel mref = | PVI x ->
    error "TODO"

  sem p_cache st eq =
  | PVal x -> (st, PVal x)
  | PVar x ->
    let readValue = mkGetState st x.id in
    match newNodeState st (onInitState readValue st).1 with (st, xHere) in
    let readPrevValue = mkGetState st xHere.id in
    let writeValue = mkPutState st xHere.id in
    let update = lam specId. lam st.
      match readValue st with (changeId, value) in
      if eqi specId changeId then
        match readPrevValue st with (_, prevValue) in
        if eq prevValue value then st else
        writeValue (specId, value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_export st store = | pval ->
    match st with PVS {readSubState = readSubState} in
    let ref = switch pval
      case PVal x then PExportVal {value = x.value}
      case PVar x then
        let readValue = mkReadRef st x.id in
        let readValue = lam vals. (readValue vals).1 in
        PExportRef {readValue = readValue}
      end in
    mapSt (lam st. store st ref) st

  sem p_pure st = | value -> (st, PVal {value = value})

  sem p_map st f =
  | PVal x -> p_pure st (f x.value)
  | PVar x ->
    let readValue = mkGetState st x.id in
    match newNodeState st (f (onInitState readValue st).1) with (st, xHere) in
    let writeValue = mkPutState st xHere.id in
    let update = lam specId. lam st.
      match readValue st with (changeId, value) in
      if eqi specId changeId then
        writeValue (specId, f value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_apply st f = | a ->
    switch (f, a)
    case (PVal f, PVal a) then p_pure st (f.value a.value)
    case (PVal f, a) then p_map st f.value a
    case (f, PVal a) then p_map st (lam f. f a.value) f
    case (PVar f, PVar a) then
      let readF = mkGetState st f.id in
      let readA = mkGetState st a.id in
      match newNodeState st ((onInitState readF st).1 (onInitState readA st).1) with (st, xHere) in
      let writeValue = mkPutState st xHere.id in
      let update = lam specId. lam st.
        match readF st with (changeIdF, f) in
        match readA st with (changeIdA, a) in
        if or (eqi specId changeIdF) (eqi specId changeIdA) then
          writeValue (specId, f a) st
        else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_bind st store st2 f =
  | PVal x ->
    -- NOTE(vipa, 2025-09-30): We will never need to re-run the
    -- creation function here, thus we inline the sub-model in the
    -- parent model, meaning we don't have to deal with two stores
    let ost = match st with PVS st in st.st in
    let st = mapSt (lam. st2) st in
    match f st x.value with (st, pval) in
    let ref = PSubmodelVal {st = match st with PVS st in st.st} in
    let st = mapSt (lam ist. store ost ref) st in
    (st, pval)
  | PVar x ->
    -- NOTE(vipa, 2025-09-30): The common case, where we *do* have
    -- to re-run the creation function, requires two stores, which
    -- requires some handling of store "levels".
    let getValue = mkGetState st x.id in
    match newState st emptyPA with (st, valsRef) in
    let getVals = mkGetState st valsRef in
    let putVals = mkPutState st valsRef in
    match newState st 0.0 with (st, localWeightRef) in
    let getWeight = mkGetState st localWeightRef in
    let putWeight = mkPutState st localWeightRef in
    match st with PVS {readSubState = readSubState, mapSubState = mapSubState, initState = initState} in
    let innerState : State =
      { valuesAbove = addPA initState.valuesAbove initState.values
      , values = emptyPA
      , permanentWeight = 0.0
      , temporaryWeight = 0.0
      } in
    let ist : PValState ist = PVS
      { nextId = 0
      , specId = match st with PVS st in st.specId
      , initState = innerState
      , level = addi (match st with PVS st in st.level) 1
      , update = []
      , st = st2
      , readSubState = mkReadRef st valsRef
      , mapSubState = mkMapRef st valsRef
      } in

    match f ist (onInitState getValue st).1 with (PVS ist2, pval) in
    let st = withInitState (putVals ist2.initState.values) st in
    let st = withInitState (putWeight ist2.initState.permanentWeight) st in

    match newState st ist2.st with (st, istRef) in
    let putIst = mkPutState st istRef in

    let mkPValReadF = lam ist. lam pval.
      switch pval
      case PVal x then match ist with PVS {specId = specId} in lam. (specId, x.value)
      case PVar x then mkGetState ist x.id
      end in
    let pvalInitRead = mkPValReadF ist pval in
    match newState st pvalInitRead with (st, pvalRef) in
    let getPValRead = mkGetState st pvalRef in
    let putPValRead = mkPutState st pvalRef in

    match newNodeState st (pvalInitRead ist2.initState).1 with (st, xHere) in
    let putValue = mkPutState st xHere.id in

    let mkUpdateF = lam fs. lam st. lam specId. lam ist.
      foldl (lam ist. lam f. f specId ist) ist fs in
    match newState st (mkUpdateF ist2.update) with (st, updateRef) in
    let getUpdate = mkGetState st updateRef in
    let putUpdate = mkPutState st updateRef in

    let st =
      let readSt = mkReadRef st istRef in
      mapSt (lam st. store st (PSubmodelRef {readSt = readSt})) st in
    let update = lam specId. lam st.
      match getValue st with (changeId, value) in
      let prevWeight = getWeight st in
      if eqi specId changeId then
        -- NOTE(vipa, 2025-09-30): The initial value has changed,
        -- i.e., we need to re-run the creation function
        let ist =
          match ist with PVS ist in PVS
          { ist with specId = specId
          , initState = {ist.initState with valuesAbove = addPA st.valuesAbove st.values}
          } in
        match f ist value with (PVS ist, pval) in
        let newWeight = ist.initState.permanentWeight in
        let st = putVals ist.initState.values st in
        let st = putWeight newWeight st in
        let st = putIst ist.st st in
        let st = putUpdate (mkUpdateF ist.update) st in
        let readF = mkPValReadF (PVS ist) pval in
        let st = putPValRead readF st in
        let st =
          { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
          , temporaryWeight = addf st.temporaryWeight ist.initState.temporaryWeight
          } in
        let value = (readF ist.initState).1 in
        putValue (specId, value) st
      else
        -- NOTE(vipa, 2025-09-30): The initial value is unchanged,
        -- i.e., we don't need to re-run the creation function, we
        -- just need to update the current sub-model
        let update = getUpdate st in
        let innerState =
          { valuesAbove = addPA st.valuesAbove st.values
          , values = getVals st
          , permanentWeight = getWeight st
          , temporaryWeight = 0.0
          } in
        let innerState = update st specId innerState in
        let newWeight = innerState.permanentWeight in
        let st = putVals innerState.values st in
        let st = putWeight newWeight st in
        let st =
          { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
          , temporaryWeight = addf st.temporaryWeight innerState.temporaryWeight
          } in
        match (getPValRead st) innerState with (changeId, value) in
        if eqi specId changeId then
          putValue (specId, value) st
        else st in
    (addUpdate update st, PVar xHere)

  sem p_select st f =
  | PVal x -> (st, f x.value)
  | PVar x ->
    let initId = match st with PVS st in st.specId in
    let readValue = mkGetState st x.id in
    let initVar = f (onInitState readValue st).1 in
    let mkPValReadF = lam specId. lam pval.
      switch pval
      case PVal x then lam. (specId, x.value)
      case PVar x then mkGetState st x.id
      end in
    let initReadF = mkPValReadF initId initVar in
    match newState st initReadF with (st, varRef) in
    let getReadF = mkGetState st varRef in
    let putReadF = mkPutState st varRef in
    match newNodeState st (onInitState initReadF st).1 with (st, xHere) in
    let writeValue = mkPutState st xHere.id in
    let update = lam specId. lam st.
      match readValue st with (changeId, value) in
      if eqi specId changeId then
        -- Value changed, pick new node
        let var = f value in
        let readF = mkPValReadF specId var in
        let st = putReadF readF st in
        match readF st with (_, value) in
        writeValue (specId, value) st
      else
        -- Value did not change, check selected value
        match (getReadF st) st with (changeId, value) in
        if eqi specId changeId
        then writeValue (specId, value) st
        else st in
    (addUpdate update st, PVar xHere)

  sem p_weight st store f =
  | PVal x ->
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight (f x.value)}) st in
    mapSt (lam st. store st (PWeightRef ())) st
  | PVar x ->
    let readValue = mkGetState st x.id in
    let initWeight = f (onInitState readValue st).1 in
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight initWeight}) st in
    let st = mapSt (lam st. store st (PWeightRef ())) st in
    match newState st initWeight with (st, weightRef) in
    let readWeight = mkGetState st weightRef in
    let writeWeight = mkPutState st weightRef in
    let update = lam specId. lam st.
      match readValue st with (changeId, value) in
      if eqi specId changeId then
        let prevWeight = readWeight st in
        let newWeight = f value in
        let st = {st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)} in
        writeWeight newWeight st
      else st in
    addUpdate update st

  sem p_assume st store = | dist ->
    let fetchDist = switch dist
      case PVal x then let specId = match st with PVS st in st.specId in lam. (specId, x.value)
      case PVar x then mkGetState st x.id
      end in
    let initDist = (onInitState fetchDist st).1 in
    let initValue = p_sample initDist in
    match newNodeState st initValue with (st, xHere) in
    match newState st (p_logObserve initDist initValue) with (st, weightRef) in
    match newState st (lam d. lam. d) with (st, driftRef) in
    let readValue = mkGetState st xHere.id in
    let writeValue = mkPutState st xHere.id in
    let readWeight = mkGetState st weightRef in
    let writeWeight = mkPutState st weightRef in
    let readDrift = mkGetState st driftRef in
    let update = lam specId. lam st.
      match readValue st with (changeIdHere, prevValue) in
      match fetchDist st with (changeIdDist, dist) in
      let prevWeight = readWeight st in
      if eqi specId changeIdHere then
        -- Draw a new sample, i.e., value changes
        let drift = readDrift st in
        let kernel = drift dist prevValue in
        let proposal = p_sample kernel in
        let reverseKernel = drift dist proposal in

        let newWeight = p_logObserve dist proposal in

        let prevToProposalProb = p_logObserve kernel proposal in
        let proposalToPrevProb = p_logObserve reverseKernel prevValue in

        let st = writeValue (specId, proposal) st in
        let st = writeWeight newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight
          (addf
            (subf newWeight prevWeight)
            (subf proposalToPrevProb prevToProposalProb))}
      else if eqi specId changeIdDist then
        -- Reuse current sample, i.e., value doesn't change
        let newWeight = p_logObserve dist prevValue in
        let st = writeWeight newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight (subf newWeight prevWeight)}
      else st in
    let readValue = mkReadRef st xHere.id in
    let readValue = lam vals. (readValue vals).1 in
    let writeValue = mkWriteRef st xHere.id in
    let writeDrift = mkWriteRef st driftRef in
    let st = mapSt (lam st. store st (PAssumeRef {readValue = readValue, writeValue = writeValue, writeDrift = writeDrift})) st in
    (addUpdate update st, PVar xHere)
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
  type SimpleState x = ([SomePAssumeRef], x)

  sem simpleStore : all a. SimpleState () -> PAssumeRef a -> SimpleState ()
  sem simpleStore rs = | r -> (snoc rs.0 (SomePAssumeRef r), rs.1)

  sem simpleExport : all x2. SimpleState () -> PExportRef x2 -> SimpleState (PExportRef x2)
  sem simpleExport rs = | r -> (rs.0, r)

  sem simpleResample : all x. Float -> PValInstance Partial (SimpleState x) -> PValInstance Partial (SimpleState x)
  sem simpleResample globalProb = | instance ->
    let st = getSt instance in
    let doResample = lam instance. lam someAssume.
      match someAssume with SomePAssumeRef x in
      resampleAssume (lam d. lam. d) x instance in
    if bernoulliSample globalProb then
      foldl doResample instance st.0
    else
      doResample instance (_chooseUniform st.0)

  sem simpleRead : all x. all complete. PValInstance complete (SimpleState (PExportRef x)) -> x
  sem simpleRead = | instance ->
    readPreviousExport (getSt instance).1 instance
end

let showHistogram : Bool = false


-- === Bern and ==

let baseline = lam.
  let a = assume (Bernoulli 0.5) in
  let b = assume (Bernoulli 0.5) in
  and a b

lang BernAnd = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    -- dist : PVal y (PDist Bool)
    match p_assume st simpleStore dist with (st, a) in
    match p_assume st simpleStore dist with (st, b) in
    match p_map st and a with (st, tmp) in
    -- and : Bool -> (Bool -> Bool)
    --
    -- tmp : PVal y (Bool -> Bool)
    match p_apply st tmp b with (st, res) in
    p_export st simpleExport res
end

lang RunBernAndMut = BernAnd + MCMCPVal + MutPVal
end

lang RunBernAndPersistent2 = BernAnd + MCMCPVal + SimplePersistentPVal2
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
    if showHistogram then printLn (hist2string toString (mkHisto res)) else () in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    if showHistogram then printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) else () in
  let run =
    use RunBernAndMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Mut" (timeF run);
  let run =
    use RunBernAndPersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
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
    let f = lam st. lam c.
      if c then
        match p_pure st (p_bernoulli 0.9) with (st, dist) in
        p_assume_ st dist
      else
        match p_pure st (p_bernoulli 0.5) with (st, dist) in
        p_assume_ st dist in
    match p_bind_ st f c with (st, res) in
    p_export st simpleExport res
end

lang RunSimpleBindMut = SimpleBind + MCMCPVal + MutPVal
end

lang RunSimpleBindPersistent2 = SimpleBind + MCMCPVal + SimplePersistentPVal2
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
    if showHistogram then printLn (hist2string toString (mkHisto res)) else () in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    if showHistogram then printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) else () in
  let run =
    use RunSimpleBindMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut" (timeF run);
  let run =
    use RunSimpleBindPersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
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
    recursive let f = lam i. lam st. lam c.
      let recur = lam x. lam y. f (addi i 1) x y in
      if c then
        match p_assume_ st dist with (st, c) in
        p_bind_ st recur c
      else
        p_pure st i in
    let start = lam x. f 0 x in
    match p_bind_ st start c with (st, res) in
    p_export st simpleExport res
end

lang ManualGeometricCache = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    match p_assume st simpleStore dist with (st, c) in
    match p_cache st eqb c with (st, c) in
    recursive let f = lam i. lam st. lam c.
      let recur = lam x. lam y. f (addi i 1) x y in
      if c then
        match p_assume_ st dist with (st, c) in
        p_bind_ st recur c
      else
        p_pure st i in
    let start = lam x. f 0 x in
    match p_bind_ st start c with (st, res) in
    p_export st simpleExport res
end

lang RunManualGeometricMut = ManualGeometric + MCMCPVal + MutPVal
end

lang RunManualGeometricPersistent2 = ManualGeometric + MCMCPVal + SimplePersistentPVal2
end

lang RunManualGeometricCacheMut = ManualGeometricCache + MCMCPVal + MutPVal
end

lang RunManualGeometricCachePersistent2 = ManualGeometricCache + MCMCPVal + SimplePersistentPVal2
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
    if showHistogram then printLn (hist2string toString (mkHisto res)) else () in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    if showHistogram then printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) else () in
  let run =
    use RunManualGeometricMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut" (timeF run);
  let run =
    use RunManualGeometricPersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2" (timeF run);
  let run =
    use RunManualGeometricCacheMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut cache" (timeF run);
  let run =
    use RunManualGeometricCachePersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2 cache" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
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

lang RunCoinOneObservePersistent2 = CoinOneObserve + MCMCPVal + SimplePersistentPVal2
end

lang RunCoinManyObservePersistent2 = CoinManyObserve + MCMCPVal + SimplePersistentPVal2
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
    if showHistogram then printLn (hist2string toString (mkHisto res)) else () in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    if showHistogram then printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) else () in
  let run =
    use RunCoinOneObserveMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut one observe" (timeF run);
  let run =
    use RunCoinManyObserveMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut many observe" (timeF run);
  let run =
    use RunCoinOneObservePersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2 one observe" (timeF run);
  let run =
    use RunCoinManyObservePersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2 many observe" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
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
    -- cancel (observe x rootDist) in

  recursive let cluster = lam nTrees. lam trees.
    if eqi nTrees 1 then head trees else
    let pair = pickpair nTrees in
    let i = pair.0 in
    let j = pair.1 in
    -- match pickpair nTrees with (i, j) in
    let l = get trees i in
    let r = get trees j in
    let trees = mapiOption
      (lam idx. lam v. if or (eqi idx i) (eqi idx j) then None () else Some v)
      trees in
    let here = assume rootDist in
    cancelRootDist (getX l);
    cancelRootDist (getX r);
    observe (getX l) (deviateFromDist here);
    observe (getX r) (deviateFromDist here);
    cluster (subi nTrees 1) (snoc trees (mkNode here l r)) in

  for_ initTrees (lam t. observe (getX t) rootDist);
  cluster (length initTrees) initTrees

-- lang TreeInferenceQuoteAutoQuote = SimpleResample
--   sem run = | st ->
--   let pickpair_p = lam st. lam n.
--     match p_pure st (p_uniformDiscrete 0 (subi n 1)) with (st, tmp) in
--     match p_assume st simpleStore tmp with (st, i) in
--     match p_pure st (p_uniformDiscrete 0 (subi n 2)) with (st, tmp) in
--     match p_assume st simpleStore tmp with (st, j) in
--     match p_map st lti j with (st, tmp) in
--     match p_apply st tmp i with (st, tmp) in
--     let f : all z. PValState () z -> Bool -> PValHList z Unknown -> (Unknown, PVal z (Int, Int)) = lam st. lam c. lam l.
--       match l with PVHCons (i, PVHCons (j, PVHNil ())) in
--       if c then
--         match p_map st (lam a. lam b. (a, b)) i with (st, tmp) in
--         match p_apply st tmp j with (st, ret) in
--         (st, ret)
--       else
--         match p_map st (lam a. lam b. (a, b)) i with (st, tmp) in
--         match p_map st addi j with (st, tmp2) in
--         match p_map st (lam f. f 1) tmp2 with (st, tmp2) in
--         match p_apply st tmp tmp2 with (st, ret) in
--         (st, ret) in
--     p_bind_ st #frozen"f" tmp (PVHCons (i, PVHCons (j, PVHNil ()))) in

--   let mapiOption
--     : all a. all a_. all b. all b_. (V x1 Int -> V F (V a_ a -> V x3 (Option (V b_ b))))
--     -> V x4 [V a_ a]
--     -> V x5 [V b_ b]
--     = lam f.
--       recursive let work = lam st. lam idx. lam as.

--   match p_pure st (p_gaussian 0.0 10.0) with (st, tmp) in
--   match p_assume st simpleStore tmp with (st, rootValue) in
--   let deviateFromDist_l = lam st. lam x. p_map st (lam x. p_gaussian x 10.0) x in
--   match deviateFromDist_l st rootValue with (st, rootDist) in
--   let cancelRootDist_l = lam st. lam x.
--     -- TODO(vipa, 2025-10-06): When converting a function we need to
--     -- figure out if it's going to be used in p_bind or p_map, for how
--     -- to handle free variables. Maybe we'll never convert a function
--     -- wholesale, rather just `if` expressions?
--     match p_map st p_logObserve rootDist with (st, tmp) in
--     match p_apply st tmp x with (st, tmp) in
--     (p_weight_ st negf tmp, ()) in
--   let cancelRootDist_p = lam st. lam x.
--     match p_map st p_logObserve rootDist with (st, tmp) in
--     match p_map st (lam f. f x) tmp with (st, tmp) in
--     (p_weight_ st negf tmp, ()) in

--   -- recursive
--   --   let cluster_p = lam st. lam nTrees. lam trees.
--   --     if eqi nTrees 1 then p_pure st (head trees) else
--   --     -- NOTE(vipa, 2025-10-06): This assumes special handling for
--   --     -- `match ... in`, with the assumption that such patterns are
--   --     -- infallible and thus don't require branching or `p_bind`
--   --     match pickpair_p st nTrees with (st, tmp) in
--   --     match p_map st (lam x. match x with (i, j) in i) tmp with (st, i) in
--   --     match p_map st (lam x. match x with (i, j) in j) tmp with (st, j) in
--   --     match p_map st (get trees) i with (st, l) in
--   --     match p_map st (get trees) j with (st, r) in
--   never
-- end

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
        let f = lam trip. lam st. lam pair.
          match trip with (idx0, idx1, idx2) in
          match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
          with (l, r) in
          if leqi l idx then
            if leqi r (addi idx 1)
            then (st, idx2)
            else (st, idx1)
          else (st, idx0)
        in
        p_bind_ st (f (idx0, idx1, idx2)) pair in
      match mapAccumL (lam st. lam f. f st) st (create (subi (length trees) 2) mkCarryOn) with (st, carryOns) in
      let f = lam trees. lam st. lam pair.
        match p_map st (lam l. lam r. (l, r)) (get trees pair.0) with (st, tmp) in
        p_apply st tmp (get trees pair.1) in
      match p_bind_ st (f trees) pair with (st, treePair) in
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

lang RunTreeInferenceTreeBindPersistent2 = TreeInferenceTreeBind + MCMCPVal + SimplePersistentPVal2
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
        let f = lam trip. lam pair.
          match trip with (idx0, idx1, idx2) in
          match if lti pair.0 pair.1 then (pair.0, pair.1) else (pair.1, pair.0)
          with (l, r) in
          if leqi l idx then
            if leqi r (addi idx 1)
            then idx2
            else idx1
          else idx0
        in
        p_select st (f (idx0, idx1, idx2)) pair in
      match mapAccumL (lam st. lam f. f st) st (create (subi (length trees) 2) mkCarryOn) with (st, carryOns) in
      let f = lam trees. lam pair. get trees pair.0 in
      match p_select st (f trees) pair with (st, l) in
      let f = lam trees. lam pair. get trees pair.1 in
      match p_select st (f trees) pair with (st, r) in
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

lang RunTreeInferenceTreeSelectPersistent2 = TreeInferenceTreeSelect + MCMCPVal + SimplePersistentPVal2
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
    if showHistogram then printLn (hist2string toString (mkHisto res)) else () in
  let summarizeBaseline = lam label. lam pair.
    match pair with (time, res) in
    printLn (join [float2string time, "ms (", label, ")"]);
    if showHistogram then printLn (hist2string toString (mkHisto (distEmpiricalSamples res).0)) else () in
  let run =
    use RunTreeInferenceTreeBindMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut tree bind" (timeF run);
  let run =
    use RunTreeInferenceTreeBindPersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam.
      let samples = (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
      samples in
  summarizePVal "pval mcmc Persistent2 tree bind" (timeF run);
  let run =
    use RunTreeInferenceTreeSelectMut in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc mut tree select" (timeF run);
  let run =
    use RunTreeInferenceTreeSelectPersistent2 in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent2 tree select" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "none", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "partial", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw partial" (timeF run);
  let run = lam.
    infer (LightweightMCMC {cps = "full", globalProb = globalProb, continue = (iterations, lam r. lam. (subi r 1, neqi r 1))}) baseline in
  summarizeBaseline "mcmc-lw full" (timeF run);
  ()

mexpr
-- TODO(vipa, 2025-09-25): For whatever reason we end up with a Decl
-- without an info field if we have `infer` above but not here. I have
-- no idea why.
let x = infer (Default ()) (lam. ()) in
printLn "\n\nDone";
()
