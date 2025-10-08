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

lang PValInterfaceBase
  -- NOTE(vipa, 2025-09-25): We put this definition in its own
  -- fragment because we can't define GADTs inside language fragments
  syn PVal x a =
end

type HCons a as
type HConsL a as
type HNil

-- NOTE(vipa, 2025-09-25): This definition is needed for the PVal
-- interface, but since it's a GADT we can't define it in a language
-- fragment.
type PValHList x l
con PVHCons : all x. all a. all l. (use PValInterfaceBase in PVal x a, PValHList x l) -> PValHList x (HCons a l)
con PVHConsL : all x. all a. all l. (use PValInterfaceBase in [PVal x a], PValHList x l) -> PValHList x (HConsL a l)
con PVHNil : all x. () -> PValHList x HNil

let eliminatePValHList
  : all ret. all x. all l. use PValInterfaceBase in (all a. all l. PVal x a -> PValHList x l -> ret)
  -> (all a. all l. [PVal x a] -> PValHList x l -> ret)
  -> ret
  -> PValHList x l
  -> ret
  = lam cons. lam consl. lam nil. lam l.
    let unlock : all l2. PValHList x l -> PValHList x l2 = unsafeCoerce in
    match unlock l with PVHCons (a, b) then cons a b else
    match unlock l with PVHConsL (a, b) then consl a b else
    match unlock l with PVHNil _ then nil else
    never

let foldPValHList : all acc. all x. all l. use PValInterfaceBase in (all a. acc -> PVal x a -> acc) -> acc -> PValHList x l -> acc
  = lam f.
    recursive let work : all l. acc -> PValHList x l -> acc = lam acc. lam l.
      let cons = lam p. lam l. work (f acc p) l in
      let consL = lam ps. lam l. work (foldl f acc ps) l in
      eliminatePValHList #frozen"cons" #frozen"consL" acc l in
    work

let mapAccumLPValHList
  : all acc. all x. all y. all l. use PValInterfaceBase in (all a. acc -> PVal x a -> (acc, PVal y a))
  -> acc
  -> PValHList x l
  -> (acc, PValHList y l)
  = lam f.
    use PValInterfaceBase in
    recursive let work : all l. acc -> PValHList x l -> (acc, PValHList y l) = lam acc. lam l.
      let unlock : all l2. PValHList x l -> PValHList x l2 = unsafeCoerce in
      match unlock l with PVHCons (p, l) then
        let lock : all a. all l2. PValHList y (HCons a l2) -> PValHList y l = unsafeCoerce in
        match f acc p with (acc, p) in
        match work acc l with (acc, l) in
        (acc, lock (PVHCons (p, l)))
      else match unlock l with PVHConsL (ps, l) then
        let lock : all a. all l2. PValHList y (HConsL a l2) -> PValHList y l = unsafeCoerce in
        match mapAccumL f acc ps with (acc, ps) in
        match work acc l with (acc, l) in
        (acc, lock (PVHConsL (ps, l)))
      else match unlock l with PVHNil _ then
        let lock : PValHList y HNil -> PValHList y l = unsafeCoerce in
        (acc, lock (PVHNil ()))
      else never in
    work

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
  sem instantiate : all st. all st2. (all y. PValState st y -> PValState st2 y) -> st -> PValInstance Complete st2
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
  sem p_cache : all st. all x. all y. all a. PValState st y
    -> (a -> a -> Bool)
    -> PVal y a
    -> (PValState st y, PVal y a)
  -- Make a probabilistic value available to read outside the model,
  -- via a model instance.
  sem p_export : all st. all st2. all x. all y. all a. PValState st y
    -> (st -> PExportRef a -> st2)
    -> PVal y a
    -> PValState st2 y

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
  sem p_bind : all st. all ist. all ist2. all st2. all y. all a. all as. all b. PValState st y
    -> (st -> PSubmodelRef ist2 -> st2)
    -> ist
    -> (all z. PValState ist z -> a -> PValHList z as -> (PValState ist2 z, PVal z b))
    -> PVal y a
    -> PValHList y as
    -> (PValState st2 y, PVal y b)
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
  sem p_weight : all st. all st2. all x. all y. all a. PValState st y
    -> (st -> PWeightRef -> st2)
    -> (a -> Float)
    -> PVal y a
    -> PValState st2 y
  -- Draw a value from a distribution.
  sem p_assume : all st. all st2. all x. all y. all a. PValState st y
    -> (st -> PAssumeRef a -> st2)
    -> PVal y (PDist a)
    -> (PValState st2 y, PVal y a)

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
  syn PVal x a = | PVal (PValRec a)

  syn PValState st y = | PVS {initId : IterationID, updates : [PState -> ()], initWeight : Float, st : st}
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

  sem initModel : all st. all st2. all a. st -> (all y. PValState st y -> PValState st2 y) -> (st2, Float, UpdateFunction)
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

  sem p_bind st store initSt2 f a = | refs ->
    match st with PVS st in
    match a with PVal a in
    let f : PValState ist y -> PValState (PValRec b, ist2) y = lam st.
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
    let st =
      { st = store st.st (PSubmodelRef {readSt = lam. deref st2})
      , initWeight = initWeight
      , updates = snoc st.updates update
      , initId = st.initId
      } in
    (PVS st, PVal {value = value, changeId = changeId})

  sem p_select st f a = | refs ->
    match st with PVS st in
    match a with PVal a in
    let pval = f (deref a.value) refs in
    let value = ref (deref (match pval with PVal x in x.value)) in
    let pval = ref pval in
    let changeId = ref (deref a.changeId) in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        let prevPVal = deref pval in
        let prevValue = deref value in
        let newPVal = f (deref a.value) refs in
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


-- === Simple Persistent PVal ===

-- This implementation is very similar to the one above, except each
-- reference is replaced by an index into a persistent array.

type Dyn
let asDyn : all a. a -> Dyn = unsafeCoerce
let fromDyn : all a. Dyn -> a = unsafeCoerce

lang SimplePersistentPVal = PValInterface
  syn RefID a = | RefID Int
  type IterationID = Int

  type State =
    { values : PA Dyn
    , permanentWeight : Float
    , temporaryWeight : Float
    }

  syn PValState st y = | PVS
    { nextId : Int
    , specId : IterationID
    , initState : State
    , update : [IterationID -> State -> State]
    , readSubState : State -> State
    , mapSubState : (State -> State) -> State -> State
    , st : st
    }

  syn PWeightRef = | PWeightRef {} -- TODO(vipa, 2025-09-29):
  syn PAssumeRef a = | PAssumeRef
    { readValue : State -> a
    , writeValue : (IterationID, a) -> State -> State
    , writeDrift : (PDist a -> a -> PDist a) -> State -> State
    }
  syn PExportRef a =
  | PExportRef {readValue : State -> a}
  | PExportVal {value : a}
  syn PSubmodelRef st =
  | PSubmodelRef {readSt : State -> st}
  | PSubmodelVal {st : st}

  type PVarRec a = {id : RefID (IterationID, a), initValue : a}
  syn PVal y a =
  | PVal {value : a}
  | PVar (PVarRec a)

  syn PValInstance complete st = | PVI
    { st : st
    , state : State
    , specId : IterationID
    , update : IterationID -> State -> State
    }

  sem addUpdate : all st. all y. (IterationID -> State -> State) -> PValState st y -> PValState st y
  sem addUpdate f = | PVS st -> PVS {st with update = snoc st.update f}

  sem mapSt : all st. all st2. all y. (st -> st2) -> PValState st y -> PValState st2 y
  sem mapSt f = | PVS st -> PVS
    { st = f st.st
    , nextId = st.nextId
    , specId = st.specId
    , update = st.update
    , initState = st.initState
    , readSubState = st.readSubState
    , mapSubState = st.mapSubState
    }

  sem newNodeState : all st. all y. all a. PValState st y -> a -> (PValState st y, PVarRec a)
  sem newNodeState st = | value ->
    match st with PVS {specId = specId} in
    match newState st (specId, value) with (st, refId) in
    (st, {id = refId, initValue = value})

  sem newState : all st. all y. all a. PValState st y -> a -> (PValState st y, RefID a)
  sem newState st = | value ->
    match st with PVS st in
    let id = st.nextId in
    let st =
      { st with nextId = addi st.nextId 1
      , initState = {st.initState with values = addPA st.initState.values (asDyn value)}
      } in
    (PVS st, RefID id)

  sem getState : all a. RefID a -> State -> a
  sem getState ref = | st -> fromDyn (getPA st.values (match ref with RefID i in i))

  sem putState : all a. RefID a -> a -> State -> State
  sem putState ref val = | st -> {st with values = setPA st.values (match ref with RefID i in i) (asDyn val)}

  sem updateState : all a. RefID a -> (a -> a) -> State -> State
  sem updateState ref f = | st -> {st with values = updatePA st.values (match ref with RefID i in i) (lam x. asDyn (f (fromDyn x)))}

  sem withInitState : all st. all y. (State -> State) -> PValState st y -> PValState st y
  sem withInitState f = | PVS st -> PVS {st with initState = f st.initState}

  sem instantiate f = | st ->
    let st = PVS
      { nextId = 0
      , specId = 0
      , initState =
        { values = emptyPA
        , permanentWeight = 0.0
        , temporaryWeight = 0.0
        }
      , update = []
      , st = st
      , readSubState = lam st. st
      , mapSubState = lam f. lam st. f st
      } in
    match f st with PVS st in
    let updates = st.update in
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
    let prevValue = a.readValue x.state in
    let state = a.writeValue (x.specId, prevValue) x.state in
    let state = a.writeDrift driftf state in
    PVI {x with state = state}

  sem readPreviousAssume aref = | PVI x ->
    match aref with PAssumeRef a in
    a.readValue x.state

  sem readPreviousExport eref = | PVI x ->
    match eref with PExportRef e in
    e.readValue x.state

  sem readPreviousSubmodel mref = | PVI x ->
    error "TODO"

  sem p_cache st eq =
  | PVal x -> (st, PVal x)
  | PVar x ->
    match newNodeState st x.initValue with (st, xHere) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        match getState xHere.id st with (_, prevValue) in
        if eq prevValue value then st else
        putState xHere.id (specId, value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_export st store = | pval ->
    match st with PVS {readSubState = readSubState} in
    let ref = switch pval
      case PVal x then PExportVal {value = x.value}
      case PVar x then PExportRef {readValue = lam st. (getState x.id (readSubState st)).1}
      end in
    mapSt (lam st. store st ref) st

  sem p_pure st = | value -> (st, PVal {value = value})

  sem p_map st f =
  | PVal x -> p_pure st (f x.value)
  | PVar x ->
    match newNodeState st (f x.initValue) with (st, xHere) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        putState xHere.id (specId, f value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_apply st f = | a ->
    switch (f, a)
    case (PVal f, PVal a) then p_pure st (f.value a.value)
    case (PVal f, a) then p_map st f.value a
    case (f, PVal a) then p_map st (lam f. f a.value) f
    case (PVar f, PVar a) then
      match newNodeState st (f.initValue a.initValue) with (st, xHere) in
      let update = lam specId. lam st.
        match getState f.id st with (changeIdF, f) in
        match getState a.id st with (changeIdA, a) in
        if or (eqi specId changeIdF) (eqi specId changeIdA) then
          putState xHere.id (specId, f a) st
        else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_bind st store st2 f pval = | refs ->
    switch pval
    case PVal x then
      -- NOTE(vipa, 2025-09-30): We will never need to re-run the
      -- creation function here, thus we inline the sub-model in the
      -- parent model, meaning we don't have to deal with two stores
      let ost = match st with PVS st in st.st in
      let st = mapSt (lam. st2) st in
      match f st x.value refs with (st, pval) in
      let ref = PSubmodelVal {st = match st with PVS st in st.st} in
      let st = mapSt (lam ist. store ost ref) st in
      (st, pval)
    case PVar x then
      -- NOTE(vipa, 2025-09-30): The common case, where we *do* have
      -- to re-run the creation function, requires two stores, which
      -- requires copying values from the outer store to the inner
      -- when they change.
      let innerState : State =
        { values = emptyPA
        , permanentWeight = 0.0
        , temporaryWeight = 0.0
        } in
      match newState st innerState with (st, stateRef) in
      match st with PVS {readSubState = readSubState, mapSubState = mapSubState} in
      let ist : PValState ist () = PVS
        { nextId = 0
        , specId = match st with PVS st in st.specId
        , initState = innerState
        , update = []
        , st = st2
        , readSubState = lam st. getState stateRef (readSubState st)
        , mapSubState = lam f. mapSubState (updateState stateRef f)
        } in
      -- NOTE(vipa, 2025-09-30): This function copies the initial
      -- values into the inner store and creates a function for
      -- updating the inner store with values from the outer store.
      let initCopyRefs : PValState ist () -> (PValState ist (), State -> IterationID -> State -> State, State -> (PValHList () as, PA Dyn), PValHList () as) = lam ist.
        type Acc = (PValState ist (), [State -> IterationID -> State -> State], [State -> PA Dyn -> PA Dyn]) in
        let copy : all a. Acc -> PVal y a -> (Acc, PVal () a) = lam acc. lam pval. switch pval
          case PVal x then (acc, PVal x)
          case PVar x then
            match acc with (ist, updates, copies) in
            match newNodeState ist x.initValue with (ist, xHere) in
            let update = lam st. lam specId. lam ist.
              match getState x.id st with (changeId, value) in
              if eqi specId changeId
              then putState xHere.id (specId, value) ist
              else ist in
            let copy = lam st. lam pa.
              addPA pa (asDyn (getState x.id st)) in
            ((ist, snoc updates update, snoc copies copy), PVar xHere)
          end in
        let mkNewRefs = lam st.
          let updateOne : all a. () -> PVal y a -> ((), PVal () a) = lam. lam pval. switch pval
            case PVal x then ((), PVal x)
            case PVar x then ((), PVar {x with initValue = (getState x.id st).1})
            end in
          mapAccumLPValHList #frozen"updateOne" () refs in
        match mapAccumLPValHList #frozen"copy" (ist, [], []) refs with ((ist, updates, copies), irefs) in
        ( ist
        , lam st. lam specId. lam ist. foldl (lam ist. lam f. f st specId ist) ist updates
        , lam st. ((mkNewRefs st).1, foldl (lam pa. lam f. f st pa) emptyPA copies)
        , irefs
        ) in
      match initCopyRefs ist with (ist, updateRefs, copyRefs, irefs) in
      -- NOTE(vipa, 2025-09-30): The `ist` value at this point can be
      -- reused for all sub-models, as long as `copyRefs` is run to
      -- update its internal state.values first. `ist2` below is only
      -- valid for the first sub-model.
      match f ist x.initValue irefs with (PVS ist2, pval) in
      let st = withInitState (putState stateRef ist2.initState) st in

      match newState st ist2.st with (st, istRef) in

      match newState st pval with (st, pvalRef) in

      let initValue = switch pval
        case PVal x then x.value
        case PVar x then x.initValue
        end in
      match newNodeState st initValue with (st, xHere) in

      let mkUpdateF = lam fs. lam st. lam specId. lam ist.
        let ist = updateRefs st specId {ist with temporaryWeight = 0.0} in
        foldl (lam ist. lam f. f specId ist) ist fs in
      match newState st (mkUpdateF ist2.update) with (st, updateRef) in

      let st = mapSt (lam st. store st (PSubmodelRef {readSt = lam st. getState istRef (readSubState st)})) st in
      let update = lam specId. lam st.
        match getState x.id st with (changeId, value) in
        if eqi specId changeId then
          -- NOTE(vipa, 2025-09-30): The initial value has changed,
          -- i.e., we need to re-run the creation function
          let prevWeight = (getState stateRef st).permanentWeight in
          match copyRefs st with (irefs, values) in
          let ist = withInitState (lam ist. {ist with values = values}) ist in
          let ist = match ist with PVS ist in PVS {ist with specId = specId} in
          -- TODO(vipa, 2025-10-01): Create new irefs with appropriate initValues
          match f ist value irefs with (PVS ist, pval) in
          let st = putState stateRef ist.initState st in
          let st = putState istRef ist.st st in
          let st = putState updateRef (mkUpdateF ist.update) st in
          let st = putState pvalRef pval st in
          let newWeight = ist.initState.permanentWeight in
          let st =
            { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
            , temporaryWeight = addf st.temporaryWeight ist.initState.temporaryWeight
            } in
          let value = switch pval
            case PVal x then x.value
            case PVar x then x.initValue
            end in
          putState xHere.id (specId, value) st
        else
          -- NOTE(vipa, 2025-09-30): The initial value is unchanged,
          -- i.e., we don't need to re-run the creation function, we
          -- just need to update the current sub-model
          let update = getState updateRef st in
          let innerState = getState stateRef st in
          let prevWeight = innerState.permanentWeight in
          let innerState = update st specId innerState in
          let newWeight = innerState.permanentWeight in
          let st =
            { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
            , temporaryWeight = addf st.temporaryWeight innerState.temporaryWeight
            } in
          let st = putState stateRef innerState st in
          match getState pvalRef st with pval in
          match pval with PVar x then
            match getState x.id innerState with (changeId, value) in
            if eqi specId changeId
            then putState xHere.id (specId, value) st
            else st
          else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_select st f pval = | refs ->
    switch pval
    case PVal x then (st, f x.value refs)
    case PVar x then
      let initId = match st with PVS st in st.specId in
      let initVar = f x.initValue refs in
      let initValue = switch initVar
        case PVal x then x.value
        case PVar x then x.initValue
        end in
      match newNodeState st initValue with (st, xHere) in
      match newState st initVar with (st, varRef) in
      let update = lam specId. lam st.
        match getState x.id st with (changeId, value) in
        if eqi specId changeId then
          -- Value changed, pick new node
          let var = f value refs in
          let value = switch var
            case PVal x then x.value
            case PVar x then (getState x.id st).1
            end in
          let st = putState varRef var st in
          putState xHere.id (specId, value) st
        else
          -- Value did not change, check selected value
          match getState varRef st with PVar x then
            match getState x.id st with (changeId, value) in
            if eqi specId changeId then
              putState xHere.id (specId, value) st
            else st
          else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_weight st store f =
  | PVal x ->
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight (f x.value)}) st in
    mapSt (lam st. store st (PWeightRef ())) st
  | PVar x ->
    let initWeight = f x.initValue in
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight initWeight}) st in
    let st = mapSt (lam st. store st (PWeightRef ())) st in
    match newState st initWeight with (st, weightRef) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        let prevWeight = getState weightRef st in
        let newWeight = f value in
        let st = {st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)} in
        putState weightRef newWeight st
      else st in
    addUpdate update st

  sem p_assume st store = | dist ->
    let initDist = switch dist
      case PVal x then x.value
      case PVar x then x.initValue
      end in
    let fetchDist = switch dist
      case PVal x then let specId = match st with PVS st in st.specId in lam. (specId, x.value)
      case PVar x then getState x.id
      end in
    match newNodeState st (p_sample initDist) with (st, xHere) in
    match newState st (p_logObserve initDist xHere.initValue) with (st, weightRef) in
    match newState st (lam d. lam. d) with (st, driftRef) in
    let update = lam specId. lam st.
      match getState xHere.id st with (changeIdHere, prevValue) in
      match fetchDist st with (changeIdDist, dist) in
      let prevWeight = getState weightRef st in
      if eqi specId changeIdHere then
        -- Draw a new sample, i.e., value changes
        let drift = getState driftRef st in
        let kernel = drift dist prevValue in
        let proposal = p_sample kernel in
        let reverseKernel = drift dist proposal in

        let newWeight = p_logObserve dist proposal in

        let prevToProposalProb = p_logObserve kernel proposal in
        let proposalToPrevProb = p_logObserve reverseKernel prevValue in

        let st = putState xHere.id (specId, proposal) st in
        let st = putState weightRef newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight
          (addf
            (subf newWeight prevWeight)
            (subf proposalToPrevProb prevToProposalProb))}
      else if eqi specId changeIdDist then
        -- Reuse current sample, i.e., value doesn't change
        let newWeight = p_logObserve dist prevValue in
        let st = putState weightRef newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight (subf newWeight prevWeight)}
      else st in
    match st with PVS {readSubState = readSubState, mapSubState = mapSubState} in
    let readValue = lam st. (getState xHere.id (readSubState st)).1 in
    let writeValue = lam val. mapSubState (putState xHere.id val) in
    let writeDrift = lam val. mapSubState (putState driftRef val) in
    let st = mapSt (lam st. store st (PAssumeRef {readValue = readValue, writeValue = writeValue, writeDrift = writeDrift})) st in
    (addUpdate update st, PVar xHere)
end


-- === Simple Persistent PVal (less copying) ===

-- This implementation is the same as the one above, except each
-- reference also knows how deep it is in sub-models, meaning we can
-- reference values in super-models without copying to sub-models.

lang SimplePersistentPVal2 = PValInterface
  syn RefID a = | RefID { idx : Int, level : Int }
  type IterationID = Int

  type StateRec =
    { values : PA Dyn
    , permanentWeight : Float
    , temporaryWeight : Float
    , level : Int
    , above : State
    }
  syn State = | State StateRec

  syn PValState st y = | PVS
    { nextId : Int
    , specId : IterationID
    , initState : StateRec
    , update : [IterationID -> StateRec -> StateRec]
    , readSubState : StateRec -> StateRec
    , mapSubState : (StateRec -> StateRec) -> StateRec -> StateRec
    , st : st
    }

  syn PWeightRef = | PWeightRef {} -- TODO(vipa, 2025-09-29):
  syn PAssumeRef a = | PAssumeRef
    { readValue : StateRec -> a
    , writeValue : (IterationID, a) -> StateRec -> StateRec
    , writeDrift : (PDist a -> a -> PDist a) -> StateRec -> StateRec
    }
  syn PExportRef a =
  | PExportRef {readValue : StateRec -> a}
  | PExportVal {value : a}
  syn PSubmodelRef st =
  | PSubmodelRef {readSt : StateRec -> st}
  | PSubmodelVal {st : st}

  type PVarRec a = {id : RefID (IterationID, a), initValue : a}
  syn PVal y a =
  | PVal {value : a}
  | PVar (PVarRec a)

  syn PValInstance complete st = | PVI
    { st : st
    , state : StateRec
    , specId : IterationID
    , update : IterationID -> StateRec -> StateRec
    }

  sem addUpdate : all st. all y. (IterationID -> StateRec -> StateRec) -> PValState st y -> PValState st y
  sem addUpdate f = | PVS st -> PVS {st with update = snoc st.update f}

  sem mapSt : all st. all st2. all y. (st -> st2) -> PValState st y -> PValState st2 y
  sem mapSt f = | PVS st -> PVS
    { st = f st.st
    , nextId = st.nextId
    , specId = st.specId
    , update = st.update
    , initState = st.initState
    , readSubState = st.readSubState
    , mapSubState = st.mapSubState
    }

  sem newNodeState : all st. all y. all a. PValState st y -> a -> (PValState st y, PVarRec a)
  sem newNodeState st = | value ->
    match st with PVS {specId = specId} in
    match newState st (specId, value) with (st, refId) in
    (st, {id = refId, initValue = value})

  sem newState : all st. all y. all a. PValState st y -> a -> (PValState st y, RefID a)
  sem newState st = | value ->
    match st with PVS st in
    let id = st.nextId in
    let st =
      { st with nextId = addi st.nextId 1
      , initState = {st.initState with values = addPA st.initState.values (asDyn value)}
      } in
    (PVS st, RefID {idx = id, level = st.initState.level})

  sem getState : all a. RefID a -> StateRec -> a
  sem getState ref = | st ->
    match ref with RefID {idx = idx, level = level} in
    recursive let work = lam steps. lam st.
      if eqi steps 0
      then fromDyn (getPA st.values idx)
      else work (subi steps 1) (match st.above with State st in st)
    in work (subi st.level level) st

  sem putState : all a. RefID a -> a -> StateRec -> StateRec
  sem putState ref val = | st ->
    match ref with RefID {idx = idx, level = level} in
    if eqi level st.level
    then {st with values = setPA st.values idx (asDyn val)}
    else error "Tried to putState on a non-local state"

  sem updateState : all a. RefID a -> (a -> a) -> StateRec -> StateRec
  sem updateState ref f = | st ->
    match ref with RefID {idx = idx, level = level} in
    if eqi level st.level
    then {st with values = updatePA st.values idx (lam x. asDyn (f (fromDyn x)))}
    else error "Tried to updateState on a non-local state"

  sem withInitState : all st. all y. (StateRec -> StateRec) -> PValState st y -> PValState st y
  sem withInitState f = | PVS st -> PVS {st with initState = f st.initState}

  sem instantiate f = | st ->
    let st = PVS
      { nextId = 0
      , specId = 0
      , initState =
        { values = emptyPA
        , permanentWeight = 0.0
        , temporaryWeight = 0.0
        , level = 0
        , above = unsafeCoerce 0  -- NOTE(vipa, 2025-10-16): This one will never be accessed, and we don't want it to be an Option to avoid extra tagging
        }
      , update = []
      , st = st
      , readSubState = lam st. st
      , mapSubState = lam f. lam st. f st
      } in
    match f st with PVS st in
    let updates = st.update in
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
    let prevValue = a.readValue x.state in
    let state = a.writeValue (x.specId, prevValue) x.state in
    let state = a.writeDrift driftf state in
    PVI {x with state = state}

  sem readPreviousAssume aref = | PVI x ->
    match aref with PAssumeRef a in
    a.readValue x.state

  sem readPreviousExport eref = | PVI x ->
    match eref with PExportRef e in
    e.readValue x.state

  sem readPreviousSubmodel mref = | PVI x ->
    error "TODO"

  sem p_cache st eq =
  | PVal x -> (st, PVal x)
  | PVar x ->
    match newNodeState st x.initValue with (st, xHere) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        match getState xHere.id st with (_, prevValue) in
        if eq prevValue value then st else
        putState xHere.id (specId, value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_export st store = | pval ->
    match st with PVS {readSubState = readSubState} in
    let ref = switch pval
      case PVal x then PExportVal {value = x.value}
      case PVar x then PExportRef {readValue = lam st. (getState x.id (readSubState st)).1}
      end in
    mapSt (lam st. store st ref) st

  sem p_pure st = | value -> (st, PVal {value = value})

  sem p_map st f =
  | PVal x -> p_pure st (f x.value)
  | PVar x ->
    match newNodeState st (f x.initValue) with (st, xHere) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        putState xHere.id (specId, f value) st
      else st in
    (addUpdate update st, PVar xHere)

  sem p_apply st f = | a ->
    switch (f, a)
    case (PVal f, PVal a) then p_pure st (f.value a.value)
    case (PVal f, a) then p_map st f.value a
    case (f, PVal a) then p_map st (lam f. f a.value) f
    case (PVar f, PVar a) then
      match newNodeState st (f.initValue a.initValue) with (st, xHere) in
      let update = lam specId. lam st.
        match getState f.id st with (changeIdF, f) in
        match getState a.id st with (changeIdA, a) in
        if or (eqi specId changeIdF) (eqi specId changeIdA) then
          putState xHere.id (specId, f a) st
        else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_bind st store st2 f pval = | refs ->
    switch pval
    case PVal x then
      -- NOTE(vipa, 2025-09-30): We will never need to re-run the
      -- creation function here, thus we inline the sub-model in the
      -- parent model, meaning we don't have to deal with two stores
      let ost = match st with PVS st in st.st in
      let st = mapSt (lam. st2) st in
      match f st x.value refs with (st, pval) in
      let ref = PSubmodelVal {st = match st with PVS st in st.st} in
      let st = mapSt (lam ist. store ost ref) st in
      (st, pval)
    case PVar x then
      -- NOTE(vipa, 2025-09-30): The common case, where we *do* have
      -- to re-run the creation function, requires two stores, which
      -- requires some handling of store "levels".
      match st with PVS {initState = initState} in
      let innerState : StateRec =
        { values = emptyPA
        , permanentWeight = 0.0
        , temporaryWeight = 0.0
        , level = addi initState.level 1
        , above = State initState
        } in
      match newState st innerState with (st, stateRef) in
      match st with PVS {readSubState = readSubState, mapSubState = mapSubState} in
      let ist : PValState ist () = PVS
        { nextId = 0
        , specId = match st with PVS st in st.specId
        , initState = innerState
        , update = []
        , st = st2
        , readSubState = lam st. getState stateRef (readSubState st)
        , mapSubState = lam f. mapSubState (updateState stateRef f)
        } in
      -- The initValue of each PVal will be up-to-date in the first
      -- iteration, so we can unsafeCoerce it to do less work. For
      -- later iterations we may have to update the initValues
      -- however, thus we make a function that does so.
      let initConvPValHList : PValHList y as -> PValHList () as = unsafeCoerce in
      let convPValHList : StateRec -> PValHList () as = lam st.
        let work : all a. () -> PVal y a -> ((), PVal () a) = lam. lam pval. switch pval
          case PVal x then ((), PVal x)
          case PVar x then ((), PVar {id = x.id, initValue = (getState x.id st).1})
          end in
        (mapAccumLPValHList #frozen"work" () refs).1 in
      -- NOTE(vipa, 2025-09-30): The `ist` value at this point can be
      -- reused for all sub-models. `ist2` below is only valid for the
      -- first sub-model.
      match f ist x.initValue (initConvPValHList refs) with (PVS ist2, pval) in
      let st = withInitState (putState stateRef ist2.initState) st in

      match newState st ist2.st with (st, istRef) in

      match newState st pval with (st, pvalRef) in

      let initValue = switch pval
        case PVal x then x.value
        case PVar x then x.initValue
        end in
      match newNodeState st initValue with (st, xHere) in

      let mkUpdateF = lam fs. lam st. lam specId. lam ist.
        foldl (lam ist. lam f. f specId ist) {ist with temporaryWeight = 0.0} fs in
      match newState st (mkUpdateF ist2.update) with (st, updateRef) in

      let st = mapSt (lam st. store st (PSubmodelRef {readSt = lam st. getState istRef (readSubState st)})) st in
      let update = lam specId. lam st.
        match getState x.id st with (changeId, value) in
        if eqi specId changeId then
          -- NOTE(vipa, 2025-09-30): The initial value has changed,
          -- i.e., we need to re-run the creation function
          let prevWeight = (getState stateRef st).permanentWeight in
          let ist = match ist with PVS ist in PVS {ist with specId = specId, initState = {ist.initState with above = State st}} in
          match f ist value (convPValHList st) with (PVS ist, pval) in
          let st = putState stateRef ist.initState st in
          let st = putState istRef ist.st st in
          let st = putState updateRef (mkUpdateF ist.update) st in
          let st = putState pvalRef pval st in
          let newWeight = ist.initState.permanentWeight in
          let st =
            { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
            , temporaryWeight = addf st.temporaryWeight ist.initState.temporaryWeight
            } in
          let value = switch pval
            case PVal x then x.value
            case PVar x then x.initValue
            end in
          putState xHere.id (specId, value) st
        else
          -- NOTE(vipa, 2025-09-30): The initial value is unchanged,
          -- i.e., we don't need to re-run the creation function, we
          -- just need to update the current sub-model
          let update = getState updateRef st in
          let innerState = getState stateRef st in
          let prevWeight = innerState.permanentWeight in
          let innerState = update st specId {innerState with above = State st} in
          let newWeight = innerState.permanentWeight in
          let st =
            { st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)
            , temporaryWeight = addf st.temporaryWeight innerState.temporaryWeight
            } in
          let st = putState stateRef innerState st in
          match getState pvalRef st with pval in
          match pval with PVar x then
            match getState x.id innerState with (changeId, value) in
            if eqi specId changeId
            then putState xHere.id (specId, value) st
            else st
          else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_select st f pval = | refs ->
    switch pval
    case PVal x then (st, f x.value refs)
    case PVar x then
      let initId = match st with PVS st in st.specId in
      let initVar = f x.initValue refs in
      let initValue = switch initVar
        case PVal x then x.value
        case PVar x then x.initValue
        end in
      match newNodeState st initValue with (st, xHere) in
      match newState st initVar with (st, varRef) in
      let update = lam specId. lam st.
        match getState x.id st with (changeId, value) in
        if eqi specId changeId then
          -- Value changed, pick new node
          let var = f value refs in
          let value = switch var
            case PVal x then x.value
            case PVar x then (getState x.id st).1
            end in
          let st = putState varRef var st in
          putState xHere.id (specId, value) st
        else
          -- Value did not change, check selected value
          match getState varRef st with PVar x then
            match getState x.id st with (changeId, value) in
            if eqi specId changeId then
              putState xHere.id (specId, value) st
            else st
          else st in
      (addUpdate update st, PVar xHere)
    end

  sem p_weight st store f =
  | PVal x ->
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight (f x.value)}) st in
    mapSt (lam st. store st (PWeightRef ())) st
  | PVar x ->
    let initWeight = f x.initValue in
    let st = withInitState (lam st. {st with permanentWeight = addf st.permanentWeight initWeight}) st in
    let st = mapSt (lam st. store st (PWeightRef ())) st in
    match newState st initWeight with (st, weightRef) in
    let update = lam specId. lam st.
      match getState x.id st with (changeId, value) in
      if eqi specId changeId then
        let prevWeight = getState weightRef st in
        let newWeight = f value in
        let st = {st with permanentWeight = addf st.permanentWeight (subf newWeight prevWeight)} in
        putState weightRef newWeight st
      else st in
    addUpdate update st

  sem p_assume st store = | dist ->
    let initDist = switch dist
      case PVal x then x.value
      case PVar x then x.initValue
      end in
    let fetchDist = switch dist
      case PVal x then let specId = match st with PVS st in st.specId in lam. (specId, x.value)
      case PVar x then getState x.id
      end in
    match newNodeState st (p_sample initDist) with (st, xHere) in
    match newState st (p_logObserve initDist xHere.initValue) with (st, weightRef) in
    match newState st (lam d. lam. d) with (st, driftRef) in
    let update = lam specId. lam st.
      match getState xHere.id st with (changeIdHere, prevValue) in
      match fetchDist st with (changeIdDist, dist) in
      let prevWeight = getState weightRef st in
      if eqi specId changeIdHere then
        -- Draw a new sample, i.e., value changes
        let drift = getState driftRef st in
        let kernel = drift dist prevValue in
        let proposal = p_sample kernel in
        let reverseKernel = drift dist proposal in

        let newWeight = p_logObserve dist proposal in

        let prevToProposalProb = p_logObserve kernel proposal in
        let proposalToPrevProb = p_logObserve reverseKernel prevValue in

        let st = putState xHere.id (specId, proposal) st in
        let st = putState weightRef newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight
          (addf
            (subf newWeight prevWeight)
            (subf proposalToPrevProb prevToProposalProb))}
      else if eqi specId changeIdDist then
        -- Reuse current sample, i.e., value doesn't change
        let newWeight = p_logObserve dist prevValue in
        let st = putState weightRef newWeight st in
        {st with temporaryWeight = addf st.temporaryWeight (subf newWeight prevWeight)}
      else st in
    match st with PVS {readSubState = readSubState, mapSubState = mapSubState} in
    let readValue = lam st. (getState xHere.id (readSubState st)).1 in
    let writeValue = lam val. mapSubState (putState xHere.id val) in
    let writeDrift = lam val. mapSubState (putState driftRef val) in
    let st = mapSt (lam st. store st (PAssumeRef {readValue = readValue, writeValue = writeValue, writeDrift = writeDrift})) st in
    (addUpdate update st, PVar xHere)
end

-- -- === Persistent PVal ===

-- -- This implementation identifies blocks of nodes that must be updated
-- -- together, finds references between such blocks, then figures out
-- -- what needs to be stored between update steps based on those
-- -- references. The current state of a model is a spine with one entry
-- -- per block, and each entry contains one value per value that *needs*
-- -- to be stored. Ephemeral values are never saved, they only live in a
-- -- temporary Map (which will hopefully disappear with partial
-- -- evaluation).

-- lang PersistentPVal = PValInterface
--   type NodeID = Int

--   -- We use `Arr` for the spine to be able to mutate it while we build
--   -- it, to not allocate excessive amounts of data as we compute each
--   -- block. In a "functional but in place" world we could replace it
--   -- with a linearly used persistent vector.
--   type Stored = Arr [Dyn]

--   type LocalState =
--     { localValues : Map NodeID Dyn
--     , temporaryWeight : Float
--     }

--   type PVarRecord a =
--     { id : NodeID
--     , block : Set NodeID
--     , propagated : Set NodeID
--     , initValue : a
--     }
--   syn PVal y a =
--   | PVar (PVarRecord a)
--   | PVal {value : a}

--   syn PExportRef a =
--   | PExportRef {block : Int, idx : Int}
--   | PExportVal {value : a}

--   syn PAssumeRef a =
--   | PAssumeRef {id : NodeID, block : Int, idx : Int}

--   syn PWeightRef =
--   | PWeightRef {} -- TODO(vipa, 2025-09-26): figure out what we want to be able to do here

--   type Block =
--     { idx : Int
--     -- There can be at most one assume per block, and it must be first
--     -- in that block. This function thus kicks off the computation
--     -- when present. The NodeID is used to look up the drift, which is
--     -- the first argument to the function. The function should redraw
--     -- the value iff the drift function is provided. Invariant: the
--     -- NodeID for the assume will be the highest id in the blocks set
--     -- of identifying ids.
--     , updateAssume : Option (NodeID, Option (PDist Dyn -> Dyn -> PDist Dyn) -> Stored -> LocalState)
--     , update : [Either (Stored -> LocalState -> ((NodeID, Bool), LocalState)) (Stored -> LocalState -> (LocalState))]
--     , calcPermanentWeight : [Stored -> LocalState -> Float]
--     , initWeight : Float
--     , requested : Map NodeID (Int, Dyn)
--     }
--   type PVSRecord st =
--     { nextId : NodeID
--     , blocks : Map (Set NodeID) Block
--     , permanentWeight : Float
--     , st : st
--     }
--   syn PValState st y = | PVS (PVSRecord st)

--   syn PValInstance complete st = | PVI
--     { stored : Stored
--     , blockWeights : [Float]
--     , permanentWeight : Float
--     , toDrift : Map NodeID (PDist Dyn -> Dyn -> PDist Dyn)
--     , evaluate : [Float] -> Stored -> Map NodeID (PDist Dyn -> Dyn -> PDist Dyn) -> (Float, PValInstance Complete st)
--     , st : st
--     }

--   sem getSt = | PVI x -> x.st

--   sem startStep = | PVI x -> PVI x

--   sem finalizeStep pred = | PVI x ->
--     match x.evaluate x.blockWeights x.stored x.toDrift with (temporaryWeight, next & PVI x2) in
--     let acceptProb = minf 0.0
--       (addf
--         (subf x.permanentWeight x2.permanentWeight)
--         temporaryWeight) in
--     if pred next acceptProb
--     then (true, next)
--     else (false, PVI {x with toDrift = mapEmpty subi})

--   sem resampleAssume drift ref = | PVI x ->
--     match ref with PAssumeRef ref in
--     let fixDriftType : (PDist a -> a -> PDist a) -> (PDist Dyn -> Dyn -> PDist Dyn) = unsafeCoerce in
--     PVI {x with toDrift = mapInsert ref.id (fixDriftType drift) x.toDrift}

--   sem readPreviousAssume ref = | PVI x ->
--     match ref with PAssumeRef ref in
--     readFromStored ref.block ref.idx x.stored

--   sem readPreviousExport ref = | PVI x ->
--     match ref with PExportRef ref in
--     readFromStored ref.block ref.idx x.stored

--   sem instantiate f = | st ->
--     let pvs = PVS
--       { nextId = 0
--       , blocks = mapEmpty setCmp
--       , permanentWeight = 0.0
--       , st = st
--       } in
--     match f pvs with PVS pvs in
--     printLn (seq2string (lam b. int2string (mapSize b.requested)) (mapValues pvs.blocks));
--     printLn (join ["num blocks: ", int2string (mapSize pvs.blocks)]);
--     let st = pvs.st in
--     let blocks = sort (lam a. lam b. subi a.1 .idx b.1 .idx) (mapBindings pvs.blocks) in
--     printLn (seq2string (lam x. int2string x.1 .idx) blocks);
--     let updateBlock
--       : (Set NodeID, Block)
--       -> Map NodeID (PDist Dyn -> Dyn -> PDist Dyn)
--       -> Float
--       -> Stored
--       -> {dirtyCache : Map NodeID Bool, temporaryWeight : Float}
--       -> ({dirtyCache : Map NodeID Bool, temporaryWeight : Float}, Float)
--       = lam pair.
--         match pair with (blockSet, block) in
--         let updateAssume : Stored -> Map NodeID (PDist Dyn -> Dyn -> PDist Dyn) -> LocalState =
--           match block.updateAssume with Some (id, updateAssume)
--           then lam st. lam toDrift. updateAssume (mapLookup id toDrift) st
--           else lam st. lam. {localValues = mapEmpty subi, temporaryWeight = 0.0} in
--         let updateNormal : Stored -> LocalState -> ([(NodeID, Bool)], LocalState) = foldl
--           (lam acc. lam f.
--             switch f
--             case Left f then lam st. lam ls.
--               match acc st ls with (caches, ls) in
--               match f st ls with (c, ls) in
--               (snoc caches c, ls)
--             case Right f then lam st. lam ls.
--               match acc st ls with (caches, ls) in
--               let ls = f st ls in
--               (caches, ls)
--             end)
--           (lam st. lam ls. ([], ls))
--           block.update in
--         let calcPermanentWeight : Stored -> LocalState -> Float = lam st. lam ls.
--           foldl (lam acc. lam f. addf acc (f st ls)) 0.0 block.calcPermanentWeight in
--         let storeRequests = map (lam x. x.0) (sort (lam a. lam b. subi a.1 .0 b.1 .0) (mapBindings block.requested)) in
--         let blockIdx = block.idx in
--         printLn (join ["block: ", int2string blockIdx, " , stored ids: ", seq2string int2string storeRequests]);
--         lam toDrift. lam prevWeight. lam st. lam acc.
--           match acc with {dirtyCache = dirtyCache, temporaryWeight = temporaryWeight} in
--           let shouldUpdate =
--             if mapAny (lam id. lam. setMem id blockSet) toDrift then true else
--             mapAny (lam id. lam dirty. if dirty then setMem id blockSet else false) dirtyCache in
--           if shouldUpdate then
--             let ls = updateAssume st toDrift in
--             match updateNormal st ls with (cacheUpdates, ls) in
--             let newWeight = calcPermanentWeight st ls in
--             printLn (join ["Updating block ", int2string blockIdx]);
--             dprintLn (arrGetExn st blockIdx);
--             arrSetExn st blockIdx (map (lam id. mapFindExn id ls.localValues) storeRequests);
--             dprintLn (arrGetExn st blockIdx);
--             let dirtyCache = foldl (lam acc. lam pair. mapInsert pair.0 pair.1 acc) dirtyCache cacheUpdates in
--             ({dirtyCache = dirtyCache, temporaryWeight = addf temporaryWeight ls.temporaryWeight}, newWeight)
--           else (acc, prevWeight)
--     in
--     let updateBlocks = mapi (lam i. lam b. (i, updateBlock b)) blocks in
--     let initPermanentWeight = pvs.permanentWeight in
--     recursive let evaluate = lam blockWeights. lam stored. lam toDrift.
--       let stored = arrSub stored 0 (arrLength stored) in
--       let step = lam acc. lam pair.
--         match pair with (i, updateBlock) in
--         updateBlock toDrift (get blockWeights i) stored acc in
--       match mapAccumL step {dirtyCache = mapEmpty subi, temporaryWeight = 0.0} updateBlocks
--         with ({temporaryWeight = temporaryWeight}, blockWeights) in
--       let pvi = PVI
--         { stored = stored
--         , blockWeights = blockWeights
--         , permanentWeight = foldl addf initPermanentWeight blockWeights
--         , toDrift = mapEmpty subi
--         , evaluate = evaluate
--         , st = st
--         } in
--       (temporaryWeight, pvi)
--     in
--     let initWeights = map (lam b. b.1 .initWeight) blocks in
--     PVI
--     { stored =
--       let st = arrCreate (length blocks)
--         (lam i.
--           let requested = mapValues (get blocks i).1 .requested in
--           let sorted = sort (lam a. lam b. subi a.0 b.0) requested in
--           let f = lam acc.
--             match acc with [a] ++ as
--             then Some (a.1, as)
--             else None () in
--           let final = unfoldr f sorted in
--           final) in
--       dprintLn st;
--       st
--     , blockWeights = initWeights
--     , permanentWeight = foldl addf initPermanentWeight initWeights
--     , toDrift = mapEmpty subi
--     , evaluate = evaluate
--     , st = st
--     }

--   sem withBlock : all st. all y. all a. (Block -> (Block, a)) -> Set NodeID -> PValState st y -> (PValState st y, a)
--   sem withBlock f blockSet = | PVS pvs ->
--     let mkNewBlock = lam.
--       { idx = mapSize pvs.blocks
--       , updateAssume = None ()
--       , update = []
--       , calcPermanentWeight = []
--       , requested = mapEmpty subi
--       , initWeight = 0.0
--       } in
--     let block = mapLookupOrElse mkNewBlock blockSet pvs.blocks in
--     match f block with (block, res) in
--     (PVS {pvs with blocks = mapInsert blockSet block pvs.blocks}, res)

--   sem newId : all st. all y. PValState st y -> (PValState st y, NodeID)
--   sem newId = | PVS pvs ->
--     (PVS {pvs with nextId = addi pvs.nextId 1}, pvs.nextId)

--   sem request : all st. all y. all a. PVarRecord a -> PValState st y -> (PValState st y, (Int, Int))
--   sem request x = | pvs ->
--     let f = lam block.
--       match
--         match mapLookup x.id block.requested with Some (idx, _) then (block, idx) else
--         let idx = mapSize block.requested in
--         ({block with requested = mapInsert x.id (idx, asDyn x.initValue) block.requested}, idx)
--       with (block, idx) in
--       (block, (block.idx, idx)) in
--     withBlock f x.block pvs

--   sem readFromStored : all a. Int -> Int -> Stored -> a
--   sem readFromStored block idx = | st ->
--     fromDyn (get (arrGetExn st block) idx)

--   sem mkFetcher : all st. all y. all a. Set NodeID -> PValState st y -> PVarRecord a -> (PValState st y, Stored -> LocalState -> a)
--   sem mkFetcher currentBlock pvs = | x ->
--     if setEq currentBlock x.block then
--       printLn "local fetch";
--       (pvs, lam. lam ls. fromDyn (mapLookupOrElse (lam. error "Failed local lookup") x.id ls.localValues))
--     else
--       match request x pvs with (pvs, (blockIdx, idx)) in
--       printLn (join ["global fetch ", int2string blockIdx, " ", int2string idx]);
--       (pvs, lam st. lam. readFromStored blockIdx idx st)

--   sem p_cache pvs eq =
--   | PVal x -> (pvs, PVal x)
--   | PVar x ->
--     let blockSet = x.propagated in
--     match newId pvs with (pvs, id) in
--     let xHere = {id = id, block = blockSet, propagated = setSingleton subi id, initValue = x.initValue} in
--     match request xHere pvs with (pvs, (block, idx)) in
--     match mkFetcher blockSet pvs x with (pvs, fetch) in
--     -- OPT(vipa, 2025-09-26): If x and xHere are in the same block
--     -- (which should be the common case, the only exception I can see
--     -- is if you cache a cached value or an assume) then we only need
--     -- to store one of them; we can fetch the new value from the
--     -- localValues map and the old value from Stored. However, note
--     -- that this is only relevant if x is already stored for some
--     -- reason, which might not be all that common?
--     let update = lam st. lam ls.
--       let new = fetch st ls in
--       let prev = readFromStored block idx st in
--       ((id, not (eq prev new)), {ls with localValues = mapInsert id (asDyn new) ls.localValues}) in
--     let f = lam block.
--       ({block with update = snoc block.update (Left update)}, PVar xHere) in
--     withBlock f blockSet pvs

--   sem p_export pvs store =
--   | PVal x ->
--     match pvs with PVS pvs in
--     PVS {pvs with st = store pvs.st (PExportVal x)}
--   | PVar x ->
--     match request x pvs with (pvs, (block, idx)) in
--     match pvs with PVS pvs in
--     PVS {pvs with st = store pvs.st (PExportRef {block = block, idx = idx})}

--   sem p_pure pvs = | v -> (pvs, PVal {value = v})

--   sem p_map pvs f =
--   | PVal x -> p_pure pvs (f x.value)
--   | PVar x ->
--     let blockSet = x.propagated in
--     printLn "p_map fetcher next";
--     match mkFetcher blockSet pvs x with (pvs, fetch) in
--     match newId pvs with (pvs, id) in
--     printLn (concat "p_map id " (int2string id));
--     let update = lam st. lam ls.
--       let a = fetch st ls in
--       {ls with localValues = mapInsert id (asDyn (f a)) ls.localValues} in
--     let initValue = f x.initValue in
--     dprintLn initValue;
--     let f = lam block.
--       ( {block with update = snoc block.update (Right update)}
--       , PVar {id = id, block = blockSet, propagated = blockSet, initValue = initValue}
--       ) in
--     withBlock f blockSet pvs

--   sem p_apply pvs f = | a ->
--     switch (f, a)
--     case (PVal f, PVal a) then p_pure pvs (f.value a.value)
--     case (PVal f, a) then p_map pvs f.value a
--     case (f, PVal a) then p_map pvs (lam f. f a.value) f
--     case (PVar f, PVar a) then
--       let blockSet = setUnion f.propagated a.propagated in
--       match mkFetcher blockSet pvs f with (pvs, fetchF) in
--       match mkFetcher blockSet pvs a with (pvs, fetchA) in
--       match newId pvs with (pvs, id) in
--       let update = lam st. lam ls.
--         let f = fetchF st ls in
--         let a = fetchA st ls in
--         {ls with localValues = mapInsert id (asDyn (f a)) ls.localValues} in
--       let f = lam block.
--         ( {block with update = snoc block.update (Right update)}
--         , PVar {id = id, block = blockSet, propagated = blockSet, initValue = f.initValue a.initValue}
--         ) in
--       withBlock f blockSet pvs
--     end

--   sem p_bind pvs store st2 f pval = | refs ->
--     error "TODO"

--   sem p_select pvs f pval = | refs ->
--     switch pval
--     case PVal x then (pvs, f x.value refs)
--     case PVar x then
--       let blockSet =
--         type St = Set NodeID in
--         let addPropagated : all a. St -> PVal y a -> St = lam acc. lam p.
--           match p with PVar x then setUnion acc x.propagated else acc in
--         foldPValHList #frozen"addPropagated" x.propagated refs in
--       match mkFetcher blockSet pvs x with (pvs, fetchA) in
--       match
--         type St = (PValState st y, Map NodeID Dyn) in
--         let f : all a. St -> PVal y a -> St = lam acc. lam p.
--           match p with PVar x then
--             match acc with (pvs, fetchers) in
--             match mkFetcher blockSet pvs x with (pvs, fetch) in
--             (pvs, mapInsert x.id (asDyn fetch) fetchers)
--           else acc in
--         foldPValHList #frozen"f" (pvs, mapEmpty subi) refs
--       with (pvs, fetchers) in
--       match newId pvs with (pvs, id) in
--       let asFetcher : Dyn -> Stored -> LocalState -> b = fromDyn in
--       let update = lam st. lam ls.
--         let a = fetchA st ls in
--         let result = switch f a refs
--           case PVal x then x.value
--           case PVar x then asFetcher (mapFindExn x.id fetchers) st ls
--           end in
--         {ls with localValues = mapInsert id (asDyn result) ls.localValues} in
--       let initValue = switch f x.initValue refs
--         case PVal x then x.value
--         case PVar x then x.initValue
--         end in
--       let f = lam block.
--         ( {block with update = snoc block.update (Right update)}
--         , PVar {id = id, block = blockSet, propagated = blockSet, initValue = initValue}
--         ) in
--       withBlock f blockSet pvs
--     end

--   sem p_weight pvs store f =
--   | PVal x ->
--     match pvs with PVS pvs in
--     PVS {pvs with permanentWeight = addf pvs.permanentWeight (f x.value)}
--   | PVar x ->
--     let blockSet = x.propagated in
--     match mkFetcher blockSet pvs x with (pvs, fetch) in
--     let calcWeight = lam st. lam ls.
--       let a = fetch st ls in
--       f a in
--     let f = lam block.
--       ( { block with calcPermanentWeight = snoc block.calcPermanentWeight calcWeight
--         , initWeight = addf block.initWeight (f x.initValue)
--         }
--       , ()
--       ) in
--     let ref = PWeightRef {} in
--     match pvs with PVS pvs in
--     (withBlock f blockSet (PVS {pvs with st = store pvs.st ref})).0

--   sem p_assume pvs store = | dist ->
--     match newId pvs with (pvs, id) in
--     match newId pvs with (pvs, weightId) in
--     let blockSet =
--       match dist with PVar x
--       then setInsert id x.propagated
--       else setSingleton subi id in
--     match
--       switch dist
--       case PVal x then ((pvs, lam. lam. x.value), x.value)
--       case PVar x then printLn (join ["var fetcher: ", int2string x.id]); (mkFetcher blockSet pvs x, x.initValue)
--       end
--     with ((pvs, fetchDist), initDist) in
--     let distId = match dist with PVar x then x.id else negi 1 in
--     let propagated = setSingleton subi id in
--     let xHere = {id = id, block = blockSet, propagated = propagated, initValue = p_sample initDist} in
--     let xWeight = {id = weightId, block = blockSet, propagated = propagated, initValue = p_logObserve initDist xHere.initValue} in
--     match request xHere pvs with (pvs, (hBlock, hIdx)) in
--     match request xWeight pvs with (pvs, (wBlock, wIdx)) in
--     let updateAssume = lam drift. lam st.
--       let ls =
--         { localValues = mapEmpty subi
--         , temporaryWeight = 0.0
--         } in
--       match
--         dprintLn st;
--         let prevWeight = readFromStored wBlock wIdx st in
--         let prevValue = readFromStored hBlock hIdx st in
--         let dist = fetchDist st ls in
--         -- printLn "pre-match on drift";
--         match drift with Some drift then
--           -- We got a drift function, i.e., we need to resample
--           let fixDriftType : (PDist Dyn -> Dyn -> PDist Dyn) -> (PDist a -> a -> PDist a) = unsafeCoerce in
--           let drift = fixDriftType drift in

--           -- printLn "pre-call of drift";
--           let kernel = drift dist prevValue in
--           -- printLn "post-call of drift";
--           let proposal = p_sample kernel in
--           -- printLn "post-call of p_sample";
--           let reverseKernel = drift dist proposal in

--           printLn (join ["pre-call of p_logObserve of dist, ", int2string distId]);
--           dprintLn dist;
--           let newWeight = p_logObserve dist proposal in
--           printLn "post-call of p_logObserve of dist";

--           let prevToProposalProb = p_logObserve kernel proposal in
--           printLn "post-call of p_logObserve of kernel";
--           let proposalToPrevProb = p_logObserve reverseKernel prevValue in
--           printLn "post-call of p_logObserve of reverseKernel";

--           (proposal, newWeight, addf (subf newWeight prevWeight) (subf proposalToPrevProb prevToProposalProb))
--         else
--           -- We did not get a drift function, just update the temporary weight
--           printLn "pre p_logObserve in else";
--           dprintLn dist;
--           dprintLn prevValue;
--           let newWeight = p_logObserve dist prevValue in
--           printLn "post p_logObserve in else";
--           (prevValue, newWeight, subf newWeight prevWeight)
--       with (result, newWeight, temporaryWeight) in
--       -- printLn "post match thing";
--       { ls with localValues =
--         let res = mapInsert id (asDyn result)
--           (mapInsert weightId (asDyn newWeight) ls.localValues) in
--         -- printLn "post-insert";
--         res
--       , temporaryWeight =
--         let res = addf ls.temporaryWeight temporaryWeight in
--         -- printLn "post-addf";
--         res
--       } in
--     let f = lam block.
--       ({block with updateAssume = Some (id, updateAssume)}, PVar xHere) in
--     let ref = PAssumeRef {id = id, block = hBlock, idx = hIdx} in
--     match pvs with PVS pvs in
--     withBlock f blockSet (PVS {pvs with st = store pvs.st ref})
-- end


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

-- lang RunBernAndPersistent = BernAnd + MCMCPVal + PersistentPVal
-- end

lang RunBernAndPersistent = BernAnd + MCMCPVal + SimplePersistentPVal
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
    use RunBernAndPersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent" (timeF run);
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

lang RunSimpleBindPersistent = SimpleBind + MCMCPVal + SimplePersistentPVal
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
    use RunSimpleBindPersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent" (timeF run);
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
    recursive let f : all z. Int -> PValState () z -> Unknown -> PValHList z Unknown -> (PValState () z, PVal z Unknown) = lam i. lam st. lam c. lam list.
      match list with PVHCons (dist, PVHNil ()) in
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

lang ManualGeometricCache = SimpleResample
  sem run = | st ->
    match p_pure st (p_bernoulli 0.5) with (st, dist) in
    match p_assume st simpleStore dist with (st, c) in
    match p_cache st eqb c with (st, c) in
    recursive let f : all z. Int -> PValState () z -> Unknown -> PValHList z Unknown -> (PValState () z, PVal z Unknown) = lam i. lam st. lam c. lam list.
      match list with PVHCons (dist, PVHNil ()) in
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

lang RunManualGeometricPersistent = ManualGeometric + MCMCPVal + SimplePersistentPVal
end

lang RunManualGeometricPersistent2 = ManualGeometric + MCMCPVal + SimplePersistentPVal2
end

lang RunManualGeometricCacheMut = ManualGeometricCache + MCMCPVal + MutPVal
end

lang RunManualGeometricCachePersistent = ManualGeometricCache + MCMCPVal + SimplePersistentPVal
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
    use RunManualGeometricPersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent" (timeF run);
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
    use RunManualGeometricCachePersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent cache" (timeF run);
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

lang RunCoinOneObservePersistent = CoinOneObserve + MCMCPVal + SimplePersistentPVal
end

lang RunCoinManyObservePersistent = CoinManyObserve + MCMCPVal + SimplePersistentPVal
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
    use RunCoinOneObservePersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent one observe" (timeF run);
  let run =
    use RunCoinManyObservePersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent many observe" (timeF run);
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

lang RunTreeInferenceTreeBindPersistent = TreeInferenceTreeBind + MCMCPVal + SimplePersistentPVal
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

lang RunTreeInferenceTreeSelectPersistent = TreeInferenceTreeSelect + MCMCPVal + SimplePersistentPVal
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
    use RunTreeInferenceTreeBindPersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam.
      let samples = (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
      samples in
  summarizePVal "pval mcmc Persistent tree bind" (timeF run);
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
    use RunTreeInferenceTreeSelectPersistent in
    let instance = instantiate #frozen"run" ([], ()) in
    lam. (mcmc {getSample = simpleRead, step = simpleResample globalProb, iterations = iterations} instance).samples in
  summarizePVal "pval mcmc Persistent tree select" (timeF run);
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
--   match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (1000000, lam r. lam. (subi r 1, neqi r 1)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (time, res) in
--   printLn (join [float2string time, "ms (MCMC lightweight)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res).0)));
--   match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 1)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeA, resA) in
--   printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
--   printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resA).0)));
--   match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 1)), globalProb = divf 1.0 (addf 1.0 (mulf 3.0 (int2float (length initTrees))))}) run) with (timeP, resP) in
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
