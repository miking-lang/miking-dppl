include "pval-interface.mc"

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
