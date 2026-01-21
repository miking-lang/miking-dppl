include "pval-interface.mc"

lang MutPVal2 = PValInterface
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
    PVI {st = st, update = update, permanentWeight = initWeight, id = 0}

  sem getSt = | PVI x -> x.st

  sem startStep = | PVI x -> PVI {x with id = addi x.id 1}

  sem finalizeStep pred = | PVI x ->
    let st =
      { id = x.id
      , permanentWeight = ref 0.0
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

    let value = ref (deref pval.value) in
    let changeId = ref (deref pval.changeId) in
    let model = ref model in
    let st2 = ref st2 in
    let pval = ref pval in

    let update = lam st.
      if eqi st.id (deref a.changeId) then
        -- We've updated the argument, create a new sub-model
        let prevValue = deref value in
        let prevModel = deref model in
        let prevSt2 = deref st2 in
        let prevPVal = deref pval in

        match _initModel st.id initSt2 f with ((newPVal, newSt2), newWeight, newModel) in
        let newValue = deref newPVal.value in
        modref st.permanentWeight (addf (deref st.permanentWeight) newWeight);

        modref value newValue;
        modref changeId st.id;
        modref model newModel;
        modref st2 newSt2;
        modref pval newPVal;

        let reset = lam.
          modref value prevValue;
          modref model prevModel;
          modref st2 prevSt2;
          modref pval prevPVal in
        modref st.reset (snoc (deref st.reset) reset)
      else
        -- We've not updated the argument, update the sub-model
        (deref model) st;
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
      , initWeight = addf st.initWeight initWeight
      , updates = snoc st.updates update
      , initId = st.initId
      } in
    (PVS st, PVal {value = value, changeId = changeId})

  sem p_match st store ist a pick = | build ->
    match st with PVS st in
    match a with PVal a in
    let picked = ref (pick (deref a.value)) in
    let bVal : Ref b = ref (optionGetOr (unsafeCoerce 0) (deref picked)) in
    let bPVal = PVal {value = bVal, changeId = a.changeId} in

    let initSt = PVS {initId = st.initId, updates = [], initWeight = 0.0, st = ist} in
    match build initSt (optionMap (lam. bPVal) (deref picked))
      with (PVS {updates = initUpdates, initWeight = initWeight, st = ist}, pvalC & PVal c) in
    let value = ref (deref c.value) in
    let changeId = ref (deref c.changeId) in
    let pvalC = ref pvalC in
    let updateRef = ref (lam st. for_ initUpdates (lam up. up st)) in
    let istRef = ref ist in

    let update = lam st.
      let updateCurrentSubGraph = lam.
        (deref updateRef) st;
        match deref pvalC with PVal c in
        if eqi st.id (deref c.changeId) then
          let prevValue = deref value in
          modref value (deref c.value);
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
        else () in
      if eqi st.id (deref a.changeId) then
        -- Input has changed, see if we need to rebuild the subgraph
        let prevPicked = deref picked in
        let newPicked = pick (deref a.value) in
        modref picked newPicked;
        modref st.reset (snoc (deref st.reset) (lam. modref picked prevPicked));
        (match newPicked with Some newPicked then
          let prevB = deref bVal in
          modref bVal newPicked;
          modref st.reset (snoc (deref st.reset) (lam. modref bVal prevB))
         else ());
        if eqb (optionIsSome prevPicked) (optionIsSome newPicked) then
          -- We're using the same branch, reuse the sub-graph
          updateCurrentSubGraph ()
        else
          -- We're not using the same branch, rebuild the sub-graph
          let prevValue = deref value in
          let prevPValC = deref pvalC in
          let prevUpdate = deref updateRef in
          let prevIst = deref istRef in
          match build initSt (optionMap (lam. bPVal) newPicked)
            with (PVS {updates = newUpdates, initWeight = newWeight, st = newIst}, newPValC & PVal c) in
          modref value (deref c.value);
          modref changeId st.id;
          modref pvalC newPValC;
          modref updateRef (lam st. for_ newUpdates (lam up. up st));
          modref istRef newIst;
          modref st.permanentWeight (addf (deref st.permanentWeight) newWeight);
          let reset = lam.
            modref value prevValue;
            modref pvalC prevPValC;
            modref updateRef prevUpdate;
            modref istRef prevIst in
          modref st.reset (snoc (deref st.reset) reset)
      else
        -- Input has not changed, update the sub-graph
        updateCurrentSubGraph ()
    in
    let st =
      { st = store st.st (PSubmodelRef {readSt = lam. deref istRef})
      , initWeight = addf st.initWeight initWeight
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
        modref st.permanentWeight (addf (deref st.permanentWeight) newWeight);
        modref w newWeight;
        modref st.reset (snoc (deref st.reset) (lam. modref w prevWeight))
      else modref st.permanentWeight (addf (deref st.permanentWeight) (deref w)) in
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
