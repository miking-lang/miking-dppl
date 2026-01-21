include "pval-interface.mc"
include "these.mc"

lang MutPVal3 = PValInterface
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
    , pretendFresh : Bool
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
      , pretendFresh = false
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
      , initWeight = initWeight
      , updates = snoc st.updates update
      , initId = st.initId
      } in
    (PVS st, PVal {value = value, changeId = changeId})

  sem p_match st store ist a pick = | build ->
    match st with PVS st in
    match a with PVal a in

    type BranchState input =
      { input : input
      , update : PState -> ()
      , output : PValRec c
      , ist : ist2
      } in

    let initSt = PVS {initId = st.initId, updates = [], initWeight = 0.0, st = ist} in
    let mkBranch : Option b -> {input : Option (PValRec b), update : PState -> (), initWeight : Float, output : PValRec c, ist : ist2} = lam input.
      let inputRec = optionMap (lam a. {value = ref a, changeId = ref st.initId}) input in
      match build initSt (optionMap (lam a. PVal a) inputRec)
        with (PVS {updates = initUpdates, initWeight = initWeight, st = ist}, PVal c) in
      { input = inputRec
      , update = lam st. for_ initUpdates (lam up. up st)
      , ist = ist
      , initWeight = initWeight
      , output = c
      } in

    let picked = ref (pick (deref a.value)) in
    let res = mkBranch (deref picked) in
    let initBranch = match res.input with Some input
      then This {update = res.update, ist = res.ist, output = res.output, input = input}
      else That {update = res.update, ist = res.ist, output = res.output} in
    let branches = ref initBranch in
    let value = ref (deref res.output.value) in
    let changeId = ref st.initId in
    let initWeight = res.initWeight in

    let update = lam st.
      let updateCurrentSubGraph = lam st. lam forceOutput.
        match
          switch (deref picked, deref branches)
          case (Some _, This thn | These (thn, _)) then (thn.update, thn.output)
          case (None _, That els | These (_, els)) then (els.update, els.output)
          end
        with (update, output) in
        update st;
        if if forceOutput then true else eqi st.id (deref output.changeId) then
          -- Sub-graph output has changed, update here
          let prevValue = deref value in
          modref value (deref output.value);
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
        else () in
      if eqi st.id (deref a.changeId) then
        -- Input has changed, propagate to sub-graph (creating it if
        -- necessary)
        let prevPicked = deref picked in
        modref picked (pick (deref a.value));
        modref st.reset (snoc (deref st.reset) (lam. modref picked prevPicked));
        let branchExists = if optionIsSome (deref picked)
          then theseHasThis (deref branches)
          else theseHasThat (deref branches) in
        if branchExists then
          -- NOTE(vipa, 2026-01-07): Branch exists, we just need to update it
          -- NOTE(vipa, 2026-01-07): We need to pretend we're creating
          -- the branch freshly if it wasn't active last iteration
          let branchChanged = not (eqb (optionIsSome prevPicked) (optionIsSome (deref picked))) in
          let st = if branchChanged
            then {st with pretendFresh = true}
            else st in
          (match deref picked with Some b then
            match theseGetThis (deref branches) with Some thn then
              let prevValue = deref thn.input.value in
              modref thn.input.value b;
              modref thn.input.changeId st.id;
              modref st.reset (snoc (deref st.reset) (lam. modref thn.input.value prevValue))
            else never
           else ());
          updateCurrentSubGraph st branchChanged
        else
          -- Branch does not exist, we need to create it.
          let prevValue = deref value in
          let res = mkBranch (deref picked) in
          modref st.permanentWeight (addf (deref st.permanentWeight) res.initWeight);
          modref value (deref res.output.value);
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
          let stored = switch (res.input, deref branches)
            case (Some input, That els) then These ({update = res.update, ist = res.ist, output = res.output, input = input}, els)
            case (None _, This thn) then These (thn, {update = res.update, ist = res.ist, output = res.output})
            end in
          modref branches stored
      else
        -- Input is unchanged, update current sub-graph
        updateCurrentSubGraph st false
    in
    let readSt = lam.
      match deref picked with Some _
      then optionMapOrElse (lam. never) (lam x. x.ist) (theseGetThis (deref branches))
      else optionMapOrElse (lam. never) (lam x. x.ist) (theseGetThat (deref branches)) in
    let st =
      { st = store st.st (PSubmodelRef {readSt = readSt})
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

  syn PChunkState x = | PCS {watched : Ref [Ref IterationID], permanentWeight : Ref Float}
  sem p_readPVal st = | PVal x ->
    match st with PCS st in
    modref st.watched (snoc (deref st.watched) x.changeId);
    deref x.value
  sem p_weightChunk st = | w ->
    match st with PCS st in
    modref st.permanentWeight (addf (deref st.permanentWeight) w)
  sem p_chunk st = | f ->
    match st with PVS st in

    let watched = ref [] in
    let localWeight = ref 0.0 in
    let ist = PCS {watched = watched, permanentWeight = localWeight} in
    let value = ref (f ist) in
    let changeId = ref st.initId in

    let update = lam st.
      if any (lam id. eqi st.id (deref id)) (deref watched) then
        let prevValue = deref value in
        let prevWatched = deref watched in
        let prevWeight = deref localWeight in
        modref watched [];
        modref localWeight 0.0;
        -- NOTE(vipa, 2026-01-08): This will affect watched and
        -- localWeight through side-effects, i.e., it must be after
        -- their resets
        modref value (f ist);
        modref changeId st.id;
        let reset = lam.
          modref watched prevWatched;
          modref localWeight prevWeight;
          modref value prevValue in
        modref st.permanentWeight (addf (deref st.permanentWeight) (deref localWeight));
        modref st.reset (snoc (deref st.reset) reset)
      else modref st.permanentWeight (addf (deref st.permanentWeight) (deref localWeight)) in

    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_subMap st store ist f = | PVal a ->
    match st with PVS st in

    let initSt = PVS {initId = st.initId, updates = [], initWeight = 0.0, st = ist} in
    match f initSt (deref a.value) with (PVS {updates = updates, initWeight = initWeight, st = ist2}, value) in
    let value = ref value in
    let changeId = ref st.initId in
    let ist2 = ref ist2 in
    let updates = ref (lam st. for_ updates (lam up. up st)) in

    let update = lam st.
      if eqi st.id (deref a.changeId) then
        -- Input changed, rebuild sub-model
        let prevValue = deref value in
        let prevIst2 = deref ist2 in
        let prevUpdates = deref updates in
        match f initSt (deref a.value) with (PVS {updates = newUpdates, initWeight = w, st = newIst2}, newValue) in
        modref st.permanentWeight (addf (deref st.permanentWeight) w);
        modref value newValue;
        modref changeId st.id;
        modref ist2 newIst2;
        modref updates (lam st. for_ newUpdates (lam up. up st));
        let reset = lam.
          modref value prevValue;
          modref ist2 prevIst2;
          modref updates prevUpdates in
        modref st.reset (snoc (deref st.reset) reset)
      else
        -- Input unchanged, just update sub-model
        (deref updates) st in

    let st =
      { st = store st.st (PSubmodelRef {readSt = lam. deref ist2})
      , initWeight = addf st.initWeight initWeight
      , updates = snoc st.updates update
      , initId = st.initId
      } in
    (PVS st, PVal {value = value, changeId = changeId})

  sem p_join st = | PVal a ->
    match st with PVS st in
    let value = ref (match deref a.value with PVal inner in deref inner.value) in
    let changeId = ref st.initId in
    let update = lam st.
      if eqi st.id (deref a.changeId) then
        -- Outer value has changed
        let prevValue = deref value in
        modref value (match deref a.value with PVal inner in deref inner.value);
        modref changeId st.id;
        modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
      else
        match deref a.value with PVal inner in
        if eqi st.id (deref inner.changeId) then
          -- Inner value has changed
          let prevValue = deref value in
          modref value (deref inner.value);
          modref changeId st.id;
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue))
        else () in
    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

  sem p_incremental st build update = | inputs ->
    match st with PVS st in

    let cachedInputs = map (lam x. match x with PVal x in ref (deref x.value)) inputs in
    let value = ref (build (map (lam x. match x with PVal x in deref x.value) inputs)) in
    let changeId = ref st.initId in

    let update = lam st.
      let mkUpdate = lam value. lam x.
        match x with PVal x in
        if eqi st.id (deref x.changeId) then
          let prevValue = deref value in
          modref value (deref x.value);
          modref st.reset (snoc (deref st.reset) (lam. modref value prevValue));
          Some {prev = prevValue, new = deref value}
        else None () in
      let updates = filterOption (zipWith mkUpdate cachedInputs inputs) in
      if null updates then () else
      let prevValue = deref value in
      modref value (update updates (deref value));
      modref changeId st.id;
      modref st.reset (snoc (deref st.reset) (lam. modref value prevValue)) in

    (PVS {st with updates = snoc st.updates update}, PVal {value = value, changeId = changeId})

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
      if st.pretendFresh then
        -- This sub-model wasn't active last step, meaning we need to
        -- act as though we're creating this node freshly, even though
        -- we're not.
        -- OPT(vipa, 2026-01-07): We probably don't need to set up
        -- reset properly here, but I'll do it anyway for now
        let prevValue = deref value in
        let prevWeight = deref w in
        modref value (p_sample (deref dist.value));
        modref w (p_logObserve (deref dist.value) (deref value));
        modref changeId st.id;
        let reset = lam.
          modref value prevValue;
          modref w prevWeight in
        modref st.reset (snoc (deref st.reset) reset)
      else if eqi st.id (deref changeId) then
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
