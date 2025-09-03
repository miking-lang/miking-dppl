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

type Node a =
  { value : Ref a
  , selfWeight : Ref Float
  -- We are dirty iff we are in this iteration
  , speculativeId : Ref IterationID
  -- We should return the cached value as changed in this iteration
  , lastChange : Ref IterationID
  -- Each function will dirty one listener for
  -- the given iteration
  , listeners : Ref (Map Symbol (IterationID -> ()))
  -- Identifying symbol, unique amongst all nodes
  , symbol : Symbol
  }

type PState =
  { specId : IterationID
  , reset : Ref [() -> ()]
  , permanentWeight : Ref Float
  , temporaryWeight : Ref Float
  }

let _mkNode : all a. Float -> a -> Node a
  = lam selfWeight. lam value.
    { value = ref value
    , selfWeight = ref selfWeight
    , speculativeId = ref 0
    , lastChange = ref 0
    , listeners = ref (mapEmpty _cmpSym)
    , symbol = gensym ()
    }
let _dirtyNode : all a. Node a -> IterationID -> ()
  = lam n. lam id.
    modref n.speculativeId id;
    mapFoldWithKey (lam. lam. lam mkDirty. mkDirty id) () (deref n.listeners)
let _listenNodeMaybeRecursive : all a. PState -> Symbol -> (IterationID -> ()) -> Node a -> Bool
  = lam st. lam sym. lam mkDirty. lam n.
    let prevEmpty = mapIsEmpty (deref n.listeners) in
    modref n.listeners (mapInsert sym mkDirty (deref n.listeners));
    if prevEmpty then
      modref st.permanentWeight (addf (deref st.permanentWeight) (deref n.selfWeight));
      true
    else false
let _maybeFreeNode : all a. PState -> Symbol -> Node a -> Bool
  = lam st. lam sym. lam n.
    let prevEmpty = mapIsEmpty (deref n.listeners) in
    modref n.listeners (mapRemove sym (deref n.listeners));
    if not prevEmpty
    then if mapIsEmpty (deref n.listeners)
      then modref st.permanentWeight (subf (deref st.permanentWeight) (deref n.selfWeight)); true
      else false
    else false

type PVal a
con PPure : all a.
  Node a -> PVal a
type PMapRec a b =
  { f : a -> (Float, b)
  , original : PVal a
  , node : Node b
  }
con PMap : all a. all b. PMapRec a b -> PVal b
type PApplyRec a b c =
  { f : a -> b -> (Float, c)
  , a : PVal a
  , b : PVal b
  , node : Node c
  }
con PApply : all a. all b. all c. PApplyRec a b c -> PVal c
type PBindRec a b =
  { f : a -> PVal b
  , original : PVal a
  , current : Ref (PVal b)
  , node : Node b
  }
con PBind : all a. all b. PBindRec a b -> PVal b
con PAssume : all a.
  { dist : PVal (PDist a)
  , oldWeight : Ref Float
  , toDrift : Ref (Option (a -> PDist a))
  , node : Node a
  } -> PVal a
con PCache : all a.
  { eq : a -> a -> Bool
  , original : PVal a
  , node : Node a
  } -> PVal a

type Change a =
  { value : a
  , changed : Bool
  }

let _cachedChange : all a. PState -> Node a -> Change a
  = lam st. lam n.
    { value = deref n.value
    , changed = eqi st.specId (deref n.lastChange)
    }

let c_map
  : all a. all b. PState -> Node b -> (a -> (Float, b)) -> Change a -> Change b
  = lam st. lam node. lam f. lam c.
    if c.changed then
      let prevV = deref node.value in
      let prevSW = deref node.selfWeight in
      let reset = lam.
        modref node.value prevV;
        modref node.selfWeight prevSW in
      let res = f c.value in
      modref node.selfWeight res.0;
      modref node.value res.1;
      modref node.lastChange st.specId;
      modref st.reset (snoc (deref st.reset) reset);
      modref st.permanentWeight (addf (subf (deref st.permanentWeight) prevSW) res.0);
      { value = res.1
      , changed = true
      }
    else
      { value = deref node.value
      , changed = false
      }
let c_apply
  : all a. all b. all c. PState -> Node c -> (a -> b -> (Float, c)) -> Change a -> Change b -> Change c
  = lam st. lam node. lam f. lam ca. lam cb.
    if or ca.changed cb.changed then
      let prevV = deref node.value in
      let prevSW = deref node.selfWeight in
      let reset = lam.
        modref node.value prevV;
        modref node.selfWeight prevSW in
      let res = f ca.value cb.value in
      modref node.selfWeight res.0;
      modref node.value res.1;
      modref node.lastChange st.specId;
      modref st.reset (snoc (deref st.reset) reset);
      modref st.permanentWeight (addf (subf (deref st.permanentWeight) prevSW) res.0);
      { value = res.1
      , changed = true
      }
    else
      { value = deref node.value
      , changed = false
      }

let _getNode : all a. PVal a -> Node a
  = lam p. switch p
    case PPure x then x
    case PMap x then x.node
    case PApply x then x.node
    case PBind x then x.node
    case PAssume x then x.node
    case PCache x then x.node
    end

recursive
  let _recalcPermWeightW : all a. (Set Symbol, Float) -> PVal a -> (Set Symbol, Float)
    = lam acc. lam p.
      match acc with (seen, w) in
      let n = _getNode p in
      if setMem n.symbol seen then acc else
      let acc = (setInsert n.symbol seen, addf w (deref n.selfWeight)) in
      switch p
      case PPure _ then acc
      case PMap x then _recalcPermWeightW acc x.original
      case PApply x then _recalcPermWeightW (_recalcPermWeightW acc x.a) x.b
      case PBind x then _recalcPermWeightW (_recalcPermWeightW acc x.original) (deref x.current)
      case PAssume x then _recalcPermWeightW acc x.dist
      case PCache x then _recalcPermWeightW acc x.original
      end
end
let _recalcPermWeight : all a. PVal a -> Float
  = lam p. (_recalcPermWeightW (setEmpty _cmpSym, 0.0) p).1

recursive
  let _listen : all a. PState -> Symbol -> (IterationID -> ()) -> PVal a -> ()
    = lam st. lam sym. lam mkDirty. lam p.
      if _listenNodeMaybeRecursive st sym mkDirty (_getNode p) then
        switch p
        case PPure _ then ()
        case PMap x then
          _listen st x.node.symbol (_dirtyNode x.node) x.original
        case PApply x then
          _listen st x.node.symbol (_dirtyNode x.node) x.a;
          _listen st x.node.symbol (_dirtyNode x.node) x.b
        case PBind x then
          _listen st x.node.symbol (_dirtyNode x.node) x.original;
          _listen st x.node.symbol (_dirtyNode x.node) (deref x.current)
        case PAssume x then
          _listen st x.node.symbol (_dirtyNode x.node) x.dist
        case PCache x then
          _listen st x.node.symbol (_dirtyNode x.node) x.original
        end
      else ()
end
recursive
  let _unlisten : all a. PState -> Symbol -> PVal a -> ()
    = lam st. lam sym. lam p.
      if _maybeFreeNode st sym (_getNode p) then
        switch p
        case PPure _ then ()
        case PMap x then
          _unlisten st x.node.symbol x.original
        case PApply x then
          _unlisten st x.node.symbol x.a;
          _unlisten st x.node.symbol x.b
        case PBind x then
          _unlisten st x.node.symbol x.original;
          _unlisten st x.node.symbol (deref x.current)
        case PAssume x then
          _unlisten st x.node.symbol x.dist
        case PCache x then
          _unlisten st x.node.symbol x.original
        end
      else ()
end


-- 1: print acceptance rate
-- 5: print when assumes are resampled/reused with different distributions
-- 10: print when nodes are looked at as dirty
let debug = ref 0

let _shallowReadPVal : all a. PVal a -> a
  = lam p. deref (_getNode p).value
recursive
  let _readPVal : all a. PState -> PVal a -> Change a
    = lam st. lam p. switch p
      case PPure x then
        {value = deref x.value, changed = false}
      case PMap x then
        let x : PMapRec Never a = x in  -- NOTE(vipa, 2025-08-29): Cheat GADTs
        if geqi (deref x.node.speculativeId) st.specId then
          (if geqi (deref debug) 10 then
            printLn (concat "dirty map: " (int2string (sym2hash x.node.symbol)))
           else ());
          modref x.node.speculativeId 0;
          c_map st x.node x.f (_readPVal st x.original)
        else _cachedChange st x.node
      case PApply x then
        let x : PApplyRec Never Never2 a = x in  -- NOTE(vipa, 2025-08-29): Cheat GADTs
        if geqi (deref x.node.speculativeId) st.specId then
          (if geqi (deref debug) 10 then
            printLn (concat "dirty apply: " (int2string (sym2hash x.node.symbol)))
           else ());
          modref x.node.speculativeId 0;
          c_apply st x.node x.f (_readPVal st x.a) (_readPVal st x.b)
        else _cachedChange st x.node
      case PBind x then
        let x : PBindRec Never a = x in  -- NOTE(vipa, 2025-08-29): Cheat GADTs
        if geqi (deref x.node.speculativeId) st.specId then
          (if geqi (deref debug) 10 then
            printLn (concat "dirty bind: " (int2string (sym2hash x.node.symbol)))
           else ());
          modref x.node.speculativeId 0;
          let c = _readPVal st x.original in
          if c.changed then
            let prevCurrent = deref x.current in
            let prevValue = deref x.node.value in
            let current = x.f c.value in
            let value = _shallowReadPVal current in
            _listen st x.node.symbol (_dirtyNode x.node) current;
            _unlisten st x.node.symbol prevCurrent;
            modref x.current current;
            modref x.node.value value;
            modref x.node.lastChange st.specId;
            let reset = lam.
              _listen st x.node.symbol (_dirtyNode x.node) prevCurrent;
              _unlisten st x.node.symbol current;
              modref x.current prevCurrent;
              modref x.node.value prevValue in
            modref st.reset (snoc (deref st.reset) reset);
            {value = value, changed = true}
          else
            c_map st x.node (lam x. (0.0, x)) (_readPVal st (deref x.current))
        else _cachedChange st x.node
      case PAssume x then
        if geqi (deref x.node.speculativeId) st.specId then
          modref x.node.speculativeId 0;
          (if geqi (deref debug) 10 then
            printLn (join ["dirty assume: ", int2string (sym2hash x.node.symbol)])
           else ());
          let c = _readPVal st x.dist in
          switch (c.changed, deref x.toDrift)
          case (true, None _) then
            -- NOTE(vipa, 2025-08-27): Reuse sample, thus accrue
            -- temporary weight and return unchanged
            let prevWeight = deref x.oldWeight in
            let newWeight = p_logObserve c.value (deref x.node.value) in
            modref x.oldWeight newWeight;
            modref st.reset (snoc (deref st.reset) (lam. modref x.oldWeight prevWeight));
            modref st.temporaryWeight (addf (deref st.temporaryWeight) (subf newWeight prevWeight));
            (if geqi (deref debug) 5 then
              printLn "reused assume:";
              dprint (deref x.node.value)
             else ());
            _cachedChange st x.node
          case (_, Some drift) then
            let prevValue = deref x.node.value in
            let prevWeight = deref x.oldWeight in

            let kernel = drift prevValue in
            let proposal = p_sample kernel in
            let reverseKernel = drift proposal in

            let newWeight = p_logObserve c.value proposal in

            let prevToProposalProb = p_logObserve kernel proposal in
            let proposalToPrevProb = p_logObserve reverseKernel prevValue in

            (if geqi (deref debug) 5 then
              printLn "resampled assume:";
              dprint prevValue;
              dprint proposal
             else ());

            modref x.node.value proposal;
            modref x.node.lastChange st.specId;
            modref x.oldWeight newWeight;
            modref st.temporaryWeight (addf (deref st.temporaryWeight) (subf newWeight prevWeight));
            modref st.reset (snoc (deref st.reset) (lam. modref x.node.value prevValue; modref x.oldWeight prevWeight));
            {value = proposal, changed = true}
          case (false, None _) then _cachedChange st x.node
          end
        else _cachedChange st x.node
      case PCache x then
        if geqi (deref x.node.speculativeId) st.specId then
          (if geqi (deref debug) 10 then
            printLn (concat "dirty cache: " (int2string (sym2hash x.node.symbol)))
           else ());
          modref x.node.speculativeId 0;
          let c = _readPVal st x.original in
          let prevValue = deref x.node.value in
          if c.changed then
            if x.eq prevValue c.value then _cachedChange st x.node else
            modref x.node.value c.value;
            modref x.node.lastChange st.specId;
            modref st.reset (snoc (deref st.reset) (lam. modref x.node.value prevValue));
            {value = c.value, changed = true}
          else _cachedChange st x.node
        else _cachedChange st x.node
      end
end


-- === Public interface for writing models ===

-- Pointed. Produce a "probabilistic" value that never changes. The
-- Float is (log) weight accumulated through `weight` and `observe`.
let p_pure : all a. Float -> a -> PVal a
  = lam selfWeight. lam value.
    PPure (_mkNode selfWeight value)
-- Functor. Map over a probabilistic value. The Float is (log) weight
-- accumulated through `weight` and `observe`.
let p_mapWeight : all a. all b. (a -> (Float, b)) -> PVal a -> PVal b
  = lam f. lam original.
    let res = f (_shallowReadPVal original) in
    let node = _mkNode res.0 res.1 in
    PMap {f = f, original = original, node = node}
let p_map : all a. all b. (a -> b) -> PVal a -> PVal b
  = lam f. lam original.
    p_mapWeight (lam x. (0.0, f x)) original
-- Applicative functor. Combine two probabilistic values. The Float is
-- (log) weight accumulated through `weight` and `observe`.
let p_applyWeight : all a. all b. all c. (a -> b -> (Float, c)) -> PVal a -> PVal b -> PVal c
  = lam f. lam a. lam b.
    let res = f (_shallowReadPVal a) (_shallowReadPVal b) in
    let node = _mkNode res.0 res.1 in
    PApply {f = f, a = a, b = b, node = node}
let p_apply : all a. all b. all c. (a -> b -> c) -> PVal a -> PVal b -> PVal c
  = lam f. lam a. lam b.
    p_applyWeight (lam a. lam b. (0.0, f a b)) a b
let p_apWeight : all a. all b. PVal (a -> (Float, b)) -> PVal a -> PVal b
  = lam f. lam a.
    p_applyWeight (lam f. lam a. f a) f a
let p_ap : all a. all b. PVal (a -> b) -> PVal a -> PVal b
  = lam f. lam a.
    p_apply (lam f. lam a. f a) f a

-- Monad. When the parameter changes we discard the old graph and
-- create a new one. Can be used to express programs that terminate
-- probabilistically, but should probably be avoided otherwise.
let p_bind : all a. all b. (a -> PVal b) -> PVal a -> PVal b
  = lam f. lam original.
    let current = f (_shallowReadPVal original) in
    let node = _mkNode 0.0 (_shallowReadPVal current) in
    PBind {f = f, original = original, current = ref current, node = node}

-- Sampling of values. Also produces a function that can be called to
-- resample this particular assume.
let p_assume : all a. PVal (PDist a) -> ((a -> PDist a) -> IterationID -> (), PVal a)
  = lam dist.
    let distV = _shallowReadPVal dist in
    let value = p_sample distV in
    let w = p_logObserve distV value in
    let node = _mkNode 0.0 value in
    let toDrift = ref (None ()) in
    let doResample = lam drift. lam specId.
      _dirtyNode node specId;
      modref toDrift (Some drift) in
    (doResample, PAssume {dist = dist, oldWeight = ref w, toDrift = toDrift, node = node})

-- Don't recompute dependent values if a newly produced value is equal
-- to the previously produced value.
let p_cache : all a. (a -> a -> Bool) -> PVal a -> PVal a
  = lam eq. lam original.
    let value = _shallowReadPVal original in
    let node = _mkNode 0.0 value in
    PCache {eq = eq, original = original, node = node}

-- === MCMC inference ===

let p_init : all a. PVal a -> {specId : IterationID, permanentWeight : Float}
  = lam p.
    let st =
      { specId = 1
      , reset = ref []
      , permanentWeight = ref 0.0
      , temporaryWeight = ref 0.0
      } in
    _listen st (gensym ()) (lam. ()) p;
    { specId = 0
    , permanentWeight = deref st.permanentWeight
    }

let p_step
  : all a. {specId : IterationID, permanentWeight : Float}
  -> (IterationID -> ())
  -> PVal a
  -> ({specId : IterationID, permanentWeight : Float}, Bool)
  = lam prevSt. lam triggerResample. lam p.
    let st =
      { specId = addi prevSt.specId 1
      , reset = ref []
      , permanentWeight = ref prevSt.permanentWeight
      , temporaryWeight = ref 0.0
      } in
    triggerResample st.specId;
    let c = _readPVal st p in
    let acceptProb = minf 0.0
      (addf
        (subf (deref st.permanentWeight) prevSt.permanentWeight)
        (deref st.temporaryWeight)) in
    if bernoulliSample (exp acceptProb) then
      ({specId = st.specId, permanentWeight = deref st.permanentWeight}, true)
    else
      for_ (deref st.reset) (lam f. f ());
      ({prevSt with specId = st.specId}, false)

let p_runInference : all a. (IterationID -> ()) -> PVal a -> Int -> [a]
  = lam triggerResample. lam p. lam totalIterations.
    recursive let work = lam st. lam accepts. lam list. lam iterations.
      if eqi iterations 0 then
        (if geqi (deref debug) 1 then
          printLn (join ["Accept rate: ", float2string (divf (int2float accepts) (int2float totalIterations))])
         else ());
        list
      else
      match p_step st triggerResample p with (st, accepted) in
      work st (if accepted then addi accepts 1 else accepts) (snoc list (_shallowReadPVal p)) (subi iterations 1)
    in work (p_init p) 0 [_shallowReadPVal p] totalIterations


-- === Debugging helpers ===

recursive
  let _pvalGraph : all a. (Set Symbol, [JsonValue]) -> PVal a -> ((Set Symbol, [JsonValue]), Symbol)
    = lam acc. lam p.
      let n = _getNode p in
      if setMem n.symbol acc.0 then (acc, n.symbol) else
      match
        switch p
        case PPure _ then (acc, "pure", "static", [])
        case PMap x then
          match _pvalGraph acc x.original with (acc, original) in
          ( acc
          , "map"
          , "static"
          , [original]
          )
        case PApply x then
          match _pvalGraph acc x.a with (acc, a) in
          match _pvalGraph acc x.b with (acc, b) in
          ( acc
          , "apply"
          , "static"
          , [a, b]
          )
        case PBind x then
          match _pvalGraph acc x.original with (acc, original) in
          match _pvalGraph acc (deref x.current) with (acc, current) in
          ( acc
          , "bind"
          , "dynamic"
          , [original, current]
          )
        case PAssume x then
          match _pvalGraph acc x.dist with (acc, dist) in
          ( acc
          , "assume"
          , "static"
          , [dist]
          )
        case PCache x then
          match _pvalGraph acc x.original with (acc, original) in
          ( acc
          , "cache"
          , "static"
          , [original]
          )
        end
      with ((seen, nodes), kind, shape, edges) in
      let here = JsonObject (mapFromSeq cmpString
        [ ("id", JsonInt (sym2hash n.symbol))
        , ("label", JsonString kind)
        , ("kind", JsonString kind)
        , ("shape", JsonString shape)
        , ("to", JsonArray (map (lam s. JsonInt (sym2hash s)) edges))
        ]) in
      ((setInsert n.symbol seen, snoc nodes here), n.symbol)
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

mexpr


-- === Smaller test models ===

let bern_and =
  let run = lam.
    let a = assume (Bernoulli 0.5) in
    let b = assume (Bernoulli 0.5) in
    and a b in
  let p_run =
    match p_assume (p_pure 0.0 (p_bernoulli 0.5)) with (driftA, a) in
    match p_assume (p_pure 0.0 (p_bernoulli 0.5)) with (driftB, b) in
    let res = p_apply and a b in
    let drifts =
      [ driftA (lam. p_bernoulli 0.5)
      , driftB (lam. p_bernoulli 0.5)
      , lam id. driftA (lam. p_bernoulli 0.5) id; driftB (lam. p_bernoulli 0.5) id
      ] in
    (lam id. _chooseUniform drifts id, res) in
  match timeF (lam. p_runInference p_run.0 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n bern_and";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
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
    match p_assume (p_pure 0.0 (p_bernoulli 0.5)) with (driftC, c) in
    let f = lam c.
      if c
      then (p_assume (p_pure 0.0 (p_bernoulli 0.9))).1
      else (p_assume (p_pure 0.0 (p_bernoulli 0.5))).1 in
    (driftC (lam. p_bernoulli 0.5), p_bind f c) in
  match timeF (lam. p_runInference p_run.0 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n simple_bind";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
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
    recursive let work = lam acc.
      let c = (p_assume (p_pure 0.0 (p_bernoulli 0.5))).1 in
      let cont = lam c.
        if c
        then work (addi acc 1)
        else p_pure 0.0 acc in
      p_bind cont c in
    match p_assume (p_pure 0.0 (p_bernoulli 0.5)) with (drift, res) in
    let f = lam c.
      if c
      then work 1
      else p_pure 0.0 0 in
    (drift (lam. p_bernoulli 0.5), p_bind f res) in
  match timeF (lam. p_runInference p_run.0 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n manual_geometric";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string int2string (histogram subi p_res));
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

let manual_geometric_cache =
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
    recursive let work = lam acc.
      let c = (p_assume (p_pure 0.0 (p_bernoulli 0.5))).1 in
      let cont = lam c.
        if c
        then work (addi acc 1)
        else p_pure 0.0 acc in
      p_bind cont c in
    match p_assume (p_pure 0.0 (p_bernoulli 0.5)) with (drift, res) in
    let f = lam c.
      if c
      then work 1
      else p_pure 0.0 0 in
    (drift (lam. p_bernoulli 0.5), p_bind f (p_cache eqb res)) in
  match timeF (lam. p_runInference p_run.0 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n manual_geometric_cache";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string int2string (histogram subi p_res));
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

-- modref debug 1;

let coin =
  let observations = [true, true, true, false, true, true, false, true] in
  let run = lam.
    let p = assume (Beta 1.0 1.0) in
    for_ observations (lam o. observe o (Bernoulli p));
    p in
  let p_run =
    match p_assume (p_pure 0.0 (p_beta 1.0 1.0)) with (drift, p) in
    let f = lam p.
      let weights = map (p_logObserve (p_bernoulli p)) observations in
      let sum = foldl addf 0.0 weights in
      (sum, p) in
    (drift (lam. p_beta 1.0 1.0), p_mapWeight f p) in
  match timeF (lam. p_runInference p_run.0 p_run.1 100000) with (p_time, p_res) in
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (100000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  match timeF (lam. infer (Importance {particles = 100000}) run) with (time2, res2) in
  printLn "\n coin";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
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

let tree_inference_chunky =
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

  let p_run =
    let pickpair = lam resamples. lam n.
      match p_assume (p_pure 0.0 (p_uniformDiscrete 0 (subi n 1))) with (resA, a) in
      match p_assume (p_pure 0.0 (p_uniformDiscrete 0 (subi n 2))) with (resB, b) in
      let f = lam i. lam j.
        if lti j i then (j,i) else (i, addi j 1) in
      ( concat resamples
        [ resA (lam. p_uniformDiscrete 0 (subi n 1))
        , resB (lam. p_uniformDiscrete 0 (subi n 2))
        ]
      , p_apply f a b
      ) in

     match p_assume (p_pure 0.0 (p_gaussian 0.0 10.0)) with (resRoot, rootValue) in
     let resRoot = resRoot (lam. p_gaussian 0.0 10.0) in
     let deviateFromDist = lam x. p_gaussian x 10.0 in
     let rootDist = p_map deviateFromDist rootValue in

     recursive let cluster = lam resamples. lam nTrees. lam trees.
       if eqi nTrees 1 then (resamples, p_map head trees) else
       match pickpair resamples nTrees with (resamples, pair) in
       match p_assume rootDist with (resHere, here) in
       let resamples = snoc resamples (resHere (lam. _shallowReadPVal rootDist)) in
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
       cluster resamples (subi nTrees 1) (p_apWeight (p_ap (p_apply f rootDist here) pair) trees) in

     let f = lam rootDist.
       ( foldl addf 0.0
         (map (lam t. p_logObserve rootDist (getX t)) initTrees)
       , initTrees
       ) in
     match cluster [resRoot] (length initTrees) (p_cache (lam. lam. true) (p_mapWeight f rootDist))
     with (resamples, tree) in
     ( lam id. _chooseUniform
       (snoc resamples
         (lam id. for_ resamples (lam f. f id)))
       id
     , tree
     ) in

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

  printLn "\n tree_inference_chunky";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  match timeF (lam. p_runInference p_run.0 p_run.1 1000000) with (p_time, p_res) in
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
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

let tree_inference_dag =
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

  let p_run =
    let pickpair = lam resamples. lam n.
      match p_assume (p_pure 0.0 (p_uniformDiscrete 0 (subi n 1))) with (resA, a) in
      match p_assume (p_pure 0.0 (p_uniformDiscrete 0 (subi n 2))) with (resB, b) in
      let f = lam i. lam j.
        if lti j i then (j,i) else (i, addi j 1) in
      ( concat resamples
        [ resA (lam. p_uniformDiscrete 0 (subi n 1))
        , resB (lam. p_uniformDiscrete 0 (subi n 2))
        ]
      , p_cache (lam a. lam b. and (eqi a.0 b.0) (eqi a.1 b.1)) (p_apply f a b)
      ) in

     match p_assume (p_pure 0.0 (p_gaussian 0.0 10.0)) with (resRoot, rootValue) in
     let resRoot = resRoot (lam. p_gaussian 0.0 10.0) in
     let deviateFromDist = lam x. p_gaussian x 10.0 in
     let rootDist = p_map deviateFromDist rootValue in

     recursive let cluster = lam resamples. lam nTrees. lam trees.
       if eqi nTrees 1 then (resamples, p_map head trees) else
       match pickpair resamples nTrees with (resamples, pair) in
       let i = p_map (lam p. p.0) pair in
       let j = p_map (lam p. p.1) pair in
       match p_assume rootDist with (resHere, here) in
       let resamples = snoc resamples (resHere deviateFromDist) in
       let fetchTree = lam idx. lam rootDist. lam here. lam trees.
         let t = get trees idx in
         ( addf
           (negf (p_logObserve rootDist (getX t)))
           (p_logObserve (deviateFromDist here) (getX t))
         , t
         ) in
       let l = p_apWeight (p_ap (p_apply fetchTree i rootDist) here) trees in
       let r = p_apWeight (p_ap (p_apply fetchTree j rootDist) here) trees in
       let trees =
         let f = lam p. lam trees. mapOption (lam x. x) (mapi (lam idx. lam v. if or (eqi idx p.0) (eqi idx p.1) then None () else Some v) trees) in
         p_apply f pair trees in
       let addMerged = lam l. lam r. lam here. lam trees.
         snoc trees (mkNode here l r) in
       let trees = p_ap (p_ap (p_apply addMerged l r) here) trees in
       cluster resamples (subi nTrees 1) trees in

     let f = lam rootDist.
       ( foldl addf 0.0
         (map (lam t. p_logObserve rootDist (getX t)) initTrees)
       , initTrees
       ) in
     match cluster [resRoot] (length initTrees) (p_cache (lam. lam. true) (p_mapWeight f rootDist))
     with (resamples, tree) in
     ( lam id. _chooseUniform
       (snoc resamples
         (lam id. for_ resamples (lam f. f id)))
       id
     , tree
     ) in

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

  printLn "\n tree_inference_dag";
  printJsonLn (JsonArray (_pvalGraph (setEmpty _cmpSym, []) p_run.1).0 .1);
  match timeF (lam. p_runInference p_run.0 p_run.1 1000000) with (p_time, p_res) in
  printLn (join [float2string p_time, "ms (PVal)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape p_res)));
  match timeF (lam. infer (LightweightMCMC {cps = "none", continue = (1000000, lam r. lam. (subi r 1, neqi r 0))}) run) with (time, res) in
  printLn (join [float2string time, "ms (MCMC lightweight)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res).0)));
  match timeF (lam. infer (LightweightMCMC {cps = "full", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeA, resA) in
  printLn (join [float2string timeA, "ms (MCMC lightweight full)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resA).0)));
  match timeF (lam. infer (LightweightMCMC {cps = "partial", align = true, continue = (1000000, lam r. lam. (subi r 1, neqi r 0))}) run) with (timeP, resP) in
  printLn (join [float2string timeP, "ms (MCMC lightweight partial)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples resP).0)));
  match timeF (lam. infer (Importance {particles = 1000000}) run) with (time2, res2) in
  printLn (join [float2string time2, "ms (IS lightweight)"]);
  printLn (hist2string (lam x. x) (histogram cmpString (map asShape (distEmpiricalSamples res2).0)));
  ()
in

()
