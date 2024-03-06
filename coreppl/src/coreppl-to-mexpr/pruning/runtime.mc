include "mexpr/ast.mc"
include "../runtime-dists.mc"

lang PruneGraph = MExprAst + RuntimeDistElementary
  syn PruneVar =
  | PruneRVar { dist: PDist Int
              , likelihood: Ref (Option [Float])
              , incomingMessages: Ref [[Float]]
              , value: Ref (Option Value)
              , marginalizedDist: Ref (Option (PDist Int))
              , states: [Int]
              , state: Ref Int
              , nexts: Ref ([(PruneVar)])
              }

  | PruneFVar { values:[[Float]]
              , input:Ref PruneVar
              }

  syn Param =
  | SeqFParam [Float]
  | PruneFParam PruneVar

  syn Value =
  | PrunedValue PruneVar
  | IntValue Int

  syn PDist a =
  | PCategorical {p : Param}

  sem getIncomingMsgs: PruneVar -> [[Float]]
  sem getIncomingMsgs =
  | PruneRVar v -> deref v.incomingMessages

  sem addMsgToPruneVar: [Float] -> PruneVar -> ()
  sem addMsgToPruneVar msg =
  | PruneFVar v -> addMsgToPruneVar msg (deref v.input)
  | PruneRVar v -> modref v.incomingMessages (cons msg (deref v.incomingMessages))

  sem getValue =
  | PruneRVar v  -> match deref v.value with Some v then v else error "has no value"
  
 sem getLikelihood =
  | PruneRVar v  -> match deref v.likelihood with Some v then v else error "has no lh"

  sem getStates =
  | (PCategorical d) ->
    switch d.p
    case SeqFParam f then range 0 (length f) 1
    case PruneFParam f then
      match f with PruneFVar f in
      range 0 (length f.values) 1
    case _ then
      error "can only be Param type"
    end

  sem zip x =
  | y -> mapi (lam i. lam e. (get x i, e)) y
/-
  sem getParams =
  | DsDistCategorical d -> [d.p]

  sem getParents =
  | t -> let params = getParams t in
    foldl (lam acc. lam p. match getParentsH p with Some v then cons v acc else acc) [] (reverse params)-/

end

lang PrunedSampling = PruneGraph

  sem createPruneRVar =
  | d -> PruneRVar { dist=d
                   , likelihood = calculatePruneLikelihood d --ref (None ())
                   , incomingMessages = ref []
                   , marginalizedDist = ref (None ())
                   , value = ref (None ())
                   , state = ref 0
                   , states = getStates d
                   , nexts = ref []}

  sem calculatePruneLikelihood =
  | PCategorical {p=SeqFParam p} -> ref (Some p)

  sem createPruneFVar f =
  | PruneRVar p -> PruneFVar {values=map f p.states, input = ref (PruneRVar p)}

  sem createObsPruneRVar d =
  | (PrunedValue (PruneRVar v))&val ->
              PruneRVar { dist = d
              , likelihood = v.likelihood
              , incomingMessages = ref []
              , marginalizedDist = ref (None ())
              , value = ref (Some val)
              , state = ref 0
              , states = getStates d
              , nexts = ref []
              }
  | (IntValue v)&val ->
    let lh = calculateObservedLH v d in
    PruneRVar { dist = d
              , likelihood = ref (Some lh)
              , incomingMessages = ref []
              , marginalizedDist = ref (None ())
              , value = ref (Some val)
              , state = ref 0
              , states = getStates d
              , nexts = ref []
              }

  -- o is always a PruneRVar because when there is an observe create that
   -- dist being a pruned or float differs only while calculating the message, value does not affect it because we use likelihood for the value, if that is a int then we convert it to a message too e.g. 0 -> [1.,0.,0.,0]
  sem observePrune  =
  | (PruneRVar r)&o ->
    match r.dist with PCategorical d  in
    let lh = getLikelihood o in
    switch d.p
    case PruneFParam (PruneFVar v) then -- observe 0. p1 or observe (pruned ) p1
      let msg = calculateMsgPrunedParam lh (PruneFVar v) in
      let unw = unweightPrune  false (deref v.input) in
      addMsgToPruneVar msg (PruneFVar v);
      let w = weightPrune  o false (deref v.input) in
      addf unw w
    case SeqFParam p then -- observe 0. (Categorical [0.25,0.25,..]) -- observe (pruned ) (Categorical [0.25,..])
      let msg = calculateMsgSeqParam lh p in
      log msg
    end

  sem cancelObservePrune  =
  | (PruneRVar r)&o ->
    match r.dist with PCategorical d  in
    let lh = getLikelihood o in
    switch d.p
    case PruneFParam (PruneFVar v) then -- observe 0. p1 or observe (pruned ) p1
      let msg = calculateMsgPrunedParam lh (PruneFVar v) in
      let unw = unweightPrune true (deref v.input) in
      addMsgToPruneVar msg (PruneFVar v);
      let w = weightPrune  o true (deref v.input) in
      addf unw w
    case SeqFParam p then -- observe 0. (Categorical [0.25,0.25,..]) -- observe (pruned ) (Categorical [0.25,..])
      let msg = calculateMsgSeqParam lh p in
      negf (log msg)
    end

  sem calculateLogWeight =
  | PruneRVar p -> match deref p.incomingMessages with msgs in
    let msgMul = foldl (lam acc. lam m. map (lam m. mulf m.0 m.1) (zip m acc)) (head msgs) (tail msgs) in
    modref p.likelihood (Some msgMul);
    match p.dist with PCategorical d in
    match d.p with SeqFParam f then
      let w = foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip msgMul f) in log w
    else never -- TODO what if PruneFParam assume in assume? marginalized dist?

  sem weightPrune o n =
  | PruneRVar p ->
    modref p.nexts (cons o (deref p.nexts));
    if n then (negf (calculateLogWeight (PruneRVar p)))
    else (calculateLogWeight (PruneRVar p))

  sem unweightPrune  n =
  | PruneRVar p ->
    match deref p.nexts with nexts in
    if null nexts then 0. else
    let w = (calculateLogWeight (PruneRVar p)) in
    if n then w
    else (negf w)

  -- [p(d|e=0), p(d|e=1), p(d|e=2),p(d|e=1)]
  sem calculateMsgPrunedParam: [Float] -> PruneVar -> [Float]
  sem calculateMsgPrunedParam lh =
  | PruneFVar v ->  map (lam p. foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip p lh)) v.values
    

  -- observe (pruned ) (Categorical [0.25,..])
  -- let d_seq_o = createObserve (pruned ) (Categorical [0.25,..]) the parent does not have many states
  -- p(d)
  sem calculateMsgSeqParam: [Float] -> [Float] -> Float
  sem calculateMsgSeqParam lh =
  | p -> foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip p lh)
    

  -- observe 0 (Categorical p1)
  sem calculateObservedLH obs =
  | PCategorical d ->
    switch d.p
    case PruneFParam v then
      match v with PruneFVar v in
      let states = getStates (PCategorical d) in
      mapi (lam i. lam. if eqi obs i then 1. else 0.) states
    case SeqFParam v then error "this should have been checked before"
    end
end

let createPruneRVar = lam d.
  use PrunedSampling in
  createPruneRVar d

let createPruneFVar = lam f. lam p.
  use PrunedSampling in
  createPruneFVar f p

let createObsPruneRVar = lam d. lam v.
  use PrunedSampling in
  createObsPruneRVar d v

let observePrune = lam o.
  use PrunedSampling in
  observePrune o

let cancelObservePrune = lam o.
  use PrunedSampling in
  cancelObservePrune o


