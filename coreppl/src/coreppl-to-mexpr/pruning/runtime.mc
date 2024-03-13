include "mexpr/ast.mc"
include "../runtime-dists.mc"

lang PruneGraph = MExprAst + RuntimeDistElementary
  syn PruneVar =
  | PruneRVar { dist: [Float]
              , likelihood: Ref ([Float])
              , incomingMessages: Ref [([Float],Bool)]
              , states: [Int]
              , lastWeight: Ref Float
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

  sem getIncomingMsgs: PruneVar -> [([Float],Bool)]
  sem getIncomingMsgs =
  | PruneRVar v -> deref v.incomingMessages

  sem addMsgToPruneVar: ([Float],Bool) -> PruneVar -> ()
  sem addMsgToPruneVar msg =
  | PruneRVar v -> modref v.incomingMessages (cons msg (deref v.incomingMessages))

 sem getLikelihood =
  | PruneRVar v  -> deref v.likelihood

  sem getStates =
  | SeqFParam f -> range 0 (length f) 1
  | PruneFParam f -> match f with PruneFVar f in
      range 0 (length f.values) 1
  sem zip x =
  | y -> mapi (lam i. lam e. (get x i, e)) y

end

lang PrunedSampling = PruneGraph

  sem createPruneRVar:[Float] -> PruneVar
  sem createPruneRVar =
  | d -> PruneRVar { dist = d
                   , likelihood = ref ((make (length d) 1.))
                   , incomingMessages = ref []
                   , states = range 0 (length d) 1
                   , lastWeight = ref 0.}

  sem createPruneFVar f =
  | PruneRVar p -> PruneFVar {values=map f p.states, input = ref (PruneRVar p)}

  sem calculateObservedLH d =
  | PrunedValue (PruneRVar obs) -> deref obs.likelihood
  | IntValue obs ->
    match d with PruneFParam (PruneFVar v) in
    let states = getStates d in
    mapi (lam i. lam. if eqi obs i then 1. else 0.) states

   -- to calculate the likelihood of depending functions
  sem observePrune cancel likelihood =
  | SeqFParam p ->
    let w = foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip p likelihood) in
    if cancel then negf (log w) else log w
  | PruneFParam (PruneFVar v) ->
    let msg = calculateMsg likelihood (PruneFVar v) in
    let input = (deref v.input) in
    let unw = unweightPrune input in
    addMsgToPruneVar (msg,cancel) input;
    let w = weightPrune input in
    addf unw w

  sem calculateLogWeight =
  | PruneRVar p -> match deref p.incomingMessages with msgs in
    let acc = make (length (head msgs).0) 1. in
    let msgMul = foldl (lam acc. lam m.
      if m.1 then map (lam m. divf m.0 m.1) (zip acc m.0)
      else map (lam m. mulf m.0 m.1) (zip acc m.0)
      ) acc msgs in
    modref p.likelihood (msgMul);
    let w = foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip msgMul p.dist) in log w

  sem weightPrune =
  | PruneRVar p ->
    let w = (calculateLogWeight (PruneRVar p)) in
    modref p.lastWeight w; w

  sem unweightPrune =
  | PruneRVar p -> negf (deref p.lastWeight)

  -- [p(d|e=0), p(d|e=1), p(d|e=2),p(d|e=1)]
  sem calculateMsg: [Float] -> PruneVar -> [Float]
  sem calculateMsg lh =
  | PruneFVar v ->  map (lam p. foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip p lh)) v.values

end

let createPruneRVar = lam d.
  use PrunedSampling in
  createPruneRVar d

let createPruneFVar = lam f. lam p.
  use PrunedSampling in
  createPruneFVar f p

let calculateObservedLH = lam d. lam v.
  use PrunedSampling in
  calculateObservedLH d v

let observePrune = lam c. lam l. lam d.
  use PrunedSampling in
  observePrune c l d






