include "math.mc"
include "seq.mc"

lang PruneGraph
  syn PruneVar =
  | PruneRVar { dist: [Float]
              , likelihood: Ref ([Float])
              , states: [Int]
              , lastWeight: Ref Float
              , identity: Symbol
              }

  | PruneFVar { values:[[Float]]
              , input: PruneVar
              }

  syn Param =
  | SeqFParam [Float]
  | PruneFParam PruneVar

  syn Value =
  | PrunedValue PruneVar
  | IntValue Int

 sem getLikelihood =
  | PruneRVar v  -> deref v.likelihood

  sem getStates =
  | SeqFParam f -> range 0 (length f) 1
  | PruneFParam f -> match f with PruneFVar f in
      range 0 (length f.values) 1
end

lang PrunedSampling = PruneGraph

  sem initializePruneRVar:[Float] -> PruneVar
  sem initializePruneRVar =
  | d -> PruneRVar { dist = d
                   , likelihood = ref ((make (length d) 1.))
                   , states = range 0 (length d) 1
                   , lastWeight = ref 0.
                   , identity = gensym ()}

  sem initializePruneFVar f =
  | PruneRVar p -> PruneFVar {values=map f p.states, input = (PruneRVar p)}

  sem calculateObservedLH d =
  | PrunedValue (PruneRVar obs) -> deref obs.likelihood
  | IntValue obs ->
    match d with PruneFParam (PruneFVar v) in
    let states = getStates d in
    mapi (lam i. lam. if eqi obs i then 1. else 0.) states

   -- to calculate the likelihood of depending functions
  sem observePrune cancel value =
  | (SeqFParam p) & d ->
    let likelihood = calculateObservedLH d value in
    let obsLh = calculateObsLh cancel likelihood value d in
    match value with PrunedValue (PruneRVar obs) in
    modref obs.likelihood obsLh;
    let logw = log (foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip obsLh obs.dist)) in
    let lastWeight = deref obs.lastWeight in
    modref obs.lastWeight logw;
    subf logw lastWeight
  | (PruneFParam (PruneFVar v)) & d ->
    let likelihood = calculateObservedLH d value in -- likelihood of observed value
    let distMsg = calculateDistMsg cancel likelihood value d in -- L_{p,s} for a certain observe
    -- Note: Commented out assuming that the pruned value does not live afterwards in a mixed observe
    /-(match value with PrunedValue (PruneRVar obs) then
      (match v.input with PruneRVar p in
        (if eqsym obs.identity p.identity then () else modref obs.likelihood (calculateObsLh cancel likelihood value d)))
     else ());-/
    weightPrune distMsg value v.input

  sem calculateLogWeight distMsg value =
  | PruneRVar p ->
    -- multiply the messages to calculate final L_{p,s}
    let msgMul = map (lam m. mulf m.0 m.1) (zip (deref p.likelihood) distMsg) in
    modref p.likelihood (msgMul);
    -- calculate L_{p,s} P(p=s) L_p
    let w = foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip msgMul p.dist) in log w


  sem weightPrune distMsg value =
  | PruneRVar p ->
    let uw = (negf (deref p.lastWeight)) in
    let w = (calculateLogWeight distMsg value (PruneRVar p)) in
    -- Note: Commented out assuming that the pruned value does not live afterwards in a mixed observe
    let xw = match value with PrunedValue (PruneRVar obs) then (if eqsym obs.identity p.identity then 0. else let lw = (deref obs.lastWeight) in /-modref obs.lastWeight w;-/lw) else 0. in
    modref p.lastWeight w;
    subf (addf uw w) xw

  -- \sum_s L_{c,x}P(c=x|p=s) P(p=s)
  sem calculateObsLh cancel lh value =
  | (PruneFParam (PruneFVar v)) -> 
    match v.input with PruneRVar p in
    let op = if cancel then divf else mulf in
    match value with PrunedValue (PruneRVar obs) in
    mapi (lam i. lam l. foldl2 (lam acc. lam v. lam c. addf acc (op c.0 (mulf c.1 (mulf (get v i) l)))) 0. v.values (zip p.dist (deref p.likelihood))) lh
  | SeqFParam p -> if cancel then zipWith divf lh p else zipWith mulf lh p
 
  -- L_{p,s} = \sum_x L_{c,x}P(c=x|p=s)
  sem calculateDistMsg cancel lh value = 
  | (PruneFParam (PruneFVar v)) -> 
    let op = if cancel then divf else mulf in
    match v.input with PruneRVar p in
    let mapDifferent = lam lh. map (lam p. foldl (lam acc. lam x. addf acc (op x.0 x.1)) 0. (zip lh p)) v.values in
    match value with PrunedValue (PruneRVar obs) then 
      if eqsym obs.identity p.identity then mapi (lam i. lam p. op 1. (get p i)) v.values 
      else let lh = zipWith mulf obs.dist lh in mapDifferent lh
    else mapDifferent lh
end

let initializePruneRVar = lam d.
  use PrunedSampling in
  initializePruneRVar d

let initializePruneFVar = lam f. lam p.
  use PrunedSampling in
  initializePruneFVar f p

let observePrune = lam c. lam v. lam d.
  use PrunedSampling in
  observePrune c v d
