include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"
include "mexpr/ast-builder.mc"
include "digraph.mc"
include "mexpr/ast.mc"
lang PruneGraph
  -- Need the runtime structures to keep track of the dependencies as well as the likelihood, value etc.
  syn PruneVar =
  | PruneRVar { dist:DsDist Int
              , likelihood: Ref (Option [Float])
              , incomingMsgs:Ref [[Float]]
              , state:Int
              -- if sampled, prune children
              , value:Ref (Option Int)
              -- if next is set unweight and weight!
              , nexts:Ref ([(PruneVar)])
              }

  -- restrictions: cannot grow exponentially
  | PruneFVar {values:[[Float]]
              , input: Ref PruneVar
              --, next: Ref (Option (Vertex a b))
              }

  sem getIncomingMsgs =
  | PruneRVar v -> deref v.incomingMsgs
  sem addMsgToVertex msg =
  | PruneFVar v -> addMsgToVertex msg (deref v.input)
  | PruneRVar v -> modref v.incomingMsgs (cons msg (deref v.incomingMsgs))

  sem setLH lh =
  | PruneRVar v ->
    modref v.likelihood (Some lh)

  sem hasValue=
  | PruneRVar v -> match deref v.value with Some _ then true else false

  sem getStates =
  | PruneRVar v -> match v.dist with (DsDistCategorical d) in 
                   match d.p with SeqFParam f then range 0 (length f) 1
                   else match d.p with PruneFParam f then
                    match f with PruneFVar f in
                    range 0 (length f.values) 1
                   else never 
  | PruneFVar v -> range 0 (length v.values) 1

  syn DsDist a =
  | DsDistCategorical {p : Param}

  sem getParams =
  | DsDistCategorical d -> [d.p]

   syn Param =
  | SeqFParam [Float]
  | PruneFParam PruneVar
end

lang PrunedSampling = PruneGraph
  sem valuePS =
  | _ -> ()

  sem createPruneRVar =
  | d ->  PruneRVar {dist=d,
              likelihood= ref (None ()),
              incomingMsgs= ref [],
              value=ref (None ()),
              state=0,
              nexts=ref []} 

  sem createObsPruneRVar d =
  | value ->  let lh = calculateObservedLH value d in
              PruneRVar {dist=d,
              likelihood = ref (Some lh),
              incomingMsgs = ref [],
              value = ref (value),
              state = 0,
              nexts = ref []} 

  sem calculateObservedLH obs =
  | PruneRVar v -> v.likelihood 
  | DsDistCategorical v -> 
    match v.p with PruneFParam v then
      match v with PruneFVar v in
      let numStates = length (get v.values 0) in
      mapi (lam i. lam. if eqi obs i then 1. else 0.) (make numStates 0)
    else match v.p with SeqFParam v then
      let numStates = length v in
      mapi (lam i. lam. if eqi obs i then 1. else 0.) (make numStates 0) 
    else never


  sem zip x =
  | y -> mapi (lam i. lam e. (get x i,e)) y

  sem unweightPrune =
  | PruneRVar p -> match deref p.nexts with nexts in
                  if null nexts then () else
                  let w = calculateLogWeight (PruneRVar p) in
                  weight (negf w)

  sem calculateMsg transitionProb =
  | PruneRVar a ->  match deref a.likelihood with Some lh in
                    match transitionProb with PruneFVar v in
                    map (lam p. foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip p lh)) v.values

  sem calculateLogWeight =
  | PruneRVar p -> match deref p.incomingMsgs with msgs in
                   let msgMul = foldl (lam acc. lam m. map (lam m. mulf m.0 m.1) (zip m acc)) (head msgs) (tail msgs) in
                   modref p.likelihood (Some msgMul);
                   match p.dist with DsDistCategorical d  in
                   match d.p with SeqFParam f then
                     let w = foldl (lam acc. lam x. addf acc (mulf x.0 x.1)) 0. (zip msgMul f) in
                     log w
                   else never -- TODO what if PruneFParam assume in assume via another function? marginalized? :

  sem weightPrune child =
  | PruneRVar p -> modref p.nexts (cons child (deref p.nexts));
                   weight (calculateLogWeight (PruneRVar p))

  sem createPruneFVar input =
  | f -> PruneFVar {values=map f (getStates input),input=ref input} in

  sem observePrune seq =
  | (PruneFVar v)&t -> 
    let msg = calculateMsg t seq in --calculateMsg from a to d
    unweightPrune (deref v.input);
    addMsgToVertex msg t;
    weightPrune seq (deref v.input)
end

-- change PruneInt type to PruneVar
-- changeType function
 
type Tree 
con Node : {left:Tree, right: Tree, seq: use PruneGraph in PruneVar, age: Float} -> Tree
con Leaf: {age: Float, seq: Int} -> Tree
let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never
--!!! if I remove it this function becomes invalid as well
let getSeq = lam n. match n with Leaf r then r.seq else never
let getNodeSeq =  lam n. match n with Node r then r.seq else never 
let matrixGet = lam row. lam col. lam tensor. tensorGetExn tensor [row, col]

let ctmc = lam i. lam q:Tensor[Float]. lam t.
  let choices = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.]] in
  let state = rvecCreate 4 (get choices i) in
  let p = matrixMul state (matrixExponential (matrixMulFloat t q)) in
  [matrixGet 0 0 p,matrixGet 0 1 p,matrixGet 0 2 p,matrixGet 0 3 p]
mexpr
use PrunedSampling in
let q =
  [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in

let q = matrixCreate [4,4] q in
let a = Leaf {age=0.0, seq=0} in --A
let b = Leaf {age=0.0, seq=2} in --G
let c = Leaf {age=0.0, seq=3} in --T
--let trees = [a,b,c] in

let maxAge = 0. in
-- first iteration
let t = assume (Exponential 10.0) in
let age = addf maxAge t in

-- not to sample from Categorical variable 'd_seq'
-- create a PruneVar given p(d_seq)
let d_seq = createPruneRVar (DsDistCategorical ({p=SeqFParam [0.25,0.25,0.25,0.25]})) in
let d = Node {age=age,seq=d_seq,left=a,right=b} in

let a_age = getAge a in
let p1 = createPruneFVar d_seq (lam s. ctmc s q (subf age a_age)) in
-- replace the observe with obs prune only if dist depends on pruneFvar; otherwise keep it the same
-- so the parameter can only be PruneFVar
let a_seq = createObsPruneRVar (DsDistCategorical ({p=PruneFParam p1})) (getSeq a) in
observePrune a_seq p1;

let b_age = getAge b in
let p2 = PruneFVar {values=map (lam s. ctmc s q (subf age b_age)) (getStates d_seq),input=ref d_seq} in
let b_seq = createObsPruneRVar (DsDistCategorical ({p=PruneFParam p2})) (getSeq b) in
observePrune b_seq p2;

let maxAge = age in
--- second iteration
let t = assume (Exponential 10.0) in
let age = addf maxAge t in

let e_seq = createPruneRVar (DsDistCategorical ({p=SeqFParam [0.25,0.25,0.25,0.25]})) in
let e = Node {age=age,seq=e_seq,left=d,right=c} in
let d_age = getAge d in
let p1 = PruneFVar {values=map (lam s. ctmc s q (subf age d_age)) (getStates e_seq),input=ref e_seq} in 
let d_seqx = createObsPruneRVar (DsDistCategorical ({p=PruneFParam p1})) (getNodeSeq d) in
observePrune d_seqx p1;
--let d_seq_neg = createPruneRVar (DsDistCategorical ({p=SeqFParam [0.25,0.25,0.25,0.25]})) in
--what is double counted is different
--weightPrunex (negf (categoricalLogPmf [0.25,0.25,0.25,0.25] (getSeq d)));

let c_age = getAge c in
let p2 = PruneFVar {values=map (lam s. ctmc s q (subf age c_age)) (getStates e_seq),input=ref e_seq} in
let c_seq = createObsPruneRVar (DsDistCategorical ({p=PruneFParam p2})) (getSeq c) in
observePrune c_seq p2;

()




