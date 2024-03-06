include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

mexpr
type Tree in
con Node : {age: Float, seq: [PruneInt], left: Tree, right: Tree} -> Tree in
con Leaf : {age: Float, seq: [Int]} -> Tree in

let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never in
let getLeafSeq = lam n. match n with Leaf r then r.seq else never
in
let getNodeSeq = lam n. match n with Node r then r.seq else never
in

let slice = lam seq. lam beg. lam mend.
    subsequence seq beg (subi mend beg) in

let zip = lam x. lam y.
  mapi (lam i. lam x. (x, get y i)) x in

let matrixGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [row, col] in

let ctmc = lam i:Int. lam q:Tensor[Float]. lam t:Float.
  let choices = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.]] in
  let state = rvecCreate 4 (get choices i) in
  let qt = (matrixExponential (matrixMulFloat t q)) in
  let p = matrixMul state qt in
  [matrixGet 0 0 p,matrixGet 0 1 p,matrixGet 0 2 p,matrixGet 0 3 p] in

recursive
let pickpairH = lam i. lam p.
  let j = assume (Categorical p) in
  if eqi i j then pickpairH i p else j
in
recursive
let pickpair = lam n.
  let p = make n (divf 1. (int2float n)) in
  let i = assume (Categorical p) in
  let j = pickpairH i p in
  (i,j) in

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params in


recursive let for = lam i. lam n. lam f. 
  if geqi i n then ()
  else f i; for (addi i 1) n f in

recursive let cluster = lam q. lam trees. lam maxAge. lam seqLen.
  let n = length trees in
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in
  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in

  let seq = iid (lam p. prune (Categorical p)) [0.25,0.25,0.25,0.25] seqLen in
  let parent = Node {age=age, seq=seq,left=leftChild, right=rightChild} in

  let leftChildAge = getAge leftChild in
  let rightChildAge = getAge rightChild in

  for 0 seqLen 
  (lam i.
    let site = get seq i in
    let p1 = ctmc (pruned site) q (subf age leftChildAge) in
    (match leftChild with Node _ then
      let lc = get (getNodeSeq leftChild) i in
      observe (pruned lc) (Categorical p1);
      cancel (observe (pruned lc) (Categorical [0.25,0.25,0.25,0.25] ))
    else 
      let lc = get (getLeafSeq leftChild) i in
      observe lc (Categorical p1));

    let p2 = ctmc (pruned site) q (subf age rightChildAge) in
    (match rightChild with Node _ then
      let rc = get (getNodeSeq rightChild) i in
      observe (pruned rc) (Categorical p2);
      cancel (observe (pruned rc) (Categorical [0.25,0.25,0.25,0.25] ))

    else 
      let rc = get (getLeafSeq rightChild) i in
      observe rc (Categorical p2); ())
  );
  
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  cluster q new_trees age seqLen

let model = lam.
  let q = [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in
  let q = matrixCreate [4,4] q in
  let a:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 3, 0, 0, 0, 0, 3, 1, 1, 3, 3, 2]} in
  let b:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 0, 1, 0, 0, 0, 3, 1, 0, 1, 1, 0]} in
  let c:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 2, 1, 0, 0, 0, 2, 0, 3, 3, 0]} in
  let d:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 3, 0, 1, 0, 0, 2, 2, 3, 1, 0]} in
  let trees:[Tree] = [a,b,c,d] in
  let tree = cluster q trees 0.0 15 in ()
in model ()

