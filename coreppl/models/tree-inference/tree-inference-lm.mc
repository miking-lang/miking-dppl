include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"
include "common.mc"

con Node : {age: Float, seq: [Int], left: Tree, right: Tree} -> Tree
let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never 
let getLeafSeq = lam n. match n with Leaf r then r.seq else never
let getNodeSeq = lam n. match n with Node r then r.seq else never

let slice = lam seq. lam beg. lam mend.
    subsequence seq beg (subi mend beg)

let zip = lam x. lam y.
  mapi (lam i. lam x. (x, get y i)) x 

let matrixGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [row, col] 

let ctmc = lam i. lam qt:Tensor[Float]. --lam t:Float.
  [matrixGet i 0 qt,matrixGet i 1 qt] 

recursive
let pickpairH = lam i. lam p.
  let j = assume (Categorical p) in
  if eqi i j then pickpairH i p else j
end

let pickpair = lam n.
  let p = make n (divf 1. (int2float n)) in
  let i = assume (Categorical p) in
  let j = pickpairH i p in
  (i,j) 

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params 

recursive let for = lam i. lam n. lam f. 
  if geqi i n then ()
  else f i; for (addi i 1) n f
end

recursive
let cluster = lam q. lam trees. lam maxAge. lam seqLen.
  let n = length trees in
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in

  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in

  let seq = iid (lam p. assume (Categorical p)) [0.5,0.5] seqLen in
  
  let leftChildAge = getAge leftChild in
  let rightChildAge = getAge rightChild in
  let qtL = (matrixExponential (matrixMulFloat (subf age leftChildAge) q)) in
  let qtR = (matrixExponential (matrixMulFloat (subf age rightChildAge) q)) in
  iteri
  (lam i. lam site.
    let p1 = ctmc site qtL in
    (match leftChild with Node _ then
      let lc = get (getNodeSeq leftChild) i in
      observe lc (Categorical p1);
      cancel (observe lc (Categorical [0.5,0.5]))
    else 
      let lc = get (getLeafSeq leftChild) i in
      (if lti lc 2 then observe lc (Categorical p1)
        else ())
    )
  ) seq;
  resample;
  iteri
  (lam i. lam site.
    let p2 = ctmc site qtR in
    (match rightChild with Node _ then
      let rc = get (getNodeSeq rightChild) i in
      observe rc (Categorical p2);
      cancel (observe rc (Categorical [0.5,0.5] ))
    else 
      let rc = get (getLeafSeq rightChild) i in
      (if lti rc 2 then observe rc (Categorical p2)
        else ())
    )
  ) seq;
  resample;
  let parent = Node {age=age, seq=seq,left=leftChild, right=rightChild} in
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  cluster q new_trees age seqLen
end
let f81 = lam pi.
  let p0 = (get pi 0) in
  let p1 = (get pi 1) in
  let lst =  [negf p1,p1,p0,negf p0] in
  matrixCreate [2,2] lst

let model = lam.
  let pi = assume (Dirichlet ([1.0, 1.0])) in
  let q = f81 pi in
  cluster q trees 0.0 seqLength
