include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

con Node : {age: Float, seq: [PruneInt], left: Tree, right: Tree} -> Tree
let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never

let slice = lam seq. lam beg. lam mend.
    subsequence seq beg (subi mend beg)

let matrixGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [row, col]

let ctmc = lam i. lam qt:Tensor[Float].
  [matrixGet i 0 qt,matrixGet i 1 qt,matrixGet i 2 qt,matrixGet i 3 qt]

let pickpair = lam n.
  let i = assume (UniformDiscrete 0 (subi n 1)) in
  let j = assume (UniformDiscrete 0 (subi n 2)) in
  if lti j i then (i,j) else (i,addi j 1)

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params

recursive
let cluster = lam q. lam trees. lam maxAge. lam seqLen. lam n.
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in
  let children = [leftChild, rightChild] in

  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in
  let qts = map (lam c. matrixExponential (matrixMulFloat (subf age (getAge c)) q)) children in

  let seq:[PruneInt] = iid (lam p. prune (Categorical p)) [0.25,0.25,0.25,0.25] seqLen in
  iteri (lam i. lam site:PruneInt.
    iter2 (lam child. lam qt.
      let p1 = ctmc (pruned site) qt in
      match child with Node n then
        let s = get n.seq i in
        observe (pruned s) (Categorical p1);
        cancelObserve (pruned s) (Categorical [0.25,0.25,0.25,0.25])
      else match child with Leaf l in
        let s = get l.seq i in
        (if lti s 4 then observe s (Categorical p1) else ());
        cancelObserve s (Categorical [0.25,0.25,0.25,0.25])
    ) children qts
  ) seq;
  resample;
  let parent = Node {age=age, seq=seq,left=leftChild, right=rightChild} in
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  cluster q new_trees age seqLen (subi n 1)
end

let model = lam.
  let q = [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in
  let q = matrixCreate [4,4] q in
  weight (mulf (int2float (muli (length trees) seqLength)) (log 0.25));
  cluster q trees 0.0 seqLength (length trees)
