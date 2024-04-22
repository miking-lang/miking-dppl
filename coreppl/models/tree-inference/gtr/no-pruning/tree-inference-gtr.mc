include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

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
  [matrixGet i 0 qt,matrixGet i 1 qt,matrixGet i 2 qt,matrixGet i 3 qt] 

let pickpair = lam n.
  let p = make (subi n 1) (divf 1. (int2float (subi n 1))) in
  let i = assume (Categorical p) in
  let i = addi i 2 in
  let p = make (subi i 1) (divf 1. (int2float (subi i 1))) in
  let j = assume (Categorical p) in
  (subi i 1,j) 

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params 

recursive
let cluster = lam q. lam trees. lam maxAge. lam seqLen.
  let n = length trees in
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in

  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in

  let seq = iid (lam p. assume (Categorical p)) [0.25,0.25,0.25,0.25] seqLen in
  
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
      cancel (observe lc (Categorical [0.25,0.25,0.25,0.25]))
    else 
      let lc = get (getLeafSeq leftChild) i in
      (if lti lc 4 then observe lc (Categorical p1)
        else ())
    );
    let p2 = ctmc site qtR in
    (match rightChild with Node _ then
      let rc = get (getNodeSeq rightChild) i in
      observe rc (Categorical p2);
      cancel (observe rc (Categorical [0.25,0.25,0.25,0.25]))
    else 
      let rc = get (getLeafSeq rightChild) i in
      (if lti rc 4 then observe rc (Categorical p2)
        else ())
    )) seq;
  let parent = Node {age=age, seq=seq,left=leftChild, right=rightChild} in
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  cluster q new_trees age seqLen
end

let gtr = lam pi. lam ri. 
  let p1r0 = (mulf (get pi 1) (get ri 0)) in
  let p2r1 = (mulf (get pi 2) (get ri 1)) in
  let p3r2 = (mulf (get pi 3) (get ri 2)) in
  let m11 = addf (addf p1r0 p2r1) p3r2 in
  let scale1 = (mulf m11 (get pi 0)) in 
  let p0r0 = (mulf (get pi 0) (get ri 0)) in
  let p2r3 = (mulf (get pi 2) (get ri 3)) in
  let p3r4 = (mulf (get pi 3) (get ri 4)) in
  let m22 = addf (addf p0r0 p2r3) p3r4 in
  let scale2 = (mulf m22 (get pi 1)) in 
  let p0r1 = (mulf (get pi 0) (get ri 1)) in
  let p1r3 = (mulf (get pi 1) (get ri 3)) in
  let p3r5 = (mulf (get pi 3) (get ri 5)) in
  let m33 = addf (addf p0r1 p1r3) p3r5 in
  let scale3 = (mulf m33 (get pi 2)) in 
  let p0r2 = (mulf (get pi 0) (get ri 2)) in
  let p1r4 = (mulf (get pi 1) (get ri 4)) in
  let p2r5 = (mulf (get pi 2) (get ri 5)) in
  let m44 = addf (addf p0r2 p1r4) p2r5 in
  let scale4 = (mulf m44 (get pi 3)) in 
  let scale = foldl addf scale1 [scale2,scale3,scale4] in
  let lst = map (lam e. divf e scale) [ negf m11, p1r0, p2r1, p3r2, p0r0, negf m22, p2r3, p3r4, p0r1, p1r3, negf m33,p3r5,p0r2,p1r4,p2r5,negf m44] in
  matrixCreate [4,4] lst

let model = lam.
  let pi = assume (Dirichlet ([1.0, 1.0, 1.0, 1.0])) in
  let er = assume (Dirichlet [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]) in
  let q = gtr pi er in 
  cluster q trees 0.0 seqLength
