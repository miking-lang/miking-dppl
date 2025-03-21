include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

type Tree
con Node : {age: Float, msg: [[Float]], left: Tree, right: Tree} -> Tree
con Leaf : {age: Float, msg: [[Float]]} -> Tree

let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never
let getMsg = lam n. match n with Leaf r then r.msg else match n with Node r then r.msg else never

let slice = lam seq. lam beg. lam mend.
    subsequence seq beg (subi mend beg)

let matrixGet = lam row. lam col. lam tensor.
  tensorGetExn tensor [row, col]

let pickpair = lam n.
  let i = assume (UniformDiscrete 0 (subi n 1)) in
  let j = assume (UniformDiscrete 0 (subi n 2)) in
  if lti j i then (i,j) else (i,addi j 1)

let iid = lam f. lam p. lam n.
  let params = make n p in
  map f params

let getLeafMessage = lam seq:Int.
  if eqi seq 0 then [1.0, 0.0, 0.0, 0.0]
  else if eqi seq 1 then [0.0, 1.0, 0.0, 0.0]
  else if eqi seq 2 then [0.0, 0.0, 1.0, 0.0]
  else if eqi seq 3 then [0.0, 0.0, 0.0, 1.0]
  else if eqi seq 4 then [1.0, 1.0, 1.0, 1.0]
  else error "Invalid state at leaf"

let getLogLikes = lam msg.
  let like = foldl (lam acc. lam x. addf acc (mulf x 0.25)) 0. msg in
  log like

let sapply = lam x. lam f.
  map f x

let ctmc = lam i. lam qt:Tensor[Float].
  [matrixGet i 0 qt,matrixGet i 1 qt,matrixGet i 2 qt,matrixGet i 3 qt]

recursive
let buildForest =  lam data. lam forest:[Tree]. lam index. lam data_len. lam seq_len.
  foldl (lam forest. lam data.
      let newMessage = sapply data getLeafMessage in
      let newLeaf = Leaf {age=0.0,msg = newMessage} in
      let newForest = join ([forest,[newLeaf]]) in
      newForest
    ) [] data 
end

recursive
let cluster = lam q. lam trees:[Tree]. lam maxAge. lam n.
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in

  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in

  let leftChildAge = getAge leftChild in
  let rightChildAge = getAge rightChild in
  
  let qtL = (matrixExponential (matrixMulFloat (subf age leftChildAge) q)) in
  let qtR = (matrixExponential (matrixMulFloat (subf age rightChildAge) q)) in

  let leftMsg = getMsg leftChild in
  let rightMsg = getMsg rightChild in

  let l_values:[[Float]] = map (lam i. ctmc i qtL) [0,1,2,3] in
  let r_values:[[Float]] = map (lam i. ctmc i qtR) [0,1,2,3] in

  let node_msg = mapi (lam i. lam lc.
    let left_in_msg = map (lam p. let t = foldl2 (lam acc. lam pi. lam lci. addf acc (mulf pi lci)) 0. p lc in t) l_values in
    (match leftChild with Node _ then
      let log_likes_left = getLogLikes lc in
      weight (negf (log_likes_left))
    else ());
    
    let rc = get rightMsg i in
     let right_in_msg  = map (lam p. let t = foldl2 (lam acc. lam pi. lam rci. addf acc (mulf pi rci)) 0. p rc in t) r_values in
    (match rightChild with Node _ then
      let log_likes_right = getLogLikes rc in
      weight (negf (log_likes_right))
    else ()); 
     let node_msg = mapi (lam i. lam lm. let rm = get right_in_msg i in mulf lm rm) left_in_msg in
     let log_likes = getLogLikes node_msg in
     weight (log_likes);
     node_msg
  ) leftMsg in
  resample;
  let parent = Node {age=age, msg = node_msg,left = leftChild, right = rightChild} in
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  cluster q new_trees age (subi n 1)
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
  let dataLen = length data in
  let trees:[Tree] = buildForest data [] 0 dataLen seqLength in
  cluster q trees 0.0 (length trees)
