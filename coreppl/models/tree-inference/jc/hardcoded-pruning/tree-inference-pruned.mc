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
    let log_likes_left = getLogLikes lc in
    weight (negf (log_likes_left));
    let rc = get rightMsg i in
    let right_in_msg  = map (lam p. let t = foldl2 (lam acc. lam pi. lam rci. addf acc (mulf pi rci)) 0. p rc in t) r_values in
    let log_likes_right = getLogLikes rc in
    weight (negf (log_likes_right));
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

let model = lam.
  let q = [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in
  let q = matrixCreate [4,4] q in
  let dataLen = length data in
  let trees:[Tree] = buildForest data [] 0 dataLen seqLength in
  weight (mulf (int2float (muli dataLen seqLength)) (log 0.25));
  cluster q trees 0.0 dataLen
