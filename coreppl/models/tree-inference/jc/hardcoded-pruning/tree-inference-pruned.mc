include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

con Node : {age: Float, msg: [[Float]], left: Tree, right: Tree} -> Tree
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
let buildForest = lam data. lam forest:[Tree]. lam index. lam data_len. lam seq_len.
  foldl (lam forest. lam data.
      let newMessage = sapply data getLeafMessage in
      let newLeaf = Leaf {age=0.0,msg = newMessage} in
      let newForest = join ([forest,[newLeaf]]) in
      newForest
    ) [] data 
end

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

  let node_msg = mapi (lam i. lam site.
    let childMsgs = zipWith (lam child. lam qt.
      let msg = get (getMsg child) i in
      let p1 = map (lam i. ctmc i qt) [0,1,2,3] in
      let in_msg = map (lam p. let t = foldl2 (lam acc. lam pi. lam lci. addf acc (mulf pi lci)) 0. p msg in t) p1 in
      let log_likes = getLogLikes msg in
      weight (negf (log_likes)); in_msg
    ) children qts in
    let node_msg = foldl (lam acc. lam m. zipWith (lam lm. lam rm. mulf lm rm) acc m) (head childMsgs) (tail childMsgs) in
    let log_likes = getLogLikes node_msg in
    weight (log_likes);
    node_msg
  ) (make seqLen 0) in
  resample;
  let parent = Node {age=age, msg = node_msg,left = leftChild, right = rightChild} in
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
  let trees:[Tree] = buildForest data [] 0 (length data) seqLen in
  iter (lam l. iter (lam s. if eqi s 4 then () else weight (log 0.25)) l) data;
  cluster q trees 0.0 seqLen (length trees)