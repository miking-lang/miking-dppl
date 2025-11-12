include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"
-- Single observe --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical params)

let modelNP = lam. 
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical params)

let modelPN = lam. 
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical params)

let modelPP = lam. 
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params)

let modelPPInCorrect = lam. 
  let x = prune (Categorical [0.4, 0.6]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params)

let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPInCorrect)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))

-- Double observe on x --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params)

let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params)

let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params)

let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params)

let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Double observe on d --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical params);
  observe d (Categorical [0.9, 0.1])

let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])

let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical params);
  observe d (Categorical [0.9, 0.1])

let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])


let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Observe on both 'x' and 'd'
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5])  in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params);
  observe d (Categorical [0.9, 0.1])
let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])
let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params);
  observe d (Categorical [0.9, 0.1])
let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])
let modelPPSwap = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned d) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPSwap)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))

-- Pruning on single variable, single observe
let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  observe x (Categorical params)
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  observe (pruned x) (Categorical params) 
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Single variable, double observe
let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  observe x (Categorical params);
  observe x (Categorical [0.9, 0.1])
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let modelNN = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = assume (Categorical [0.7,0.3]) in
  let p = fun b in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = assume (Categorical [0.5,0.5]) in
  let p = fun a in
  cancel (observe b (Categorical [0.7,0.3]));
  observe b (Categorical p);
  observe e (Categorical p)
let modelNP = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = assume (Categorical [0.7,0.3]) in
  let p = fun b in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  cancel (observe b (Categorical [0.7,0.3]));
  observe b (Categorical p);
  observe e (Categorical p)
let modelPN = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = assume (Categorical [0.5,0.5]) in
  let p = fun a in
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe (pruned b) (Categorical p);
  observe e (Categorical p)
let modelPP = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe (pruned b) (Categorical p);
  observe e (Categorical p)
let modelPPSwap = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  observe (pruned b) (Categorical p);
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe e (Categorical p)
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPSwap)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))


let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  cancel (observe x (Categorical [0.9, 0.1]));
  cancel (observe x (Categorical [0.5, 0.5]));
  cancel (observe x (Categorical params))
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  cancel (observe (pruned x) (Categorical [0.9, 0.1]));
  cancel (observe (pruned x) (Categorical [0.5, 0.5]));
  cancel (observe (pruned x) (Categorical params))
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
   (observe x (Categorical params));
   (observe 1 (Categorical params));
   (observe 0 (Categorical params))
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
   (observe (pruned x) (Categorical params));
   (observe 1 (Categorical params));
   (observe 0 (Categorical params))
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  cancel (observe x (Categorical params));
  observe x (Categorical [0.9, 0.1])
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  cancel (observe (pruned x) (Categorical params));
  observe (pruned x) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- tree inference
type Tree
con Leaf : {age: Float, seq: [Int]} -> Tree

let a:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 3, 0, 0, 0, 0, 3, 1, 1, 3, 3, 2]} 
let b:Tree = Leaf {age=0.0, seq=[1, 1, 0, 2, 0, 1, 0, 0, 0, 3, 1, 0, 1, 1, 0]} 
let c:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 2, 1, 0, 0, 0, 2, 0, 3, 3, 0]}
let d:Tree = Leaf {age=0.0, seq=[0, 0, 1, 1, 0, 3, 0, 1, 0, 0, 2, 2, 3, 1, 0]}


let trees:[Tree] = [a,b,c,d]
let seqLength = 15
con Node : all a. {age: Float, seq: [Int], left: Tree, right: Tree} -> Tree
con NodeP : all a. {age: Float, seq: [PruneInt], left: Tree, right: Tree} -> Tree
let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never
let getAgeP = lam n. match n with NodeP r then r.age else match n with Leaf r then r.age else never

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
let clusterP = lam q. lam trees. lam maxAge. lam seqLen. lam n.
  if eqi n 1 then trees else
  let pairs = pickpair n in
  let leftChild = get trees pairs.0 in
  let rightChild = get trees pairs.1 in
  let children = [leftChild, rightChild] in

  let t = assume (Exponential 10.0) in
  let age = addf t maxAge in
  let qts = map (lam c. matrixExponential (matrixMulFloat (subf age (getAgeP c)) q)) children in

  let seq = iid (lam p. prune (Categorical p)) [0.25,0.25,0.25,0.25] seqLen in
  iteri (lam i. lam site:PruneInt.
    iter2 (lam child. lam qt.
      let p1 = ctmc (pruned site) qt in
      match child with NodeP n then
        let s:PruneInt = get n.seq i in 
        observe (pruned s) (Categorical p1);
        cancel (observe (pruned s) (Categorical [0.25,0.25,0.25,0.25]))
      else match child with Leaf l in
        let s = get l.seq i in
        (if lti s 4 then 
        observe s (Categorical p1);
        cancel (observe s (Categorical [0.25,0.25,0.25,0.25])) else ())
    ) children qts
  ) seq;
  resample;
  let parent = NodeP {age=age, seq=seq,left=leftChild, right=rightChild} in
  let min = mini pairs.0 pairs.1 in
  let max = maxi pairs.0 pairs.1 in
  let new_trees = join ([slice trees 0 min, slice trees (addi min 1) max, slice trees (addi max 1) n, [parent]]) in
  clusterP q new_trees age seqLen (subi n 1)
end
let modelTreeInferenceP = lam.
  let q = [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in
  let q = matrixCreate [4,4] q in
  iter (lam l. match l with Leaf l in iter (lam s. if eqi s 4 then () else weight (log 0.25)) l.seq) trees;
  clusterP q trees 0.0 seqLength (length trees);()

let treeNormConst = distEmpiricalNormConst (infer (Importance {particles = 10000, cps = "none",prune=true}) modelTreeInferenceP)
utest treeNormConst with -79.28202 using eqfApprox 1e-5
mexpr
()


