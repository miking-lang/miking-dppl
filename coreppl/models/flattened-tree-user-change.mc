include "matrix.mc"
include "ext/matrix-ext.mc"
include "ext/dist-ext.mc"

mexpr
type Tree in
con Node : {left:Tree, right: Tree, age: Float} -> Tree in
con Leaf : {age: Float,seq:Int} -> Tree in

let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never
in
let getSeq = lam n. match n with Leaf r then r.seq else never
in
let getNodeSeq = lam n. match n with Node r then r.seq else never
in
let matrixGet = lam row. lam col. lam tensor. tensorGetExn tensor [row, col] in

let ctmc = lam i:Int. lam q:Tensor[Float]. lam t:Float. --lam i:Int. 
  let choices = [[1.,0.,0.,0.],[0.,1.,0.,0.],[0.,0.,1.,0.],[0.,0.,0.,1.]] in
  let state = rvecCreate 4 (get choices i) in
  let p = matrixMul state (matrixExponential (matrixMulFloat t q)) in
  [matrixGet 0 0 p,matrixGet 0 1 p,matrixGet 0 2 p,matrixGet 0 3 p] in

   let q =
  [negf 1., divf 1. 3., divf 1. 3., divf 1. 3.,
   divf 1. 3., negf 1., divf 1. 3., divf 1. 3.,
   divf 1. 3., divf 1. 3.,negf 1., divf 1. 3.,
   divf 1. 3., divf 1. 3., divf 1. 3., negf 1.] in

let q = matrixCreate [4,4] q in

let a = Leaf {age=0.0, seq=0} in --A
let b = Leaf {age=0.0, seq=2} in --G
let c = Leaf {age=0.0, seq=3} in --T

let maxAge = 0. in
-- first iteration
let t = assume (Exponential 10.) in
let age = addf maxAge t in

let d_seq = prune (Categorical [0.25,0.25,0.25,0.25]) in
let d = Node {age=age,left=a,right=b} in
let a_age = getAge a in
let p1 = ctmc (pruned d_seq) q (subf age a_age) in--in
observe (getSeq a) (Categorical p1);

let b_age = getAge b in
let p2 =  ctmc (pruned d_seq) q (subf age b_age)  in 
observe (getSeq b) (Categorical p2);

let maxAge = age in

--- second iteration ---
let t = assume (Exponential 10.) in
let age = addf maxAge t in

let e_seq = prune (Categorical [0.25,0.25,0.25,0.25]) in

let e = Node {age=age,left=d,right=c} in
-- what are the alternatives
-- if user rewriting helps, then user should
let d_age = getAge d in
let p1 = ctmc  (pruned e_seq)  q (subf age d_age) in

observe (pruned d_seq) (Categorical p1);
cancel (observe (pruned d_seq) (Categorical [0.25,0.25,0.25,0.25])
);
let c_age = getAge c in
let p2 = ctmc (pruned e_seq) q (subf age c_age)  in
observe (getSeq c) (Categorical p2)
