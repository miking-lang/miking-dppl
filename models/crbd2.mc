
include "bool.mc"
include "math.mc"

mexpr

let log = lam x. x in
let exp = lam x. x in
let minusInfinity = 0. in

let weight = lam. () in
let assume = lam. 0. in
let uniform = lam a. lam b. 0. in
let poisson = lam. 0. in
let gamma = lam. lam. 0 in
let exponential = lam. 0 in
let bernoulli = lam. 0 in

type Tree in
con Node : {left : Tree, right : Tree, age : Float } -> Tree in
con Leaf : {age : Float} -> Tree in

let getAge = lam n. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never in

let tree = Node {left = Leaf {age = 0.}, right = Leaf {age = 0.}, age = 5.0} in

recursive
let countLeaves = lam tree.
  match tree with Node r then
    addf (countLeaves r.left) (countLeaves r.right)
  else 1.
in


recursive
let lnFactorial = lam n.
  if eqi n 1 then 0.
  else addf (log (int2float n)) (lnFactorial (subi n 1))
in


recursive
  let iter = lam n. lam startTime. lam branchLength. lam lambda. lam mu. lam rho.
    if eqi n 0 then
      true
    else
      let eventTime = assume (uniform (subf startTime branchLength) startTime) in
      if crbdGoesUndetected eventTime lambda mu rho then
        iter (subi n 1) startTime branchLength lambda mu rho
      else
        false

  let crbdGoesUndetected = lam startTime. lam lambda. lam mu. lam rho.
     let duration = assume (exponential mu) in
     if and (gtf duration startTime) (eqi (assume (bernoulli 0.5)) 1) then
       false
     else
       let branchLength = if ltf duration startTime then duration else startTime in
       let n = assume (poisson (mulf lambda branchLength)) in
       iter n startTime branchLength lambda mu rho
in


recursive
let simBranch = lam n. lam startTime. lam stopTime. lam lambda. lam mu. lam rho.
  if eqi n 0 then 0.
  else
    let currentTime = assume (uniform stopTime startTime) in
    if crbdGoesUndetected currentTime lambda mu rho then
      let v = simBranch (subf n 1) startTime stopTime lambda mu rho in
      addf v (log 2.)
    else
      negf inf
in


recursive
let simTree = lam tree. lam parent. lam lambda. lam mu. lam rho.
  let lnProb1 = mulf (negf mu) (subf (getAge parent) (getAge tree)) in
  let lnProb2 = match tree with Node _ then log lambda else log rho in

  let startTime = getAge parent in
  let stopTime = getAge tree in
  let n = assume (poisson (mulf lambda (subf startTime stopTime))) in
  let lnProb3 = simBranch n startTime stopTime lambda mu rho in

  weight (addf lnProb1 (addf lnProb2 lnProb3));

  match tree with Node _ then
    simTree tree.left tree lambda mu rho;
    simTree tree.right tree lambda mu rho
  else ()
in

let rho = 0.5684210526315789 in

let lambda = assume (gamma 1.0 1.0) in
let mu = assume (gamma 1.0 0.5) in


let numLeaves = countLeaves tree in
let corrFactor = subf (mulf (subf numLeaves 1.) (log 2.)) (lnFactorial numLeaves) in
weight corrFactor;


simTree tree.left tree lambda mu rho;
simTree tree.right tree lambda mu rho;

lambda
