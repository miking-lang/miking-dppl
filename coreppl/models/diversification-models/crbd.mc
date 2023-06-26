include "phylo.mc"
include "tree-alcedinidae.mc"
include "math.mc"

mexpr

-- Priors
let lambda = assume (Gamma 1.0 1.0) in
let mu = assume (Gamma 1.0 0.5) in

recursive let survives = lam tBeg.
  let t = subf tBeg (assume (Exponential (addf lambda mu))) in
  if ltf t 0. then
    assume (Bernoulli rho)
  else
    if assume (Bernoulli (divf lambda (addf lambda mu))) then
      if survives t then
        true
      else
        survives t
    else
      false
in

recursive let walk = lam node. lam parentAge.
  let nodeAge = getAge node in
  recursive let simHiddenSpeciation = lam tBeg.
    let t = subf tBeg (assume (Exponential lambda)) in
    if gtf t nodeAge then
      if survives t then
        weight (negf inf)
      else
        weight (log 2.);
        simHiddenSpeciation t
    else ()
  in
  simHiddenSpeciation parentAge;
  observe 0 (Poisson (mulf mu (subf parentAge nodeAge)));
  match node with Node n then
    observe 0. (Exponential lambda);
    resample;
    walk n.left nodeAge;
    walk n.right nodeAge
  else match node with Leaf _ then
    observe true (Bernoulli rho);
    resample
  else never
in

let numLeaves = countLeaves tree in
weight (subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (logFactorial numLeaves));
match tree with Node root in
walk root.left root.age;
walk root.right root.age;
lambda
