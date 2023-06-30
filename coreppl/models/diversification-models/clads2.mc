include "phylo.mc"
include "tree-alcedinidae.mc"
include "math.mc"
include "bool.mc"

mexpr

-- Priors
let lambda = assume (Gamma 1.0 1.0) in
let mu = assume (Gamma 1.0 0.5) in
let sigma = sqrt (divf 0.2 (assume (Gamma 1.0 1.0))) in
let log_alpha = assume (Gaussian 0. sigma) in

recursive let survives = lam tBeg. lam multiplier.
  let multiplier = mulf multiplier (exp (assume (Gaussian log_alpha sigma))) in
  if or (ltf multiplier 1e-5) (gtf multiplier 1e5) then
    true
  else
    let t = subf tBeg (assume (Exponential (mulf multiplier (addf lambda mu)))) in
    if ltf t 0. then
      assume (Bernoulli rho)
    else
      if assume (Bernoulli (divf lambda (addf lambda mu))) then
        -- Speciation
        if survives t multiplier then
          true
        else
          survives t multiplier
      else
        -- Extinction
        false
in

recursive let walk = lam node. lam parentAge. lam multiplier.
  let nodeAge = getAge node in
  recursive let simHiddenSpeciation = lam tBeg. lam multiplier.
    let multiplier = mulf multiplier (exp (assume (Gaussian log_alpha sigma))) in
    if or (ltf multiplier 1e-5) (gtf multiplier 1e5) then
      weight (negf inf);
      multiplier
    else
      let t = subf tBeg (assume (Exponential (mulf multiplier lambda))) in
      if gtf t nodeAge then
        if survives t multiplier then
          weight (negf inf);
          multiplier
        else
          observe 0 (Poisson (mulf (mulf mu multiplier) (subf tBeg t)));
          weight (log 2.);
          simHiddenSpeciation t multiplier
      else
        observe 0 (Poisson (mulf (mulf mu multiplier) (subf tBeg nodeAge)));
        multiplier
  in
  let multiplier = simHiddenSpeciation parentAge multiplier in
  match node with Node n then
    observe 0. (Exponential (mulf multiplier lambda));
    walk n.left nodeAge multiplier;
    walk n.right nodeAge multiplier
  else match node with Leaf _ then
    observe true (Bernoulli rho);
  else never
in

let numLeaves = countLeaves tree in
weight (subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (logFactorial numLeaves));
match tree with Node root in
walk root.left root.age 1.;
walk root.right root.age 1.;
lambda
