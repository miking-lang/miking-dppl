include "phylo.mc"
include "math.mc"

let crbd: Tree -> Float -> Float = lam tree. lam rho.

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

  recursive let simHiddenSpeciation = lam nodeAge. lam tBeg.
    let t = subf tBeg (assume (Exponential lambda)) in
    if gtf t nodeAge then
      if survives t then
        -- NOTE: We bind weights and observes to wX (where X is an integer)
        -- just for identification purposes when testing the alignment
        -- analysis.
        let w1 = weight (negf inf) in
        w1
      else
        let w2 = weight (log 2.) in
        simHiddenSpeciation nodeAge t
    else ()
  in

  recursive let walk = lam node. lam parentAge.
    let nodeAge = getAge node in
    simHiddenSpeciation nodeAge parentAge;
    let w3 = observe 0 (Poisson (mulf mu (subf parentAge nodeAge))) in
    match node with Node n then
      let w4 = observe 0. (Exponential lambda) in
      walk n.left nodeAge;
      walk n.right nodeAge
    else match node with Leaf _ then
      let w5 = observe true (Bernoulli rho) in
      w5
    else never
  in

  let numLeaves = countLeaves tree in
  weight (subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (logFactorial numLeaves));
  match tree with Node root in
  walk root.left root.age;
  walk root.right root.age;
  lambda

