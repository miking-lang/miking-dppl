------------------------------------------------
-- The Constant-Rate Birth-Death (CRBD) model --
------------------------------------------------

-- The prelude includes a few PPL helper functions
include "pplprelude.mc"

-- The tree.mc file defines the general tree structure
include "tree.mc"

-- The tree-instance.mc file includes the actual tree and the rho constant
include "tree-instance.mc"

mexpr

-- CRBD goes undetected, including iterations. Mutually recursive functions.
recursive
  let iter: Int -> Float -> Float -> Float -> Float -> Float -> Bool =
    lam n: Int.
    lam startTime: Float.
    lam branchLength: Float.
    lam lambda: Float.
    lam mu: Float.
    lam rho: Float.
      if eqi n 0 then
        true
      else
        let eventTime = assume (Uniform (subf startTime branchLength) startTime) in
        if crbdGoesUndetected eventTime lambda mu rho then
          iter (subi n 1) startTime branchLength lambda mu rho
        else
          false

  let crbdGoesUndetected: Float -> Float -> Float -> Float -> Bool =
    lam startTime: Float.
    lam lambda: Float.
    lam mu: Float.
    lam rho: Float.
      let duration = assume (Exponential mu) in
      let cond =
        -- `and` does not use short-circuiting: using `if` as below is more
        -- efficient
        if (gtf duration startTime) then
          (eqBool (assume (Bernoulli rho)) true)
        else false
      in
      if cond then
        false
      else
        let branchLength = if ltf duration startTime then duration else startTime in
        let n = assume (Poisson (mulf lambda branchLength)) in
        iter n startTime branchLength lambda mu rho
in

-- Simulation of branch
recursive
let simBranch: Int -> Float -> Float -> Float -> Float -> Float -> Float =
  lam n: Int.
  lam startTime: Float.
  lam stopTime: Float.
  lam lambda: Float.
  lam mu: Float.
  lam rho: Float.
    if eqi n 0 then 0.
    else
      let currentTime = assume (Uniform stopTime startTime) in
      if crbdGoesUndetected currentTime lambda mu rho then
        let v = simBranch (subi n 1) startTime stopTime lambda mu rho in
        addf v (log 2.)
      else
        negf inf
in

-- Simulating along the tree structure
recursive
let simTree: Tree -> Tree -> Float -> Float -> Float -> () =
  lam tree: Tree.
  lam parent: Tree.
  lam lambda: Float.
  lam mu: Float.
  lam rho: Float.
    let lnProb1 = mulf (negf mu) (subf (getAge parent) (getAge tree)) in
    let lnProb2 = match tree with Node _ then log lambda else log rho in

    let startTime = getAge parent in
    let stopTime = getAge tree in
    let n = assume (Poisson (mulf lambda (subf startTime stopTime))) in
    let lnProb3 = simBranch n startTime stopTime lambda mu rho in

    weight (addf lnProb1 (addf lnProb2 lnProb3));
    resample;

    match tree with Node { left = left, right = right } then
      simTree left tree lambda mu rho;
      simTree right tree lambda mu rho
    else ()
in

-- Priors
let lambda = assume (Gamma 1.0 1.0) in
let mu = assume (Gamma 1.0 0.5) in

-- Adjust for normalizing constant
let numLeaves = countLeaves tree in
let corrFactor =
  subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (lnFactorial numLeaves) in
weight corrFactor;
resample;

-- Start of the simulation along the two branches
(match tree with Node { left = left, right = right } then
  simTree left tree lambda mu rho;
  simTree right tree lambda mu rho
else ());

lambda

-- Returns the posterior for the lambda
