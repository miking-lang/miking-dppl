------------------------------------------------
-- The Constant-Rate Birth-Death (CRBD) model --
------------------------------------------------

-- The prelude includes a few PPL helper functions
include "../common/pplprelude.mc"

-- The tree.mc file defines the general tree structure
include "../common/tree.mc"

-- The tree-instance.mc file includes the actual tree and the rho constant
include "../../data/tree-instance.mc"

mexpr

-- CRBD goes undetected, including iterations. Mutually recursive functions.
recursive
  let iter =
    lam n: Int.
    lam startTime: Float.
    lam branchLength: Float.
    lam lambda: DelayFloat.
    lam mu: DelayFloat.
    lam rho: Float.
      if eqi n 0 then
        true
      else
        let eventTime = assume (Uniform (subf startTime branchLength) startTime) in
        if crbdGoesUndetected eventTime lambda mu rho then
          iter (subi n 1) startTime branchLength lambda mu rho
        else
          false

  let crbdGoesUndetected=
    lam startTime: Float.
    lam lambda: DelayFloat.
    lam mu: DelayFloat.
    lam rho: Float.
      let x = delayed mu in
      let duration = assume (Exponential x) in
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
        let l = delayed lambda in
        let n = assume (Poisson (mulf l branchLength)) in
        iter n startTime branchLength lambda mu rho
in

-- Simulation of branch
recursive
let simBranch =
  lam n: Int.
  lam startTime: Float.
  lam stopTime: Float.
  lam lambda: DelayFloat.
  lam mu: DelayFloat.
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
let simTree =
  lam tree: Tree.
  lam parent: Tree.
  lam lambda: DelayFloat.
  lam mu: DelayFloat.
  lam rho: Float.
   -- let lnProb1 = mulf (negf mu) (subf (getAge parent) (getAge tree)) in
    (match tree with Node _ then let lambd = delayed lambda in observe 0. (Exponential lambd) else observe true (Bernoulli rho) );
    let m = delayed mu in
    observe 0 (Poisson (mulf m (subf (getAge parent) (getAge tree))));
    let startTime = getAge parent in
    let stopTime = getAge tree in
    let l =  delayed lambda in
    let n = assume (Poisson (mulf l (subf startTime stopTime))) in
    let lnProb3 = simBranch n startTime stopTime lambda mu rho in

    weight lnProb3;
    resample;

    match tree with Node { left = left, right = right } then
      simTree left tree lambda mu rho;
      simTree right tree lambda mu rho
    else ()
in

-- Priors
let lambda = delay (Gamma 1.0 1.0) in
let mu = delay (Gamma 1.0 1.0) in

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
delayed lambda

-- Returns the posterior for the lambda
