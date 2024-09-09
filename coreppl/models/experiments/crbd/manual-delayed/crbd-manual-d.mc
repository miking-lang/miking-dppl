------------------------------------------------
-- The Constant-Rate Birth-Death (CRBD) model --
------------------------------------------------

-- The prelude includes a few PPL helper functions
include "../common/pplprelude.mc"

-- The tree.mc file defines the general tree structure
include "../common/tree.mc"

-- The tree-instance.mc file includes the actual tree and the rho constant
include "../../data/tree-instance.mc"
include "ext/dist-ext.mc"
include "string.mc"
mexpr

-- CRBD goes undetected, including iterations. Mutually recursive functions.
recursive
  let iter=
    lam n: Int.
    lam startTime: Float.
    lam branchLength: Float.
    lam lambda.
    lam mu.
    lam rho: Float.
      if eqi n 0 then
        false
      else
        let eventTime = assume (Uniform (subf startTime branchLength) startTime) in
        if crbdGoesUndetected eventTime lambda mu rho then
          true
        else
          iter (subi n 1) startTime branchLength lambda mu rho

  --survives
  let crbdGoesUndetected =
    lam startTime: Float.
    lam lambda.
    lam mu.
    lam rho: Float.
    --let duration = assume (Exponential mu) in
      let duration = (lomaxSample (deref mu.0)  (divf 1. (deref mu.1)) ) in
      modref mu.0 (addf (deref mu.0) 1.);
      modref mu.1 (divf (deref mu.1) (addf 1. (mulf (deref mu.1) duration)));

      let cond =
        -- `and` does not use short-circuiting: using `if` as below is more
        -- efficient
        if (gtf duration startTime) then
          (eqBool (assume (Bernoulli (subf 1. rho))) true)
        else false
      in
      if cond then
        true
      else
        let branchLength = if ltf duration startTime then duration else startTime in
        let rhs = divf 1. (addf (mulf branchLength (deref lambda.1)) 1.) in
        let n =  (negativeBinomialSample (roundfi (deref lambda.0)) rhs) in
        
        modref lambda.0 (addf (deref lambda.0) (int2float n));
        modref lambda.1 (divf (deref lambda.1) (addf 1. (mulf (deref lambda.1) branchLength)));
        iter n startTime branchLength lambda mu rho
in

-- Simulation of branch
recursive
let simBranch=
  lam n: Int.
  lam startTime: Float.
  lam stopTime: Float.
  lam lambda.
  lam mu.
  lam rho: Float.
    if eqi n 0 then 0.
    else
      let currentTime = assume (Uniform stopTime startTime) in
      if crbdGoesUndetected currentTime lambda mu rho then
         negf inf
      else
        let v = simBranch (subi n 1) startTime stopTime lambda mu rho in
        addf v (log 2.)
       in

-- Simulating along the tree structure
recursive
let simTree=
  lam tree: Tree.
  lam parent: Tree.
  lam lambda.
  lam mu.
  lam rho: Float.
    let startTime = getAge parent in
    let stopTime = getAge tree in
    let branchLength = subf startTime stopTime in

    (match tree with Node _ then
      -- observe 0. (Exponential lambda)
      weight (lomaxLogPdf (deref lambda.0) (divf 1. (deref lambda.1)) 0.);
      modref lambda.0 (addf (deref lambda.0) 1.)
    else observe true (Bernoulli rho));

 -- let n = assume (Poisson (mulf lambda branchLength)) in
    let rhs = divf 1. (addf (mulf branchLength (deref lambda.1)) 1.) in
    let n =  (negativeBinomialSample (ceilfi (deref lambda.0)) rhs)  in
    modref lambda.0 (addf (deref lambda.0) (int2float n));
    modref lambda.1 (divf (deref lambda.1) (addf 1. (mulf (deref lambda.1) branchLength)));

   --  observe 0 (Poisson (mulf mu branchLength));
    let rhs = divf 1. (addf (mulf branchLength (deref mu.1)) 1.) in
    weight (negativeBinomialLogPmf (ceilfi (deref mu.0)) rhs 0);

    modref mu.0 (addf (deref mu.0) 0.0);
    modref mu.1 (divf (deref mu.1) (addf 1. (mulf (deref mu.1) branchLength)));


    let lnProb3 = simBranch n startTime stopTime lambda mu rho in
    weight lnProb3;
    (match tree with Node { left = left, right = right } then
      simTree left tree lambda mu rho;
      simTree right tree lambda mu rho
    else ())
in

-- Priors
let l_k = ref 1. in
let l_0 = ref 1. in
let lambda = (l_k, l_0) in

let mu_k = ref 1. in
let mu_0 = ref 0.5 in
let mu = (mu_k, mu_0) in

-- Adjust for normalizing constant
let numLeaves = countLeaves tree in
let corrFactor =
  subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (lnFactorial numLeaves) in
weight corrFactor;

-- Start of the simulation along the two branches
(match tree with Node { left = left, right = right } then
  simTree left tree lambda mu rho;
  simTree right tree lambda mu rho
else ());
()
--assume (Gamma (deref l_k) (deref l_0))
-- Returns the posterior for the lambda
