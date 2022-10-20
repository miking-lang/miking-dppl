include "common.mc"
include "math.mc"
include "string.mc"

let inferModel = lam prior. lam.
  let x = assume prior in
  observe true (Bernoulli (exp x));
  x

-- Repeatedly run inference on the prior until we get a 1 from randIntU.
recursive let loop : Dist Float -> Dist Float = lam prior.
  let posterior = infer (BPF {particles = 10}) (inferModel prior) in
  if eqi (randIntU 0 2) 1 then
    posterior
  else loop posterior
end

let obsBernoulliTrue = lam x.
  observe true (Bernoulli x)

let model = lam alpha. lam beta.
  let x = assume (Beta alpha beta) in
  obsBernoulliTrue x;
  x

mexpr

let alpha = divf (int2float (randIntU 0 100)) 10.0 in
let beta = divf (int2float (randIntU 0 100)) 10.0 in

-- Run the model using each of the available runtimes
let p = randIntU 0 10 in
let dist = infer (Importance {particles = p}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = infer (BPF {particles = p}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = infer (APF {particles = p}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = infer (LightweightMCMC {iterations = p, aligned = false, globalProb = 0.5}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = infer (NaiveMCMC {iterations = p}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = infer (TraceMCMC {iterations = p}) (lam. model alpha beta) in
printRes float2string dist;
printLn (create 20 (lam. '='));

let dist = loop (Uniform 1.0 2.0) in
printRes float2string dist;
printLn (create 20 (lam. '='))
