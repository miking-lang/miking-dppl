include "common.mc"
include "math.mc"
include "string.mc"

let inferModel = lam prior. lam.
  match prior with prior in
  let x = assume prior in
  observe true (Bernoulli x); resample;
  x

-- Repeatedly run inference on the prior until we get a 1 from randIntU.
recursive let loop : Dist Float -> Dist Float = lam prior.
  let posterior = infer (BPF {particles = 10}) (inferModel prior) in
  if eqi (randIntU 0 2) 1 then
    posterior
  else loop posterior
end

let printRes = lam printFun. lam dist.
  match distEmpiricalSamples dist with (samples, weights) in
  recursive let work = lam samples. lam weights.
    match (samples, weights) with ([s] ++ samples, [w] ++ weights) then
      print (printFun s);
      print " ";
      print (float2string w);
      print "\n";
      work samples weights
    else ()
  in work samples weights

let printDist = lam dist.
  (if distEmpiricalDegenerate dist then
    printLn "Empirical distribution has only negative infinity weights"
  else printRes float2string dist);
  printLn (create 20 (lam. '='))

mexpr

let dist = loop (Uniform 0.0 1.0) in
printDist dist
